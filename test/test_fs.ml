open Wand

let run s = Runner.run_string s

let ok label input expected =
  Alcotest.(check (result string string)) label (Ok expected) (run input)

let contains haystack needle =
  let hn = String.length haystack and nn = String.length needle in
  let rec go i = i + nn <= hn && (String.sub haystack i nn = needle || go (i + 1)) in
  go 0

(* Trust anchor for test/wand/test_fs.wand: verifies FS.write_file/
   read_file round-trip against a real, OCaml-managed temp file. Every
   other FS.wand fixture test builds on this to create/read its own
   scratch files without needing OCaml-side scaffolding. *)
let test_read_write_round_trip () =
  let tmp = Filename.temp_file "wand_test_" ".txt" in
  let src = Printf.sprintf
    {|import FS
import Path
let () = FS.write_file! (Path.of_string "%s") "hello world"
FS.read_file! (Path.of_string "%s")|} tmp tmp in
  (try ok "write_file then read_file round-trips" src "hello world"
   with e -> (try Sys.remove tmp with _ -> ()); raise e);
  (try Sys.remove tmp with _ -> ())


(* ── The tree a glob walks ─────────────────────────────────────────────── *)

(* `FS.glob_in pat dir` answers with what is under `dir`. A symlink is an
   entry like any other -- it can match, and comes back as itself -- but
   walking through one leaves the directory the caller named: a link to
   /etc had a glob over ./data answering with files ./data does not
   contain, and a link back to an ancestor sent the walk round in a circle
   until the path outgrew what the system would take. *)

let rec rm_rf path =
  match Unix.lstat path with
  | { Unix.st_kind = Unix.S_DIR; _ } ->
    Array.iter (fun e -> rm_rf (Filename.concat path e)) (Sys.readdir path);
    Unix.rmdir path
  | _ -> Sys.remove path
  | exception Unix.Unix_error _ -> ()

let with_tree f =
  let root = Filename.temp_file "wand_glob_" "" in
  Sys.remove root;
  Unix.mkdir root 0o755;
  Fun.protect ~finally:(fun () -> rm_rf root) (fun () -> f root)

let write path text =
  Out_channel.with_open_text path (fun oc -> Out_channel.output_string oc text)

let glob_in pat dir =
  let src = Printf.sprintf
    {|import FS
import List
import Path
List.map Path.to_string (FS.glob_in %s (Path.of_string "%s"))|} pat dir
  in
  match run src with
  | Ok v -> v
  | Error m -> Alcotest.failf "glob failed: %s" m

let test_a_glob_stays_under_its_directory () =
  with_tree (fun root ->
    let base = Filename.concat root "data" in
    let outside = Filename.concat root "secret" in
    Unix.mkdir base 0o755;
    Unix.mkdir outside 0o755;
    write (Filename.concat base "a.conf") "x";
    write (Filename.concat outside "hidden.conf") "x";
    Unix.symlink outside (Filename.concat base "link");
    let out = glob_in "**.conf" base in
    if contains out "hidden.conf" then
      Alcotest.failf "the glob followed a link out of its directory: %s" out;
    if not (contains out "a.conf") then
      Alcotest.failf "the glob missed a real file: %s" out)

let test_a_glob_answers_with_a_matching_link () =
  with_tree (fun root ->
    let base = Filename.concat root "data" in
    Unix.mkdir base 0o755;
    write (Filename.concat root "target.conf") "x";
    Unix.symlink (Filename.concat root "target.conf")
      (Filename.concat base "via-link.conf");
    let out = glob_in "**.conf" base in
    if not (contains out "via-link.conf") then
      Alcotest.failf "a link that matches is still an answer: %s" out)

(* A link to an ancestor: the walk must end, and end with the real files. *)
let test_a_glob_does_not_circle () =
  with_tree (fun root ->
    let base = Filename.concat root "data" in
    Unix.mkdir base 0o755;
    write (Filename.concat base "a.conf") "x";
    Unix.symlink root (Filename.concat base "up");
    let out = glob_in "**.conf" base in
    if contains out "up/" then
      Alcotest.failf "the walk went through the link: %s" out)

(* ── The permissions a file is created with ────────────────────────────── *)

(* `FS.write_file` took the channel default of 0666 while `FS.create_file`
   and `FS.append` asked for 0644, so which of the three a script called
   decided whether the file it wrote could be written by anyone else --
   visible whenever the umask does not hide it. `FS.copy` had the same
   default, which cost a copied script its executable bit and made a copy
   of a 0600 file readable by everyone. *)

let mode_of path = (Unix.stat path).Unix.st_perm

(* Under a permissive umask, since a umask can only take bits away: what is
   being tested is the mode the code asks for. *)
let with_open_umask f =
  let old = Unix.umask 0o000 in
  Fun.protect ~finally:(fun () -> ignore (Unix.umask old)) f

let wand_ok src =
  match run src with
  | Ok _ -> ()
  | Error m -> Alcotest.failf "script failed: %s" m

let test_written_files_are_not_world_writable () =
  with_tree (fun root ->
    with_open_umask (fun () ->
      let path = Filename.concat root "written" in
      wand_ok (Printf.sprintf
        {|import FS
import Path
FS.write_file! (Path.of_string "%s") "x"|} path);
      Alcotest.(check int) "write_file asks for 0644" 0o644 (mode_of path)))

let test_a_copy_carries_the_source_mode () =
  with_tree (fun root ->
    with_open_umask (fun () ->
      let script = Filename.concat root "script.sh" in
      let secret = Filename.concat root "secret" in
      write script "#!/bin/sh\n";
      write secret "s";
      Unix.chmod script 0o755;
      Unix.chmod secret 0o600;
      let copy_to src dst =
        wand_ok (Printf.sprintf
          {|import FS
import Path
FS.copy! (Path.of_string "%s") (Path.of_string "%s")|} src dst)
      in
      let script_copy = Filename.concat root "script-copy.sh" in
      let secret_copy = Filename.concat root "secret-copy" in
      copy_to script script_copy;
      copy_to secret secret_copy;
      Alcotest.(check int) "a copied script is still executable" 0o755
        (mode_of script_copy);
      Alcotest.(check int) "a copied private file is still private" 0o600
        (mode_of secret_copy);
      (* Overwriting is not the place to widen a mode somebody chose. *)
      let existing = Filename.concat root "existing" in
      write existing "old";
      Unix.chmod existing 0o600;
      copy_to script existing;
      Alcotest.(check int) "an existing destination keeps its own mode" 0o600
        (mode_of existing)))


(* ── What a rehearsal hands back ───────────────────────────────────────── *)

(* `--dry-run` does not create a temp directory; it reports the request and
   answers with a name. That name used to be `/tmp/wand-dry-run-dir` every
   time, in a directory every user on the machine can write, so anyone
   could hold the path first -- or put a symlink there -- and a script
   reading back what it believed it had just been given would read what was
   left for it. The name is unpredictable now, and the line reporting it
   still names the one the script was handed.

   Run through the real binary: what is under test is the rehearsal a
   person invokes, report and value together. *)

let wand_binary =
  let dir = Filename.dirname (Filename.dirname Sys.executable_name) in
  Filename.concat (Filename.concat dir "bin") "wand.exe"

let rehearse src =
  let path = Filename.temp_file "wand_dry_run" ".wand" in
  write path src;
  let cmd =
    String.concat " " (List.map Filename.quote [wand_binary; "--dry-run"; path])
    ^ " 2>&1"
  in
  let ic = Unix.open_process_in cmd in
  let out = In_channel.input_all ic in
  ignore (Unix.close_process_in ic);
  (try Sys.remove path with Sys_error _ -> ());
  out

(* A command run with `Shell.inspect!` is one the script says only reads,
   so a rehearsal runs it and hands its real output on, and says so. Every
   other command is still withheld. *)
let test_a_rehearsal_runs_what_only_inspects () =
  if not (Sys.file_exists wand_binary) then
    Alcotest.failf "wand binary not found at %s" wand_binary;
  let out = rehearse {|uses {IO, Shell(echo)}
import IO
import Shell
let a = Shell.inspect! $*(echo looked)
let b = $(echo withheld)
IO.println "%{a}|%{b}"|} in
  Alcotest.(check (list string)) "the read ran, the other command did not"
    [ "ran (inspect): echo looked";
      "would run: echo withheld -> \"\"";
      "looked|" ]
    (String.split_on_char '\n' (String.trim out))

(* `Shell.inspect_with!` is `inspect!` with text for stdin: a rehearsal runs
   it, and the command reads what it is given. *)
let test_a_rehearsal_runs_what_inspects_with_input () =
  if not (Sys.file_exists wand_binary) then
    Alcotest.failf "wand binary not found at %s" wand_binary;
  let out = rehearse {|uses {IO, Shell(cat)}
import IO
import Shell
let a = "given" |> Shell.inspect_with! $*(cat)
IO.println a|} in
  Alcotest.(check (list string)) "the read ran with its input"
    [ "ran (inspect): cat";
      "given" ]
    (String.split_on_char '\n' (String.trim out))

let temp_dir_script =
  {|uses {FS.Read, FS.Write, IO}
import FS
import IO
import Path
with FS.temp_dir "build-" as d -> IO.println (Path.to_string d)|}

let test_a_rehearsal_names_a_fresh_directory () =
  if not (Sys.file_exists wand_binary) then
    Alcotest.failf "wand binary not found at %s" wand_binary;
  let first  = rehearse temp_dir_script in
  let second = rehearse temp_dir_script in
  List.iter (fun out ->
    if contains out "/tmp/wand-dry-run-dir" then
      Alcotest.failf "the rehearsal still names a fixed path:\n%s" out;
    (* The report says what it substituted, and the script printed what it
       was given: the same name has to appear twice. The third line is the
       bracket releasing it -- the rehearsal remembers the directory it
       said it would create, so `exists?` is true and the release runs, as
       it would in a real run. *)
    match String.split_on_char '\n' (String.trim out) with
    | [reported; printed; released] ->
      if not (contains reported printed) then
        Alcotest.failf "the report and the value disagree:\n%s" out;
      if not (contains released printed) then
        Alcotest.failf "the release names a different directory:\n%s" out;
      if not (contains released "would delete recursively") then
        Alcotest.failf "the release was not reported:\n%s" out;
      if Sys.file_exists printed then
        Alcotest.failf "the rehearsal created %s" printed
    | _ -> Alcotest.failf "unexpected rehearsal output:\n%s" out) [first; second];
  if first = second then
    Alcotest.failf "two rehearsals were handed the same name:\n%s" first

(* A rehearsal answers a read from what it withheld, so a script that reads
   back what it wrote takes the path it would really take. Before this it
   failed on the read -- late, after reporting two steps as though they had
   happened, which is the one thing a rehearsal is for. *)

let rehearsal_answers label script expected =
  let out = rehearse script in
  if not (contains out expected) then
    Alcotest.failf "%s: expected %S in the rehearsal:\n%s" label expected out

let test_a_rehearsal_reads_back_what_it_wrote () =
  rehearsal_answers "a file written and read"
    {|uses {FS.Read, FS.Write, IO}
import FS
import IO
import Path
with FS.temp_dir "ex_" as d -> (
  let f = Path.join d ./x;
  FS.write_file! f "hi";
  IO.println (FS.read_file! f))|}
    "hi";
  rehearsal_answers "an append onto it"
    {|uses {FS.Read, FS.Write, IO}
import FS
import IO
import Path
with FS.temp_dir "ex_" as d -> (
  let f = Path.join d ./x;
  FS.write_file! f "a";
  FS.append! f "b";
  IO.println (FS.read_file! f))|}
    "ab";
  rehearsal_answers "a question about it"
    {|uses {FS.Read, FS.Write, IO}
import FS
import IO
import Path
with FS.temp_dir "ex_" as d -> (
  let f = Path.join d ./x;
  FS.write_file! f "hi";
  IO.println "%{FS.exists? f} %{FS.size! f}")|}
    "true 2B";
  rehearsal_answers "a deleted file is gone"
    {|uses {FS.Read, FS.Write, IO}
import FS
import IO
import Path
with FS.temp_dir "ex_" as d -> (
  let f = Path.join d ./x;
  FS.write_file! f "hi";
  FS.delete! f;
  IO.println "%{FS.exists? f}")|}
    "false";
  rehearsal_answers "a listing shows what was written"
    {|uses {FS.Read, FS.Write, IO}
import FS
import IO
import List
import Path
with FS.temp_dir "ex_" as d -> (
  FS.write_file! (Path.join d ./a) "1";
  FS.write_file! (Path.join d ./b) "2";
  IO.println "%{List.length (FS.list_dir! d)}")|}
    "2";
  rehearsal_answers "a stream reads it back"
    {|uses {FS.Read, FS.Write, IO}
import FS
import IO
import Path
import Stream
with FS.temp_dir "ex_" as d -> (
  let f = Path.join d ./x;
  FS.write_lines! f (Stream.of_list ["a", "b"]);
  IO.println "%{Stream.to_list (FS.stream_lines f)}")|}
    {|["a", "b"]|};
  rehearsal_answers "a glob sees what was written"
    {|uses {FS.Read, FS.Write, IO}
import FS
import IO
import List
import Path
with FS.temp_dir "ex_" as d -> (
  FS.write_file! (Path.join d ./a.log) "1";
  FS.write_file! (Path.join d ./b.log) "2";
  FS.write_file! (Path.join d ./c.txt) "3";
  IO.println "%{List.length (FS.glob_in *.log d)}")|}
    "2";
  rehearsal_answers "a variable set and read"
    {|uses {Env, IO}
import Env
import IO
import Option
Env.set "WAND_REHEARSAL_TEST" "x"
IO.println (Option.default "unset" (Env.get "WAND_REHEARSAL_TEST"))|}
    "x"

(* And nothing it remembers reaches the disk. *)
let test_a_rehearsal_writes_nothing () =
  let dir = Filename.temp_file "wand_rehearsal" "" in
  Sys.remove dir;
  let target = Filename.concat dir "x" in
  let out =
    rehearse (Printf.sprintf
      {|uses {FS.Read, FS.Write, IO}
import FS
import IO
FS.mkdir! %s
FS.write_file! %s "hi"
IO.println (FS.read_file! %s)|} dir target target)
  in
  if not (contains out "hi") then
    Alcotest.failf "the rehearsal did not answer the read:\n%s" out;
  if Sys.file_exists dir then
    Alcotest.failf "the rehearsal created %s" dir;
  if Sys.file_exists target then
    Alcotest.failf "the rehearsal created %s" target


(* ── Publishing a file whole ───────────────────────────────────────────── *)

(* The three ways the hand-written version of an atomic write is wrong.
   None of them shows up on the machine where the script is written, which
   is why each one is a test rather than a paragraph. *)

let write_atomic_to path content =
  wand_ok (Printf.sprintf
    {|import FS
import Path
FS.write_atomic! (Path.of_string "%s") "%s"|} path content)

(* `FS.rename` is `Unix.rename`, which cannot cross a filesystem. A version
   built on `FS.temp_file` puts the temp file in the OS temp directory and
   fails with EXDEV wherever that is a different device -- a Linux box where
   /tmp is tmpfs, which is not the mac the script was written on.

   TMPDIR names somewhere that does not exist, so anything reaching for the
   OS temp directory fails here and a temp file beside the target does not
   notice. *)
let test_the_temp_file_is_beside_the_target () =
  with_tree (fun root ->
    let gone = Filename.concat root "no-such-temp-dir" in
    let old = Sys.getenv_opt "TMPDIR" in
    Unix.putenv "TMPDIR" gone;
    Fun.protect
      ~finally:(fun () ->
        match old with
        | Some v -> Unix.putenv "TMPDIR" v
        | None -> Unix.putenv "TMPDIR" "")
      (fun () ->
        let target = Filename.concat root "published" in
        write_atomic_to target "whole";
        Alcotest.(check string) "the file was published" "whole"
          (In_channel.with_open_text target In_channel.input_all)))

(* Renaming replaces the inode, so the published file's mode is whatever the
   temp file was created as unless the target's own is carried over. Without
   this a 644 configuration file becomes 600 on the first atomic write. *)
let test_an_existing_target_keeps_its_mode () =
  with_tree (fun root ->
    with_open_umask (fun () ->
      let target = Filename.concat root "config" in
      write target "before";
      Unix.chmod target 0o640;
      write_atomic_to target "after";
      Alcotest.(check int) "the mode survived the rename" 0o640
        (mode_of target)))

(* A new file should not depend on which function wrote it. *)
let test_a_new_target_is_created_like_write_file () =
  with_tree (fun root ->
    with_open_umask (fun () ->
      let atomic = Filename.concat root "atomic" in
      let plain = Filename.concat root "plain" in
      write_atomic_to atomic "x";
      wand_ok (Printf.sprintf
        {|import FS
import Path
FS.write_file! (Path.of_string "%s") "x"|} plain);
      Alcotest.(check int) "write_atomic creates what write_file creates"
        (mode_of plain) (mode_of atomic)))

(* `FS.write_file` opens and truncates, so it writes through a link. A
   rename replaces the link itself, which turns
   `/etc/app/config -> config.v3` into a regular file and is the opposite of
   what a deploy publishing through it asked for. *)
let test_a_symlink_is_written_through () =
  with_tree (fun root ->
    let real = Filename.concat root "config.v3" in
    let link = Filename.concat root "config" in
    write real "old";
    Unix.symlink "config.v3" link;
    write_atomic_to link "new";
    Alcotest.(check bool) "the link is still a link" true
      ((Unix.lstat link).Unix.st_kind = Unix.S_LNK);
    Alcotest.(check string) "the link's target was written" "new"
      (In_channel.with_open_text real In_channel.input_all))

(* The temp file lives in the target's directory for the length of the
   write, so it must not be there afterwards -- and a `*.conf` glob in
   another process must not match it while it is. *)
let test_no_temp_file_is_left_behind () =
  with_tree (fun root ->
    let target = Filename.concat root "kept.conf" in
    write_atomic_to target "x";
    Alcotest.(check (list string)) "only the published file is there"
      ["kept.conf"]
      (List.sort String.compare (Array.to_list (Sys.readdir root))))

(* A failed write leaves the target as it was and takes its temp file with
   it. The directory is unwritable, so the temp file cannot be created. *)
let test_a_failed_write_leaves_nothing () =
  with_tree (fun root ->
    let dir = Filename.concat root "locked" in
    Unix.mkdir dir 0o755;
    let target = Filename.concat dir "config" in
    write target "before";
    Unix.chmod dir 0o500;
    Fun.protect ~finally:(fun () -> Unix.chmod dir 0o755) (fun () ->
      match run (Printf.sprintf
        {|import FS
import Path
FS.write_atomic! (Path.of_string "%s") "after"|} target) with
      | Ok _ -> Alcotest.fail "the write was expected to fail"
      | Error m ->
        if not (contains m "write_atomic") then
          Alcotest.failf "the error does not name the operation: %s" m);
    Alcotest.(check string) "the target is untouched" "before"
      (In_channel.with_open_text target In_channel.input_all);
    Alcotest.(check (list string)) "no temp file survived" ["config"]
      (List.sort String.compare (Array.to_list (Sys.readdir dir))))

(* A rehearsal reports the publication and writes nothing, as every other
   `FS.Write` does. *)
let test_a_rehearsal_withholds_an_atomic_write () =
  let target = Filename.concat (Filename.get_temp_dir_name ()) "wand-atomic-rehearsal" in
  (try Sys.remove target with Sys_error _ -> ());
  let out = rehearse (Printf.sprintf
    {|uses {FS.Read, FS.Write, IO}
import FS
import IO
import Path
let f = Path.of_string "%s"
let () = FS.write_atomic! f "hi"
IO.println (FS.read_file! f)|} target)
  in
  if not (contains out "would write atomically") then
    Alcotest.failf "the rehearsal did not report the write:\n%s" out;
  if not (contains out "hi") then
    Alcotest.failf "the rehearsal did not answer the read:\n%s" out;
  if Sys.file_exists target then begin
    Sys.remove target;
    Alcotest.failf "the rehearsal wrote %s" target
  end



(* The streaming publish shares `write_atomic`'s three steps, so what is
   tested here is what is its own: the ending, and that a rehearsal follows
   it. *)

let write_lines_atomic_to path lines =
  Printf.sprintf
    {|import FS
import Path
import Stream
FS.write_lines_atomic! (Path.of_string "%s") (Stream.of_list [%s])|} path
    (String.concat ", " (List.map (Printf.sprintf "\"%s\"") lines))

(* An existing target keeps its mode through the streaming form too, because
   it is the same rename. A second copy of that logic is what this rules
   out. *)
let test_a_streamed_publish_keeps_the_mode () =
  with_tree (fun root ->
    with_open_umask (fun () ->
      let target = Filename.concat root "config" in
      write target "before";
      Unix.chmod target 0o640;
      wand_ok (write_lines_atomic_to target ["x"]);
      Alcotest.(check string) "the lines were published" "x\n"
        (In_channel.with_open_text target In_channel.input_all);
      Alcotest.(check int) "the mode survived" 0o640 (mode_of target)))

(* The reason the sink has two endings. `FS.write_lines` writes into the
   target, so the same failing stream leaves it holding "a\n" -- the old
   contents destroyed and the new ones incomplete. This one leaves it
   alone. *)
let failing_stream path writer =
  Printf.sprintf
    {|import FS
import List
import Path
import Stream
let boom = fn line -> if line == "b" then List.head! [] else line
let _ = try (Stream.of_list ["a", "b"] |> Stream.map boom
             |> FS.%s (Path.of_string "%s"))
()|} writer path

let test_a_raising_stream_publishes_nothing () =
  with_tree (fun root ->
    let target = Filename.concat root "config" in
    write target "old";
    wand_ok (failing_stream target "write_lines_atomic!");
    Alcotest.(check string) "the target is untouched" "old"
      (In_channel.with_open_text target In_channel.input_all);
    Alcotest.(check (list string)) "no temp file survived" ["config"]
      (List.sort String.compare (Array.to_list (Sys.readdir root))))

(* The contrast, so the test above is measuring the difference rather than
   an accident of how the stream happens to run. *)
let test_the_plain_form_leaves_a_partial_file () =
  with_tree (fun root ->
    let target = Filename.concat root "config" in
    write target "old";
    wand_ok (failing_stream target "write_lines!");
    Alcotest.(check string) "the plain form wrote what it had" "a\n"
      (In_channel.with_open_text target In_channel.input_all))

(* A rehearsal ends the way a real run would. The publication is withheld
   and reported; the failing one is withheld and leaves the overlay alone,
   so a read after it still answers what is on the disk. *)
let test_a_rehearsal_withholds_a_streamed_publish () =
  let target =
    Filename.concat (Filename.get_temp_dir_name ()) "wand-atomic-lines-rehearsal"
  in
  (try Sys.remove target with Sys_error _ -> ());
  let out = rehearse (Printf.sprintf
    {|uses {FS.Read, FS.Write, IO}
import FS
import IO
import Path
import Stream
let f = Path.of_string "%s"
let () = FS.write_lines_atomic! f (Stream.of_list ["a"])
IO.println (FS.read_file! f)|} target)
  in
  if not (contains out "would write lines atomically to") then
    Alcotest.failf "the rehearsal did not report the publication:\n%s" out;
  if not (contains out "a") then
    Alcotest.failf "the rehearsal did not answer the read:\n%s" out;
  if Sys.file_exists target then begin
    Sys.remove target;
    Alcotest.failf "the rehearsal wrote %s" target
  end

(* ── delete_tree walks by descriptor ────────────────────────────────────── *)

(* The property, without a race to lose: once the walk has opened a
   directory, replacing the *name* it came from cannot redirect what the walk
   does next. Deterministic, because the swap happens between the open and
   the use rather than being hoped for while wand runs.

   The path form had nothing to hold. It asked `lstat` whether a name was a
   directory, then asked `readdir` for its contents, then asked `rmdir` to
   remove it -- three lookups of one name, so a swap between any two of them
   sent the deletions somewhere else. *)
let test_delete_tree_holds_what_it_opened () =
  with_tree (fun root ->
    let victim = Filename.concat root "victim" in
    let bystander = Filename.concat root "bystander" in
    Unix.mkdir victim 0o700;
    Unix.mkdir bystander 0o700;
    write (Filename.concat victim "mine.txt") "";
    write (Filename.concat bystander "precious.txt") "";
    let root_fd = Unix.openfile root [Unix.O_RDONLY; Unix.O_CLOEXEC] 0 in
    Fun.protect ~finally:(fun () -> Unix.close root_fd) (fun () ->
      match Runner.openat_dir root_fd "victim" with
      | None -> Alcotest.fail "victim is a directory and did not open as one"
      | Some fd ->
        Fun.protect ~finally:(fun () -> Unix.close fd) (fun () ->
          (* The swap: the directory is moved aside and the name it had now
             points at the bystander. A descriptor follows the directory,
             not the name. *)
          let moved = Filename.concat root "moved" in
          Unix.rename victim moved;
          Unix.symlink bystander victim;
          Alcotest.(check (list string)) "the descriptor still lists what it opened"
            ["mine.txt"] (Runner.readdir_fd fd);
          Runner.unlinkat fd "mine.txt" false;
          Alcotest.(check bool) "and deleted through it" false
            (Sys.file_exists (Filename.concat moved "mine.txt"));
          Alcotest.(check bool) "the bystander is untouched" true
            (Sys.file_exists (Filename.concat bystander "precious.txt")))))

(* A symlink is unlinked, never descended into, so a tree that links outside
   itself takes only the link. `O_NOFOLLOW` decides it now, where an `lstat`
   decided it before. *)
let test_delete_tree_does_not_follow_a_link_out () =
  with_tree (fun root ->
    let tree = Filename.concat root "tree" in
    let outside = Filename.concat root "outside" in
    Unix.mkdir tree 0o700;
    Unix.mkdir outside 0o700;
    write (Filename.concat outside "keep.txt") "";
    Unix.symlink outside (Filename.concat tree "link");
    Unix.symlink "/nowhere/at/all" (Filename.concat tree "dangling");
    Runner.delete_tree tree;
    Alcotest.(check bool) "the tree is gone" false (Sys.file_exists tree);
    Alcotest.(check bool) "what it linked to is not" true
      (Sys.file_exists (Filename.concat outside "keep.txt")))

(* Nesting deeper than one directory's worth, and a path that is not there.
   Deleting what is already gone has always been success. *)
let test_delete_tree_depth_and_absence () =
  with_tree (fun root ->
    let deep = Filename.concat root "a/b/c/d/e" in
    ignore (Sys.command (Printf.sprintf "mkdir -p %s" (Filename.quote deep)));
    write (Filename.concat deep "leaf.txt") "";
    Runner.delete_tree (Filename.concat root "a");
    Alcotest.(check bool) "the whole tree is gone" false
      (Sys.file_exists (Filename.concat root "a"));
    Runner.delete_tree (Filename.concat root "never-existed"))

(* ── Locks ─────────────────────────────────────────────────────────────── *)

(* What a lock is worth having for is what happens to it when the process
   holding it goes away, so these run the real binary in real processes.
   Nothing here can be observed from inside one program. *)

let lock_script body =
  let path = Filename.temp_file "wand_lock_" ".wand" in
  write path body;
  path

let spawn script =
  Unix.create_process wand_binary [| wand_binary; script |]
    Unix.stdin Unix.stdout Unix.stderr

let run_script script =
  let cmd =
    String.concat " " (List.map Filename.quote [wand_binary; script]) ^ " 2>&1"
  in
  let ic = Unix.open_process_in cmd in
  let out = In_channel.input_all ic in
  ignore (Unix.close_process_in ic);
  out

let taker lock =
  lock_script (Printf.sprintf
    {|uses {FS.Write, IO}
import FS
import IO
import Path
let () = with FS.lock (Path.of_string "%s") as taken ->
  match taken with
  | Ok(_) -> IO.println "took it"
  | Error(FS.Held) -> IO.println "held"
  | Error(FS.Denied(why)) -> IO.println "denied: %%{why}"|} lock)

(* A holder that takes the lock, says so, and holds it for `hold` -- long
   enough to be killed, or short enough for a waiter to outlast. *)
let holder ?(hold = "30s") lock =
  lock_script (Printf.sprintf
    {|uses {Clock, FS.Write, IO}
import FS
import IO
import Clock
import Path
let () = with FS.lock! (Path.of_string "%s") as _ ->
  (IO.println "held"; Clock.sleep %s)|} lock hold)

(* The lock file is opened by the holder before the taker looks, so the
   taker's answer is about the lock and not about a race to start. *)
let wait_for_file path =
  let rec go n =
    if Sys.file_exists path then ()
    else if n = 0 then Alcotest.failf "the holder never created %s" path
    else (ignore (Unix.select [] [] [] 0.05); go (n - 1))
  in
  go 100

let test_a_second_process_is_told_it_is_held () =
  with_tree (fun root ->
    let lock = Filename.concat root "guard.lock" in
    let h = holder lock and t = taker lock in
    let pid = spawn h in
    Fun.protect
      ~finally:(fun () ->
        (try Unix.kill pid Sys.sigkill with Unix.Unix_error _ -> ());
        ignore (Unix.waitpid [] pid);
        List.iter (fun f -> try Sys.remove f with Sys_error _ -> ()) [h; t])
      (fun () ->
        wait_for_file lock;
        (* The file exists before the flock is taken, briefly. Retry until
           the holder has it, rather than racing the answer. *)
        let rec settle n =
          let out = run_script t in
          if contains out "held" then out
          else if n = 0 then out
          else (ignore (Unix.select [] [] [] 0.05); settle (n - 1))
        in
        let out = settle 40 in
        if not (contains out "held") then
          Alcotest.failf "a second process took a held lock: %s" out))

(* The property a pid file cannot offer. `kill -9` runs nothing on the way
   out -- no release, no cleanup -- and the lock is still gone, because it
   was the kernel's rather than the script's. *)
let test_a_killed_holder_releases_the_lock () =
  with_tree (fun root ->
    let lock = Filename.concat root "guard.lock" in
    let h = holder lock and t = taker lock in
    let pid = spawn h in
    Fun.protect
      ~finally:(fun () ->
        List.iter (fun f -> try Sys.remove f with Sys_error _ -> ()) [h; t])
      (fun () ->
        wait_for_file lock;
        ignore (Unix.select [] [] [] 0.3);
        Unix.kill pid Sys.sigkill;
        ignore (Unix.waitpid [] pid);
        let out = run_script t in
        if not (contains out "took it") then
          Alcotest.failf "the lock outlived the process holding it: %s" out))

(* A lock file is never deleted, on purpose -- deleting it is the race it
   exists to prevent. The empty file left behind is the documented cost. *)
let test_the_lock_file_stays () =
  with_tree (fun root ->
    let lock = Filename.concat root "guard.lock" in
    let t = taker lock in
    Fun.protect ~finally:(fun () -> try Sys.remove t with Sys_error _ -> ())
      (fun () ->
        let out = run_script t in
        if not (contains out "took it") then
          Alcotest.failf "the lock was not taken: %s" out;
        Alcotest.(check bool) "the lock file is still there" true
          (Sys.file_exists lock)))

(* A flock belongs to the open file description, not to the process, so a
   descriptor a child inherits keeps the lock after the holder has exited
   and released it. The lock file was the one descriptor wand opened without
   O_CLOEXEC: a script that started a background process while holding the
   lock left the next run reporting Held with nothing holding it, which is
   exactly the cron guard the lock is for. *)
let test_a_background_child_does_not_keep_the_lock () =
  with_tree (fun root ->
    let lock = Filename.concat root "guard.lock" in
    let h =
      lock_script (Printf.sprintf
        {|uses {FS.Write, IO, Shell(sh)}
import FS
import IO
import Path
let () = with FS.lock! (Path.of_string "%s") as _ ->
  (let _ = $(sh -c "sleep 30 >/dev/null 2>&1 </dev/null & echo started");
   IO.println "held")|} lock)
    in
    let t = taker lock in
    Fun.protect
      ~finally:(fun () ->
        List.iter (fun f -> try Sys.remove f with Sys_error _ -> ()) [h; t])
      (fun () ->
        let held = run_script h in
        if not (contains held "held") then
          Alcotest.failf "the holder did not take the lock: %s" held;
        let out = run_script t in
        if not (contains out "took it") then
          Alcotest.failf
            "the lock outlived its holder, in a child that inherited it: %s"
            out))

(* The point of a waiting acquire: a second run queues behind the first
   instead of standing down. Needs two processes, so it lives here. *)
let test_a_wait_queues_behind_a_holder () =
  with_tree (fun root ->
    let lock = Filename.concat root "guard.lock" in
    let h = holder ~hold:"600ms" lock in
    let w =
      lock_script (Printf.sprintf
        {|uses {Clock, FS.Write, IO}
import FS
import IO
import Path
let () = with FS.lock_wait 20s (Path.of_string "%s") as taken ->
  match taken with
  | Ok(_) -> IO.println "waited and got it"
  | Error(FS.Held) -> IO.println "gave up"
  | Error(FS.Denied(why)) -> IO.println "denied: %%{why}"|} lock)
    in
    let pid = spawn h in
    Fun.protect
      ~finally:(fun () ->
        (try Unix.kill pid Sys.sigkill with Unix.Unix_error _ -> ());
        (try ignore (Unix.waitpid [] pid) with Unix.Unix_error _ -> ());
        List.iter (fun f -> try Sys.remove f with Sys_error _ -> ()) [h; w])
      (fun () ->
        wait_for_file lock;
        let out = run_script w in
        if not (contains out "waited and got it") then
          Alcotest.failf "the wait did not get the lock: %s" out))

(* A rehearsal takes the lock and declines to wait. A rehearsal that waited
   out a real budget would be useless on exactly the scripts that need one,
   and the line it reports has to say the wait was skipped so `Held` is not
   read as what a real run would have got. *)
let test_a_rehearsal_does_not_wait () =
  with_tree (fun root ->
    let lock = Filename.concat root "guard.lock" in
    let h = holder ~hold:"30s" lock in
    let w = Printf.sprintf
      {|uses {Clock, FS.Write, IO}
import FS
import IO
import Path
let () = with FS.lock_wait 30s (Path.of_string "%s") as taken ->
  match taken with
  | Ok(_) -> IO.println "took it"
  | Error(_) -> IO.println "held"|} lock
    in
    let pid = spawn h in
    Fun.protect
      ~finally:(fun () ->
        (try Unix.kill pid Sys.sigkill with Unix.Unix_error _ -> ());
        (try ignore (Unix.waitpid [] pid) with Unix.Unix_error _ -> ());
        try Sys.remove h with Sys_error _ -> ())
      (fun () ->
        wait_for_file lock;
        (* Settle: the file exists a moment before the flock is taken. *)
        ignore (Unix.select [] [] [] 0.3);
        let started = Unix.gettimeofday () in
        let out = rehearse w in
        let elapsed = Unix.gettimeofday () -. started in
        if not (contains out "a rehearsal does not wait") then
          Alcotest.failf "the rehearsal did not say it skipped the wait:\n%s" out;
        (* Thirty seconds of budget against a lock somebody else holds. A
           rehearsal that waited would still be running. *)
        if elapsed > 10. then
          Alcotest.failf "the rehearsal waited %.1fs of its 30s budget" elapsed))

(* A rehearsal takes the lock rather than withholding it. Withholding would
   let a `--dry-run` run beside a real one, which is what the lock is for. *)
let test_a_rehearsal_takes_the_lock () =
  with_tree (fun root ->
    let lock = Filename.concat root "guard.lock" in
    let out = rehearse (Printf.sprintf
      {|uses {FS.Write, IO}
import FS
import IO
import Path
let () = with FS.lock (Path.of_string "%s") as taken ->
  match taken with
  | Ok(_) -> IO.println "took it"
  | Error(_) -> IO.println "not taken"|} lock)
    in
    if not (contains out "took it") then
      Alcotest.failf "a rehearsal did not take the lock:\n%s" out;
    Alcotest.(check bool) "the rehearsal made the lock file" true
      (Sys.file_exists lock))

(* ── Suite ─────────────────────────────────────────────────────────────── *)

let () =
  Alcotest.run "FS" [
    "real", [
      Alcotest.test_case "read/write round trip" `Quick test_read_write_round_trip;
    ];
    "what a glob walks", [
      Alcotest.test_case "stays under its directory" `Quick
        test_a_glob_stays_under_its_directory;
      Alcotest.test_case "a matching link is an answer" `Quick
        test_a_glob_answers_with_a_matching_link;
      Alcotest.test_case "a link to an ancestor does not circle" `Quick
        test_a_glob_does_not_circle;
    ];
    "a rehearsal", [
      Alcotest.test_case "names a fresh directory" `Quick
        test_a_rehearsal_names_a_fresh_directory;
      Alcotest.test_case "a rehearsal runs what only inspects" `Quick
        test_a_rehearsal_runs_what_only_inspects;
      Alcotest.test_case "a rehearsal runs what inspects with input" `Quick
        test_a_rehearsal_runs_what_inspects_with_input;
      Alcotest.test_case "reads back what it wrote" `Quick
        test_a_rehearsal_reads_back_what_it_wrote;
      Alcotest.test_case "writes nothing" `Quick
        test_a_rehearsal_writes_nothing;
    ];
    "how a file is created", [
      Alcotest.test_case "write_file asks for 0644" `Quick
        test_written_files_are_not_world_writable;
      Alcotest.test_case "a copy carries the source mode" `Quick
        test_a_copy_carries_the_source_mode;
    ];
    "publishing a file whole", [
      Alcotest.test_case "the temp file is beside the target" `Quick
        test_the_temp_file_is_beside_the_target;
      Alcotest.test_case "an existing target keeps its mode" `Quick
        test_an_existing_target_keeps_its_mode;
      Alcotest.test_case "a new target is created like write_file" `Quick
        test_a_new_target_is_created_like_write_file;
      Alcotest.test_case "a symlink is written through" `Quick
        test_a_symlink_is_written_through;
      Alcotest.test_case "no temp file is left behind" `Quick
        test_no_temp_file_is_left_behind;
      Alcotest.test_case "a failed write leaves nothing" `Quick
        test_a_failed_write_leaves_nothing;
      Alcotest.test_case "a rehearsal withholds it" `Quick
        test_a_rehearsal_withholds_an_atomic_write;
    ];
    "publishing a stream", [
      Alcotest.test_case "an existing target keeps its mode" `Quick
        test_a_streamed_publish_keeps_the_mode;
      Alcotest.test_case "a raising stream publishes nothing" `Quick
        test_a_raising_stream_publishes_nothing;
      Alcotest.test_case "the plain form leaves a partial file" `Quick
        test_the_plain_form_leaves_a_partial_file;
      Alcotest.test_case "a rehearsal withholds it" `Quick
        test_a_rehearsal_withholds_a_streamed_publish;
    ];
    "delete_tree", [
      Alcotest.test_case "holds what it opened" `Quick
        test_delete_tree_holds_what_it_opened;
      Alcotest.test_case "does not follow a link out" `Quick
        test_delete_tree_does_not_follow_a_link_out;
      Alcotest.test_case "depth, and a path that is not there" `Quick
        test_delete_tree_depth_and_absence;
    ];
    "a lock", [
      Alcotest.test_case "a second process is told it is held" `Slow
        test_a_second_process_is_told_it_is_held;
      Alcotest.test_case "a killed holder releases it" `Slow
        test_a_killed_holder_releases_the_lock;
      Alcotest.test_case "the lock file stays" `Quick
        test_the_lock_file_stays;
      Alcotest.test_case "a background child does not keep it" `Slow
        test_a_background_child_does_not_keep_the_lock;
      Alcotest.test_case "a rehearsal takes it" `Quick
        test_a_rehearsal_takes_the_lock;
      Alcotest.test_case "a wait queues behind a holder" `Slow
        test_a_wait_queues_behind_a_holder;
      Alcotest.test_case "a rehearsal does not wait" `Slow
        test_a_rehearsal_does_not_wait;
    ];
  ]
