open Wand

(* A `with` releases however the script ends, including when the script is
   stopped rather than finishing: `exit`, Ctrl-C, or a `kill`. Only a
   process that is destroyed rather than stopped -- SIGKILL -- skips it.

   Signals cannot be tested honestly in-process: what is under test is
   whether an interrupted program unwinds, so the program has to really be
   interrupted. Each case starts the real wand binary on a script, signals
   it, and checks from here that the directory the script was holding is
   gone.

   The child writes the directory's name to a file the parent chose, since
   the name is generated at run time and the parent has to know what to
   look for after the child is gone. *)

let script marker =
  Printf.sprintf
    {|import FS
import Path
with FS.temp_dir "wand_sig_" as d ->
  let () = FS.write_file! (Path.of_string "%s") (Path.to_string d) in
  let () = FS.write_file! (Path.of_string "%%{Path.to_string d}/held.txt") "x" in
  let _ = $(sleep 2) in ()|}
    marker

(* Every signalled case runs the real binary rather than a forked copy of
   this test process. What is under test is the interpreter a user runs --
   and a fork-without-exec child of the OCaml 5 runtime is not sound ground
   to take signals on: the runtime threads that signal delivery leans on do
   not survive the fork, and under `dune build @runtest` load a SIGINT
   landing mid-recursion killed such children with SIGSEGV (so nothing
   unwound, and the release this suite exists to verify never ran), while
   the real binary rode the same load clean, 48 runs out of 48. The Par
   case below already ran this way because of domains; the reasoning is
   the same.

   The child's script waits where a script usually is when someone
   interrupts it; the marker says it has acquired, so the signal is sent
   then rather than after a guessed sleep -- a test that races is worse
   than no test. *)
let wand_binary =
  let dir = Filename.dirname (Filename.dirname Sys.executable_name) in
  Filename.concat (Filename.concat dir "bin") "wand.exe"

let signalled_run ?(signal = Sys.sigint) ~marker src =
  if not (Sys.file_exists wand_binary) then
    Alcotest.failf "wand binary not found at %s" wand_binary;
  let path = Filename.temp_file "wand_sig_script" ".wand" in
  Out_channel.with_open_text path (fun oc -> Out_channel.output_string oc src);
  let devnull = Unix.openfile "/dev/null" [Unix.O_WRONLY] 0o644 in
  let pid = Unix.create_process wand_binary [| wand_binary; path |]
              Unix.stdin devnull devnull in
  let rec await_acquire tries =
    let held = try In_channel.with_open_text marker In_channel.input_all with _ -> "" in
    if String.trim held <> "" then String.trim held
    else if tries = 0 then ""
    else begin ignore (Unix.select [] [] [] 0.05); await_acquire (tries - 1) end
  in
  let dir = await_acquire 200 in
  Unix.kill pid signal;
  let (_, status) = Unix.waitpid [] pid in
  Unix.close devnull;
  (try Sys.remove path with _ -> ());
  let code = match status with
    | Unix.WEXITED n -> n
    | Unix.WSIGNALED n -> 128 + n
    | Unix.WSTOPPED n -> 128 + n
  in
  (* Kept apart from `code` because 128+n and a real exit with that code
     are indistinguishable there -- a child killed by raw SIGINT also
     reports 130 -- and which one happened is the first question a
     failure raises. *)
  let status_text = match status with
    | Unix.WEXITED n -> Printf.sprintf "exited %d" n
    | Unix.WSIGNALED n -> Printf.sprintf "killed by signal %d" n
    | Unix.WSTOPPED n -> Printf.sprintf "stopped by signal %d" n
  in
  let present = dir <> "" && Sys.file_exists dir in
  if present then ignore (Sys.command (Printf.sprintf "rm -rf %s" (Filename.quote dir)));
  (try Sys.remove marker with _ -> ());
  Alcotest.(check bool) "the child acquired before being signalled" true (dir <> "");
  (code, present, status_text)

let test_sigint_releases () =
  let marker = Filename.temp_file "wand_sig_m" "" in
  Sys.remove marker;
  let (code, present, status) = signalled_run ~signal:Sys.sigint ~marker (script marker) in
  if present then Alcotest.failf "the directory is still there (child %s)" status;
  Alcotest.(check int) "exits 130, as a shell reports an interrupt" 130 code

let test_sigterm_releases () =
  let marker = Filename.temp_file "wand_sig_m" "" in
  Sys.remove marker;
  let (code, present, status) = signalled_run ~signal:Sys.sigterm ~marker (script marker) in
  if present then Alcotest.failf "the directory is still there (child %s)" status;
  Alcotest.(check int) "exits 143" 143 code

(* The interrupt landing inside `acquire`, rather than in the body. The
   release is installed only once acquire returns, so a resource that had
   already become real -- the file written, the lock taken -- had nothing to
   give it back, and an interrupt in that window left it held. The window is
   small, which is what made this a demo that passed everywhere except a
   loaded machine.

   The marker is written after the resource exists and before the work that
   follows it, so the parent signals while acquire is still running. The
   work is pure: a command would end by another route, and this is the one
   that check_interrupt governs. *)
let acquire_script marker =
  Printf.sprintf
    {|import FS
import List
import Path
import Resource
let held = "%s.held"
let r =
  let acquire = fn () ->
    let () = FS.write_file! (Path.of_string held) "x" in
    let () = FS.write_file! (Path.of_string "%s") held in
    let _ = List.fold_left (fn a _ -> a + 1) 0 (List.range 0 500000) in
    held
  in
  let release = fn h -> FS.delete! (Path.of_string h) in
  Resource.make acquire release
with r as h -> h|}
    marker marker

let test_interrupt_during_acquire_releases () =
  let marker = Filename.temp_file "wand_sig_m" "" in
  Sys.remove marker;
  let (code, present, status) = signalled_run ~signal:Sys.sigint ~marker (acquire_script marker) in
  if present then
    Alcotest.failf "what acquire had taken was not given back (child %s)" status;
  Alcotest.(check int) "exits 130" 130 code

(* Nothing survives SIGKILL. Stated as a test so the limit is recorded
   rather than discovered. *)
let test_sigkill_cannot_release () =
  let marker = Filename.temp_file "wand_sig_m" "" in
  Sys.remove marker;
  let (code, present, _) = signalled_run ~signal:Sys.sigkill ~marker (script marker) in
  Alcotest.(check bool) "the directory is left behind, as it must be" true present;
  Alcotest.(check int) "killed, not stopped" (128 + Sys.sigkill) code

(* Workers run on their own domains, so each has to see the request for
   itself; and the calling domain must not unwind until they are joined, or
   it would leave workers running and their brackets unreleased. *)
(* The worker prefix carries this run's pid: the leftover scan below reads
   the shared temp directory, and an unscoped prefix would count another
   concurrently running copy of this suite's workers as this run's leak. *)
let worker_prefix = Printf.sprintf "wand_sigw_%d_" (Unix.getpid ())

let par_script marker =
  Printf.sprintf
    {|import FS
import Path
import Par
with FS.temp_dir "wand_sig_" as outer ->
  let () = FS.write_file! (Path.of_string "%s") (Path.to_string outer) in
  Par.each 4 (fn n ->
    with FS.temp_dir "%s" as d ->
    let () = FS.write_file! (Path.of_string "%%{Path.to_string outer}/%%{n}") (Path.to_string d) in
    let _ = $(sleep 2) in ()) [1, 2, 3, 4]|}
    marker worker_prefix

let test_par_workers_release () =
  let marker = Filename.temp_file "wand_sig_m" "" in
  Sys.remove marker;
  let (code, present, status) = signalled_run ~marker (par_script marker) in
  if present then
    Alcotest.failf "the caller's directory is still there (child %s)" status;
  Alcotest.(check int) "exits 130" 130 code;
  (* Each worker recorded its own directory in the caller's, which is gone
     with them -- so what is checked here is that none survived it. *)
  let leftovers =
    let p = worker_prefix in
    let pl = String.length p in
    Sys.readdir (Filename.get_temp_dir_name ())
    |> Array.to_list
    |> List.filter (fun n -> String.length n > pl && String.sub n 0 pl = p)
  in
  List.iter (fun n ->
    ignore (Sys.command (Printf.sprintf "rm -rf %s"
      (Filename.quote (Filename.concat (Filename.get_temp_dir_name ()) n))))) leftovers;
  Alcotest.(check int) "no worker left its directory behind" 0 (List.length leftovers)

(* `exit` unwinds like anything else, and keeps its code. *)
let exit_script marker code =
  Printf.sprintf
    {|import FS
import Path
import Proc
with FS.temp_dir "wand_sig_" as d ->
  let () = FS.write_file! (Path.of_string "%s") (Path.to_string d) in
  Proc.exit %d|}
    marker code

let test_exit_releases () =
  List.iter (fun n ->
    let marker = Filename.temp_file "wand_sig_m" "" in
    let src = exit_script marker n in
    let code =
      match Runner.run_string src with
      | _ -> 0
      | exception Evaluator.Interrupted c -> c
    in
    let dir = String.trim (In_channel.with_open_text marker In_channel.input_all) in
    let present = Sys.file_exists dir in
    if present then ignore (Sys.command (Printf.sprintf "rm -rf %s" (Filename.quote dir)));
    (try Sys.remove marker with _ -> ());
    Alcotest.(check int) (Printf.sprintf "exit %d keeps its code" n) n code;
    Alcotest.(check bool)
      (Printf.sprintf "exit %d released first" n) false present
  ) [0; 1; 3; 42]

(* `Par.all!` runs its branches as a race. A signal stops every branch,
   and the race used to take that for "no branch finished": the script
   failed with `race: no thunk finished` and exit 1, where it should stop
   as a stopped script does. *)
let par_all_script marker =
  Printf.sprintf
    {|import Clock
import FS
import Par
import Path
with FS.temp_dir "wand_sig_" as d ->
  let () = FS.write_file! (Path.of_string "%s") (Path.to_string d) in
  Par.all! [fn () -> Clock.sleep 5s, fn () -> Clock.sleep 5s]|}
    marker

let test_sigterm_in_par_all () =
  let marker = Filename.temp_file "wand_sig_m" "" in
  Sys.remove marker;
  let (code, present, status) =
    signalled_run ~signal:Sys.sigterm ~marker (par_all_script marker) in
  if present then Alcotest.failf "the directory is still there (child %s)" status;
  Alcotest.(check int) "exits 143" 143 code

(* `exit` in one branch of `Par.all!` stops the program then, with its code.
   It used to be lost: the race waited for the other branch and the script
   went on. The other branch sleeps far longer than the check allows. *)
let exit_in_par_script marker code =
  Printf.sprintf
    {|import Clock
import FS
import Par
import Path
import Proc
with FS.temp_dir "wand_sig_" as d ->
  let () = FS.write_file! (Path.of_string "%s") (Path.to_string d) in
  Par.all! [fn () -> Proc.exit %d, fn () -> Clock.sleep 30s]|}
    marker code

let test_exit_in_par_all () =
  List.iter (fun n ->
    let marker = Filename.temp_file "wand_sig_m" "" in
    let started = Unix.gettimeofday () in
    let code =
      match Runner.run_string (exit_in_par_script marker n) with
      | _ -> -1
      | exception Evaluator.Interrupted c -> c
    in
    let took = Unix.gettimeofday () -. started in
    let dir = String.trim (In_channel.with_open_text marker In_channel.input_all) in
    let present = Sys.file_exists dir in
    if present then ignore (Sys.command (Printf.sprintf "rm -rf %s" (Filename.quote dir)));
    (try Sys.remove marker with _ -> ());
    Alcotest.(check int) (Printf.sprintf "exit %d keeps its code" n) n code;
    Alcotest.(check bool) (Printf.sprintf "exit %d released first" n) false present;
    Alcotest.(check bool)
      (Printf.sprintf "exit %d did not wait for the other branch (%.1fs)" n took)
      true (took < 10.0)
  ) [0; 3]

(* A server told to stop stops accepting, lets the request in progress
   finish, and then exits as a stopped process does. The request is sent
   from here, and the signal goes once the handler says it has started. *)
let test_http_server_drains () =
  if not (Sys.file_exists wand_binary) then
    Alcotest.failf "wand binary not found at %s" wand_binary;
  let port = 18000 + (Unix.getpid () mod 1000) in
  let marker = Filename.temp_file "wand_drain" "" in
  Sys.remove marker;
  let src = Printf.sprintf
    "uses {Clock, FS.Write, Net.Listen}\n\
     import Clock\nimport FS\nimport HTTP\nimport Path\nimport String\n\
     let route (req: HTTP.Incoming) = (\n\
       FS.write_file! (Path.of_string %S) \"started\";\n\
       Clock.sleep 1s;\n\
       HTTP.reply 200 \"finished\"\n\
     )\n\
     HTTP.serve! HTTP.Server(port = String.to_port! \":%d\", limit = 4, grace = 5s) route\n"
    marker port in
  let path = Filename.temp_file "wand_drain" ".wand" in
  Out_channel.with_open_text path (fun oc -> output_string oc src);
  let devnull = Unix.openfile "/dev/null" [Unix.O_WRONLY] 0o644 in
  let pid = Unix.create_process wand_binary [| wand_binary; path |]
              Unix.stdin devnull devnull in
  let addr = Unix.ADDR_INET (Unix.inet_addr_loopback, port) in
  let rec connect tries =
    let fd = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    match Unix.connect fd addr with
    | () -> fd
    | exception Unix.Unix_error _ when tries > 0 ->
      Unix.close fd; ignore (Unix.select [] [] [] 0.05); connect (tries - 1)
  in
  let fd = connect 200 in
  let req = "GET /slow HTTP/1.1\r\nHost: x\r\n\r\n" in
  ignore (Unix.write_substring fd req 0 (String.length req));
  let rec started tries =
    if Sys.file_exists marker then true
    else if tries = 0 then false
    else (ignore (Unix.select [] [] [] 0.05); started (tries - 1))
  in
  let began = started 200 in
  Unix.kill pid Sys.sigterm;
  ignore (Unix.select [] [] [] 0.2);
  let refused =
    let fd2 = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    let r = match Unix.connect fd2 addr with
      | () -> false
      | exception Unix.Unix_error _ -> true in
    Unix.close fd2; r
  in
  let buf = Buffer.create 256 and chunk = Bytes.create 256 in
  let rec drain () =
    match Unix.read fd chunk 0 256 with
    | 0 -> ()
    | n -> Buffer.add_subbytes buf chunk 0 n; drain ()
    | exception Unix.Unix_error _ -> ()
  in
  drain ();
  Unix.close fd;
  let (_, status) = Unix.waitpid [] pid in
  Unix.close devnull;
  (try Sys.remove path with _ -> ());
  (try Sys.remove marker with _ -> ());
  let answer = Buffer.contents buf in
  let has sub =
    let n = String.length sub and m = String.length answer in
    let rec go i = i + n <= m && (String.sub answer i n = sub || go (i + 1)) in
    go 0
  in
  Alcotest.(check bool) "the handler started before the signal" true began;
  Alcotest.(check bool) "a new connection is refused" true refused;
  Alcotest.(check bool) (Printf.sprintf "the request finished: %S" answer) true
    (has "200 OK" && has "finished");
  Alcotest.(check bool) "exits 143, as a stopped process does" true
    (status = Unix.WEXITED 143)

let () =
  Alcotest.run "Signals" [
    "a stopped script still releases", [
      Alcotest.test_case "SIGINT"  `Quick test_sigint_releases;
      Alcotest.test_case "SIGTERM" `Quick test_sigterm_releases;
      Alcotest.test_case "exit n"  `Quick test_exit_releases;
      Alcotest.test_case "Par workers" `Quick test_par_workers_release;
      Alcotest.test_case "SIGTERM in Par.all!" `Quick test_sigterm_in_par_all;
      Alcotest.test_case "exit in Par.all!" `Quick test_exit_in_par_all;
      Alcotest.test_case "during acquire" `Quick test_interrupt_during_acquire_releases;
      Alcotest.test_case "an HTTP server drains" `Quick test_http_server_drains;
    ];
    "the limit", [
      Alcotest.test_case "SIGKILL cannot" `Quick test_sigkill_cannot_release;
    ];
  ]
