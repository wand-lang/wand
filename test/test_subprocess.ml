(* What a command does with its pipes, tested against the real binary.

   Every case here hung, died or lost output before: draining stdout to the
   end and only then reading stderr deadlocks as soon as the child fills the
   pipe nobody is emptying, and a closed reader downstream killed wand where
   it stood. A deadlock cannot be tested in-process -- a stuck run would
   take the suite with it -- so each case starts wand on a script with a
   deadline, and a child still running when it passes is the failure. *)

let wand_binary =
  let dir = Filename.dirname (Filename.dirname Sys.executable_name) in
  Filename.concat (Filename.concat dir "bin") "wand.exe"

let write_script src =
  let path = Filename.temp_file "wand_subproc" ".wand" in
  Out_channel.with_open_text path (fun oc -> Out_channel.output_string oc src);
  path

let read_file path = In_channel.with_open_text path In_channel.input_all

(* Waits with a deadline. The child's own streams go to files rather than
   pipes: a parent reading two pipes one after the other is the very bug
   under test, and the test should not have to be right about it to report
   that wand is. *)
let wait_within seconds pid =
  let deadline = Unix.gettimeofday () +. seconds in
  let rec poll () =
    match Unix.waitpid [Unix.WNOHANG] pid with
    | (0, _) ->
      if Unix.gettimeofday () > deadline then begin
        (try Unix.kill pid Sys.sigkill with Unix.Unix_error _ -> ());
        ignore (Unix.waitpid [] pid);
        None
      end else begin ignore (Unix.select [] [] [] 0.02); poll () end
    | (_, status) -> Some status
    | exception Unix.Unix_error (Unix.EINTR, _, _) -> poll ()
  in
  poll ()

(* Returns None when the run did not finish in time. *)
let run_script ?(seconds = 10.0) src =
  if not (Sys.file_exists wand_binary) then
    Alcotest.failf "wand binary not found at %s" wand_binary;
  let path = write_script src in
  let out_path = Filename.temp_file "wand_subproc_out" "" in
  let err_path = Filename.temp_file "wand_subproc_err" "" in
  let out = Unix.openfile out_path [Unix.O_WRONLY; Unix.O_TRUNC] 0o644 in
  let err = Unix.openfile err_path [Unix.O_WRONLY; Unix.O_TRUNC] 0o644 in
  let pid = Unix.create_process wand_binary [| wand_binary; path |]
              Unix.stdin out err in
  let status = wait_within seconds pid in
  Unix.close out; Unix.close err;
  let result =
    Option.map (fun status ->
      let code = match status with
        | Unix.WEXITED n -> n
        | Unix.WSIGNALED n | Unix.WSTOPPED n -> 128 + n
      in
      (code, read_file out_path, read_file err_path)) status
  in
  List.iter (fun p -> try Sys.remove p with Sys_error _ -> ())
    [path; out_path; err_path];
  result

let finished what = function
  | Some r -> r
  | None -> Alcotest.failf "%s did not finish -- the run is stuck" what

(* Half a megabyte, comfortably past a pipe buffer on every system wand
   runs on: the child blocks writing stderr, and a wand that is waiting for
   the end of stdout never gets there. *)
let big = 500_000

let test_captured_stderr_past_the_buffer () =
  let (code, out, _) =
    finished "a command writing 500KB to stderr"
      (run_script (Printf.sprintf
         {|uses {IO, Shell(sh)}
import IO
import String
let r = $?(sh -c 'yes err | head -c %d >&2; echo done')
IO.println "%%{r.code} %%{r.stdout} %%{String.length r.stderr}"|}
         big))
  in
  Alcotest.(check int) "the script ran" 0 code;
  (* At least, not exactly: the generator on the left of the pipe is cut off
     mid-write and says so on the same stream. What matters is that none of
     it was lost and none of it blocked. *)
  match String.split_on_char ' ' (String.trim out) with
  | ["0"; "done"; n] when int_of_string n >= big -> ()
  | _ -> Alcotest.failf "code, stdout and the size of stderr came back as %S" out

let test_stdin_past_the_buffer () =
  let (code, out, _) =
    finished "a command fed 400KB on stdin"
      (run_script
         {|uses {IO, Shell(sh)}
import IO
import String
let sent = String.repeat 400000 "x"
IO.println "%{String.length (sent |> $(sh -c 'cat'))}"|})
  in
  Alcotest.(check int) "the script ran" 0 code;
  Alcotest.(check string) "everything written came back" "400000\n" out

(* `$()` lets a command's stderr through to the caller's, so the explanation
   of a failure reaches whoever is watching. Giving the command something on
   stdin does not change who it is talking to. *)
let test_piped_command_keeps_its_stderr () =
  let (code, out, err) =
    finished "a command on the right of |>"
      (run_script
         {|uses {IO, Shell(sh)}
import IO
IO.println ("got " ++ ("hello" |> $(sh -c 'echo explained >&2; cat')))|})
  in
  Alcotest.(check int) "the script ran" 0 code;
  Alcotest.(check string) "the answer" "got hello\n" out;
  Alcotest.(check string) "and the command's own report" "explained\n" err

(* The reader downstream goes away mid-run -- `wand report.wand | head -1`.
   Killed by SIGPIPE, wand would stop between two instructions and leave
   whatever the script was holding; the marker file is how the test sees
   that the release ran. *)
let test_closed_reader_still_releases () =
  let marker = Filename.temp_file "wand_subproc_marker" "" in
  let src = Printf.sprintf
    {|uses {IO, FS.Write}
import FS
import IO
import List
import Path
import Resource
import String
let mark = fn s -> FS.write_file! (Path.of_string "%s") s
let held = Resource.make (fn () -> mark "held") (fn _ -> mark "released")
with held as _ -> List.each (fn i -> IO.println "%%{i} %%{String.repeat 200 "x"}") (List.range 1 5000)|}
    marker
  in
  let path = write_script src in
  let (r, w) = Unix.pipe ~cloexec:true () in
  let devnull = Unix.openfile "/dev/null" [Unix.O_WRONLY] 0o644 in
  let pid = Unix.create_process wand_binary [| wand_binary; path |]
              Unix.stdin w devnull in
  Unix.close w;
  (* One line's worth, then the reader is gone -- what `head -1` does. *)
  ignore (Unix.read r (Bytes.create 4096) 0 4096);
  Unix.close r;
  let status = finished "a script whose reader closed" (wait_within 10.0 pid) in
  Unix.close devnull;
  List.iter (fun p -> try Sys.remove p with Sys_error _ -> ()) [path];
  let state = String.trim (read_file marker) in
  (try Sys.remove marker with Sys_error _ -> ());
  Alcotest.(check string) "the bracket released on the way out" "released" state;
  match status with
  | Unix.WEXITED n ->
    Alcotest.(check int) "exits 141, as a shell reports a closed pipe" 141 n
  | Unix.WSIGNALED n ->
    Alcotest.failf "killed by signal %d instead of unwinding" n
  | Unix.WSTOPPED n -> Alcotest.failf "stopped by signal %d" n

(* A stream stopped early ends everything its command started: the shell
   and the pipeline under it. Only the shell was signalled, so `sleep | cat`
   went on after wand had stopped reading (#91). The sleep carries an odd
   length so that it is this test's and no one else's. *)
let test_an_early_stop_ends_the_whole_command () =
  let marker = "31.7319" in
  (* `[s]leep` so the pattern does not match the `pgrep` command line itself
     -- its own argv holds `[s]leep <marker>`, which the regex `[s]leep` does
     not match, while the real `sleep <marker>` does. Without it the counter
     found its own shell and never read zero. *)
  let pat = "[s]leep " ^ marker in
  let count () =
    let ic = Unix.open_process_in (Printf.sprintf "pgrep -f '%s' | wc -l" pat) in
    let n = int_of_string (String.trim (In_channel.input_all ic)) in
    ignore (Unix.close_process_in ic); n
  in
  let (pull, finish) =
    Wand.Runner.stream_command (Printf.sprintf "echo a; sleep %s | cat" marker) in
  ignore (pull ());
  finish true;
  (* The shell is reaped by `finish`, but the `sleep` it started is in the
     group and, once signalled, is reparented to init and reaped there -- so
     it can outlast `finish` by a moment on a loaded machine. Poll for it to
     go rather than guess a sleep. *)
  let rec wait tries =
    if count () = 0 then 0
    else if tries = 0 then count ()
    else (Unix.sleepf 0.1; wait (tries - 1))
  in
  let left = wait 50 in
  if left > 0 then
    ignore (Sys.command (Printf.sprintf "pkill -f '%s'" pat));
  Alcotest.(check int) "nothing it started is left" 0 left

(* A command longer than the system takes in one command line is refused
   with E2BIG. The refusal escaped as a fatal `Unix_error` that `try`
   could not see (#171): on the direct path, through `/bin/sh -c`, under
   `Shell.in_dir`, on stdin and in a stream. Now each is an error the
   script catches. 1.2MB is past the whole limit on macOS (1MB) and past
   the limit for one argument on Linux (128KB). *)
let test_too_long_is_an_error_a_script_catches () =
  let (code, out, err) =
    finished "a command longer than the system limit"
      (run_script
         {|uses {FS.Read, IO, Shell(cat, printf)}
import IO
import List
import Path
import Shell
import Stream
import String
let big = String.repeat 1200000 "x"
let said r =
  match r with
  | Ok _ -> "ran"
  | Error why -> if String.contains? "longer than the system allows" why then "refused" else why
IO.println (said (try $(printf %{big})))
IO.println (said (try $(printf %{big} | cat)))
IO.println (said (try Shell.inspect! (Shell.in_dir (Path.of_string ".") $*(printf %{big}))))
IO.println (said (try ("in" |> $(cat %{big}))))
IO.println (said (try (Shell.stream $*(printf %{big}) |> Stream.to_list)))
IO.println $(printf ok)|})
  in
  Alcotest.(check string) "no fatal error" "" err;
  Alcotest.(check int) "the script ran to the end" 0 code;
  Alcotest.(check string) "each spawn is refused and caught"
    "refused\nrefused\nrefused\nrefused\nrefused\nok\n" out

(* A refused spawn closes the pipes it opened for the command, so a script
   that catches the refusal in a loop does not run out of descriptors. *)
let test_a_refused_spawn_closes_its_pipes () =
  let open_fds () = Array.length (Sys.readdir "/dev/fd") in
  let cmd = "printf " ^ String.make 1_200_000 'x' in
  let refused f =
    match f () with
    | _ -> Alcotest.fail "the spawn was not refused"
    | exception Wand.Evaluator.EvalError _ -> ()
  in
  let before = open_fds () in
  refused (fun () -> ignore (Wand.Runner.spawn_in cmd));
  refused (fun () -> ignore (Wand.Runner.spawn_quiet cmd));
  refused (fun () -> ignore (Wand.Runner.spawn_stdin cmd));
  refused (fun () -> ignore (Wand.Runner.spawn_full cmd));
  refused (fun () -> ignore (Wand.Runner.spawn_argv [| "printf"; String.make 1_200_000 'x' |] ""));
  refused (fun () -> ignore (Wand.Runner.stream_command cmd));
  Alcotest.(check int) "no descriptor is left open" before (open_fds ())

(* `Shell.in_dir` gave its directory to `cd` as it was written. With
   `CDPATH` set, a relative directory was looked up there, and `cd` printed
   where it went into the command's output; `-` went to `$OLDPWD` (#172).
   The directory is now made absolute first, so each spawn runs where
   `Shell.stream` runs it: the directory that the script named, under the
   one wand runs in. *)
let test_in_dir_reads_the_directory_as_written () =
  let base = Filename.temp_dir "wand_in_dir" "" in
  List.iter (fun d -> Sys.mkdir (Filename.concat base d) 0o755)
    ["sub"; "cdp"; "-"];
  Sys.mkdir (Filename.concat (Filename.concat base "cdp") "sub") 0o755;
  let saved_cwd = Sys.getcwd () in
  let saved_env = List.map (fun k -> (k, Sys.getenv_opt k)) ["CDPATH"; "OLDPWD"] in
  let restore () =
    Sys.chdir saved_cwd;
    List.iter (fun (k, v) -> Unix.putenv k (Option.value v ~default:"")) saved_env;
    Domain.DLS.set Wand.Evaluator.ambient_shell_dir None;
    ignore (Sys.command (Printf.sprintf "rm -rf '%s'" base))
  in
  Fun.protect ~finally:restore (fun () ->
    Sys.chdir base;
    let here = Sys.getcwd () in
    Unix.putenv "CDPATH" (Filename.concat here "cdp");
    Unix.putenv "OLDPWD" "/";
    let output_in dir cmd =
      Domain.DLS.set Wand.Evaluator.ambient_shell_dir (Some dir);
      let (pid, r) = Wand.Runner.spawn_in cmd in
      let ic = Unix.in_channel_of_descr r in
      let out = In_channel.input_all ic in
      close_in ic;
      ignore (Wand.Runner.reap pid);
      String.trim out
    in
    let streamed_in dir cmd =
      Domain.DLS.set Wand.Evaluator.ambient_shell_dir (Some dir);
      let (pull, finish) = Wand.Runner.stream_command cmd in
      let line = pull () in
      finish false;
      match line with
      | Some (Wand.Evaluator.VString s) -> s
      | _ -> Alcotest.fail "the stream gave no line"
    in
    let sub = Filename.concat here "sub" in
    Alcotest.(check string) "a direct command" sub (output_in "sub" "/bin/pwd");
    Alcotest.(check string) "a command through sh" sub (output_in "sub" "pwd; true");
    Alcotest.(check string) "a stream" sub (streamed_in "sub" "pwd");
    Alcotest.(check string) "a directory named -"
      (Filename.concat here "-") (output_in "-" "/bin/pwd"))

let () =
  Alcotest.run "Subprocess" [
    "Shell.in_dir", [
      Alcotest.test_case "reads the directory as written" `Quick
        test_in_dir_reads_the_directory_as_written;
    ];
    "a spawn the system refuses", [
      Alcotest.test_case "is an error a script catches" `Quick
        test_too_long_is_an_error_a_script_catches;
      Alcotest.test_case "closes its pipes" `Quick
        test_a_refused_spawn_closes_its_pipes;
    ];
    "neither stream waits on the other", [
      Alcotest.test_case "stderr past the pipe buffer" `Quick
        test_captured_stderr_past_the_buffer;
      Alcotest.test_case "stdin past the pipe buffer" `Quick
        test_stdin_past_the_buffer;
    ];
    "output goes where it is meant to", [
      Alcotest.test_case "a piped command keeps its stderr" `Quick
        test_piped_command_keeps_its_stderr;
      Alcotest.test_case "a closed reader releases first" `Quick
        test_closed_reader_still_releases;
    ];
    "a stream stopped early", [
      Alcotest.test_case "ends the whole command" `Quick
        test_an_early_stop_ends_the_whole_command;
    ];
  ]
