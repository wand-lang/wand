open Evaluator

(* ── Errors ───────────────────────────────────────────────────────────────── *)

(* A stage failure as one structured diagnostic. The functions that still
   answer with a string render it through `Diag.legacy`, so the text the CLI
   prints and the data the JSON and the language server read are the same
   fact. Anything else propagates. *)
let diag_of_exn = function
  | Lexer.LexError (loc, msg)          -> Diag.error ~code:"E-LEX" ~loc msg
  | Parser.ParseError (loc, msg)       -> Diag.error ~code:"E-PARSE" ?loc msg
  | Typechecker.TypeError msg          -> Diag.error ~code:"E-TYPE" msg
  | Typechecker.TypeErrorAt (loc, msg) -> Diag.error ~code:"E-TYPE" ~loc msg
  (* Module loading has a code and a position of its own. Everything it
     refuses is something a person wrote at an import site, so it reports
     like a type error rather than like an internal failure. *)
  | Module_types.ImportError msg       -> Diag.error ~code:"E-IMPORT" msg
  | Module_types.ImportErrorAt (loc, msg) -> Diag.error ~code:"E-IMPORT" ~loc msg
  | Failure msg                        -> Diag.error ~code:"E-FAIL" msg
  | e -> raise e

let legacy_of_exn e = Diag.legacy (diag_of_exn e)

(* ── Default effect handlers ──────────────────────────────────────────────── *)

(* Read what a child writes without ever waiting on one pipe while it waits
   on another. Draining stdout to the end and only then reading stderr is a
   deadlock: the child fills the pipe wand is not reading, blocks on that
   write, and so never reaches the end of the pipe wand is waiting on. A
   megabyte of stderr is enough. Writing the child's stdin has the same
   shape -- a command that answers as it reads fills stdout while wand is
   still writing -- so all three move together, in blocks, driven by
   whichever is ready. *)

let close_noerr fd = try Unix.close fd with Unix.Unix_error _ -> ()

(* A signal arriving mid-call is a reason to look again, not to give up on
   output the child has already written. A timeout of -1 waits for as long
   as it takes; a deadline supplies a slice instead. *)
let rec select_ready ?(timeout = -1.0) reads writes =
  try Unix.select reads writes [] timeout
  with Unix.Unix_error (Unix.EINTR, _, _) -> select_ready ~timeout reads writes

let rec read_chunk fd buf =
  try Unix.read fd buf 0 (Bytes.length buf)
  with Unix.Unix_error (Unix.EINTR, _, _) -> read_chunk fd buf

(* Moves everything and closes every descriptor it is given, so what is
   left to do afterwards is the wait. `out` and `err` are the ends wand
   reads; `into` is the child's stdin, absent when the child inherits
   wand's. *)
(* How long a command may run, and what to do when it has run that long.
   `Shell.timeout` supplies one; every other spawn passes none and waits for
   as long as the command takes.

   The deadline is counted in slices rather than measured against a clock.
   Each slice is a fresh `select` timeout, so nothing here reads the time,
   and a machine that steps its clock mid-command cannot shorten or extend
   the deadline.

   Expiry is a sequence, not an event: SIGTERM, then a fixed grace, then
   SIGKILL. A command that catches SIGTERM and tidies up gets to; one that
   ignores it does not get to keep running. The grace is fixed and not a
   second parameter -- a caller who wants to think about TERM against KILL
   is a caller who should be writing the signal handling out. *)
type deadline = {
  budget : float;
  grace  : float;
  kill   : int -> unit;
  (* Whether the child has already exited. The grace exists to let a child
     tidy up after SIGTERM; once it is gone there is nothing to wait for,
     and waiting anyway means serving out the grace for a pipe that a
     grandchild is holding. *)
  gone   : unit -> bool;
}

let pump ?(stdin = "") ?out ?err ?into ?deadline () =
  let chunk = Bytes.create 65536 in
  let out_buf = Buffer.create 65536 and err_buf = Buffer.create 65536 in
  let reading = ref (List.filter_map Fun.id [out; err]) in
  let writing =
    ref (match into with
         | None -> []
         | Some fd ->
           if stdin = "" then (close_noerr fd; [])
           else (Unix.set_nonblock fd; [fd]))
  in
  let sent = ref 0 in
  let total = String.length stdin in
  let stop_reading fd =
    close_noerr fd;
    reading := List.filter (fun f -> f <> fd) !reading
  in
  let stop_writing fd = close_noerr fd; writing := [] in
  let slice = 0.05 in
  (* `None` once the deadline has run its course, so the loop then waits for
     the killed child's pipes to close and no longer counts anything. *)
  let remaining = ref (Option.map (fun d -> d.budget) deadline) in
  let stage = ref `Running in
  let expired = ref false in
  (* A killed child can still have children of its own holding the pipes
     open -- `sh -c "sleep 30"` leaves the sleep. Once the sequence has run
     to SIGKILL, waiting for end-of-file would be waiting for a process
     nobody asked about, so the loop stops and the answer is the timeout. *)
  let giving_up () = !stage = `Killed in
  while (!reading <> [] || !writing <> []) && not (giving_up ()) do
    let timeout =
      match !remaining with
      | None -> -1.0
      | Some left -> if left < slice then left else slice
    in
    let (ready_r, ready_w, _) = select_ready ~timeout !reading !writing in
    (match !remaining, deadline with
     | Some _, Some d when !stage = `Terminated && d.gone () ->
       (* Signalled, and already exited: whatever still holds the pipes is
          not the command wand started. *)
       stage := `Killed; remaining := None
     | Some left, Some d when ready_r = [] && ready_w = [] ->
       let left = left -. timeout in
       if left > 0.0 then remaining := Some left
       else begin
         expired := true;
         match !stage with
         | `Running -> d.kill Sys.sigterm; stage := `Terminated;
                       remaining := Some d.grace
         | `Terminated -> d.kill Sys.sigkill; stage := `Killed;
                          remaining := None
         | `Killed -> remaining := None
       end
     | _ -> ());
    if giving_up () then () else begin
    List.iter (fun fd ->
      let n = read_chunk fd chunk in
      if n = 0 then stop_reading fd
      else Buffer.add_subbytes
             (if Some fd = out then out_buf else err_buf) chunk 0 n)
      ready_r;
    List.iter (fun fd ->
      match Unix.single_write_substring fd stdin !sent
              (min 65536 (total - !sent)) with
      | n -> sent := !sent + n; if !sent >= total then stop_writing fd
      | exception Unix.Unix_error
          ((Unix.EAGAIN | Unix.EWOULDBLOCK | Unix.EINTR), _, _) -> ()
      (* A child that stopped reading -- `head`, or one that failed -- has
         had what it took. That is the command's business to report through
         its exit status, not an error in the writer. *)
      | exception Unix.Unix_error (Unix.EPIPE, _, _) -> stop_writing fd)
      ready_w
    end
  done;
  List.iter close_noerr !reading;
  (match !writing with [fd] -> close_noerr fd | _ -> ());
  (Buffer.contents out_buf, Buffer.contents err_buf, !expired)

(* ── Children ─────────────────────────────────────────────────────────────── *)

(* Every process wand starts, so that stopping wand stops them too. A command
   left running after the script that started it has gone is the bash failure
   this language exists to avoid, and it is worse here: the script's own
   cleanup may be waiting on a process nobody is watching any more.

   Held in an atomic list rather than behind a mutex because the signal
   handler reads it, and a handler that blocked on a lock the interrupted
   code was already holding would never return. *)
let children : int list Atomic.t = Atomic.make []

let rec remember pid =
  let old = Atomic.get children in
  if not (Atomic.compare_and_set children old (pid :: old)) then remember pid

let rec forget pid =
  let old = Atomic.get children in
  let now = List.filter (fun p -> p <> pid) old in
  if not (Atomic.compare_and_set children old now) then forget pid

(* Stop every process wand started. Failures are ignored on purpose: a
   child that has already exited is exactly the case this is racing with. *)
let stop_children signal =
  List.iter (fun pid -> try Unix.kill pid signal with Unix.Unix_error _ -> ())
    (Atomic.get children)

(* A command with nothing shell-special in it is exec'd directly rather
   than through `/bin/sh -c` -- make's optimization, decided by
   `Shell_scan.direct_words`, worth ~5ms per spawn on macOS where /bin/sh
   is bash. Semantics stay the shell's: `create_process` searches PATH as
   sh would, and a spawn the direct path cannot make -- the program
   missing being the common case -- is retried through sh, which reports
   it exactly as it always has (its own line on stderr, exit 127) instead
   of surfacing a Unix_error the sh path never raised. *)
let create_process_for cmd stdin stdout stderr =
  (* A wand String is a byte string, and a NUL can arrive in one from a
     command's output, a file read or `Base64.decode!`. A command line
     cannot carry one: the kernel takes it as the end of the argument, so
     `create_process` refuses the whole call with EINVAL. The refusal used
     to escape as a fatal error -- neither `try` nor `$?()` could see it.
     Asked here, it is an ordinary raise the caller can catch. The quoting
     is not at fault: the byte is rejected, never truncated. *)
  if String.contains cmd '\000' then
    raise (Evaluator.EvalError
      "this command holds a NUL byte, which no command line can carry -- \
       take it out of the value before splicing it in");
  let via_sh () =
    Unix.create_process "/bin/sh" [| "/bin/sh"; "-c"; cmd |]
      stdin stdout stderr
  in
  match Shell_scan.direct_words cmd with
  | Some (w0 :: _ as ws) ->
    (try Unix.create_process w0 (Array.of_list ws) stdin stdout stderr
     with Unix.Unix_error _ -> via_sh ())
  | _ -> via_sh ()

(* The ends wand keeps are close-on-exec, or one command's pipe would be
   inherited by the next command's process: with several running at once,
   a child would hold another's write end open and the reader would wait
   for an end-of-file that never came. The ends handed to create_process
   are duplicated onto the child's stdio, which clears the flag. *)
let spawn_in cmd =
  let (r, w) = Unix.pipe ~cloexec:true () in
  let pid = create_process_for cmd Unix.stdin w Unix.stderr in
  Unix.close w;
  remember pid;
  (pid, r)

(* Output nobody will look at goes to /dev/null rather than down a pipe wand
   then has to keep emptying: the child writes as fast as it likes and wand
   has one thing to wait for. *)
let spawn_quiet cmd =
  let devnull = Unix.openfile "/dev/null" [Unix.O_WRONLY; Unix.O_CLOEXEC] 0o666 in
  let pid = create_process_for cmd Unix.stdin devnull Unix.stderr in
  Unix.close devnull;
  remember pid;
  pid

(* Stdin piped, stderr the caller's own -- what `$()` does, which is what a
   command on the right of `|>` should do too: a command that explains its
   failure on stderr should be heard, not swallowed for having been given
   input. *)
let spawn_stdin cmd =
  let (out_r, out_w) = Unix.pipe ~cloexec:true () in
  let (in_r,  in_w)  = Unix.pipe ~cloexec:true () in
  let pid = create_process_for cmd in_r out_w Unix.stderr in
  Unix.close out_w; Unix.close in_r;
  remember pid;
  (pid, out_r, in_w)

let spawn_full cmd =
  let (out_r, out_w) = Unix.pipe ~cloexec:true () in
  let (err_r, err_w) = Unix.pipe ~cloexec:true () in
  let (in_r,  in_w)  = Unix.pipe ~cloexec:true () in
  let pid = create_process_for cmd in_r out_w err_w in
  Unix.close out_w; Unix.close err_w; Unix.close in_r;
  remember pid;
  (pid, out_r, err_r, in_w)

(* A status the deadline's `gone` check already took, so `reap` does not
   wait for a child that has been waited for. *)
let rec reap_or pid reaped =
  match !reaped with
  | Some status -> forget pid; status
  | None -> reap pid

(* Called once the pipes are drained and closed, so the child is not waiting
   on a reader that has gone away. *)
and reap pid =
  match Unix.waitpid [] pid with
  | (_, status) -> forget pid; status
  | exception Unix.Unix_error (Unix.EINTR, _, _) -> reap pid

(* OCaml numbers signals with its own negative constants, so the raw value
   in a message means nothing to anyone reading it. *)
let signal_name n =
  if n = Sys.sigterm then "SIGTERM" else if n = Sys.sigint then "SIGINT"
  else if n = Sys.sigkill then "SIGKILL" else if n = Sys.sigsegv then "SIGSEGV"
  else if n = Sys.sighup then "SIGHUP" else if n = Sys.sigpipe then "SIGPIPE"
  else if n = Sys.sigquit then "SIGQUIT" else if n = Sys.sigabrt then "SIGABRT"
  else Printf.sprintf "signal %d" n

(* A command that died because wand is stopping is not the script's failure
   to report -- wand killed it on the way out. Surfacing it would put a
   confusing error in front of the reason the script is ending. *)
let died_from_our_own_stop () = Atomic.get Evaluator.interrupt_requested <> 0

let command_signalled cmd n =
  if died_from_our_own_stop () then
    raise (Evaluator.Interrupted (Atomic.get Evaluator.interrupt_requested))
  else
    raise (EvalError (Printf.sprintf "command killed by %s: %s" (signal_name n) cmd))

(* Running a command can end the program as well as fail it: wand kills its
   children when it is stopping, and the command's death arrives here. Both
   travel back through the continuation, so the body unwinds and releases
   what it holds -- raising here instead would abandon it. *)
let attempt f =
  try Ok (f ()) with
  | EvalError _ as e -> Error e
  | Evaluator.Interrupted _ as e -> Error e

(* A sink over an open file: one line, then a newline, and close at the end.

   Committing and aborting are the same call here, and that is not an
   oversight. These lines go straight into the target, so a stream that
   raised half way through has already left a half-written file on the disk;
   there is nothing to take back, and closing is all either ending has to
   do. The two endings differ only for a sink that publishes at the end. *)
let channel_sink oc =
  let close () = close_out_noerr oc in
  VLineSink ((fun line -> output_string oc line; output_char oc '\n'),
             close, close)

let strip_trailing_newline s =
  let n = String.length s in
  let i = ref n in
  while !i > 0 && s.[!i - 1] = '\n' do decr i done;
  String.sub s 0 !i

(* The deadline the current `Shell.timeout` set, if any, turned into what
   `pump` wants. Fixed grace: five seconds is long enough for a shell to
   flush and exit, and short enough not to double the wait. *)
let timeout_grace = 5.0

let deadline_for pid reaped =
  match Domain.DLS.get Evaluator.shell_deadline with
  | None -> None
  | Some ms ->
    Some { budget = float_of_int ms /. 1000.;
           grace  = timeout_grace;
           kill   = (fun signal ->
             try Unix.kill pid signal with Unix.Unix_error _ -> ());
           gone   = (fun () ->
             match !reaped with
             | Some _ -> true
             | None ->
               (match Unix.waitpid [Unix.WNOHANG] pid with
                | (0, _) -> false
                | (_, status) -> reaped := Some status; true
                | exception Unix.Unix_error _ -> true)) }

(* A command that ran out of time. `Shell.timeout` turns it into an `Error`;
   anywhere else it is an ordinary raise, which is what a deadline nobody
   set can never produce. *)
let timed_out cmd =
  let ms = match Domain.DLS.get Evaluator.shell_deadline with
    | Some ms -> ms | None -> 0 in
  raise (EvalError (Printf.sprintf "%s: %s" Evaluator.timeout_prefix
    (Printf.sprintf "timed out after %s: %s"
       (Evaluator.format_dur_ms ms) cmd)))

let exec_command cmd =
  let (pid, out_r) = spawn_in cmd in
  let reaped = ref None in
  let (output, _, expired) =
    pump ~out:out_r ?deadline:(deadline_for pid reaped) () in
  let status = reap_or pid reaped in
  if expired then timed_out cmd;
  let output = strip_trailing_newline output in
  match status with
  | Unix.WEXITED 0   -> output
  | Unix.WEXITED n   -> raise (EvalError (Printf.sprintf "command exited with code %d: %s" n cmd))
  | Unix.WSIGNALED n -> command_signalled cmd n
  | Unix.WSTOPPED  n -> raise (EvalError (Printf.sprintf "command stopped by signal %d: %s" n cmd))

(* A command read as it goes, for `Shell.stream`. What the caller gets back
   is a puller over the child's stdout and a stop; the child outlives
   neither.

   Stopping it is the pattern `Shell.timeout` already uses: SIGTERM, then
   SIGKILL after the fixed five-second grace. The read end is closed first,
   so a command that is still writing takes EPIPE and usually goes on its
   own -- which is how `head` stops `yes`. wand signals the command it
   started and nothing below it: `sh -c "tail -f x"` leaves the `tail`
   behind, exactly as `Shell.timeout` does.

   The exit status is read only when the stream ran out on its own. A
   command wand killed because a `take` was satisfied did not fail -- wand
   ended it -- so its status is wand's own signal coming back, and reporting
   it would turn the ordinary early stop into an error. *)
let stream_command cmd =
  let (pid, out_r) = spawn_in cmd in
  let ic = Unix.in_channel_of_descr out_r in
  let pull () =
    match In_channel.input_line ic with
    | Some line -> Some (Evaluator.VString line)
    | None -> None
    (* The child was killed and the pipe went with it. That is the early
       stop arriving from the other end, not a line. *)
    | exception Sys_error _ -> None
  in
  let waited = ref false in
  let wait_for_it () =
    match Unix.waitpid [] pid with
    | (_, status) -> forget pid; Some status
    | exception Unix.Unix_error (Unix.EINTR, _, _) -> None
    | exception Unix.Unix_error _ -> forget pid; None
  in
  let rec wait_until deadline =
    match Unix.waitpid [Unix.WNOHANG] pid with
    | (0, _) ->
      if Unix.gettimeofday () < deadline then begin
        (* Nothing to read and nothing to write: this is a sleep spelled
           with what is already imported. *)
        (try ignore (Unix.select [] [] [] 0.02) with Unix.Unix_error _ -> ());
        wait_until deadline
      end else begin
        (try Unix.kill pid Sys.sigkill with Unix.Unix_error _ -> ());
        ignore (wait_for_it ())
      end
    | (_, _) -> forget pid
    | exception Unix.Unix_error (Unix.EINTR, _, _) -> wait_until deadline
    | exception Unix.Unix_error _ -> forget pid
  in
  let finish early =
    if not !waited then begin
      waited := true;
      close_in_noerr ic;
      if early then begin
        (try Unix.kill pid Sys.sigterm with Unix.Unix_error _ -> ());
        wait_until (Unix.gettimeofday () +. timeout_grace)
      end else
        match wait_for_it () with
        | Some (Unix.WEXITED 0) | None -> ()
        | Some (Unix.WEXITED n) ->
          raise (EvalError
            (Printf.sprintf "command exited with code %d: %s" n cmd))
        | Some (Unix.WSIGNALED n) -> command_signalled cmd n
        | Some (Unix.WSTOPPED n) ->
          raise (EvalError
            (Printf.sprintf "command stopped by signal %d: %s" n cmd))
    end
  in
  (pull, finish)

let exec_command_quiet cmd =
  match reap (spawn_quiet cmd) with
  | Unix.WEXITED 0   -> ()
  | Unix.WEXITED n   -> raise (EvalError (Printf.sprintf "command exited with code %d: %s" n cmd))
  | Unix.WSIGNALED n -> command_signalled cmd n
  | Unix.WSTOPPED  n -> raise (EvalError (Printf.sprintf "command stopped by signal %d: %s" n cmd))

let exec_command_exit_code cmd =
  match reap (spawn_quiet cmd) with
  | Unix.WEXITED n   -> n
  | Unix.WSIGNALED _ -> 128
  | Unix.WSTOPPED  _ -> 128

let exec_command_stdin cmd stdin =
  let (pid, out_r, in_w) = spawn_stdin cmd in
  let reaped = ref None in
  let (stdout, _, expired) =
    pump ~stdin ~out:out_r ~into:in_w ?deadline:(deadline_for pid reaped) () in
  let stdout = strip_trailing_newline stdout in
  let status = reap_or pid reaped in
  if expired then timed_out cmd;
  match status with
  | Unix.WEXITED 0   -> stdout
  | Unix.WEXITED n   -> raise (EvalError (Printf.sprintf "command exited with code %d: %s" n cmd))
  | Unix.WSIGNALED n -> command_signalled cmd n
  | Unix.WSTOPPED  n -> raise (EvalError (Printf.sprintf "command stopped by signal %d: %s" n cmd))

let capture ?(stdin = "") cmd =
  let (pid, out_r, err_r, in_w) = spawn_full cmd in
  let reaped = ref None in
  let (stdout, stderr, expired) =
    pump ~stdin ~out:out_r ~err:err_r ~into:in_w
      ?deadline:(deadline_for pid reaped) () in
  if expired then timed_out cmd;
  let code = match reap_or pid reaped with
    | Unix.WEXITED n   -> n
    | Unix.WSIGNALED _ -> 128
    | Unix.WSTOPPED  _ -> 128
  in
  (strip_trailing_newline stdout, stderr, code)

let exec_command_full cmd = capture cmd

let exec_command_full_stdin cmd stdin = capture ~stdin cmd

let shell_result stdout stderr code =
  VConstr (Ctor.Builtin "ShellResult", [VString stdout; VString stderr; VInt code])

(* ── Rehearsal and tracing ────────────────────────────────────────────────── *)

type mode = Normal | Trace | DryRun

(* The mode a program is running under. Workers spawned by Par install the
   same handlers on their own domain: an effect performed on one domain does
   not reach a handler on another, so a worker without them would either
   escape a rehearsal or fail outright. *)
let current_mode = ref Normal

(* Reports come from several domains at once, so a line is written whole
   rather than interleaved with another worker's. *)
let report_lock = Mutex.create ()

let report fmt =
  Printf.ksprintf (fun line ->
    Mutex.lock report_lock;
    prerr_string line;
    flush stderr;
    Mutex.unlock report_lock) fmt

(* How an operation reads in a report. Every operation name a user sees comes
   through here, so what they are called is one decision in one place rather
   than a vocabulary spread through the output. *)
(* The method a request asks with, as the wire spells it. *)
let method_name = function
  | VConstr (c, _) -> String.uppercase_ascii (Ctor.name c)
  | _ -> "GET"

let describe_operation name (v : value) =
  (* A report is read to judge blast radius, so each operation shows the one
     argument that decides it -- the command, the path, the variable -- and a
     size where the content matters but its text does not. *)
  let text = function
    | VString s | VPath s -> s
    | other -> to_text other
  in
  let first = function
    | VTuple (a :: _) -> text a
    | other -> text other
  in
  let with_size v = match v with
    | VTuple [target; VString contents] ->
      Printf.sprintf "%s (%d bytes)" (text target) (String.length contents)
    | other -> text other
  in
  let pair = function
    | VTuple [a; b] -> text a ^ " -> " ^ text b
    | other -> text other
  in
  match name with
  | "Shell!run" | "Shell!run_quiet" | "Shell!capture" | "Shell!exit_code"->
    Some ("run", first v)
  | "Shell!stream" -> Some ("read the output of", first v)
  | "FS!write_file"   -> Some ("write", with_size v)
  | "FS!write_atomic" -> Some ("write atomically", with_size v)
  | "FS!append"    -> Some ("append to", with_size v)
  | "FS!write_lines"  -> Some ("write lines to", text v)
  | "FS!write_lines_atomic" -> Some ("write lines atomically to", text v)
  | "FS!append_lines" -> Some ("append lines to", text v)
  | "FS!create_file"    -> Some ("create", text v)
  | "FS!delete"    -> Some ("delete", text v)
  | "FS!mkdir"   -> Some ("create directory", text v)
  | "FS!rename"    -> Some ("rename", pair v)
  | "FS!copy"      -> Some ("copy", pair v)
  | "FS!temp_file" -> Some ("create temp file", first v)
  | "FS!temp_dir"  -> Some ("create temp directory", text v)
  (* A lock is not withheld by a rehearsal, so these only ever report a
     real one -- but taking a guard is exactly the kind of thing `--trace`
     exists to show, and a trace that showed the deploy and not the lock
     around it would be reporting the smaller half. *)
  (* A request reports the method and the URL, because those are the two
     things a reader of a plan wants: what it would do, and to whom. *)
  | "Net!http" ->
    (match v with
     | VConstr (_, (VURL (url, _) | VString url) :: meth :: _) ->
       Some (String.lowercase_ascii (method_name meth), url)
     | other -> Some ("request", text other))
  | "Net!download" ->
    (match v with
     | VTuple [(VURL (url, _) | VString url); dest] ->
       Some ("download", url ^ " -> " ^ text dest)
     | other -> Some ("download", text other))
  | "FS!lock"      -> Some ("lock", text v)
  | "FS!lock_wait" -> Some ("lock, waiting up to", pair v)
  | "FS!unlock"    -> Some ("release the lock on", text v)
  | "FS!delete_tree" -> Some ("delete recursively", text v)
  | "FS!copy_tree" -> Some ("copy recursively", pair v)
  | "Env!set"      -> Some ("set", pair v)
  | "Env!clear"    -> Some ("clear", text v)
  | "FS!read_file"    -> Some ("read", text v)
  | "Hash!file"    -> Some ("hash", pair v)
  | "FS!list_dir"        -> Some ("list", text v)
  | "FS!glob"      -> Some ("glob", first v)
  | "Proc!exit"         -> Some ("exit", text v)
  | "Clock!sleep"       -> Some ("wait", text v)
  | _              -> None

(* One file onto another. A copy of an executable is executable, and a copy
   of a private file is private: the destination is created with the
   source's permissions rather than the channel default, which turned 0600
   into 0644 and dropped the bit that made a copied script runnable. A
   destination that already exists keeps its own permissions -- what `cp`
   does, and the copy is not the place to widen a file somebody else's mode
   was chosen for. *)
let copy_file src dst =
  let mode = (Unix.stat src).Unix.st_perm in
  let existed = Sys.file_exists dst in
  (* A block at a time. The whole file used to be read into a string first,
     so copying a file took the file's size in memory -- and `FS.copy` is
     what a script reaches for on the large ones. *)
  In_channel.with_open_bin src (fun ic ->
    Out_channel.with_open_gen
      [Open_wronly; Open_creat; Open_trunc; Open_binary] mode dst
      (fun oc ->
        let buf = Bytes.create 65536 in
        let rec go () =
          let n = In_channel.input ic buf 0 (Bytes.length buf) in
          if n > 0 then (Out_channel.output oc buf 0 n; go ())
        in
        go ()));
  (* The open honours the umask, which can only take bits away; a copy is
     meant to carry the source's own mode, so a new file is set to it
     outright. *)
  if not existed then Unix.chmod dst mode

(* Taking a name back out of the environment. OCaml's Unix has `putenv` and
   no inverse, so this is a C stub -- see lib/ext/env.c for why the empty
   string is not the same answer. *)
external unsetenv : string -> unit = "wand_unsetenv"

(* A private directory under a name nobody else can hold first, in one step.
   See lib/ext/tempdir.c for the window this closes. *)
external mkdtemp : string -> string = "wand_mkdtemp"

(* Walking a tree by descriptor instead of by path. See lib/ext/dirfd.c. *)
external openat_dir :
  Unix.file_descr -> string -> Unix.file_descr option = "wand_openat_dir"
external readdir_fd : Unix.file_descr -> string list = "wand_readdir_fd"
external unlinkat : Unix.file_descr -> string -> bool -> unit = "wand_unlinkat"

(* A directory and everything under it, depth first.

   Every step names an entry relative to a directory this holds open, so a
   name that is replaced after it has been looked at cannot redirect the next
   step. The path-based form asked three separate questions about one
   name -- `lstat` said "directory", `readdir` listed it, `rmdir` removed
   it -- with nothing tying the three answers to the same object, so
   something able to write in the tree could swap a directory for a symlink
   between two of them and have the deletions land wherever it pointed.
   `rm -rf` walks this way too; fts(3) does it by changing directory, which
   is not open to wand because the cwd belongs to the process and `Par` runs
   work on other domains.

   A symlink is unlinked and never descended into -- `O_NOFOLLOW` is what
   decides that here, where an `lstat` decided it before -- so the tree is
   still the only thing that goes.

   It holds one descriptor per level of depth, which the path form did not.
   That is the cost of the trade, and it is small: 600 levels deep is fine
   under an ordinary limit, and only a limit cut to 64 refuses it. A tree
   deep enough to run out stops with `Too many open files`, because
   `openat_dir` answers None for "not a directory" and raises for everything
   else -- read the other way it would have tried to unlink a directory and
   reported EISDIR, which says nothing about what went wrong. *)
let delete_tree path =
  let parent = Filename.dirname path and name = Filename.basename path in
  match Unix.openfile parent [Unix.O_RDONLY; Unix.O_CLOEXEC] 0 with
  (* No parent, so nothing under it either. Deleting what is not there has
     always been success. *)
  | exception Unix.Unix_error ((Unix.ENOENT | Unix.ENOTDIR), _, _) -> ()
  | parent_fd ->
    let close fd = try Unix.close fd with Unix.Unix_error _ -> () in
    Fun.protect ~finally:(fun () -> close parent_fd) (fun () ->
      let rec rm dir_fd entry =
        match openat_dir dir_fd entry with
        (* A file, a symlink, or already gone. *)
        | None -> unlinkat dir_fd entry false
        | Some fd ->
          Fun.protect ~finally:(fun () -> close fd)
            (fun () -> List.iter (rm fd) (readdir_fd fd));
          unlinkat dir_fd entry true
      in
      rm parent_fd name)

(* ── Locks ─────────────────────────────────────────────────────────────── *)

external flock_try : Unix.file_descr -> int * string = "wand_flock_try"

(* Which paths this process holds, and the descriptor holding each.

   The guard against this process's own second acquire is `flock` itself,
   not this table. A lock belongs to the open file description rather than
   to the process, and each acquire does its own `open`, so a second `Par`
   worker -- or a nested bracket in the same worker -- holds a distinct
   description and conflicts with the first exactly as another process
   would. Disabling the check below leaves every lock test passing, which is
   how that was established rather than assumed.

   What the table is for is the descriptor: closing it is what releases the
   lock, so a release has to be able to find the one its acquire took. The
   check in front of `flock` is then a fast path that saves a syscall, and
   the domain it records is how a wait tells a nested self-hold -- which can
   never be released -- from another worker, which will release.

   The key is the resolved path, because `/var/run/x`, `./x` from that
   directory and a path through a symlinked parent are one file, and a table
   keyed on the spelling would miss its own entry. `Unix.realpath` answers
   it, which is why the file is created before the table is consulted.

   A lock is not re-entrant. A `with FS.lock p` inside another one on the
   same path fails with `Held`, which is what any other process is told, and
   is a nesting mistake rather than a thing to permit. *)
let held_locks : (string, Unix.file_descr * int) Hashtbl.t = Hashtbl.create 8
let held_locks_mutex = Mutex.create ()

let with_held_locks f =
  Mutex.lock held_locks_mutex;
  Fun.protect ~finally:(fun () -> Mutex.unlock held_locks_mutex) f

(* Take the lock on `path`, or answer that it is held. Anything else -- the
   directory is not there, the file cannot be opened -- raises, because it
   is a broken script rather than a busy one, and a caller that exits 0 on
   "someone else is running" must not exit 0 on "this never worked".

   The lock file is created if it is missing and is never removed. Unlinking
   it is the race that makes a lock useless: another process may already
   have opened it and be about to lock it, and after the unlink the two hold
   locks on two inodes with one name. The empty file left behind is
   harmless. *)
let take_lock path =
  let fd =
    (* Close-on-exec, as every other descriptor wand opens is. A flock
       belongs to the open file description rather than to the process, so a
       copy inherited by a child keeps the lock after wand has exited and
       released it -- until the child dies. That defeats the cron guard this
       is for: a script that starts a background process while holding the
       lock left the next run reporting `Held`, with nothing holding it. *)
    try Unix.openfile path [Unix.O_RDWR; Unix.O_CREAT; Unix.O_CLOEXEC] 0o644
    with Unix.Unix_error (e, f, _) ->
      raise (EvalError (Printf.sprintf "lock: %s: %s" f (Unix.error_message e)))
  in
  let close () = try Unix.close fd with Unix.Unix_error _ -> () in
  let key =
    try Unix.realpath path
    with Unix.Unix_error _ -> path
  in
  with_held_locks (fun () ->
    if Hashtbl.mem held_locks key then (close (); None)
    else
      match flock_try fd with
      | 0, _ ->
        Hashtbl.replace held_locks key (fd, (Domain.self () :> int));
        Some key
      | 1, _ -> close (); None
      | _, why ->
        close ();
        raise (EvalError (Printf.sprintf "lock: %s: %s" path why)))

(* Wait up to `budget_ms` for the lock, then give up and answer `Held`.

   A poll rather than a blocking `flock`, because `flock` has no timeout and
   the alternative is `SIGALRM` around a blocking call. A signal is delivered
   to the process rather than to the domain that armed it, so under `Par` an
   alarm meant for one worker can land on another -- an expensive bug for a
   feature whose whole job is to be dependable. Fifty milliseconds of latency
   after a release costs nothing; twelve hundred syscalls over a minute costs
   nothing either.

   A wait that expires is `Held`, not a case of its own. "I waited and
   somebody still has it" tells a caller what `Held` tells it -- stand down
   -- and a new constructor would break the exhaustiveness of every match
   already written against `LockError`.

   A `Denied` stops the wait at once. A directory that is not there will not
   appear because we polled it, and a broken script should not spend the
   budget finding that out. *)
let poll_ms = 50

let take_lock_wait path budget_ms =
  let deadline = Unix.gettimeofday () +. (float_of_int budget_ms /. 1000.) in
  let rec attempt () =
    (* Asked afresh each time round: the entry that matters is the one there
       now, and a self-hold cannot appear part way through a wait. *)
    let self_held =
      with_held_locks (fun () ->
        match Hashtbl.find_opt held_locks (try Unix.realpath path with Unix.Unix_error _ -> path) with
        | Some (_, owner) -> owner = (Domain.self () :> int)
        | None -> false)
    in
    (* A wait on a lock this bracket already holds can never end, so it is
       answered now rather than after the budget. Another worker holding it
       is the opposite: it will be released, which is what waiting is for. *)
    if self_held then None
    else
      match take_lock path with
      | Some key -> Some key
      | None ->
        let left = deadline -. Unix.gettimeofday () in
        if left <= 0. then None
        else begin
          ignore (Unix.select [] [] [] (min (float_of_int poll_ms /. 1000.) left));
          attempt ()
        end
  in
  attempt ()

(* Closing the descriptor is what releases the lock, so there is one place
   the lock can be given back and it is this one. A key the table does not
   know is not an error: a release runs however the bracket ended, including
   after an acquire that never took anything. *)
let release_lock key =
  with_held_locks (fun () ->
    match Hashtbl.find_opt held_locks key with
    | None -> ()
    | Some (fd, _) ->
      Hashtbl.remove held_locks key;
      (try Unix.close fd with Unix.Unix_error _ -> ()))

(* What a rehearsal withholds. Reads run even in a rehearsal, so that
   control flow follows the path a real run would take; a change is
   withheld and reported instead.

   A sleep changes nothing and is withheld anyway. A rehearsal of a deploy
   that retries with backoff would otherwise take the backoff, and nobody
   waits an hour to be told what a script would do. `--trace` is a real run
   and sleeps for real. *)
(* A request whose method is not safe to repeat. `--dry-run` follows the
   filesystem rule -- reads go through, writes are held and reported -- and
   the protocol already draws that line for us: `GET` and `HEAD` are the
   methods defined not to change anything, so a rehearsal runs them and
   reports everything else.

   Withholding every request instead would make a rehearsal useless on the
   scripts that need one most: a deploy that fetches its configuration
   before it posts would take a branch a real run never takes. Sending
   everything is not a rehearsal. *)
let unsafe_request = function
  | VConstr (_, _ :: meth :: _) ->
    (match method_name meth with "GET" | "HEAD" -> false | _ -> true)
  | _ -> false

let is_mutation_value name v =
  match name with
  | "Net!http" -> unsafe_request v
  (* A download writes a file, so it is withheld like any other write. *)
  | "Net!download" -> true
  | _ -> false

let is_mutation = function
  | "Clock!sleep"
  | "Shell!run" | "Shell!run_quiet" | "Shell!capture" | "Shell!exit_code" | "Shell!stream" | "FS!write_lines" | "FS!write_lines_atomic" | "FS!append_lines" | "FS!write_file" | "FS!write_atomic" | "FS!append" | "FS!create_file" | "FS!delete" | "FS!mkdir" | "FS!rename" | "FS!copy" | "FS!temp_file" | "FS!temp_dir" | "FS!delete_tree" | "FS!copy_tree" | "Env!set" | "Env!clear"-> true
  | _ -> false

(* What an operation hands back when it is reported instead of carried out.
   Whatever this is steers the rest of the script, so a rehearsal says what it
   substituted rather than letting the script appear to have real output. *)
(* A rehearsal answers a temp-file request with a name rather than a file,
   and the name has to be one nobody else can hold first. `/tmp/wand-dry-run-dir`
   was the same path every time, in a directory every user on the machine can
   write: anyone could create it, or a symlink under it, and wait -- a script
   reading back what it believes it just created would read what was left
   there instead. Eight bytes of randomness per call, and the temp directory
   the environment names rather than `/tmp` outright, which on macOS is
   already a private one. *)
let random_tag () =
  let bytes = Bytes.create 8 in
  (try
     let fd = Unix.openfile "/dev/urandom" [Unix.O_RDONLY; Unix.O_CLOEXEC] 0 in
     Fun.protect ~finally:(fun () -> try Unix.close fd with Unix.Unix_error _ -> ())
       (fun () -> ignore (Unix.read fd bytes 0 (Bytes.length bytes)))
   with Unix.Unix_error _ | Sys_error _ ->
     (* Without /dev/urandom the name is merely unlikely to collide, which
        is the most this can offer. *)
     Bytes.blit_string
       (Printf.sprintf "%08x" (Hashtbl.hash (Unix.gettimeofday (), Unix.getpid ())))
       0 bytes 0 8);
  String.concat ""
    (List.init (Bytes.length bytes)
       (fun i -> Printf.sprintf "%02x" (Char.code (Bytes.get bytes i))))

let dry_run_path suffix =
  Filename.concat (Filename.get_temp_dir_name ())
    (Printf.sprintf "wand-dry-run-%s%s" (random_tag ()) suffix)

(* The path a write lands on. `FS.write_file` writes through a symlink,
   because it opens and truncates whatever the link resolves to, and an
   atomic write has to target the same file or two functions that differ
   only in atomicity would differ in what they write to.

   A link that points nowhere resolves to the name it points at, which is
   the file a write through it would create. The depth bound is what stops
   a loop -- `a -> b -> a` -- from spinning here forever. *)
let rec resolve_link ?(depth = 0) p =
  if depth > 40 then failwith "too many levels of symbolic link"
  else
    match Unix.lstat p with
    | { Unix.st_kind = Unix.S_LNK; _ } ->
      let target = Unix.readlink p in
      let target =
        if Filename.is_relative target
        then Filename.concat (Filename.dirname p) target
        else target
      in
      resolve_link ~depth:(depth + 1) target
    | _ -> p
    | exception Unix.Unix_error _ -> p

(* Write `content` to `path` so that no reader ever sees it half-written.

   The mode: an existing target's own, set outright so the umask cannot trim
   it; a new file's is 0644 through the umask, which is what every other
   write in `FS` asks for. `write_atomic` is not the place a file's
   permissions change.

   The temp file is dot-prefixed and carries the target's name, so an
   ordinary `*.conf` glob in another process does not match it and one left
   behind by a hard kill says what it was. It is removed if any step
   fails. *)
(* A publication in progress: the file being filled, and where it goes.

   Three steps rather than one function, because a whole string and a stream
   of lines are the same publication with different amounts of it known at
   once. Splitting it here is what keeps one implementation of the rename,
   the mode and the sync -- two copies would be two things to keep in
   agreement, and the second copy is the one that would drift. *)
type publication = {
  pub_fd : Unix.file_descr;
  pub_tmp : string;
  pub_target : string;
  pub_mode : int option;
}

let open_atomic path =
  let target = resolve_link path in
  let dir = Filename.dirname target in
  let mode =
    match Unix.stat target with
    | { Unix.st_perm; _ } -> Some st_perm
    | exception Unix.Unix_error _ -> None
  in
  let tmp =
    Filename.concat dir
      (Printf.sprintf ".%s.wand-tmp-%s" (Filename.basename target)
         (random_tag ()))
  in
  let fd =
    Unix.openfile tmp [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_EXCL; Unix.O_CLOEXEC] 0o644
  in
  { pub_fd = fd; pub_tmp = tmp; pub_target = target; pub_mode = mode }

let write_publication p content =
  let n = String.length content in
  let rec go off =
    if off < n then
      let written = Unix.write_substring p.pub_fd content off (n - off) in
      if written = 0 then failwith "short write" else go (off + written)
  in
  go 0

(* Contents before the rename, so the published file is never a name with
   nothing behind it. *)
let commit_atomic p =
  Unix.fsync p.pub_fd;
  Unix.close p.pub_fd;
  (match p.pub_mode with Some m -> Unix.chmod p.pub_tmp m | None -> ());
  Unix.rename p.pub_tmp p.pub_target

(* Nothing is published and nothing is left lying beside the target. Both
   steps tolerate having already happened, because this runs on the way out
   of a failure and must not raise one of its own. *)
let abort_atomic p =
  (try Unix.close p.pub_fd with Unix.Unix_error _ -> ());
  (try Unix.unlink p.pub_tmp with Unix.Unix_error _ -> ())

let write_atomic path content =
  let p = open_atomic path in
  match write_publication p content; commit_atomic p with
  | () -> ()
  | exception e -> abort_atomic p; raise e

let substitute_for name =
  match name with
  | "Shell!run"-> Some (VString "", "\"\"")
  | "Shell!capture"->
    Some (shell_result "" "" 0, "exit 0, no output")
  | "Shell!exit_code" -> Some (VInt 0, "0")
  (* A stream of nothing. The lines a rehearsal cannot have are no lines,
     which is the same answer `Shell!run`'s empty String gives. *)
  | "Shell!stream" -> Some (VList [], "no output")
  (* A request that was not sent still has to answer, and what it answers
     steers the rest of the script. `202 Accepted` with no body says the
     server took it and said nothing, which is the least a caller can read
     into. The line reporting it says the request was withheld. *)
  | "Net!http" ->
    Some (VConstr (Ctor.Builtin "HTTPResponse",
                   [VInt 202; VMap Evaluator.vmap_empty; VString ""]), "202, no body")
  | "FS!temp_file"      -> let p = dry_run_path ""     in Some (VPath p, p)
  | "FS!temp_dir"       -> let p = dry_run_path "-dir" in Some (VPath p, p)
  | _ -> None

(* The spawn-time half of a `Shell(git, curl)` manifest. The site's own
   file's bound arrives via `Evaluator.ambient_shell_allow` -- set around
   the perform, threaded across domains by Par -- and is checked here, at
   the moment of actual spawn, over the fully resolved command line. A
   mock, a rehearsal, or any other handler that intercepted the effect
   never spawns, so it never trips this. *)
let guard_shell cmd =
  match Domain.DLS.get Evaluator.ambient_shell_allow with
  | None -> ()
  | Some allow ->
    List.iter (fun w ->
      match (w : Shell_scan.word_class) with
      | Shell_scan.Literal word when not (Shell_scan.allowed ~allow word) ->
        raise (EvalError (Printf.sprintf
          "this command runs '%s', which the manifest's %s does not allow"
          word (Shell_scan.render_label ("Shell", Some allow))))
      | Shell_scan.Compound kw ->
        raise (EvalError (Printf.sprintf
          "this command uses shell control flow ('%s'), which the \
           manifest's %s cannot bound; write the loop in wand, or declare \
           bare Shell" kw (Shell_scan.render_label ("Shell", Some allow))))
      | _ -> ())
      (Shell_scan.scan_string cmd).Shell_scan.words

(* ── The transport ───────────────────────────────────────────────────────
   Bytes reach a host through a `curl` subprocess. TLS is the reason: OCaml's
   standard library has none, and the two ways to link one in-process both
   cost more than this release is buying. Which way the bytes move is not
   part of what the design settled -- the manifest is checked on the URL, so
   the guarantee holds identically either way, and moving in-process later
   touches no script.

   What it costs is one asterisk, and the reference carries it: a narrowed
   `Shell(git)` means only `git` runs *from this script*, not that only `git`
   runs. *)

(* The manifest's bound on where bytes may go, checked over a host the run
   resolved. Set beside the perform by `net_http`, so it is the bound of the
   file that built the request rather than of the standard library the send
   happens inside.

   A redirect is checked here as well as the URL the caller wrote. That is
   the whole reason the bound has to travel: a hop's host is not known until
   the server names it, and a 302 is the one thing that can send a body to a
   host nobody wrote down. `None` is a file that declared bare `Net`, and
   nothing is checked -- the reading `ambient_shell_allow` already gives. *)
let guard_net ~what host =
  match Domain.DLS.get Evaluator.ambient_net_allow with
  | None -> ()
  | Some allow ->
    if not (Narrow.allowed ~rule:Narrow.host ~allow host) then
      raise (EvalError (Printf.sprintf
        "%s '%s', which %s does not allow"
        what host (Shell_scan.render_label ("Net", Some allow))))

(* One exec, no shell. The words are wand's own rather than a command line
   anyone wrote, so there is nothing for a shell to read and no reason to
   pay for one -- and `guard_shell` is not on this path, which is the
   asterisk above stated in code: the transport is the one subprocess a
   narrowed `Shell` does not bound. *)
let spawn_argv argv stdin_text =
  let (out_r, out_w) = Unix.pipe ~cloexec:true () in
  let (err_r, err_w) = Unix.pipe ~cloexec:true () in
  let (in_r,  in_w)  = Unix.pipe ~cloexec:true () in
  let pid =
    Unix.create_process argv.(0) argv in_r out_w err_w
  in
  Unix.close out_w; Unix.close err_w; Unix.close in_r;
  remember pid;
  let write_stdin () =
    (try
       let n = String.length stdin_text in
       let rec go off =
         if off < n then
           let k = Unix.write_substring in_w stdin_text off (n - off) in
           if k > 0 then go (off + k)
       in
       go 0
     with Unix.Unix_error _ -> ());
    try Unix.close in_w with Unix.Unix_error _ -> ()
  in
  write_stdin ();
  let read fd =
    let buf = Buffer.create 4096 in
    let chunk = Bytes.create 65536 in
    let rec go () =
      match Unix.read fd chunk 0 (Bytes.length chunk) with
      | 0 -> ()
      | n -> Buffer.add_subbytes buf chunk 0 n; go ()
      | exception Unix.Unix_error (Unix.EINTR, _, _) -> go ()
    in
    go (); Buffer.contents buf
  in
  let out = read out_r in
  let err = read err_r in
  (try Unix.close out_r with Unix.Unix_error _ -> ());
  (try Unix.close err_r with Unix.Unix_error _ -> ());
  let status = reap pid in
  (out, err, status)

(* What a response looked like on the wire, taken apart. `curl -i` writes the
   status line, the headers, a blank line, and then the body. *)
let split_response text =
  let rec find i =
    if i + 1 >= String.length text then None
    else if text.[i] = '\n' && text.[i + 1] = '\n' then Some (i, i + 2)
    else if i + 3 < String.length text
            && text.[i] = '\r' && text.[i + 1] = '\n'
            && text.[i + 2] = '\r' && text.[i + 3] = '\n' then Some (i, i + 4)
    else find (i + 1)
  in
  match find 0 with
  | None -> (String.trim text, "")
  | Some (head_end, body_start) ->
    (String.sub text 0 head_end,
     String.sub text body_start (String.length text - body_start))

let header_lines head =
  String.split_on_char '\n' head
  |> List.map (fun l ->
       let n = String.length l in
       if n > 0 && l.[n - 1] = '\r' then String.sub l 0 (n - 1) else l)
  |> List.filter (fun l -> l <> "")

(* A header name is case-insensitive on the wire and a `Map` is not, so the
   module lowercases on the way in and the reference says so. A request
   cannot repeat a header, which is fine for requests; a response genuinely
   repeats `Set-Cookie`, and the last one wins here until `HTTP.header_list`
   exists to answer that shape properly. *)
let parse_headers lines =
  List.filter_map (fun line ->
    match String.index_opt line ':' with
    | None -> None
    | Some i ->
      let name = String.lowercase_ascii (String.trim (String.sub line 0 i)) in
      let value = String.trim (String.sub line (i + 1) (String.length line - i - 1)) in
      if name = "" then None else Some (name, value)) lines

let status_of head =
  match header_lines head with
  | first :: _ ->
    (match String.split_on_char ' ' first with
     | _ :: code :: _ -> (try int_of_string code with _ -> 0)
     | _ -> 0)
  | [] -> 0

(* A relative `Location` is resolved against the URL it came from, which is
   what a client does and what makes a same-host redirect the ordinary
   case. *)
let resolve_location ~base loc =
  if String.length loc > 7
     && (String.sub loc 0 7 = "http://" || String.sub loc 0 8 = "https://")
  then loc
  else
    let scheme_host =
      match String.index_opt base ':' with
      | Some i when i + 2 < String.length base ->
        let rest = String.sub base (i + 3) (String.length base - i - 3) in
        (match String.index_opt rest '/' with
         | Some j -> String.sub base 0 (i + 3 + j)
         | None -> base)
      | _ -> base
    in
    if String.length loc > 0 && loc.[0] = '/' then scheme_host ^ loc
    else scheme_host ^ "/" ^ loc

(* Fetch straight to a file, so a large artifact never becomes a value.
   `-D -` puts the headers on stdout and `-o` puts the body in the file, so
   a redirect can still be read and checked without holding what it points
   at. Each hop overwrites the file, and the last one is what stays. *)
let rec http_download ~hops ~url ~dest ~timeout_ms =
  guard_net ~what:"this download reaches" (Evaluator.host_of_url url);
  let seconds = max 1 ((timeout_ms + 999) / 1000) in
  let argv =
    [| "curl"; "-sS"; "--noproxy"; "*"; "--max-redirs"; "0";
       "--max-time"; string_of_int seconds; "-D"; "-"; "-o"; dest; url |]
  in
  let (out, err, status) = spawn_argv argv "" in
  (match status with
   | Unix.WEXITED 0 -> ()
   | _ ->
     let why = String.trim err in
     raise (EvalError (Printf.sprintf "download: %s"
       (if why = "" then "the request did not complete" else why))));
  let code = status_of out in
  let hdrs = parse_headers (List.tl (header_lines out)) in
  if code >= 300 && code < 400 && hops > 0 then
    match List.assoc_opt "location" hdrs with
    | Some loc when String.trim loc <> "" ->
      let next = resolve_location ~base:url (String.trim loc) in
      guard_net ~what:"this download is redirected to"
        (Evaluator.host_of_url next);
      http_download ~hops:(hops - 1) ~url:next ~dest ~timeout_ms
    | _ -> code
  else code

(* Send one request and answer what came back.

   Redirects are followed here rather than by `curl`, because each hop's host
   has to be checked against the manifest and `curl` cannot be told to ask.
   `--max-redirs 0` keeps it from following on its own.

   Proxy environment variables are refused. `curl` honours `HTTPS_PROXY` by
   default, and honouring it would send the body to a host the manifest never
   named -- and make every fetch read the environment, so every script that
   fetched anything would declare `Env`. `--noproxy '*'` is what makes the
   rule the reference states true on a machine that sets one. *)
let rec http_send ~hops ~url ~meth ~headers ~body ~timeout_ms =
  guard_net ~what:"this request reaches" (Evaluator.host_of_url url);
  let seconds = max 1 ((timeout_ms + 999) / 1000) in
  let head_only = meth = "HEAD" in
  let argv =
    [ "curl"; "-sS"; "--noproxy"; "*"; "--max-redirs"; "0";
      "--max-time"; string_of_int seconds ]
    @ (if head_only then ["--head"] else ["-i"; "-X"; meth])
    @ (if body = "" then [] else ["--data-binary"; "@-"])
    @ List.concat_map (fun (k, v) -> ["-H"; k ^ ": " ^ v]) headers
    @ [url]
  in
  let (out, err, status) = spawn_argv (Array.of_list argv) body in
  (match status with
   | Unix.WEXITED 0 -> ()
   | _ ->
     let why = String.trim err in
     raise (EvalError (Printf.sprintf "http: %s"
       (if why = "" then "the request did not complete" else why))));
  let (head, payload) = split_response out in
  let code = status_of head in
  let hdrs = parse_headers (List.tl (header_lines head)) in
  (* A 3xx with somewhere to go, and budget left to go there. Every hop is
     checked, so a manifest naming two hosts admits a redirect between them
     and one naming a single host does not. *)
  if code >= 300 && code < 400 && hops > 0 then
    match List.assoc_opt "location" hdrs with
    | Some loc when String.trim loc <> "" ->
      let next = resolve_location ~base:url (String.trim loc) in
      guard_net ~what:"this request is redirected to"
        (Evaluator.host_of_url next);
      (* A redirect answers with the method it was given, except that the
         three that change one are the reason `303` exists: after it, and
         after a `301` or `302` on a POST, a client asks with GET. That is
         what every other client does and what servers are written for. *)
      let meth =
        if code = 303 || ((code = 301 || code = 302) && meth = "POST")
        then "GET" else meth
      in
      let body = if meth = "GET" then "" else body in
      http_send ~hops:(hops - 1) ~url:next ~meth ~headers ~body ~timeout_ms
    | _ -> (code, hdrs, payload)
  else (code, hdrs, payload)

(* Downstream has gone -- `wand report.wand | head -3`. SIGPIPE is ignored
   (see `install_signal_handlers`), so the write comes back as an error
   instead of killing wand where it stands, and the script unwinds: a `with`
   bracket gives back what it holds before the run ends. 141 is 128 + SIGPIPE,
   which is the code a shell reports for a command that died on one. *)
let broken_pipe msg =
  let needle = "Broken pipe" in
  let n = String.length needle and m = String.length msg in
  let rec at i = i + n <= m && (String.sub msg i n = needle || at (i + 1)) in
  at 0

let pipe_closed = Evaluator.Interrupted 141

(* The cases below end in `_ -> None`, so an operation this handler does not
   recognise -- an unknown name, or a payload of the wrong shape -- falls
   through to OCaml's own `Effect.Unhandled` and ends the program with a
   fatal error naming an internal constructor. That is wand's bug to report,
   not the reader's to decode, so it comes back as a wand error like any
   other and the caller that already renders `EvalError` renders this too. *)
let unhandled_operation name =
  EvalError (Printf.sprintf
    "no handler for '%s' -- this is a bug in wand, not in the script" name)

let run_with_default_handler (thunk : unit -> value) : value =
  try
  Effect.Deep.match_with thunk ()
    { Effect.Deep.
        retc = (fun v -> v);
        exnc = raise;
        effc = fun (type a) (eff : a Effect.t) ->
          match eff with
          | WandEffect ("IO!print", v) ->
            Some (fun (k : (a, value) Effect.Deep.continuation) ->
              match print_string (to_text v) with
              | () -> Effect.Deep.continue k VUnit
              | exception Sys_error m when broken_pipe m ->
                Effect.Deep.discontinue k pipe_closed)
          | WandEffect ("IO!println", v) ->
            Some (fun (k : (a, value) Effect.Deep.continuation) ->
              match print_endline (to_text v) with
              | () -> Effect.Deep.continue k VUnit
              | exception Sys_error m when broken_pipe m ->
                Effect.Deep.discontinue k pipe_closed)
          (* stderr is where a script reports what went wrong, so it is
             flushed rather than left in a buffer that a later `Proc.exit`
             would discard. *)
          | WandEffect ("IO!print_err", v) ->
            Some (fun (k : (a, value) Effect.Deep.continuation) ->
              match output_string stderr (to_text v); flush stderr with
              | () -> Effect.Deep.continue k VUnit
              | exception Sys_error m when broken_pipe m ->
                Effect.Deep.discontinue k pipe_closed)
          | WandEffect ("IO!println_err", v) ->
            Some (fun (k : (a, value) Effect.Deep.continuation) ->
              match output_string stderr (to_text v ^ "\n"); flush stderr with
              | () -> Effect.Deep.continue k VUnit
              | exception Sys_error m when broken_pipe m ->
                Effect.Deep.discontinue k pipe_closed)
          | WandEffect ("Hash!file", VTuple [VString algo; (VString p | VPath p)]) ->
            Some (fun (k : (a, value) Effect.Deep.continuation) ->
              match (try Ok (hash_file_hex algo p)
                     with Sys_error m -> Error ("hash_file: " ^ m)) with
              | Ok hex  -> Effect.Deep.continue    k (VString hex)
              | Error m -> Effect.Deep.discontinue k (EvalError m))
          | WandEffect ("FS!stream_lines", (VString p | VPath p)) ->
            Some (fun (k : (a, value) Effect.Deep.continuation) ->
              match (try Ok (open_in p)
                     with Sys_error m -> Error ("stream_lines: " ^ m)) with
              | Ok ic   -> Effect.Deep.continue    k (VLineSource ic)
              | Error m -> Effect.Deep.discontinue k (EvalError m))
          | WandEffect ("IO!stdin_lines", VUnit) ->
            Some (fun (k : (a, value) Effect.Deep.continuation) ->
              Effect.Deep.continue k (VLineSource stdin))
          | WandEffect ("IO!read_line", VUnit) ->
            Some (fun (k : (a, value) Effect.Deep.continuation) ->
              match In_channel.input_line stdin with
              | Some line -> Effect.Deep.continue k (VString line)
              (* End of input is not a line, and returning "" would make it
                 indistinguishable from a blank one. `IO.read_line` wraps
                 this into a Result. *)
              | None -> Effect.Deep.discontinue k (EvalError "end of input"))
          (* Building a command runs nothing, so there is nothing to guard
             and nothing to report: the answer is the line it was given.
             The check that matters for a literal is the typechecker's, at
             the site where the words are written. *)
          | WandEffect ("Shell!command", VString cmd) ->
            Some (fun (k : (a, value) Effect.Deep.continuation) ->
              Effect.Deep.continue k (VString cmd))
          | WandEffect ("Shell!stream", VString cmd) ->
            Some (fun (k : (a, value) Effect.Deep.continuation) ->
              match attempt (fun () ->
                guard_shell cmd;
                let (pull, finish) = stream_command cmd in
                VProcSource (pull, finish)) with
              | Ok v    -> Effect.Deep.continue    k v
              | Error e -> Effect.Deep.discontinue k e)
          | WandEffect ("Shell!run", VString cmd) ->
            Some (fun (k : (a, value) Effect.Deep.continuation) ->
              match attempt (fun () -> guard_shell cmd; exec_command cmd) with
              | Ok s    -> Effect.Deep.continue    k (VString s)
              | Error e -> Effect.Deep.discontinue k e)
          | WandEffect ("Shell!run_quiet", VString cmd) ->
            Some (fun (k : (a, value) Effect.Deep.continuation) ->
              match attempt (fun () -> guard_shell cmd; exec_command_quiet cmd) with
              | Ok ()   -> Effect.Deep.continue    k VUnit
              | Error e -> Effect.Deep.discontinue k e)
          | WandEffect ("Shell!exit_code", VString cmd) ->
            Some (fun (k : (a, value) Effect.Deep.continuation) ->
              match attempt (fun () -> guard_shell cmd;
                                        exec_command_exit_code cmd) with
              | Error e -> Effect.Deep.discontinue k e
              | Ok code -> Effect.Deep.continue k (VInt code))
          | WandEffect ("Shell!run", VTuple [VString cmd; VString stdin]) ->
            Some (fun (k : (a, value) Effect.Deep.continuation) ->
              match attempt (fun () -> guard_shell cmd; exec_command_stdin cmd stdin) with
              | Ok s    -> Effect.Deep.continue    k (VString s)
              | Error e -> Effect.Deep.discontinue k e)
          | WandEffect ("Shell!capture", VString cmd) ->
            Some (fun (k : (a, value) Effect.Deep.continuation) ->
              (* Through `attempt` like the others: `$?()` reports an exit
                 code rather than raising on one, but a deadline that ran
                 out is a raise, and it has to reach the call site's `try`
                 rather than escape the handler. *)
              match attempt (fun () -> guard_shell cmd; exec_command_full cmd) with
              | Error e -> Effect.Deep.discontinue k e
              | Ok (stdout, stderr, code) ->
                Effect.Deep.continue k (shell_result stdout stderr code))
          | WandEffect ("Shell!capture", VTuple [VString cmd; VString stdin]) ->
            Some (fun (k : (a, value) Effect.Deep.continuation) ->
              match attempt (fun () -> guard_shell cmd;
                                        exec_command_full_stdin cmd stdin) with
              | Error e -> Effect.Deep.discontinue k e
              | Ok (stdout, stderr, code) ->
                Effect.Deep.continue k (shell_result stdout stderr code))
          (* 0644, the mode the rest of `FS` writes with. The channel is
             answered rather than the content taken, so the file is opened
             once and the lines arrive as the stream produces them. *)
          | WandEffect ("FS!write_lines", (VString path | VPath path)) ->
            Some (fun (k : (a, value) Effect.Deep.continuation) ->
              match (try Ok (Out_channel.open_gen
                              [Open_wronly; Open_creat; Open_trunc] 0o644 path)
                     with Sys_error m -> Error ("write_lines: " ^ m)) with
              | Ok oc   -> Effect.Deep.continue    k (channel_sink oc)
              | Error m -> Effect.Deep.discontinue k (EvalError m))
          (* The publishing sink. `write_atomic` with the content arriving a
             line at a time rather than all at once, so it is the same three
             steps: open beside the target, fill, and rename over it.

             The abort is the arm that only exists for this sink. A stream
             that raises part way has produced a file that is missing its
             tail, and renaming that over the target would deliver a torn
             file atomically -- precisely what the operation is for. So the
             temp file goes and the target is left holding what it held. *)
          | WandEffect ("FS!write_lines_atomic", (VString path | VPath path)) ->
            Some (fun (k : (a, value) Effect.Deep.continuation) ->
              match (try Ok (open_atomic path)
                     with Sys_error m -> Error ("write_lines_atomic: " ^ m)
                        | Unix.Unix_error (e, f, _) ->
                          Error (Printf.sprintf "write_lines_atomic: %s: %s" f
                                   (Unix.error_message e))) with
              | Error m -> Effect.Deep.discontinue k (EvalError m)
              | Ok p ->
                Effect.Deep.continue k
                  (VLineSink
                     ((fun line -> write_publication p (line ^ "\n")),
                      (fun () -> commit_atomic p),
                      (fun () -> abort_atomic p))))
          | WandEffect ("FS!append_lines", (VString path | VPath path)) ->
            Some (fun (k : (a, value) Effect.Deep.continuation) ->
              match (try Ok (Out_channel.open_gen
                              [Open_wronly; Open_creat; Open_append] 0o644 path)
                     with Sys_error m -> Error ("append_lines: " ^ m)) with
              | Ok oc   -> Effect.Deep.continue    k (channel_sink oc)
              | Error m -> Effect.Deep.discontinue k (EvalError m))
          | WandEffect ("FS!read_file", (VString path | VPath path)) ->
            Some (fun (k : (a, value) Effect.Deep.continuation) ->
              match (try Ok (In_channel.with_open_text path In_channel.input_all)
                     with Sys_error m -> Error ("read_file: " ^ m)) with
              | Ok s    -> Effect.Deep.continue    k (VString s)
              | Error m -> Effect.Deep.discontinue k (EvalError m))
          (* 0644, as `FS.create_file` and `FS.append` already asked for.
             This one took the channel default of 0666, which a umask
             usually trims to the same thing and does not have to: under
             `umask 0` -- a container, a daemon, a CI runner that set it --
             the file a script wrote came out world-writable, while the
             file its sibling wrote two lines later did not. *)
          | WandEffect ("FS!write_file", VTuple [(VString path | VPath path); VString content]) ->
            Some (fun (k : (a, value) Effect.Deep.continuation) ->
              match (try Out_channel.with_open_gen
                           [Open_wronly; Open_creat; Open_trunc] 0o644 path
                           (fun oc -> Out_channel.output_string oc content); Ok ()
                     with Sys_error m -> Error ("write_file: " ^ m)) with
              | Ok ()   -> Effect.Deep.continue    k VUnit
              | Error m -> Effect.Deep.discontinue k (EvalError m))
          (* Publish a file whole: write beside the target, then rename
             over it. A reader sees the old contents or the new ones and
             never a half-written file.

             Three things here are the reason this is a function rather
             than a paragraph in the documentation, because the calls a
             person composes by hand are not the calls that are correct.

             The temp file goes in the target's own directory. `FS.rename`
             is `Unix.rename`, which cannot cross a filesystem, and the OS
             temp directory is a different one often enough -- `/tmp` on
             Linux is usually tmpfs -- that the hand-written version passes
             on a mac and fails in CI.

             An existing target keeps its mode. Renaming replaces the inode,
             so without this a 644 configuration file becomes whatever the
             temp file was created as, silently, on every write.

             A symlink is written through rather than replaced, which is
             what `FS.write_file` does. A deploy publishing to
             `/etc/app/config -> config.v3` means the file at the end of the
             link; renaming onto the link itself turns it into a regular
             file and is the opposite of what was asked.

             The temp file is synced before the rename, so a file that is
             there after a power loss is whole. The containing directory is
             not synced, so the rename itself may be lost -- durability of
             the publication costs a second sync per file, and atomicity is
             what the name promises. *)
          | WandEffect ("FS!write_atomic", VTuple [(VString path | VPath path); VString content]) ->
            Some (fun (k : (a, value) Effect.Deep.continuation) ->
              match (try Ok (write_atomic path content)
                     with Sys_error m -> Error ("write_atomic: " ^ m)
                        | Unix.Unix_error (e, f, _) ->
                          Error (Printf.sprintf "write_atomic: %s: %s" f
                                   (Unix.error_message e))) with
              | Ok ()   -> Effect.Deep.continue    k VUnit
              | Error m -> Effect.Deep.discontinue k (EvalError m))
          (* Taking a lock answers with the path it was taken on -- the
             resolved one, so what the bracket releases is what the table
             knows. `None` is the one failure a caller is expected to
             branch on: somebody else is running. *)
          | WandEffect ("FS!lock", (VString path | VPath path)) ->
            Some (fun (k : (a, value) Effect.Deep.continuation) ->
              match take_lock path with
              | Some key ->
                Effect.Deep.continue k
                  (VConstr (Ctor.Builtin "Some", [VPath key]))
              | None ->
                Effect.Deep.continue k Evaluator.v_none
              | exception EvalError m -> Effect.Deep.discontinue k (EvalError m))
          | WandEffect ("FS!lock_wait",
                        VTuple [(VString path | VPath path); VDuration d]) ->
            Some (fun (k : (a, value) Effect.Deep.continuation) ->
              match take_lock_wait path (Evaluator.parse_dur_ms d) with
              | Some key ->
                Effect.Deep.continue k
                  (VConstr (Ctor.Builtin "Some", [VPath key]))
              | None ->
                Effect.Deep.continue k Evaluator.v_none
              | exception EvalError m -> Effect.Deep.discontinue k (EvalError m))
          | WandEffect ("FS!unlock", (VString path | VPath path)) ->
            Some (fun (k : (a, value) Effect.Deep.continuation) ->
              release_lock path; Effect.Deep.continue k VUnit)
          (* One operation for the protocol, so a mock has one case to
             write and a test cannot believe itself sealed while a second
             operation reaches the network. *)
          | WandEffect ("Net!http",
                        VConstr (_, [(VURL (url, _) | VString url); meth; VMap header_map;
                                     VString body; VDuration timeout;
                                     VInt redirects])) ->
            Some (fun (k : (a, value) Effect.Deep.continuation) ->
              let headers =
                List.map (fun (name, v) ->
                  (name, match v with VString s -> s | other -> to_text other))
                  (Evaluator.vmap_list header_map)
              in
              match (try
                       Ok (http_send ~hops:(max 0 redirects) ~url
                             ~meth:(method_name meth) ~headers ~body
                             ~timeout_ms:(Evaluator.parse_dur_ms timeout))
                     with EvalError m -> Error m) with
              | Ok (code, hdrs, payload) ->
                Effect.Deep.continue k
                  (VConstr (Ctor.Builtin "HTTPResponse",
                            [VInt code;
                             VMap (Evaluator.vmap_of_list (List.map (fun (h, v) -> (h, VString v)) hdrs));
                             VString payload]))
              | Error m -> Effect.Deep.discontinue k (EvalError m))
          | WandEffect ("Net!download",
                        VTuple [(VURL (url, _) | VString url); VPath dest]) ->
            Some (fun (k : (a, value) Effect.Deep.continuation) ->
              match (try Ok (http_download ~hops:5 ~url ~dest
                               ~timeout_ms:(5 * 60 * 1000))
                     with EvalError m -> Error m) with
              | Ok code when code >= 200 && code < 300 ->
                Effect.Deep.continue k VUnit
              | Ok code ->
                Effect.Deep.discontinue k (EvalError (Printf.sprintf
                  "download: %s answered %d" url code))
              | Error m -> Effect.Deep.discontinue k (EvalError m))
          | WandEffect ("FS!mkdir", VPath path) ->
            Some (fun (k : (a, value) Effect.Deep.continuation) ->
              let rec mkdir_p p =
                if Sys.file_exists p then ()
                else begin mkdir_p (Filename.dirname p); Unix.mkdir p 0o755 end
              in
              match (try mkdir_p path; Ok ()
                     with Unix.Unix_error (e, _, _) -> Error ("mkdir_p: " ^ Unix.error_message e)) with
              | Ok ()   -> Effect.Deep.continue    k VUnit
              | Error m -> Effect.Deep.discontinue k (EvalError m))
          | WandEffect ("FS!list_dir", VPath path) ->
            Some (fun (k : (a, value) Effect.Deep.continuation) ->
              match (try
                       let entries = Sys.readdir path in
                       Array.sort String.compare entries;
                       Ok (Array.to_list (Array.map (fun s ->
                         VPath (Filename.concat path s)) entries))
                     with Sys_error m -> Error ("ls: " ^ m)) with
              | Ok vs   -> Effect.Deep.continue    k (VList vs)
              | Error m -> Effect.Deep.discontinue k (EvalError m))
          | WandEffect ("FS!append", VTuple [VPath path; VString content]) ->
            Some (fun (k : (a, value) Effect.Deep.continuation) ->
              match (try Out_channel.with_open_gen
                           [Open_wronly; Open_creat; Open_append] 0o644 path
                           (fun oc -> Out_channel.output_string oc content); Ok ()
                     with Sys_error m -> Error ("append: " ^ m)) with
              | Ok ()   -> Effect.Deep.continue    k VUnit
              | Error m -> Effect.Deep.discontinue k (EvalError m))
          | WandEffect ("FS!create_file", VPath path) ->
            Some (fun (k : (a, value) Effect.Deep.continuation) ->
              match (try Out_channel.with_open_gen
                           [Open_wronly; Open_creat; Open_trunc] 0o644 path
                           (fun _ -> ()); Ok ()
                     with Sys_error m -> Error ("create_file: " ^ m)) with
              | Ok ()   -> Effect.Deep.continue    k VUnit
              | Error m -> Effect.Deep.discontinue k (EvalError m))
          | WandEffect ("FS!temp_file", VTuple [VString prefix; VString suffix]) ->
            Some (fun (k : (a, value) Effect.Deep.continuation) ->
              match (try Ok (Filename.temp_file prefix suffix)
                     with Sys_error m -> Error ("temp_file: " ^ m)) with
              | Ok path -> Effect.Deep.continue    k (VPath path)
              | Error m -> Effect.Deep.discontinue k (EvalError m))
          | WandEffect ("FS!temp_dir", VString prefix) ->
            Some (fun (k : (a, value) Effect.Deep.continuation) ->
              (* One step, 0700, under a name that did not exist a moment
                 before. It used to take a unique file name, remove the file
                 and make a directory of the same name -- and between those
                 two the name belonged to nobody, which in a shared /tmp is
                 a name another process can take. *)
              match (try
                       Ok (mkdtemp
                             (Filename.concat (Filename.get_temp_dir_name ())
                                (prefix ^ "XXXXXX")))
                     with Failure m -> Error ("temp_dir: " ^ m)
                        | Sys_error m -> Error ("temp_dir: " ^ m)
                        | Unix.Unix_error (e, _, _) ->
                          Error ("temp_dir: " ^ Unix.error_message e)) with
              | Ok path -> Effect.Deep.continue    k (VPath path)
              | Error m -> Effect.Deep.discontinue k (EvalError m))
          | WandEffect ("FS!delete_tree", VPath path) ->
            Some (fun (k : (a, value) Effect.Deep.continuation) ->
              match (try delete_tree path; Ok ()
                     with Failure m -> Error ("delete_tree: " ^ m)
                        | Sys_error m -> Error ("delete_tree: " ^ m)
                        | Unix.Unix_error (e, _, _) ->
                          Error ("delete_tree: " ^ Unix.error_message e)) with
              | Ok ()   -> Effect.Deep.continue    k VUnit
              | Error m -> Effect.Deep.discontinue k (EvalError m))
          | WandEffect ("FS!rename", VTuple [VPath old_; VPath new_]) ->
            Some (fun (k : (a, value) Effect.Deep.continuation) ->
              match (try Unix.rename old_ new_; Ok ()
                     with Unix.Unix_error (e, _, _) ->
                       Error ("rename: " ^ Unix.error_message e)) with
              | Ok ()   -> Effect.Deep.continue    k VUnit
              | Error m -> Effect.Deep.discontinue k (EvalError m))
          | WandEffect ("FS!copy", VTuple [VPath src; VPath dst]) ->
            Some (fun (k : (a, value) Effect.Deep.continuation) ->
              match (try copy_file src dst; Ok ()
                     with
                     | Sys_error m -> Error ("copy: " ^ m)
                     | Unix.Unix_error (e, _, _) ->
                       Error ("copy: " ^ Unix.error_message e)) with
              | Ok ()   -> Effect.Deep.continue    k VUnit
              | Error m -> Effect.Deep.discontinue k (EvalError m))
          (* The tree under `src`, placed at `dst`. A directory is created
             with the source's permissions, a file is copied by the same
             rule a single copy uses, and a symlink is recreated as a
             symlink rather than followed -- a tree that links to itself
             would otherwise be copied until the disk filled. *)
          | WandEffect ("FS!copy_tree", VTuple [VPath src; VPath dst]) ->
            Some (fun (k : (a, value) Effect.Deep.continuation) ->
              let rec cp s d =
                let st = Unix.lstat s in
                match st.Unix.st_kind with
                | Unix.S_LNK ->
                  (* `symlink` fails on a name that exists, and a re-run of
                     a copy is an ordinary thing to do. Asked with `lstat`,
                     because `Sys.file_exists` follows the link: a dangling
                     one at the destination answered "not there", and the
                     re-run failed with EEXIST on the name it had just been
                     told was free. *)
                  (match Unix.lstat d with
                   | _ -> Sys.remove d
                   | exception Unix.Unix_error _ -> ());
                  Unix.symlink (Unix.readlink s) d
                | Unix.S_DIR ->
                  if not (Sys.file_exists d) then Unix.mkdir d st.Unix.st_perm;
                  Array.iter (fun e -> cp (Filename.concat s e) (Filename.concat d e))
                    (Sys.readdir s)
                | _ -> copy_file s d
              in
              match (try cp src dst; Ok ()
                     with
                     | Sys_error m -> Error ("copy_tree: " ^ m)
                     | Unix.Unix_error (e, _, _) ->
                       Error ("copy_tree: " ^ Unix.error_message e)) with
              | Ok ()   -> Effect.Deep.continue    k VUnit
              | Error m -> Effect.Deep.discontinue k (EvalError m))
          (* Read-only operations: performed so a trace can see them, and
             carried out by the same implementations the builtins used. A
             failure has to be delivered into the continuation rather than
             raised here, or it escapes the handler instead of reaching the
             `try` at the call site. *)
          | WandEffect ("FS!cwd", v) ->
            Some (fun (k : (a, value) Effect.Deep.continuation) ->
              match Evaluator.fs_cwd_impl v with
              | result -> Effect.Deep.continue k result
              | exception (EvalError _ as e) -> Effect.Deep.discontinue k e)
          | WandEffect ("FS!mtime", v) ->
            Some (fun (k : (a, value) Effect.Deep.continuation) ->
              match Evaluator.fs_mtime_impl v with
              | result -> Effect.Deep.continue k result
              | exception (EvalError _ as e) -> Effect.Deep.discontinue k e)
          | WandEffect ("FS!size", v) ->
            Some (fun (k : (a, value) Effect.Deep.continuation) ->
              match Evaluator.fs_size_impl v with
              | result -> Effect.Deep.continue k result
              | exception (EvalError _ as e) -> Effect.Deep.discontinue k e)
          | WandEffect ("FS!glob", VTuple [pattern; dir]) ->
            Some (fun (k : (a, value) Effect.Deep.continuation) ->
              match (match List.assoc "fs_glob_impl" Evaluator.stdlib_eval_env with
                     | VBuiltin f ->
                       (match f pattern with
                        | VBuiltin g -> g dir
                        | other -> other)
                     | other -> other) with
              | result -> Effect.Deep.continue k result
              | exception (EvalError _ as e) -> Effect.Deep.discontinue k e)
          | WandEffect ("Env!set", VTuple [VString name; VString value]) ->
            Some (fun (k : (a, value) Effect.Deep.continuation) ->
              Unix.putenv name value;
              Effect.Deep.continue k VUnit)
          | WandEffect ("Env!clear", VString name) ->
            Some (fun (k : (a, value) Effect.Deep.continuation) ->
              (* Removed, not emptied. `putenv name ""` leaves the variable
                 in the environment holding nothing, which a child can tell
                 from its absence -- `${FOO-fallback}` takes the empty
                 string and skips the fallback. *)
              unsetenv name;
              Effect.Deep.continue k VUnit)
          | WandEffect ("FS!delete", VPath path) ->
            Some (fun (k : (a, value) Effect.Deep.continuation) ->
              let rm () =
                (* `Sys.is_directory` follows a link, so a symlink *to* a
                   directory was handed to `rmdir`, which got the link and
                   answered "Not a directory". What `delete` removes is the
                   name it was given. *)
                match Unix.lstat path with
                | { Unix.st_kind = Unix.S_DIR; _ } -> Unix.rmdir path
                | _ -> Sys.remove path
                | exception Unix.Unix_error _ ->
                  (* Not there, or unreadable: `Sys.remove` says which. *)
                  Sys.remove path
              in
              match (try rm (); Ok ()
                     with Sys_error m -> Error ("remove: " ^ m)
                        | Unix.Unix_error (e, _, _) -> Error ("remove: " ^ Unix.error_message e)) with
              | Ok ()   -> Effect.Deep.continue    k VUnit
              | Error m -> Effect.Deep.discontinue k (EvalError m))
          | WandEffect ("IO!read_all", VUnit) ->
            Some (fun (k : (a, value) Effect.Deep.continuation) ->
              Effect.Deep.continue k (VString (In_channel.input_all stdin)))
          | WandEffect ("IO!flush", VUnit) ->
            Some (fun (k : (a, value) Effect.Deep.continuation) ->
              flush stdout;
              Effect.Deep.continue k VUnit)
          (* Anything else that was registered as performing: the default
             behaviour is simply to run the implementation it was built
             from. Failures go back through the continuation so a `try` at
             the call site still sees them. *)
          | WandEffect (name, v) when Hashtbl.mem Evaluator.direct_impl name ->
            Some (fun (k : (a, value) Effect.Deep.continuation) ->
              match (Hashtbl.find Evaluator.direct_impl name) v with
              | result -> Effect.Deep.continue k result
              (* Back through the continuation, not raised beside it: an
                 exception raised here would abandon the body rather than
                 unwind it, and every `with` the body is holding would go
                 unreleased. That is how `exit` skipped cleanup. *)
              | exception ((EvalError _ | Interrupted _) as e) ->
                Effect.Deep.discontinue k e)
          | _ -> None
    }
  with Effect.Unhandled (WandEffect (name, _)) ->
    raise (unhandled_operation name)

(* ── Import resolution ────────────────────────────────────────────────────── *)

(* Path resolution, `import`-expression matching, and other purely
   type/AST-level pieces of this live in `Module_types` (shared with
   `Evaluator`'s `Types` primitives, which typecheck imports without
   evaluating them). Only the parts that need `Evaluator.value`/`eval` stay
   here. *)
let add_ext          = Module_types.add_ext
let entry_path       = Module_types.entry_path
let resolve_import    = Module_types.resolve_import
let namespace_name_of = Module_types.namespace_name_of
let local_tenv_of     = Module_types.local_tenv_of
let is_private        = Module_types.is_private
let strip_located     = Module_types.strip_located
let import_kind_of    = Module_types.import_kind_of

type import_env = {
  tenv     : (string * Ast.type_def) list;
  type_env : Typechecker.env;
  eval_env : env;
  (* What a type name written in this file means: the short name a reader
     writes, and the canonical name it stands for. *)
  type_names : (string * string) list;
  (* The interfaces in scope, travelling the way type definitions do: a
     contract is read where a module claims it and where a parameter is
     annotated with it, and both can be in a file that only imports it. *)
  ifaces : (string * Ast.interface_def) list;
  (* What evaluating the imported modules' bindings performs. An import
     evaluates them, so the file that writes the import performs this. *)
  load_effects : Effect_set.EffSet.t;
}

let empty_import_env =
  { tenv = []; type_env = []; eval_env = []; type_names = []; ifaces = [];
    load_effects = Effect_set.EffSet.empty }

(* ── Multi-clause merging ─────────────────────────────────────────────────── *)

(* True for patterns that unconditionally match (no structural constraint). *)
let is_catchall_pat = function
  | Ast.PVar _ | Ast.Wild -> true
  | _ -> false

(* Extract the match cases from a previously merged VFix, or return a single case. *)
let extract_arms arity existing_params existing_body =
  let fresh     = List.init arity (fun i -> Printf.sprintf "_p%d" i) in
  let fresh_pats = List.map (fun v -> Ast.PVar v) fresh in
  let scrutinee = match fresh with
    | [v] -> Ast.Var v
    | vs  -> Ast.Tuple (List.map (fun v -> Ast.Var v) vs)
  in
  if existing_params = fresh_pats then
    match strip_located existing_body with
    | Ast.Match (scrut, cases) when strip_located scrut = scrutinee -> cases
    | body ->
      let pat = match existing_params with [p] -> p | ps -> Ast.PTuple ps in
      [(pat, None, body)]
  else
    let pat = match existing_params with [p] -> p | ps -> Ast.PTuple ps in
    [(pat, None, existing_body)]

(* Merge a new clause into an existing same-arity VFix.
   Specific patterns are placed before catch-all patterns so that
   a base case added after a catch-all still fires correctly.
   Within each group the new clause takes precedence. *)
let merge_clause env name arity params body existing_params existing_body =
  let fresh     = List.init arity (fun i -> Printf.sprintf "_p%d" i) in
  let scrutinee = match fresh with
    | [v] -> Ast.Var v
    | vs  -> Ast.Tuple (List.map (fun v -> Ast.Var v) vs)
  in
  let new_pat  = match params with [p] -> p | ps -> Ast.PTuple ps in
  let new_case  = (new_pat, None, body) in
  let old_arms = extract_arms arity existing_params existing_body in
  let (new_sp, new_ca) =
    if is_catchall_pat new_pat then ([], [new_case]) else ([new_case], []) in
  let (old_sp, old_ca) =
    List.partition (fun (p, _, _) -> not (is_catchall_pat p)) old_arms in
  let cases = new_sp @ old_sp @ new_ca @ old_ca in
  VFix (name, env, List.map (fun v -> Ast.PVar v) fresh, Ast.Match (scrutinee, cases))

(* Evaluate a single top-level item; imports already merged into env *)
(* `modul` is the key of the module whose items these are, when they belong
   to one. A script's own declarations belong to no module: nothing else can
   name its types, so its constructors stay `Local`. *)
let run_item ?modul env item =
  match item with
  | Ast.TLLet (_, [], body) when Option.is_some (import_kind_of body) -> env  (* pre-loaded *)
  | Ast.TLLet (name, [], body) ->
    (name, eval env body) :: env
  | Ast.TLLet (name, params, body) ->
    (* A definition that only forwards to a builtin is that builtin. *)
    (match Evaluator.forwarding_builtin env params body with
     | Some v -> (name, v) :: env
     | None   -> (name, VFix (name, env, params, body)) :: env)
  | Ast.TLLetRec bindings ->
    List.fold_left (fun acc (name, _, _) ->
      (name, VFixGroup (bindings, env, name)) :: acc) env bindings
  | Ast.TLLetPat (_, body) when Option.is_some (import_kind_of body) -> env  (* pre-loaded *)
  | Ast.TLLetPat (pat, e) ->
    Evaluator.bind_pat ~prefix:true pat (eval env e) env
  | Ast.TLImport _ -> env  (* already loaded by load_imports_for *)
  (* An implementation declares the module's own bindings, so each one lands
     exactly where writing it as a top-level `let` would put it. Nothing is
     hoisted or registered: the block is a claim over bindings, and the
     bindings are ordinary. *)
  | Ast.TLImplement (im, _) ->
    List.fold_left (fun env (name, params, body, _) ->
      match params with
      | [] -> (name, eval env body) :: env
      | _ ->
        (match Evaluator.forwarding_builtin env params body with
         | Some v -> (name, v) :: env
         | None   -> (name, VFix (name, env, params, body)) :: env))
      env im.Ast.im_binds
  | Ast.TLInterface _ -> env  (* a contract declares no value *)
  (* An alias to a type with one constructor names that constructor too, so
     the alias binds it. An alias to anything else binds nothing. *)
  | Ast.TLType (Ast.Alias (aname, _, target), _) ->
    let rec target_name (te : Ast.type_expr) =
      match te with
      | Ast.TEName n -> Some n
      | Ast.TEQual (_, n) -> Some n
      | Ast.TEApp (f, _) -> target_name f
      | _ -> None
    in
    (match target_name target with
     | Some tn ->
       (* A record constructor is not a value in scope -- it is built by
          naming its fields -- so forwarding it means giving the alias the
          same constructor identity rather than the same value. Without
          this, `type Req = HTTPRequest` typechecked and then failed at run
          time with "unknown constructor 'Req'". *)
       (match Hashtbl.find_opt Evaluator.ctor_of_name tn with
        | Some c -> Hashtbl.replace Evaluator.ctor_of_name aname c
        | None -> ());
       (match Evaluator.lookup_var tn env with
        | Some ((VConstr _ | VPartialConstr _) as v) -> (aname, v) :: env
        | _ -> env)
     | None -> env)
  | Ast.TLType (Ast.Variants (tname, params, ctors), _) ->
    (* A single-constructor type with named fields can have its decoder
       derived, so the definition is kept where the derivation can find it.
       Anything else -- several constructors, positional fields, a generic --
       has no shape a decoder could read, and is not recorded. *)
    (match ctors with
     | [ctor] when ctor.Ast.fields <> []
                       && List.for_all (fun (n, _) -> n <> None) ctor.Ast.fields ->
       (* Under the canonical name, which a field type mentions, and under the
          short one, which a file writes as `T.decoder`. *)
       let ident =
         match modul with
         | Some m -> Ctor.Owned (m, ctor.Ast.name)
         | None -> Ctor.Local ctor.Ast.name
       in
       let entry = (ident, params, ctor.Ast.fields) in
       Hashtbl.replace Evaluator.derivable tname entry;
       (match modul with
        | Some m ->
          Hashtbl.replace Evaluator.derivable
            (Module_types.canonical_type ~modul:m tname) entry
        | None -> ())
     | _ -> Hashtbl.remove Evaluator.derivable tname);
    let position = ref (-1) in
    List.fold_left (fun env ctor ->
      incr position;
      let field_names = List.map fst ctor.Ast.fields in
      let ident =
        match modul with
        | Some m -> Ctor.Owned (m, ctor.Ast.name)
        | None -> Ctor.Local ctor.Ast.name
      in
      Hashtbl.replace Evaluator.constr_fields ident field_names;
      Hashtbl.replace Evaluator.constr_defaults ident ctor.Ast.defaults;
      (* Where it stands in the declaration, which is the order it sorts in. *)
      Hashtbl.replace Evaluator.constr_index ident !position;
      Evaluator.register_ctor ident;
      Evaluator.forget_ctor_env ();
      let v = match ctor.Ast.fields with
        | [] -> VConstr (ident, [])
        | fs -> VPartialConstr (ident, List.length fs, [])
      in
      (ctor.Ast.name, v) :: env
    ) env ctors
  | Ast.TLExpr _ -> env

(* The constructors of a set of type definitions, as evaluation bindings.
   Type definitions cross an import boundary, so their constructors have to
   as well: the typechecker learns them from the definition, and this is the
   evaluator's half of the same fact. *)
(* The module that declared these has already registered them, with the
   identity that says so. This only binds the names, and asks the evaluator
   which identity each one means rather than deciding again. *)
let ctor_bindings_of ?modul tenv =
  List.concat_map (fun (_, tdef) ->
    match tdef with
    | Ast.Alias _ -> []
    | Ast.Variants (_, _, ctors) ->
      List.map (fun (ctor : Ast.ctor_def) ->
        (* Named by the module that declares them where one is known: the
           bare-name index holds one constructor per name, and two modules
           may each declare `Live`. *)
        let ident = match modul with
          | Some m -> Ctor.Owned (m, ctor.Ast.name)
          | None -> Evaluator.ctor_named ctor.Ast.name
        in
        let v = match ctor.Ast.fields with
          | [] -> VConstr (ident, [])
          | fs -> VPartialConstr (ident, List.length fs, [])
        in
        (ctor.Ast.name, v)) ctors) tenv

(* Run top-level items, dropping a fresh index in every so often: a file's
   own definitions accumulate in front of the base, and without this a name
   defined early is walked past by everything defined later. *)
let fold_items step env items =
  let (env, _) =
    List.fold_left (fun (env, since) item ->
      let env = step env item in
      if since >= Evaluator.index_every then (Evaluator.index_env env, 0)
      else (env, since + 1)
    ) (env, 0) items
  in
  env

(* ── Module loading ───────────────────────────────────────────────────────── *)

(* What a loaded module hands back: everything it can see, the values it
   exports, their runtime bindings, the types it declares itself, and its
   docs. Its own types are kept apart from everything it can see, because a
   qualified name reaches only what the module declares. *)
type module_result =
  import_env
  * (string * Typechecker.scheme) list
  * env
  * (string * Ast.type_def) list
  * (string * string) list
  (* What evaluating this module's bindings performs: the effects an import
     of it performs in the file that writes the import. *)
  * Effect_set.EffSet.t

(* A module's cache key, by path: the hash of its source and of everything it
   imports. Recorded as modules load so a parent can fold its children's keys
   into its own -- which is what makes a stale entry unreachable instead of
   merely wrong. Per process, like `cache`. *)
let module_keys : (string, string) Hashtbl.t = Hashtbl.create 16

(* Load imports for a program.
   `cache` maps path -> result so each module is loaded once per run_program.
   `loading` detects import cycles. *)
(* `item_locs` is what `Parser.parse_program_with_locs` returns alongside
   the program, in the same order. It is optional because most callers do
   not have it and a diagnostic without a position is still a diagnostic --
   but `wand t` and the language server do have it, and an import error
   they cannot point at is one the editor cannot underline.

   A module loaded from here loads its own imports without locations, so a
   failure deep in a chain arrives as a bare `ImportError` and is pinned to
   the import that started the chain. That is the line the reader wrote. *)
(* Loading a module for its signature and loading it to use it are two
   different questions, and one cache would answer the second with the
   first. `wand t` and the language server want the types; only a run wants
   the values. Keyed apart so a session that checks a line and then runs it
   does not run against the bindings the check stood up. *)
let module_cache_key ~evaluate path =
  (if evaluate then "run:" else "sig:") ^ path

let rec load_imports_for ?(item_locs = []) ~base_dir ~cache ~loading ~evaluate prog =
  List.fold_left (fun (acc, acc_docs) (item_index, item) ->
    let at_import f =
      try f () with
      | Module_types.ImportError msg ->
        (match List.nth_opt item_locs item_index with
         | Some (start_loc, _) ->
           raise (Module_types.ImportErrorAt (start_loc, msg))
         | None -> raise (Module_types.ImportError msg))
    in
    let load_kind kind =
      let src_ref = resolve_import base_dir kind in
      let key = Module_types.key_of src_ref in
      match Hashtbl.find_opt cache (module_cache_key ~evaluate key) with
      | Some cached -> cached
      | None ->
        if List.mem key !loading then raise (Module_types.ImportError ("import cycle detected: " ^ key))
        else load_module src_ref ~cache ~loading ~evaluate
    in
    let bind_field own_type own_eval field alias =
      let t = match List.assoc_opt field own_type with
        | Some s -> s
        | None -> raise (Module_types.ImportError (Printf.sprintf "module has no exported symbol '%s'" field))
      in
      let v = match List.assoc_opt field own_eval with
        | Some v -> v
        | None -> raise (Module_types.ImportError (Printf.sprintf "module has no exported symbol '%s'" field))
      in
      ((alias, t), (alias, v))
    in
    (* An import brings in exactly what it names -- the namespace, or the
       fields a destructuring pattern lists -- and nothing else. The module's
       own environment is deliberately not spliced in: its functions are
       closures that already carry the scope they were written in, so they do
       not need the importer's, and splicing it would put every name in the
       module into scope unqualified, whether or not the import asked for it.

       Type definitions are the exception and do propagate, along with their
       constructors, because a value of an imported type is matched by
       constructor here. *)
    (* A module's own types, under the name this file gave the import:
       `Foo.Status` as well as `Status`. Step 4 stops adding the bare key. *)
    (* A module's own types, keyed canonically, and the names this file may
       write for them: `Foo.Status` for a namespace import. *)
    let qualified_types ~modul alias own_tenv =
      let canon n = Module_types.canonical_type ~modul n in
      let entries = List.map (fun (n, d) -> (canon n, d)) own_tenv in
      let names = match alias with
        | None -> []
        | Some a -> List.map (fun (n, _) -> (a ^ "." ^ n, canon n)) own_tenv
      in
      (entries, names)
    in
    (* `extra_names` is what a destructuring selected, under the names it
       gave them. `own_tenv` is the module's own, for reaching them through
       it. *)
    let add_import ~modul ?alias ?(own_tenv = []) ?(extra_names = [])
        ?(load_eff = Effect_set.EffSet.empty)
        modul_import type_entries eval_entries mod_docs =
      let (own_entries, qual_names) = qualified_types ~modul alias own_tenv in
      (* An import brings what it names. A module's types used to arrive
         under their bare names as well, so two modules that declared one
         name collided and no file could say which it meant. The definitions
         still travel -- a value of an imported type is read and matched
         here -- but under their canonical names, which no file writes. *)
      ({ tenv     = own_entries @ modul_import.tenv @ acc.tenv;
         type_env = type_entries @ acc.type_env;
         eval_env = eval_entries @ acc.eval_env;
         type_names = extra_names @ qual_names @ acc.type_names;
         (* Reached the way an imported type is: through the name this file
            bound, and only that way. `ord.Ord` says which module's `Ord` it
            means, which is the whole of what the qualifier is for. A
            destructuring binds no namespace, so it brings none. *)
         ifaces =
           (match alias with
            | None -> []
            | Some a ->
              List.map (fun (n, i) -> (a ^ "." ^ n, i)) modul_import.ifaces)
           @ acc.ifaces;
         load_effects = Effect_set.EffSet.union load_eff acc.load_effects },
       mod_docs @ acc_docs)
    in
    at_import @@ fun () ->
    match item with
    | Ast.TLImport kind ->
      let ns_name = namespace_name_of kind in
      let modul = Module_types.key_of (resolve_import base_dir kind) in
      let (modul_import, own_type, own_eval, own_tenv, mod_docs, load_eff) =
        load_kind kind in
      let prefixed_docs = List.map (fun (n, d) -> (ns_name ^ "." ^ n, d)) mod_docs in
      add_import ~modul ~alias:ns_name ~own_tenv ~load_eff modul_import
        [(ns_name, (let (ms, cs) = Typechecker.split_claims own_type in
           Typechecker.Namespace (ms, cs)))]
        (* The namespace holds the module's constructors as well as its
           values, so `Foo.Live` reads the one `Foo` declares rather than
           whichever `Live` was registered last. *)
        [(ns_name, VRecord (Evaluator.vrecord_make (own_eval @ ctor_bindings_of ~modul own_tenv)))]
        prefixed_docs
    | Ast.TLLet (name, [], body) when Option.is_some (import_kind_of body) ->
      let kind = Option.get (import_kind_of body) in
      let modul = Module_types.key_of (resolve_import base_dir kind) in
      let (modul_import, own_type, own_eval, own_tenv, mod_docs, load_eff) =
        load_kind kind in
      let prefixed_docs = List.map (fun (n, d) -> (name ^ "." ^ n, d)) mod_docs in
      add_import ~modul ~alias:name ~own_tenv ~load_eff modul_import
        [(name, (let (ms, cs) = Typechecker.split_claims own_type in
           Typechecker.Namespace (ms, cs)))]
        [(name, VRecord (Evaluator.vrecord_make (own_eval @ ctor_bindings_of ~modul own_tenv)))]
        prefixed_docs
    | Ast.TLLetPat (pat, body) when Option.is_some (import_kind_of body) ->
      let kind = Option.get (import_kind_of body) in
      let modul = Module_types.key_of (resolve_import base_dir kind) in
      let (modul_import, own_type, own_eval, own_tenv, mod_docs, load_eff) =
        load_kind kind in
      let (type_entries, eval_entries, extra_docs, selected_tenv) = match pat with
        | Ast.PVar name ->
          let pdocs = List.map (fun (n, d) -> (name ^ "." ^ n, d)) mod_docs in
          [(name, (let (ms, cs) = Typechecker.split_claims own_type in
           Typechecker.Namespace (ms, cs)))],
          [(name, VRecord (Evaluator.vrecord_make (own_eval @ ctor_bindings_of ~modul own_tenv)))],
          pdocs,
          own_tenv
        | Ast.PMap binds ->
          (* An uppercase name is a type or one of its constructors. Both are
             selected the way a value is, and renamed the same way. *)
          let is_upper n = n <> "" && n.[0] >= 'A' && n.[0] <= 'Z' in
          let (uppers, lowers) =
            List.partition (fun (field, _) -> is_upper field) binds in
          let te, ee = List.map (fun (field, p) ->
            match p with
            | Ast.PVar alias -> bind_field own_type own_eval field alias
            | _ -> raise (Module_types.ImportError "import destructuring only supports name bindings")
          ) lowers |> List.split in
          (* `{TestOutcome = Outcome}`: the new name is uppercase, so it
             parses as a constructor pattern rather than a variable. *)
          let alias_of field p =
            match p with
            | Ast.PVar a -> a
            | Ast.PConstr (a, []) -> a
            | _ -> raise (Module_types.ImportError (Printf.sprintf
                "'%s' is renamed to a name, as in {%s = Other}" field field))
          in
          let selected_tenv = List.concat_map (fun (field, p) ->
            let alias = alias_of field p in
            match List.assoc_opt field own_tenv with
            (* A type. Renaming it renames its constructor too where it has
               one, which is what an alias to a single-constructor type
               does. *)
            | Some tdef -> [(alias, tdef)]
            | None ->
              (* Not a type of its own: a constructor of one. The type comes
                 with it, under its own name, because a value of it is
                 matched by constructor. *)
              (match List.find_opt (fun (_, tdef) ->
                       match tdef with
                       | Ast.Variants (_, _, ctors) ->
                         List.exists (fun c -> c.Ast.name = field) ctors
                       | Ast.Alias _ -> false) own_tenv with
               | Some (tname, tdef) ->
                 if alias <> field then
                   raise (Module_types.ImportError (Printf.sprintf
                     "'%s' is one constructor of '%s', and renaming it would \
                      leave the others under the old name; rename the type, \
                      or reach it through the module" field tname));
                 [(tname, tdef)]
               | None -> raise (Module_types.ImportError (Printf.sprintf
                   "module has no type or constructor '%s'" field)))) uppers
          in
          let ctor_ee = List.concat_map (fun (field, p) ->
            let alias = alias_of field p in
            List.filter_map (fun (n, v) ->
              if n = field then Some (alias, v) else None)
              (ctor_bindings_of ~modul selected_tenv)) uppers
          in
          te, ee @ ctor_ee, [], selected_tenv
        | Ast.PList _ ->
          (* The 0.17 spelling. A list pattern on an import no longer
             selects members; the braces that do are one keystroke away. *)
          raise (Module_types.ImportError
            "an import is destructured with braces -- let {foo, bar} = \
             import ..., not [foo, bar]")
        | _ -> raise (Module_types.ImportError "unsupported pattern in import destructuring")
      in
      (* A destructured import brings the types it names, under the names it
         gives them. A namespace import brings the module's own, reachable
         through it. *)
      let alias = match pat with Ast.PVar n -> Some n | _ -> None in
      (* A namespace import can reach all of the module's types. A
         destructuring reaches the ones it named, under the names it gave
         them. *)
      let (own_tenv', extra_names) =
        match pat with
        | Ast.PVar _ -> (own_tenv, [])
        | _ ->
          ([], List.map (fun (alias, _) ->
             (* `selected_tenv` is keyed by the alias; the canonical name is
                the module's own for that type. *)
             (alias, Module_types.canonical_type ~modul
                       (match List.find_opt (fun (n, d) ->
                                List.mem_assoc alias selected_tenv
                                && List.assoc alias selected_tenv == d
                                && n <> "") own_tenv with
                        | Some (n, _) -> n
                        | None -> alias))) selected_tenv)
      in
      add_import ~modul ?alias ~own_tenv:own_tenv' ~extra_names ~load_eff modul_import
        type_entries eval_entries extra_docs
    | _ -> (acc, acc_docs)
  ) (empty_import_env, []) (List.mapi (fun i it -> (i, it)) prog.Ast.items)

and load_module src_ref ~cache ~loading ~evaluate =
  (* Embedded or on disk, a module is a name and some source from here on:
     the name keys the caches and the cycle check, and nothing below asks
     where the bytes came from. *)
  let path = Module_types.key_of src_ref in
  let src = Module_types.read_source src_ref in
  let tokens =
    (* Every position from here names this file, so an error raised inside an
       imported module says which one rather than a bare line number the
       reader cannot place. *)
    try Lexer.tokenize ~file:path src
    with Lexer.LexError (loc, msg) ->
      raise (Module_types.ImportError (Printf.sprintf "lex error in '%s': %d:%d: %s"
                  path loc.Token.line loc.Token.col msg))
  in
  let prog =
    try Parser.parse_program tokens
    with Parser.ParseError (loc, msg) ->
      let pos = match loc with
        | Some l -> Printf.sprintf "%d:%d: " l.Token.line l.Token.col
        | None -> ""
      in
      raise (Module_types.ImportError (Printf.sprintf "parse error in '%s': %s%s" path pos msg))
  in
  let base_dir = Filename.dirname path in
  loading := path :: !loading;
  let (imported, imp_docs) = load_imports_for ~base_dir ~cache ~loading ~evaluate prog in
  (* Settled here for the reason `run_program` settles the entry: the
     typechecker and the evaluator have to be handed the same program.

     A module was skipping this, so `type P2 = Point` beside the `Point` it
     names stayed a variant declaring a nullary constructor called `Point` --
     which took the real one's fields with it, for every file that imported
     the module and whether or not it ever wrote the alias. *)
  let prog = Typechecker.settle_aliases ~init_tenv:imported.tenv prog in
  (* The key covers this module's source and its imports' keys, which cover
     theirs. Parsing happens either way -- it is a fifth of what inference
     costs, and the import list has to be read to know what the key depends
     on. Inference is what the entry saves. *)
  (* A binding's right-hand side arrives wrapped in its position, so the
     import has to be read through that wrapper rather than matched raw.
     Matched raw, every `let x = import ./y` fell out of the key and a change
     to `y` left `x` served from the cache: `wand t` passed code that does not
     typecheck, and only clearing the cache said otherwise. A bare
     `import M` matched, which is why the standard library never showed it --
     a module that never changes cannot go stale. *)
  let dep_keys =
    List.filter_map (fun item ->
      let kind = match item with
        | Ast.TLImport kind -> Some kind
        | Ast.TLLet (_, [], body) | Ast.TLLetPat (_, body) ->
          Module_types.import_kind_of body
        | _ -> None
      in
      match kind with
      | None -> None
      | Some kind ->
        Hashtbl.find_opt module_keys
          (Module_types.key_of (resolve_import base_dir kind)))
      prog.Ast.items
  in
  (* This module's own types, by the name a reader writes and the name they
     are known by everywhere else. *)
  let own_type_names =
    List.filter_map (function
      | Ast.TLType ((Ast.Variants (n, _, _) | Ast.Alias (n, _, _)), _) ->
        Some (n, Module_types.canonical_type ~modul:path n)
      | _ -> None) prog.Ast.items
  in
  let own_key = Compile_cache.key ~path ~source:src ~deps:dep_keys in
  Hashtbl.replace module_keys path own_key;
  (* Only the module's own share is written down. What inference returns is
     `own @ constructors @ stdlib @ imports`, and the last two are the same
     for everyone -- storing them would put a copy of the whole standard
     library in every entry, which costs more to write and read back than
     the inference it saves. The tail is rebuilt from what is already in
     hand. *)
  let tail_len =
    List.length Typechecker.stdlib_type_env + List.length imported.type_env
  in
  let rebuild own_part =
    own_part @ Typechecker.stdlib_type_env @ imported.type_env
  in
  let refresh = List.map (fun (n, s) -> (n, Typechecker.refresh_scheme s)) in
  let inferred =
    match Compile_cache.find own_key with
    | Some (own_part, own_type, load_eff) ->
      Ok (rebuild (refresh own_part), refresh own_type, load_eff)
    | None ->
      (match Typechecker.infer_program_env_with_own
               ~init_tenv:imported.tenv ~init_env:imported.type_env
               ~init_ifaces:imported.ifaces
               ~init_effects:imported.load_effects
               ~type_names:(own_type_names @ imported.type_names) prog with
       | Ok (type_env, own_type, load_eff) as ok ->
         let n_own = List.length type_env - tail_len in
         if n_own >= 0 then begin
           let own_part = List.filteri (fun i _ -> i < n_own) type_env in
           (* Stored only if the tail really is what it is assumed to be.
              Compared by name: a scheme is a graph of mutable variables and
              deep-comparing several hundred of them would cost more than the
              inference being cached. *)
           if List.map fst (rebuild own_part) = List.map fst type_env then
             Compile_cache.store own_key (own_part, own_type, load_eff)
         end;
         ok
       | Error _ as e -> e)
  in
  let result =
    (match inferred with
     | Error msg -> raise (Module_types.ImportError ("type error: " ^ msg))
     | Ok (type_env, own_type, own_load_eff) ->
       (* Indexed here: everything a module can see that it did not define
          itself is fixed by this point, and every name the module goes on to
          look up sits in front of it. *)
       let base = index_env (stdlib_eval_env @ imported.eval_env) in
       let full_eval =
         if not evaluate then
           (* The caller asked what this module is, not what it does, so the
              body is never run: each exported name stands in the
              environment with nothing behind it. `wand t`, the language
              server and the linters all read types from here, and a value
              reached from an analysis load is a bug in the caller -- it
              says so rather than answering with a plausible unit. *)
           List.map (fun (n, _) ->
             (n, VBuiltin (fun _ ->
                raise (Evaluator.EvalError (Printf.sprintf
                  "internal error: '%s' of module '%s' was used by a path \
                   that loaded the module for its types only" n path)))))
             own_type
           @ base
         else
           (* Under the same handler a script's own body runs under. An
              import evaluates the module's bindings, and
              `let greeting = $(hostname)` at the top of one is work like any
              other -- it used to reach an empty handler stack and end the
              program with OCaml's own `Effect.Unhandled`, because the load
              happened before the handler went in rather than because the
              effect was not allowed. The manifest still decides what a
              module may do; this decides only that it is asked. *)
           let out = ref base in
           ignore (run_with_default_handler (fun () ->
             with_file_net (net_bound_of_manifest prog.Ast.manifest) (fun () ->
               out := fold_items (run_item ~modul:path) base prog.Ast.items);
             VUnit));
           !out
       in
       let n_own = List.length full_eval - List.length base in
       let own_eval = List.filteri (fun i _ -> i < n_own) full_eval
         |> List.filter (fun (n, _) -> not (is_private n)) in
       let own_type = List.filter (fun (n, _) -> not (is_private n)) own_type in
       let own = local_tenv_of prog in
       let own_names = List.map fst own in
       (* A derived decoder reads the field types off the declaration, and a
          field that names one of the module's own types has to name it
          canonically: two modules may each declare a `Meta`, and the
          bare-name index holds one of them. `run_item` registered these from
          the raw declaration, where the field still says `Meta`, so a nested
          field decoded to whichever module was loaded last. Registered again
          here, where the canonicalised declaration is. *)
       if evaluate then
         List.iter (fun (n, d) ->
           match Module_types.canonicalise_tdef ~modul:path own_names d with
           | Ast.Variants (_, params, [ctor])
             when ctor.Ast.fields <> []
                  && List.for_all (fun (fn, _) -> fn <> None) ctor.Ast.fields ->
             Hashtbl.replace Evaluator.derivable
               (Module_types.canonical_type ~modul:path n)
               (Ctor.Owned (path, ctor.Ast.name), params, ctor.Ast.fields)
           | _ -> ()) own;
       let full_import =
         { tenv = List.map (fun (n, d) ->
                    (Module_types.canonical_type ~modul:path n,
                     Module_types.canonicalise_tdef ~modul:path own_names d))
                    own
                  @ imported.tenv;
           type_env;
           eval_env = full_eval;
           type_names = own_type_names @ imported.type_names;
           (* This module's own, under the names it declared them with, for
              its importer to qualify. Its imports' are deliberately not
              here: an interface belongs to the module that declares it, and
              reaching one through a module that merely imported it would
              put a name in scope that no file asked for. *)
           ifaces =
             List.filter_map (function
               | Ast.TLInterface (i, _) -> Some (i.Ast.if_name, i)
               | _ -> None) prog.Ast.items;
           load_effects = own_load_eff } in
       (full_import, own_type, own_eval,
        List.map (fun (n, d) ->
          (n, Module_types.canonicalise_tdef ~modul:path own_names d)) own,
        prog.Ast.docs @ imp_docs, own_load_eff))
  in
  Hashtbl.replace cache (module_cache_key ~evaluate path) result;
  loading := List.filter (fun p -> p <> path) !loading;
  result

(* The signature of a standard library module the buffer has *not* imported:
   the editor asks when deciding whether `FS.write_file!` can resolve, what
   to show on hover, and what an auto-import commits the manifest to. Exact
   name only -- the case fallback that softens `import list` at run time
   would make an auto-edit guess, and a guessed edit is the one thing that
   tier must never produce. Loading is exactly what `import M` does, cached
   per process like the module caches. *)
let stdlib_sig_cache :
  (string, (Typechecker.env * (string * string) list) option) Hashtbl.t =
  Hashtbl.create 8

let stdlib_module_sig name :
  (Typechecker.env * (string * string) list) option =
  match Hashtbl.find_opt stdlib_sig_cache name with
  | Some r -> r
  | None ->
    let r =
      if not (List.mem_assoc name Stdlib_embed.table) then None
      else
        match
          load_module (Module_types.resolve_stdlib name)
            ~cache:(Hashtbl.create 8) ~loading:(ref []) ~evaluate:false
        with
        | (_, own_type, _, _, docs, _) -> Some (own_type, docs)
        | exception _ -> None
    in
    Hashtbl.replace stdlib_sig_cache name r;
    r

(* Where a program's names are defined: each top-level binding, pattern
   name, type and constructor, at its item's first token. Imports are left
   out on purpose -- `import FS` binds FS, but the definition a jump wants
   is the module's source, which the editor reaches through the stdlib
   tables rather than a line that merely names it. *)
let defs_of_program (prog : Ast.program)
    (item_locs : (Token.loc * Token.loc) list) : (string * Token.loc) list =
  let locs = Array.of_list item_locs in
  List.concat
    (List.mapi (fun i (item : Ast.top_item) ->
       let loc =
         if i < Array.length locs then fst locs.(i) else Token.point 1 1 0
       in
       (* A member of an implementation is a definition in its own right, so
          it answers with the position of its own `let`. Given the block's
          position, an editor put every member's signature on one line. *)
       match item with
       | Ast.TLImplement (im, _) ->
         List.map (fun (n, _, _, l) -> (n, l)) im.Ast.im_binds
       | _ ->
       let names = match item with
         | Ast.TLLet (name, _, _) -> [name]
         | Ast.TLLetRec bs -> List.map (fun (n, _, _) -> n) bs
         | Ast.TLLetPat (pat, _) -> Lint.pat_names pat
         | Ast.TLType (Ast.Alias (tname, _, _), _) -> [tname]
         | Ast.TLType (Ast.Variants (tname, _, ctors), _) ->
           tname :: List.map (fun (c : Ast.ctor_def) -> c.Ast.name) ctors
         | Ast.TLInterface (i, _) -> [i.Ast.if_name]
         | Ast.TLImplement _ -> []
         | Ast.TLImport _ | Ast.TLExpr _ -> []
       in
       List.map (fun n -> (n, loc)) names)
       prog.Ast.items)

(* A standard library module's source text and definition sites, for the
   editor's go-to-definition: the jump target is a virtual document served
   from these same bytes, so the two cannot disagree. Parse only -- no
   inference -- and cached per process. *)
let stdlib_src_cache :
  (string, (string * (string * Token.loc) list) option) Hashtbl.t =
  Hashtbl.create 8

let stdlib_module_source_and_defs name :
  (string * (string * Token.loc) list) option =
  match Hashtbl.find_opt stdlib_src_cache name with
  | Some r -> r
  | None ->
    let r =
      if not (List.mem_assoc name Stdlib_embed.table) then None
      else
        match
          let src =
            Module_types.read_source (Module_types.resolve_stdlib name) in
          let (prog, item_locs) =
            Parser.parse_program_with_locs (Lexer.tokenize src) in
          (src, defs_of_program prog item_locs)
        with
        | r -> Some r
        | exception _ -> None
    in
    Hashtbl.replace stdlib_src_cache name r;
    r

(* ── Run a parsed program ─────────────────────────────────────────────────── *)

(* ── What a rehearsal remembers ───────────────────────────────────────────── *)

(* A rehearsal withholds a change and reports it, and it runs reads for real
   so the script takes the path it would really take. Those two together are
   a contradiction the moment a script reads back what it wrote: the write
   was withheld, so the read finds nothing, and the rehearsal fails on a line
   the real run never fails on -- after reporting two steps as though they
   had happened.

   So a rehearsal remembers what it withheld, and answers later reads from
   that memory before it looks at the disk. A path this does not name is not
   known to it and the read goes to the disk, which is what keeps the rest of
   the tree honest.

   It is not a sandbox and must not become one. Nothing is intercepted here
   that was not intercepted before; this only decides what a withheld
   operation answers with. It lives beside the rehearsal rather than in the
   default handler, because the default handler is the one implementation of
   each operation and a rehearsal must not own a second one.

   The contents are in memory. A rehearsal of a script that writes a large
   file holds that file, which is the cost of answering the read after it.
   The alternative is a scratch tree on disk, and writing to disk is the one
   thing a rehearsal promises not to do. *)
type remembered =
  | RFile of string * float     (* contents, and when the rehearsal wrote it *)
  | RDir
  | RGone

type overlay = {
  paths : (string, remembered) Hashtbl.t;
  vars  : (string, string option) Hashtbl.t;   (* None: cleared *)
}

let new_overlay () = { paths = Hashtbl.create 16; vars = Hashtbl.create 8 }

(* The rehearsal in progress, or None during a real run. One per run, and
   the rehearsal's handler is the only reader and writer. *)
let rehearsal : overlay option ref = ref None

let with_overlay f = match !rehearsal with None -> () | Some o -> f o

(* Trailing slashes make two spellings of one directory, and a path built by
   `Path.join` and one written as a literal have to agree. *)
let normal p =
  let n = String.length p in
  if n > 1 && p.[n - 1] = '/' then String.sub p 0 (n - 1) else p

let under ~dir p =
  let dir = normal dir and p = normal p in
  String.length p > String.length dir + 1
  && String.sub p 0 (String.length dir + 1) = dir ^ "/"

let remember_file o p contents =
  Hashtbl.replace o.paths (normal p) (RFile (contents, Unix.gettimeofday ()))

let rec remember_dir o p =
  let p = normal p in
  if p <> "" && p <> "/" && not (Hashtbl.mem o.paths p) then begin
    Hashtbl.replace o.paths p RDir;
    remember_dir o (Filename.dirname p)
  end

let forget_path o p = Hashtbl.replace o.paths (normal p) RGone

(* What a path holds now: the overlay's answer, or the disk's, or nothing.
   A rehearsal reads the disk through the same calls a real run makes. *)
let contents_of o p =
  match Hashtbl.find_opt o.paths (normal p) with
  | Some (RFile (c, _)) -> Some c
  | Some RDir -> None
  | Some RGone -> None
  | None ->
    (try Some (In_channel.with_open_text p In_channel.input_all)
     with Sys_error _ -> None)

let remember_delete_tree o p =
  let p = normal p in
  Hashtbl.iter (fun k _ -> if under ~dir:p k then Hashtbl.replace o.paths k RGone)
    (Hashtbl.copy o.paths);
  Hashtbl.replace o.paths p RGone

(* Every file under a real directory, so a copied tree can be read back. A
   rehearsal walks it once; a real copy would have read the same files. *)
let rec real_files dir =
  match Sys.readdir dir with
  | entries ->
    Array.to_list entries
    |> List.concat_map (fun e ->
         let p = Filename.concat dir e in
         if (try Sys.is_directory p with Sys_error _ -> false)
         then real_files p else [p])
  | exception Sys_error _ -> []

(* One withheld operation, written into the overlay. Every case here is a
   case `is_mutation` withholds; anything it does not withhold really
   happens and needs no memory of its own. *)
let remember_change name (v : value) =
  with_overlay (fun o ->
    let path = function
      | VPath p | VString p -> Some p
      | _ -> None
    in
    match name, v with
    | ("FS!write_file" | "FS!write_atomic"), VTuple [p; VString content] ->
      (match path p with Some p -> remember_file o p content | None -> ())
    | "FS!append", VTuple [p; VString content] ->
      (match path p with
       | Some p ->
         let before = match contents_of o p with Some c -> c | None -> "" in
         remember_file o p (before ^ content)
       | None -> ())
    | "FS!create_file", p ->
      (match path p with
       | Some p -> if contents_of o p = None then remember_file o p ""
       | None -> ())
    | "FS!delete", p ->
      (match path p with Some p -> forget_path o p | None -> ())
    | "FS!delete_tree", p ->
      (match path p with Some p -> remember_delete_tree o p | None -> ())
    | "FS!mkdir", p ->
      (match path p with Some p -> remember_dir o p | None -> ())
    | "FS!rename", VTuple [a; b] ->
      (match path a, path b with
       | Some a, Some b ->
         (match contents_of o a with
          | Some c -> remember_file o b c
          | None -> remember_dir o b);
         forget_path o a
       | _ -> ())
    | "FS!copy", VTuple [a; b] ->
      (match path a, path b with
       | Some a, Some b ->
         (match contents_of o a with Some c -> remember_file o b c | None -> ())
       | _ -> ())
    | "FS!copy_tree", VTuple [a; b] ->
      (match path a, path b with
       | Some a, Some b ->
         remember_dir o b;
         List.iter (fun f ->
           let rest =
             String.sub f (String.length (normal a) + 1)
               (String.length f - String.length (normal a) - 1) in
           match contents_of o f with
           | Some c -> remember_file o (Filename.concat b rest) c
           | None -> ()) (real_files a)
       | _ -> ())
    | "Env!set", VTuple [VString name; VString value] ->
      Hashtbl.replace o.vars name (Some value)
    | "Env!clear", VString name -> Hashtbl.replace o.vars name None
    | _ -> ())

(* What a read answers during a rehearsal, when the overlay knows about the
   path. `None` means it does not, and the operation goes to the default
   handler and reads the disk.

   Every read is answered here, whether or not it was withheld -- reads are
   never withheld, so this is the only place a rehearsal's own writes can
   become visible. *)
let overlay_read name (v : value) : (value, string) result option =
  match !rehearsal with
  | None -> None
  | Some o ->
    let path = function
      | VPath p | VString p -> Some (normal p)
      | _ -> None
    in
    let known p = Hashtbl.find_opt o.paths p in
    (* A directory the overlay made, or one holding a file it made. *)
    let is_dir p =
      match known p with
      | Some RDir -> Some true
      | Some (RFile _) -> Some false
      | Some RGone -> Some false
      | None ->
        if Hashtbl.fold (fun k e found ->
             found || (e <> RGone && under ~dir:p k)) o.paths false
        then Some true else None
    in
    let missing p = Error (Printf.sprintf "%s: No such file or directory" p) in
    let bool_answer b = Some (Ok (VBool b)) in
    match name, path v, v with
    | ("FS!read_file" | "FS!stream_lines"), Some p, _ ->
      (match known p with
       | Some (RFile (c, _)) ->
         Some (Ok (if name = "FS!read_file" then VString c
                   else VList (List.map (fun l -> VString l)
                                 (String.split_on_char '\n'
                                    (if c <> "" && c.[String.length c - 1] = '\n'
                                     then String.sub c 0 (String.length c - 1)
                                     else c)
                                  |> fun ls -> if c = "" then [] else ls))))
       | Some RGone -> Some (missing p)
       | Some RDir -> Some (Error (Printf.sprintf "%s: Is a directory" p))
       | None -> None)
    | "Hash!file", _, VTuple [VString algo; p] ->
      (match path p with
       | Some p ->
         (match known p with
          | Some (RFile (c, _)) -> Some (Ok (VString (hash_string_hex algo c)))
          | Some RGone -> Some (missing p)
          | _ -> None)
       | None -> None)
    | "FS!exists?", Some p, _ ->
      (match known p with
       | Some RGone -> bool_answer false
       | Some _ -> bool_answer true
       | None -> (match is_dir p with Some true -> bool_answer true | _ -> None))
    | "FS!file?", Some p, _ ->
      (match known p with
       | Some (RFile _) -> bool_answer true
       | Some (RDir | RGone) -> bool_answer false
       | None -> None)
    | "FS!dir?", Some p, _ ->
      (match known p with
       | Some RGone | Some (RFile _) -> bool_answer false
       | Some RDir -> bool_answer true
       | None -> (match is_dir p with Some d -> bool_answer d | None -> None))
    | "FS!size", Some p, _ ->
      (match known p with
       | Some (RFile (c, _)) ->
         Some (Ok (VSize (Printf.sprintf "%dB" (String.length c))))
       | Some RGone -> Some (missing p)
       | _ -> None)
    (* The moment the rehearsal wrote it. Nothing else could be true: the
       real run has not happened. *)
    | "FS!mtime", Some p, _ ->
      (match known p with
       | Some (RFile (_, at)) ->
         let tm = Unix.gmtime at in
         Some (Ok (VDateTime (Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02dZ"
           (tm.Unix.tm_year + 1900) (tm.Unix.tm_mon + 1) tm.Unix.tm_mday
           tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec)))
       | Some RGone -> Some (missing p)
       | _ -> None)
    (* The disk's entries and the overlay's together, minus what it says is
       gone. A rehearsal that writes three files and lists the directory
       sees three more entries than the disk has, which is what the real run
       would see. *)
    | "FS!list_dir", Some p, _ ->
      (match known p with
       | Some RGone -> Some (missing p)
       | _ ->
         let real =
           match Sys.readdir p with
           | entries -> Array.to_list entries |> List.map (Filename.concat p)
           | exception Sys_error _ -> []
         in
         let added =
           Hashtbl.fold (fun k e acc ->
             if e <> RGone && Filename.dirname k = p then k :: acc else acc)
             o.paths []
         in
         let gone k = Hashtbl.find_opt o.paths (normal k) = Some RGone in
         let all =
           List.sort_uniq compare
             (List.filter (fun k -> not (gone k)) real @ added) in
         if real = [] && added = [] && is_dir p = None then None
         else Some (Ok (VList (List.map (fun p -> VPath p) all))))
    (* A glob is a read of a directory, so it answers with the disk's matches
       and the overlay's together. Matching is the same compile the walk
       uses, against the path relative to the base, so a pattern cannot mean
       one thing on disk and another here. *)
    | "FS!glob", _, VTuple [pattern; dir] ->
      let pat = match pattern with
        | VGlob g | VString g -> Some g | _ -> None in
      let base = match dir with
        | VPath d | VString d -> Some (normal d) | _ -> None in
      (match pat, base with
       | Some pat, Some base when Hashtbl.length o.paths > 0 ->
         (match Evaluator.glob_compile pat with
          | Error _ -> None
          | Ok re ->
            let real =
              match List.assoc "fs_glob_impl" Evaluator.stdlib_eval_env with
              | VBuiltin f ->
                (match f pattern with
                 | VBuiltin g -> (match g dir with VList vs -> vs | _ -> [])
                 | _ -> [])
              | _ -> []
            in
            let gone p = Hashtbl.find_opt o.paths (normal p) = Some RGone in
            let kept =
              List.filter (function VPath p -> not (gone p) | _ -> true) real in
            let added =
              Hashtbl.fold (fun k e acc ->
                match e with
                | RFile _ when under ~dir:base k ->
                  let rel =
                    String.sub k (String.length base + 1)
                      (String.length k - String.length base - 1) in
                  if Re.execp re rel then VPath k :: acc else acc
                | _ -> acc) o.paths []
            in
            let seen = List.filter_map
                         (function VPath p -> Some p | _ -> None) kept in
            let fresh =
              List.filter (function VPath p -> not (List.mem p seen) | _ -> true)
                added in
            Some (Ok (VList (kept @ List.sort compare fresh))))
       | _ -> None)
    (* The environment has the same hole: `Env!set` is withheld, so a script
       that sets a variable and reads it back was told what the shell had.
       Same overlay, same rule. *)
    | "Env!get", _, VString name ->
      (match Hashtbl.find_opt o.vars name with
       | Some (Some v) -> Some (Ok (VString v))
       | Some None -> Some (Error ("env: variable not set: " ^ name))
       | None -> None)
    | "Env!all", _, VUnit ->
      if Hashtbl.length o.vars = 0 then None
      else
        let real =
          Array.to_list (Unix.environment ())
          |> List.filter_map (fun s ->
               match String.index_opt s '=' with
               | Some i -> Some (String.sub s 0 i,
                                 String.sub s (i + 1) (String.length s - i - 1))
               | None -> None)
        in
        let changed = Hashtbl.fold (fun k v acc -> (k, v) :: acc) o.vars [] in
        let kept =
          List.filter (fun (k, _) -> not (List.mem_assoc k changed)) real
          @ List.filter_map (fun (k, v) ->
              match v with Some v -> Some (k, v) | None -> None) changed
        in
        Some (Ok (VList (List.map (fun (k, v) ->
          VTuple [VString k; VString v]) (List.sort compare kept))))
    | _ -> None

(* A rehearsal answers a request for a temp file or directory with a name
   rather than a file, so the overlay learns the name it handed out. Without
   this a script that makes a scratch directory and asks whether it is there
   is told no. *)
let remember_substitute name (sub : (value * string) option) =
  with_overlay (fun o ->
    match name, sub with
    | "FS!temp_dir", Some (VPath p, _) -> remember_dir o p
    | "FS!temp_file", Some (VPath p, _) -> remember_file o p ""
    | _ -> ())

(* What a withheld sink answers with: a sink that collects the lines and
   writes them into the overlay when the stream ends. The stream is still
   run, so the source is still read and the stages still perform what they
   perform -- a rehearsal reports the reads under a write, as it always
   has. *)
let substitute_sink name (v : value) : value option =
  match !rehearsal, name, v with
  | Some o, ("FS!write_lines" | "FS!append_lines" | "FS!write_lines_atomic"),
    (VPath p | VString p) ->
    let buf = Buffer.create 256 in
    if name = "FS!append_lines" then
      (match contents_of o p with Some c -> Buffer.add_string buf c | None -> ());
    let remember () = remember_file o p (Buffer.contents buf) in
    (* A rehearsal follows the path a real run would take, so it has to end
       the way a real run would. The two straight-to-the-file sinks leave a
       part-written file behind when a stream raises, so the overlay
       remembers one. The publishing sink leaves the target untouched, so
       the overlay remembers nothing and a later read in the same rehearsal
       still answers what is on the disk. *)
    let abort = if name = "FS!write_lines_atomic" then (fun () -> ()) else remember in
    Some (VLineSink
            ((fun line -> Buffer.add_string buf line; Buffer.add_char buf '\n'),
             remember, abort))
  | _ -> None

(* Wraps a program in the chosen mode. The mode handler sits inside the
   default one, so an operation it decides to allow is simply performed
   again and carried out as usual -- there is one implementation of each
   operation, and a rehearsal cannot drift from a real run by reimplementing
   them. *)
let run_in_mode mode (thunk : unit -> value) : value =
  current_mode := mode;
  rehearsal := (if mode = DryRun then Some (new_overlay ()) else None);
  match mode with
  | Normal -> run_with_default_handler thunk
  | Trace | DryRun ->
    (* A rehearsal or trace is an observer, so Par sends its workers' effects
       back here to be reported rather than letting them run on their own. *)
    Evaluator.observed (fun () ->
    run_with_default_handler (fun () ->
      Effect.Deep.match_with thunk ()
        { Effect.Deep.
            retc = (fun v -> v);
            exnc = raise;
            effc = fun (type a) (eff : a Effect.t) ->
              match eff with
              | WandEffect (name, v) ->
                Some (fun (k : (a, value) Effect.Deep.continuation) ->
                  let described = describe_operation name v in
                  let withhold =
                    mode = DryRun && (is_mutation name || is_mutation_value name v)
                  in
                  (* The one operation a rehearsal neither carries out nor
                     withholds. Two rules already settled point opposite ways
                     here: a rehearsal takes a lock for real, so it cannot
                     run beside a real run, and a rehearsal withholds a sleep,
                     so nobody waits out a backoff to be told what a script
                     would do.

                     So it takes the lock and declines to wait. When the lock
                     is free the two are the same thing. They differ only when
                     it is held, which is exactly when waiting costs the most
                     and says the least -- and the line below says the wait
                     was skipped, so a rehearsal that reports `Held` is not
                     mistaken for a real run that would have got the lock. *)
                  let skip_wait = mode = DryRun && name = "FS!lock_wait" in
                  (* An exit is neither carried out nor withheld either: a
                     run ends here, so a rehearsal that carried on would
                     report writes a run would never reach. It ends, and
                     says that is why -- a plan that simply stopped read as
                     a plan that had finished. *)
                  let stops_here = mode = DryRun && name = "Proc!exit" in
                  (* Decided once: the substitute is now a fresh name each
                     time it is asked for, and the line reporting it has to
                     name the one the script was actually handed. *)
                  let substitute = if withhold then substitute_for name else None in
                  (match described with
                   | Some (verb, what) ->
                     if withhold then
                       (match substitute with
                        | Some (_, shown) ->
                          report "would %s: %s -> %s\n" verb what shown
                        | None -> report "would %s: %s\n" verb what)
                     else if skip_wait then
                       report "%s: %s (a rehearsal does not wait)\n" verb what
                     else if stops_here then
                       report "%s: %s (the rehearsal ends here, as a run \
                               would)\n" verb what
                     else report "%s: %s\n" verb what
                   | None -> ());
                  if withhold then begin
                    (* Withheld, so nothing outside the program changed --
                       and the rehearsal writes down what it would have
                       been, so the reads after it answer as the real run
                       would. *)
                    remember_change name v;
                    remember_substitute name substitute;
                    match substitute_sink name v, substitute with
                    | Some sink, _ -> Effect.Deep.continue k sink
                    | None, Some (v, _) -> Effect.Deep.continue k v
                    | None, None        -> Effect.Deep.continue k VUnit
                  end
                  else if skip_wait then
                    (* The non-waiting acquire, which is the same operation
                       with the budget spent down to nothing. *)
                    let path = match v with VTuple (p :: _) -> p | other -> other in
                    (match (try Ok (Effect.perform (WandEffect ("FS!lock", path)))
                            with EvalError m -> Error m) with
                     | Ok result -> Effect.Deep.continue k result
                     | Error m -> Effect.Deep.discontinue k (EvalError m))
                  else
                    (* A read of something the rehearsal already changed is
                       answered from what it remembers. Anything it does not
                       know goes to the default handler, which owns the real
                       behaviour and reads the real disk. *)
                    match overlay_read name v with
                    | Some (Ok result) -> Effect.Deep.continue k result
                    | Some (Error m) -> Effect.Deep.discontinue k (EvalError m)
                    | None ->
                    match (try Ok (Effect.perform (WandEffect (name, v)))
                           with EvalError m -> Error m) with
                    | Ok result -> Effect.Deep.continue    k result
                    | Error m   -> Effect.Deep.discontinue k (EvalError m))
              | _ -> None
        }))

(* ── Par ─────────────────────────────────────────────────────────────────── *)

(* Fork-join, and nothing else. Workers never outlive the call, there is no
   handle to a running one, and the only way to start any is these two
   functions -- so a script cannot build unstructured concurrency out of
   them.

   Each worker installs the program's handlers on its own domain, because an
   effect performed on one domain does not reach a handler on another. A
   worker without them would escape a rehearsal, which is the one thing a
   rehearsal must not permit.

   A failure is a value: one item raising does not cancel its siblings or
   fail the call, it comes back as an Error in that item's place. *)
let () = Evaluator.with_default_handler := run_with_default_handler

let run_program ?(mode = Normal) ~base_dir prog =
  let cache = Hashtbl.create 8 in
  let loading = ref [] in
  let (imp, _) = load_imports_for ~base_dir ~cache ~loading ~evaluate:true prog in
  (* Settled once, here, so the typechecker and the evaluator are handed the
     same program: `type This = That` is an alias to both of them or a
     variant to both, never one to each. *)
  let prog = Typechecker.settle_aliases ~init_tenv:imp.tenv prog in
  (match Typechecker.infer_program_env ~init_tenv:imp.tenv ~init_env:imp.type_env
           ~init_ifaces:imp.ifaces ~type_names:imp.type_names prog with
   | Error msg -> Error ("type error: " ^ msg)
   | Ok _ ->
     let result = run_in_mode mode (fun () ->
       with_file_net (net_bound_of_manifest prog.Ast.manifest) (fun () ->
       let ((_, last), _) = List.fold_left (fun ((env, last), since) item ->
         (* Each statement starts without a position, so a failure before it
            reaches one is not reported against the statement before it. *)
         Evaluator.forget_loc ();
         let (env, last) =
           match item with
           | Ast.TLExpr e -> (env, eval env e)
           | _            -> (run_item env item, last)
         in
         (* A file's own definitions pile up in front of the base, so a fresh
            index goes in every so often: without it everything defined late
            walks past everything defined early. *)
         if since >= Evaluator.index_every then ((Evaluator.index_env env, last), 0)
         else ((env, last), since + 1)
       ) ((index_env (base_eval_env @ imp.eval_env), VUnit), 0) prog.Ast.items
       in last)
     ) in
     (* A request that arrived with nothing left to evaluate would otherwise
        be dropped, and the script would report success after being asked to
        stop. Whatever it was holding has already been released by the
        unwinding above -- this is only about saying so. *)
     Evaluator.check_interrupt ();
     (* What a script leaves behind is its output, not a value being
        inspected: a script that ends in a string wrote that string. The
        REPL and `wand e` show the value instead, and quote it. *)
     Ok (to_text result))

(* ── Public API ───────────────────────────────────────────────────────────── *)

let run_string src =
  try
    let tokens = Lexer.tokenize src in
    let prog   = Parser.parse_program tokens in
    run_program ~base_dir:(Sys.getcwd ()) prog
  with
  | EvalError msg -> Error ("eval error: " ^ Evaluator.stamp_loc msg)
  | (Lexer.LexError _ | Parser.ParseError _ | Module_types.ImportError _
    | Module_types.ImportErrorAt _ | Failure _) as e ->
    Error (legacy_of_exn e)

(* ── Stopping ─────────────────────────────────────────────────────────────── *)

(* A script that is stopped from outside should leave nothing behind, so a
   signal becomes an exception and unwinds like anything else -- every `with`
   on the stack releases on the way out.

   Installing a handler also un-ignores the signal: a script started as a
   background job inherits SIG_IGN for SIGINT, and would otherwise not stop
   at all. *)
let interrupting = Atomic.make false

let install_signal_handlers () =
  let stop signal code (_ : int) =
    (* Taking the request also hands the signal back to the system, so a
       second one stops the process outright, without cleanup, the way it
       would have if wand had never installed a handler.

       This is bounded by how OCaml delivers signals: a handler runs when
       the program next reaches a safe point, so neither the first request
       nor the second is seen while it sits in a syscall waiting on a slow
       command. At a terminal that does not arise -- the whole process group
       is signalled, the command dies with it, and the wait ends at once.
       Signalling wand alone, as a supervisor does, waits for the commands
       already running. *)
    if not (Atomic.exchange interrupting true) then begin
      Sys.set_signal signal Sys.Signal_default;
      Evaluator.request_interrupt code;
      (* Stop what we started. Until the commands wand is waiting on end, it
         is inside a read and cannot act on the request at all -- so the
         script's own cleanup is waiting on processes nobody is watching any
         more. Terminated rather than interrupted, because a child signalled
         alongside its parent has already had its chance to notice. *)
      stop_children Sys.sigterm
    end
  in
  (* 128 + the signal number, which is what a shell reports and what CI
     reads, so nothing downstream has to learn a wand-specific code. *)
  Sys.set_signal Sys.sigint  (Sys.Signal_handle (stop Sys.sigint  130));
  Sys.set_signal Sys.sigterm (Sys.Signal_handle (stop Sys.sigterm 143));
  (* A closed reader downstream must not kill wand outright: dying at the
     write leaves whatever the script holds -- a temp directory, a lock, a
     process -- exactly as it was. Ignored, the write fails instead, and the
     failure travels back through the script the way any other does. *)
  Sys.set_signal Sys.sigpipe Sys.Signal_ignore

(* For a session that survives an interrupt. A script stops at the first
   one, so it never needs this; a prompt takes as many as it is given. *)
let rearm_signal_handlers () =
  Atomic.set interrupting false;
  Evaluator.clear_interrupt ();
  install_signal_handlers ()

let run_file ?(mode = Normal) path =
  let full = entry_path path in
  try
    let src      = In_channel.with_open_text full In_channel.input_all in
    (* The file the run was asked for. A position in it is reported bare,
       because it is the file the reader is looking at; a position from
       anywhere else says where it is. *)
    Evaluator.entry_file := full;
    let tokens   = Lexer.tokenize ~file:full src in
    let prog     = Parser.parse_program tokens in
    let base_dir = Filename.dirname full in
    run_program ~mode ~base_dir prog
  with
  | Sys_error msg         -> Error ("cannot open file: " ^ msg)
  | EvalError msg -> Error ("eval error: " ^ Evaluator.stamp_loc msg)
  | (Lexer.LexError _ | Parser.ParseError _ | Module_types.ImportError _
    | Module_types.ImportErrorAt _ | Failure _) as e ->
    Error (legacy_of_exn e)

(* ── `wand s` ──────────────────────────────────────────────────────────── *)

(* A test file's top-level expressions are the `stdlib/Test.wand` module's
   `Pass`/`Fail` constructors (see Test.wand's `test` function), or a
   `Suite` -- a group's label over its children, nested arbitrarily --
   which lands here as one leaf outcome per child, labeled with the path
   of group labels that led to it. A raised runtime error is reported the
   same way a deliberate Fail would be, just without a caller-chosen
   message. Any other top-level expression's value is simply not a test
   outcome and is ignored (still executed normally, e.g. ordinary setup
   code/side effects). Only lex/parse/type errors for the whole file are
   fatal -- each TLExpr's *evaluation* is isolated so one failing/raising
   test doesn't stop the rest of the file. *)
type test_outcome = TPass of string | TFail of string | TError of string

(* A test file whose assertions are discarded reports a pass however the run
   went, so running it answers a question it cannot actually answer. The
   runner refuses it rather than printing a verdict it does not have --
   `wand t` says the same thing, but nobody runs `wand t` on a file they are
   about to run. *)
let drop2_refusals findings =
  List.filter_map (fun (f : Lint.finding) ->
    if f.Lint.rule = Lint_rules.V_DROP2 then
      Some (Printf.sprintf "%d:%d: %s: %s"
              f.Lint.loc.Token.line f.Lint.loc.Token.col
              (Lint_rules.code Lint_rules.V_DROP2) f.Lint.text)
    else None) findings

let run_test_program ~base_dir ?(item_locs = []) prog
  : (test_outcome list, string) result =
  let cache = Hashtbl.create 8 in
  let loading = ref [] in
  let (imp, _) = load_imports_for ~base_dir ~cache ~loading ~evaluate:true prog in
  (* Settled before anything reads the program's own declarations: an
     alias parses as a variant with one nullary constructor, and a lint
     or a tenv built from that has the alias declaring a constructor over
     the very name it aliases. *)
  let prog = Typechecker.settle_aliases ~init_tenv:imp.tenv prog in
  match Typechecker.infer_program_env_with_own
          ~init_tenv:imp.tenv ~init_env:imp.type_env
          ~init_ifaces:imp.ifaces ~init_effects:imp.load_effects
          ~type_names:imp.type_names prog with
  | Error msg -> Error ("type error: " ^ msg)
  | Ok (_, own_type_env, _) ->
    match drop2_refusals (Lint.check prog item_locs own_type_env) with
    | _ :: _ as refusals -> Error (String.concat "\n" refusals)
    | [] ->
    (* Nothing is discarded, so the outcomes below are the whole verdict. *)
    let outcomes = ref [] in
    ignore (run_with_default_handler (fun () ->
      with_file_net (net_bound_of_manifest prog.Ast.manifest) (fun () ->
      ignore (List.fold_left (fun env item ->
        Evaluator.forget_loc ();
        match item with
        | Ast.TLExpr e ->
          let result =
            try Ok (eval env e)
            with
            | EvalError msg -> Error (Evaluator.stamp_loc msg)
            | Failure msg   -> Error msg
          in
          let with_path path s = String.concat " / " (path @ [s]) in
          let rec collect path v = match v with
            (* `TestOutcome` is `Test`'s, so its constructors are owned by
               that module. Matched by name: this reads whatever the file
               under test imported as `Test`, which a development override
               can point elsewhere. *)
            | VConstr (c, [VString label]) when Ctor.name c = "Pass" ->
              outcomes := !outcomes @ [TPass (with_path path label)]
            | VConstr (c, [VString msg]) when Ctor.name c = "Fail" ->
              outcomes := !outcomes @ [TFail (with_path path msg)]
            | VConstr (c, [VString label; VList children])
              when Ctor.name c = "Suite" ->
              List.iter (collect (path @ [label])) children
            | _ -> ()
          in
          (match result with
           | Ok v    -> collect [] v
           | Error m -> outcomes := !outcomes @ [TError m]);
          env
        | _ -> run_item env item
      ) (index_env (base_eval_env @ imp.eval_env)) prog.Ast.items));
      VUnit
    ));
    Ok !outcomes

(* A test file is one named `test_*.wand`. The prefix rather than a suffix,
   because a script's tests live beside the script: `deploy.wand` and
   `test_deploy.wand` in one directory, where the prefix sorts every test
   together and away from the things being tested. *)
let is_test_file name =
  let prefix = "test_" and ext = ".wand" in
  Filename.check_suffix name ext
  && String.length name > String.length prefix + String.length ext
  && String.sub name 0 (String.length prefix) = prefix

(* Every test file at or below `root`, in a stable order so a run's output
   is comparable to the last one. Directories a script has no business
   descending into are skipped: `_build` holds dune's copies of these very
   files, and finding each test twice under two paths is worse than useless. *)
let skipped_dir name =
  name = "_build" || name = "_opam" || name = ".git" || name = "node_modules"

(* A broken symlink answers neither question, so it is not a directory and
   not a test: stepping around it beats crashing a test run over it. *)
let is_dir path = try Sys.is_directory path with Sys_error _ -> false

(* The walk does not descend through a symlink. `Sys.is_directory` follows
   one, so a link to a directory was walked into and `wand s` discovered and
   ran test files outside the tree it was pointed at, with every effect a
   test has. What a run covers has to be what the tree holds, and a link to a
   directory is a name for a tree it does not.

   A linked *file* is still read. It is one name, listed where the reader can
   see it, and it is how dune's sandbox presents every fixture in a
   `source_tree` dep -- refusing those would leave the wand-level suites
   discovering nothing. The directory is where a single link quietly brings
   in everything below it. *)
let is_walkable path =
  match Unix.lstat path with
  | { Unix.st_kind = Unix.S_DIR; _ } -> true
  | _ | exception Unix.Unix_error _ -> false

(* A file named directly is taken as given, whatever it is called: naming a
   file is saying which one you mean, and a walk is where the name has to be
   matched. *)
let find_files_named wanted root =
  let found = ref [] in
  let rec walk dir =
    match Sys.readdir dir with
    | exception Sys_error _ -> ()  (* unreadable: not this command's problem *)
    | entries ->
      Array.sort compare entries;
      Array.iter (fun name ->
        let path = Filename.concat dir name in
        if is_walkable path then begin
          if not (skipped_dir name) then walk path
        end else if wanted name then found := path :: !found
      ) entries
  in
  if is_dir root then walk root
  else if Sys.file_exists root then found := [root];
  List.rev !found

let find_test_files root = find_files_named is_test_file root

(* Every `.wand` file at or below `root`, for the commands that check a tree
   rather than a file. The same skips as the test walk, and the same refusal
   to follow a directory symlink. *)
let find_wand_files root =
  find_files_named (fun name -> Filename.check_suffix name ".wand") root

let run_test_file path : (test_outcome list, string) result =
  let full = entry_path path in
  try
    let src      = In_channel.with_open_text full In_channel.input_all in
    let tokens   = Lexer.tokenize src in
    let (prog, item_locs) = Parser.parse_program_with_locs tokens in
    let base_dir = Filename.dirname full in
    run_test_program ~base_dir ~item_locs prog
  with
  | Sys_error msg         -> Error ("cannot open file: " ^ msg)
  | EvalError msg -> Error ("eval error: " ^ Evaluator.stamp_loc msg)
  | (Lexer.LexError _ | Parser.ParseError _ | Module_types.ImportError _
    | Module_types.ImportErrorAt _ | Failure _) as e ->
    Error (legacy_of_exn e)

(* Under --json the only stdout contract is the JSON value, but a test is
   free to print -- IO.println is itself a thing the wand tests test. Run
   them with stdout routed to stderr, so their prints stay visible
   without corrupting the stream a consumer parses. *)
let with_stdout_to_stderr f =
  flush stdout;
  let saved = Unix.dup Unix.stdout in
  Unix.dup2 Unix.stderr Unix.stdout;
  Fun.protect
    ~finally:(fun () ->
      flush stdout;
      Unix.dup2 saved Unix.stdout;
      Unix.close saved)
    f

(* `wand s --json`: one object for the whole run, printed when the run
   completes -- a well-formed JSON value cannot stream test by test. A pass
   carries its `label`; a fail carries `message`, where the Test module has
   already written the label into the text ("label: reason") -- it is not
   recovered by parsing, per the rule diag.ml states. A test that raised
   rather than failed an assertion reports "error"; both count as failed,
   as in the text output. A file that would not load contributes to
   `errors` instead of `tests`. *)
let test_results_json
    (results : (string * (test_outcome list, string) result) list) : string =
  let field key v = Printf.sprintf "\"%s\":\"%s\"" key (Diag.escape_json v) in
  let tests =
    List.concat_map (fun (path, result) ->
      match result with
      | Error _ -> []
      | Ok outcomes ->
        List.map (fun outcome ->
          let rest = match outcome with
            | TPass label -> [field "status" "pass"; field "label" label]
            | TFail msg   -> [field "status" "fail"; field "message" msg]
            | TError msg  -> [field "status" "error"; field "message" msg]
          in
          "{" ^ String.concat "," (field "file" path :: rest) ^ "}"
        ) outcomes
    ) results
  in
  let errors =
    List.filter_map (fun (path, result) ->
      match result with
      | Error m -> Some ("{" ^ field "file" path ^ "," ^ field "message" m ^ "}")
      | Ok _ -> None
    ) results
  in
  let outcomes =
    List.concat_map
      (fun (_, r) -> match r with Ok os -> os | Error _ -> []) results
  in
  let passed =
    List.length (List.filter (function TPass _ -> true | _ -> false) outcomes)
  in
  Printf.sprintf "{\"tests\":[%s],\"errors\":[%s],\"passed\":%d,\"failed\":%d}"
    (String.concat "," tests) (String.concat "," errors)
    passed (List.length outcomes - passed)

(* ── REPL session ─────────────────────────────────────────────────────────── *)

type repl_result =
  | RBind     of string * string
  | RGroup    of (string * string) list
  | RType     of string
  | RVal      of string * string
  | RTypeExpr of string
  | RHoles    of string list
  | RSilent

type session = {
  s_tenv      : (string * Ast.type_def) list;
  s_type_env  : Typechecker.env;
  s_eval_env  : env;
  s_cache     : (string, module_result) Hashtbl.t;
  s_base_dir  : string;
  s_last_load : string option;
  s_sources   : (string * string) list;  (* name -> source text *)
  s_docs      : (string * string) list;  (* name -> doc string *)
  (* The types an import brought in, kept across steps. A session declares
     its imports a line at a time, so a step that names `Digest.Sha256`
     usually is not the step that wrote `import Digest`. Without this the
     constructors of an imported module were reachable only within the one
     step that imported it, which is why every stdlib module that declares
     a type was unusable from the REPL and from `-e`. *)
  s_type_names : (string * string) list;
  (* Kept across steps for the same reason, and with the same problem behind
     it: the step that writes `(m: ord.Ord Int)` is usually not the step that
     wrote `import ./ord`. *)
  s_ifaces : (string * Ast.interface_def) list;
}

let make_session ?(base_dir = Sys.getcwd ()) () = {
  s_tenv      = [];
  s_type_env  = [];
  s_eval_env  = [];
  s_cache     = Hashtbl.create 8;
  s_base_dir  = base_dir;
  s_last_load = None;
  s_sources   = [];
  s_docs      = [];
  s_type_names = [];
  s_ifaces    = [];
}

let lookup_type (sess : session) (name : string) : string option =
  match String.split_on_char '.' name with
  | [ns; member] ->
    (match List.assoc_opt ns sess.s_type_env with
     | Some (Typechecker.Namespace (members, _)) ->
       (match List.assoc_opt member members with
        | Some s -> Some (Typechecker.string_of_scheme s)
        | None   -> None)
     | _ -> None)
  | [plain] ->
    (match List.assoc_opt plain sess.s_type_env with
     | Some s -> Some (Typechecker.string_of_scheme s)
     | None   -> None)
  | _ -> None

(* ── `--json` for the query commands ──────────────────────────────────────
   `wand d --json` prints these: the same facts the text
   output states, as one JSON value on stdout. A fact the session lacks is
   null rather than omitted, so a consumer reads "no doc" as an answer and
   not as a schema difference. *)

(* A doc string, split into what it says and what it claims.

   An example is a line that opens with the session's prompt, and what it is
   expected to produce is the lines under it, up to a blank line or the next
   prompt. A prompt with nothing under it expects nothing and is a step
   rather than a claim -- which is how one example sets up the next.

   An expression too long for one line carries on under the session's
   continuation prompt, as it would in a session. Without that, an example
   that opens a `with` and writes a file inside it is one line of 140
   characters, and the doc it is meant to explain is the thing it makes
   unreadable.

   Kept as blocks rather than as a list of examples, because `wand d -x`
   shows the doc with its examples run in place, and that needs the prose
   back in the order it was written. What counts as an example is decided
   here, once: two copies of that rule would drift, and the drift would
   show up as an example that one command checks and the other does not. *)
type doc_block =
  | Prose   of string
  | Example of string * string list

let doc_blocks (doc : string) : doc_block list =
  let lines = String.split_on_char '\n' doc in
  let is_prompt l = String.length l >= 3 && String.sub l 0 3 = ">> " in
  let is_more l = String.length l >= 3 && String.sub l 0 3 = ".. " in
  let body l = String.sub l 3 (String.length l - 3) in
  let rec go acc = function
    | [] -> List.rev acc
    | l :: rest when is_prompt l ->
      (* Lines under the continuation prompt are the rest of the expression,
         not what it produces. *)
      let rec more expr = function
        | l :: rest when is_more l -> more (expr ^ "\n" ^ body l) rest
        | rest -> (expr, rest)
      in
      let (expr, rest) = more (body l) rest in
      let rec take out = function
        | [] -> (List.rev out, [])
        | l :: _ as here when is_prompt l || String.trim l = "" ->
          (List.rev out, here)
        | l :: rest -> take (l :: out) rest
      in
      let (expected, rest') = take [] rest in
      go (Example (expr, expected) :: acc) rest'
    | l :: rest -> go (Prose l :: acc) rest
  in
  go [] lines

let doc_examples (doc : string) : (string * string list) list =
  List.filter_map (function
    | Example (expr, expected) -> Some (expr, expected)
    | Prose _ -> None) (doc_blocks doc)

let doc_json (sess : session) (name : string) : string =
  let field key = function
    | Some v -> Printf.sprintf "\"%s\":\"%s\"" key (Diag.escape_json v)
    | None   -> Printf.sprintf "\"%s\":null" key
  in
  Printf.sprintf "{\"name\":\"%s\",%s,%s}"
    (Diag.escape_json name)
    (field "type" (lookup_type sess name))
    (field "doc" (List.assoc_opt name sess.s_docs))

let binding_json name scheme =
  Printf.sprintf "{\"name\":\"%s\",\"type\":\"%s\"}"
    (Diag.escape_json name)
    (Diag.escape_json (Typechecker.string_of_scheme scheme))

let scope_json (sess : session) : string =
  let entries =
    List.sort (fun (a, _) (b, _) -> String.compare a b) sess.s_type_env
  in
  let entry (name, s) =
    match s with
    | Typechecker.Namespace _ ->
      Printf.sprintf "{\"name\":\"%s\",\"module\":true}" (Diag.escape_json name)
    | _ -> binding_json name s
  in
  "[" ^ String.concat "," (List.map entry entries) ^ "]"

(* The names a module exports, or None when the name is not a module. *)
let module_members (sess : session) (modname : string) : string list option =
  match List.assoc_opt modname sess.s_type_env with
  | Some (Typechecker.Namespace (members, _)) ->
    Some (List.sort String.compare (List.map fst members))
  | _ -> None

let module_json (sess : session) (modname : string) : (string, string) result =
  match List.assoc_opt modname sess.s_type_env with
  | Some (Typechecker.Namespace (members, _)) ->
    let sorted =
      List.sort (fun (a, _) (b, _) -> String.compare a b) members
    in
    Ok ("[" ^ String.concat ","
          (List.map (fun (n, s) -> binding_json (modname ^ "." ^ n) s) sorted)
        ^ "]")
  | Some _ -> Error (modname ^ " is a binding, not a module")
  | None   -> Error ("Unknown module '" ^ modname ^ "'")

(* Every module's members in one listing. `wand d <module>` answers for one
   module; this is that answer for all of them, in module order and then by
   name, so a tool that wants the whole surface makes one call rather than a
   loop over the module list -- and a loop's output cannot be depended on to
   stay in step with what is on disk. *)
(* The interfaces a module's member answers to, named as a reader writes
   them. A member is marked when the module claimed an interface that
   declares it -- nothing is worked out from the member's shape, because
   conformance is what the module said rather than what it happens to
   have. *)
let member_ifaces claims member =
  List.filter_map (fun (iname, _) ->
    match Typechecker.iface_member_names iname with
    | Some names when List.mem member names -> Some iname
    | _ -> None) claims

(* `[Ord]` after a member's type, where it answers to one. Square brackets
   because round ones are type application: `Int (Ord)` is how `List Int` is
   written, so a reader copying the type into an annotation could take the
   interface for a last argument. A `[` never appears in a wand type. *)
let iface_suffix ifaces =
  if ifaces = [] then "" else " [" ^ String.concat ", " ifaces ^ "]"

let module_claims (sess : session) modname =
  match List.assoc_opt modname sess.s_type_env with
  | Some (Typechecker.Namespace (_, claims)) -> claims
  | _ -> []

let index (sess : session) : (string * Typechecker.scheme) list =
  let modules =
    List.sort String.compare
      (List.filter_map (fun (n, s) ->
         match s with Typechecker.Namespace _ -> Some n | _ -> None)
        sess.s_type_env)
  in
  List.concat_map (fun modname ->
    match List.assoc_opt modname sess.s_type_env with
    | Some (Typechecker.Namespace (members, _)) ->
      List.map (fun (n, s) -> (modname ^ "." ^ n, s))
        (List.sort (fun (a, _) (b, _) -> String.compare a b) members)
    | _ -> []) modules

(* The same listing with each member's interfaces beside it. *)
let index_with_ifaces (sess : session) =
  List.map (fun (qualified, scheme) ->
    match String.index_opt qualified '.' with
    | None -> (qualified, scheme, [])
    | Some i ->
      let modname = String.sub qualified 0 i in
      let member =
        String.sub qualified (i + 1) (String.length qualified - i - 1) in
      (qualified, scheme, member_ifaces (module_claims sess modname) member))
    (index sess)

(* One aligned block per module: the column is set by the longest name in
   that module and resets at the next. Not across the whole listing -- names
   run from 8 to 23 characters, so one column would pad every short line by
   fifteen spaces. And not a third column after the type: the longest type in
   the library is 83 characters, which would start the interfaces past column
   130, so the brackets fall where they fall. *)
let aligned_rows rows =
  let modul n = match String.index_opt n '.' with
    | Some i -> String.sub n 0 i
    | None -> n
  in
  let rec groups acc = function
    | [] -> List.rev acc
    | ((n, _, _) :: _) as rest ->
      let m = modul n in
      let (here, later) =
        List.partition (fun (x, _, _) -> modul x = m) rest in
      (* `partition` keeps the order, and the listing is already grouped by
         module, so a later module cannot be pulled into this one. *)
      groups (here :: acc) later
  in
  List.concat_map (fun group ->
    let width =
      List.fold_left (fun w (n, _, _) -> max w (String.length n)) 0 group in
    List.map (fun (n, t, ifaces) ->
      Printf.sprintf "%-*s : %s%s" width n t (iface_suffix ifaces)) group)
    (groups [] rows)

let index_lines (sess : session) : string list =
  aligned_rows
    (List.map (fun (n, scheme, ifaces) ->
       (n, Typechecker.string_of_scheme scheme, ifaces))
       (index_with_ifaces sess))

(* `wand d <Module>`: the module's own members, aligned the same way. *)
let module_lines (sess : session) modname : string list option =
  match List.assoc_opt modname sess.s_type_env with
  | Some (Typechecker.Namespace (members, claims)) ->
    Some (aligned_rows
            (List.map (fun (n, scheme) ->
               (modname ^ "." ^ n, Typechecker.string_of_scheme scheme,
                member_ifaces claims n))
               (List.sort (fun (a, _) (b, _) -> String.compare a b) members)))
  | _ -> None

(* `Int.max` -> the interfaces `Int` claims that declare `max`. A name with
   no module part answers with none: a binding of a script's own answers to
   nothing. *)
let name_ifaces (sess : session) qualified =
  match String.index_opt qualified '.' with
  | None -> []
  | Some i ->
    let modname = String.sub qualified 0 i in
    let member = String.sub qualified (i + 1) (String.length qualified - i - 1) in
    member_ifaces (module_claims sess modname) member

(* Each entry gains `implements` beside `name` and `type`, null where there
   is none. Tools read that and never the aligned text. *)
let index_json (sess : session) : string =
  "[" ^ String.concat ","
          (List.map (fun (n, scheme, ifaces) ->
             Printf.sprintf "{\"name\":\"%s\",\"type\":\"%s\",\"implements\":%s}"
               (Diag.escape_json n)
               (Diag.escape_json (Typechecker.string_of_scheme scheme))
               (if ifaces = [] then "null"
                else "[" ^ String.concat ","
                       (List.map (fun i -> "\"" ^ Diag.escape_json i ^ "\"") ifaces)
                     ^ "]"))
             (index_with_ifaces sess))
  ^ "]"

let last_non_import prog =
  List.fold_left (fun acc item ->
    match item with Ast.TLImport _ -> acc | other -> Some other
  ) None prog.Ast.items

let run_session (sess : session) (src : string) : (session * repl_result, string) result =
  try
    let tokens = Lexer.tokenize src in
    let prog   = Parser.parse_program tokens in
    let loading = ref [] in
    let (imp, imp_docs) = load_imports_for ~base_dir:sess.s_base_dir ~cache:sess.s_cache ~loading ~evaluate:true prog in
    (* A session declares its types a line at a time, so the ones to settle
       against are the ones it already has. *)
    let prog =
      Typechecker.settle_aliases ~init_tenv:(imp.tenv @ sess.s_tenv) prog in
    let merged_tenv     = local_tenv_of prog @ imp.tenv @ sess.s_tenv in
    let merged_type_env = imp.type_env @ sess.s_type_env in
    let merged_type_names = imp.type_names @ sess.s_type_names in
    let merged_ifaces = imp.ifaces @ sess.s_ifaces in
    match Typechecker.infer_program_full_with_own
            ~init_tenv:merged_tenv ~init_env:merged_type_env
            ~init_ifaces:merged_ifaces ~init_effects:imp.load_effects
            ~type_names:merged_type_names prog with
    | Error (loc, msg, _) -> Error (Diag.legacy (Diag.error ~code:"E-TYPE" ?loc msg))
    | Ok (full_type_env, own_type_env, last_t, hole_types) ->
      let dedup lst =
        let seen = Hashtbl.create 16 in
        List.filter (fun (k, _) ->
          if Hashtbl.mem seen k then false
          else (Hashtbl.add seen k (); true)) lst
      in
      if hole_types <> [] then begin
        (* Holes present — skip evaluation, report hole types *)
        let new_sources =
          List.filter_map (function
            | Ast.TLLet (name, _, _) -> Some (name, src)
            | _ -> None) prog.Ast.items
        in
        let new_sess = { sess with
          s_tenv     = dedup (local_tenv_of prog @ imp.tenv @ sess.s_tenv);
          s_type_env = dedup (own_type_env @ imp.type_env @ sess.s_type_env);
          s_sources  = new_sources @ sess.s_sources;
          s_docs     = prog.Ast.docs @ imp_docs @ sess.s_docs;
          s_type_names = dedup merged_type_names;
          s_ifaces     = dedup merged_ifaces;
        } in
        let hole_strs = List.map Typechecker.string_of_typ hole_types in
        Ok (new_sess, RHoles hole_strs)
      end else begin
        let base_eval = index_env (base_eval_env @ imp.eval_env @ sess.s_eval_env) in
        let env_ref  = ref base_eval in
        let last_ref = ref VUnit in
        ignore (run_with_default_handler (fun () ->
          with_file_net (net_bound_of_manifest prog.Ast.manifest) (fun () ->
          List.iter (fun item ->
            Evaluator.forget_loc ();
            match item with
            | Ast.TLLet (_, [], body) when Option.is_some (import_kind_of body) -> ()  (* pre-loaded *)
            | Ast.TLLet (name, [], body) ->
              env_ref := (name, eval !env_ref body) :: !env_ref
            | Ast.TLLet (name, params, body) ->
              let arity = List.length params in
              let v = match List.assoc_opt name !env_ref with
                | Some (VFix (_, _, ep, eb)) when List.length ep = arity ->
                  merge_clause !env_ref name arity params body ep eb
                | _ -> VFix (name, !env_ref, params, body)
              in
              env_ref := (name, v) :: !env_ref
            | Ast.TLLetRec bindings ->
              List.iter (fun (name, _, _) ->
                env_ref := (name, VFixGroup (bindings, !env_ref, name)) :: !env_ref
              ) bindings
            | Ast.TLLetPat (_, body) when Option.is_some (import_kind_of body) -> ()  (* pre-loaded *)
            | Ast.TLLetPat (pat, e) ->
              env_ref := Evaluator.bind_pat ~prefix:true pat (eval !env_ref e) !env_ref
            | Ast.TLImplement (im, _) ->
              List.iter (fun (name, params, body, _) ->
                let v = match params with
                  | [] -> eval !env_ref body
                  | _  -> VFix (name, !env_ref, params, body)
                in
                env_ref := (name, v) :: !env_ref) im.Ast.im_binds
            | Ast.TLInterface _ -> ()
            | Ast.TLType (Ast.Alias _, _) -> ()
            | Ast.TLType (Ast.Variants (_, _, ctors), _) ->
              List.iter (fun ctor ->
                let ident = Ctor.Local ctor.Ast.name in
                Hashtbl.replace constr_fields ident (List.map fst ctor.Ast.fields);
                Hashtbl.replace constr_defaults ident ctor.Ast.defaults;
                register_ctor ident;
                forget_ctor_env ();
                env_ref := (ctor.Ast.name,
                  match ctor.Ast.fields with
                  | [] -> VConstr (ident, [])
                  | _  -> VPartialConstr (ident, List.length ctor.Ast.fields, [])
                ) :: !env_ref
              ) ctors
            | Ast.TLExpr e ->
              last_ref := eval !env_ref e
            | Ast.TLImport _ -> ()
          ) prog.Ast.items);
          VUnit));
        let new_eval_env = !env_ref in
        let last_v       = !last_ref in
        let n_own = List.length new_eval_env - List.length base_eval in
        let own_eval_env = List.filteri (fun i _ -> i < n_own) new_eval_env in
        let new_sources =
          List.filter_map (function
            | Ast.TLLet (name, _, _) -> Some (name, src)
            | _ -> None) prog.Ast.items
        in
        (* Keep only namespace entries from imports — raw primitives come from
           the typechecker/evaluator base and don't belong in the session. *)
        let new_sess = { sess with
          s_tenv     = dedup (local_tenv_of prog @ imp.tenv @ sess.s_tenv);
          s_type_env = dedup (own_type_env @ imp.type_env @ sess.s_type_env);
          s_eval_env = dedup (own_eval_env @ imp.eval_env @ sess.s_eval_env);
          s_sources  = new_sources @ sess.s_sources;
          s_docs     = prog.Ast.docs @ imp_docs @ sess.s_docs;
          s_type_names = dedup merged_type_names;
          s_ifaces     = dedup merged_ifaces;
        } in
        (* The REPL edits definitions; files declare them. A new clause for an
           existing function merges into it here (merge_clause above), which
           is only safe to do silently if the result is visible -- so report
           how many equations the function now has. *)
        let equation_count name =
          match List.assoc_opt name new_eval_env with
          | Some (VFix (_, _, params, body)) ->
            let arity = List.length params in
            let synthetic = List.mapi (fun i p -> match p with
              | Ast.PVar v -> v = Printf.sprintf "_p%d" i
              | _ -> false) params
            in
            if arity > 0 && List.for_all (fun b -> b) synthetic then
              (match strip_located body with
               | Ast.Match (_, cases) when List.length cases > 1 -> Some (List.length cases)
               | _ -> None)
            else None
          | _ -> None
        in
        let display = match last_non_import prog with
          | None -> RSilent
          | Some (Ast.TLLet (name, _, _)) ->
            let ty = match List.assoc_opt name full_type_env with
              | Some s -> Typechecker.string_of_scheme s
              | None   -> "?"
            in
            (match equation_count name with
             | Some n -> RBind (name, Printf.sprintf "%s, %d equations" ty n)
             | None   -> RBind (name, ty))
          | Some (Ast.TLLetRec bindings) ->
            (* A mutual group binds several names at once; echo each, the
               way a lone binding is echoed. *)
            RGroup (List.map (fun (name, _, _) ->
              let ty = match List.assoc_opt name full_type_env with
                | Some s -> Typechecker.string_of_scheme s
                | None   -> "?"
              in
              (name, ty)) bindings)
          | Some (Ast.TLLetPat _) -> RSilent
          | Some (Ast.TLType (Ast.Variants (name, _, _), _))
          | Some (Ast.TLType (Ast.Alias (name, _, _), _)) -> RType name
          | Some (Ast.TLExpr _) ->
            (* A Unit answer from an expression that performed something has
               already been seen: `IO.println "hi"` put `hi` on the screen,
               and `() : Unit` under it would be noise. A Unit answer from an
               expression that performed nothing was never shown at all, so
               it answers -- `()` itself, and anything else that evaluates to
               it without reaching outside. *)
            let performed =
              match !Typechecker.expr_item_effects with
              | (_, eff) :: _ -> not (Effect_set.EffSet.is_empty eff)
              | []            -> false
            in
            (match last_v with
             | VUnit when performed -> RSilent
             | v -> RVal (show_value v, Typechecker.string_of_typ last_t))
          | Some _ -> RSilent
        in
        Ok (new_sess, display)
      end
  with
  | EvalError msg -> Error ("runtime error: " ^ Evaluator.stamp_loc msg)
  | (Lexer.LexError _ | Parser.ParseError _ | Module_types.ImportError _
    | Module_types.ImportErrorAt _ | Failure _) as e ->
    Error (legacy_of_exn e)

(* Typecheck a file without running it, resolving its imports the same way
   running it would. The editing loop and CI both want an answer that costs
   nothing and changes nothing. *)
(* Whether a path names one of the standard library's own modules. The
   library is embedded, so the only files this can be true of are the
   sources it was built from -- `wand t stdlib/List.wand` in the tree, or a
   directory `WAND_STDLIB` points at. Asked of the directory the file is
   actually in, so a user file called FS.wand is a user file. *)
let is_stdlib_file full =
  let dir = Filename.dirname full in
  Module_types.is_stdlib_dir dir
  && List.mem_assoc
       (Filename.remove_extension (Filename.basename full))
       Stdlib_embed.table

(* Everything a check of one file's text establishes, in one place. The
   editor asks again on every keystroke, so this is also the shape a
   language server serves hover and diagnostics from. *)
type source_check = {
  sc_type     : string;                  (* the file's final type *)
  sc_holes    : string list;             (* hole types, in order *)
  sc_findings : Lint.finding list;
  sc_env      : Typechecker.env;         (* the file's own names *)
  sc_scope    : Typechecker.env;         (* everything in scope: own, imports, base *)
  sc_docs     : (string * string) list;  (* name -> doc string *)
  sc_defs     : (string * Token.loc) list;  (* name -> its definition site *)
  sc_locals   : (Token.loc * (string * string) list) list;
  (* per top-level item: its extent and the local binders typed inside it
     (parameters, `let ... in` names, pattern variables) -- what a hover
     answers for names sc_scope never sees. Innermost binding first. *)
  sc_manifest : Token.loc option;
  (* the extent of `uses {...}`, when the file declares one. A label inside
     it is an effect and nothing else, which is not something the name on
     its own can say: `Env` is a module everywhere else in the file. *)
  sc_effects  : (string * string list option) list;
  (* what the file reaches outside itself to do, inferred rather than read
     off the manifest: a file with no manifest is unbounded, not sealed.
     The labels a manifest would name, so `Raise` is not among them, each
     with the binaries narrowing it where every command word in the file is
     literal. *)
}

(* Checks text that need not exist on disk -- an editor's unsaved buffer.
   `path` says where the text lives, which decides how its imports resolve
   and whether it is checked as a stdlib module. A failure comes back as a
   structured diagnostic; `Diag.legacy` recovers the old error string. *)
let typecheck_source ~path (src : string) : (source_check, Diag.t) result =
  let full = entry_path path in
  try
    let tokens   = Lexer.tokenize src in
    let (prog, item_locs) = Parser.parse_program_with_locs tokens in
    let base_dir = Filename.dirname full in
    let cache = Hashtbl.create 8 in
    let loading = ref [] in
    let (imp, imp_docs) = load_imports_for ~item_locs ~base_dir ~cache ~loading ~evaluate:false prog in
    let prog = Typechecker.settle_aliases ~init_tenv:imp.tenv prog in
    (* A file in the stdlib is checked as what it is: a module, whose body
       calls the raw builtins the modules are built from. Checked as a
       script it fails on the first one, so nothing here could be checked
       at all -- and a module that goes wrong would be found only when
       something imported it. The rule is the file's home rather than an
       option, because an option meant for the people writing the standard
       library is one more thing in everyone else's way. *)
    let base_env =
      if is_stdlib_file full then Typechecker.stdlib_type_env
      else Typechecker.builtin_type_env
    in
    match Typechecker.infer_program_full_with_own ~base_env
            ~init_tenv:imp.tenv ~init_env:imp.type_env
            ~init_ifaces:imp.ifaces ~init_effects:imp.load_effects
            ~type_names:imp.type_names prog with
    | Error (loc, msg, fix) ->
      (* An unbound name is the one error a check carries on past, so there
         may be several. They travel with the first, which is what every
         consumer that shows one diagnostic already takes. *)
      let others =
        match !Typechecker.unbound_names with
        | _ :: rest ->
          List.map (fun (l, m) -> Diag.error ~code:"E-TYPE" ?loc:l m) rest
        | [] -> []
      in
      Error { (Diag.error ~code:"E-TYPE" ?loc ?fix msg) with Diag.others }
    | Ok (full_type_env, own_type_env, last_t, holes) ->
      (* Read before the record, because a record's fields are evaluated in
         no stated order and these come off the check that just ran. *)
      let effects =
        let shell = Typechecker.shell_suggestion () in
        List.map (fun e ->
          let n = Effect_set.name_of e in
          (n, if n = "Shell" then shell else None))
          (Effect_set.EffSet.elements !Typechecker.last_file_effects)
      in
      Ok { sc_type     = Typechecker.string_of_typ last_t;
           sc_holes    = List.map Typechecker.string_of_typ holes;
           sc_findings = Lint.check prog item_locs own_type_env;
           sc_env      = own_type_env;
           sc_scope    = full_type_env;
           sc_docs     = prog.Ast.docs @ imp_docs;
           sc_defs     = defs_of_program prog item_locs;
           sc_locals   =
             (let all = !Typechecker.local_binders in
              List.mapi (fun i (start_loc, end_loc) ->
                (Token.span_to start_loc end_loc,
                 List.filter_map (fun (j, (n, t)) ->
                   if j = i then Some (n, Typechecker.string_of_typ t)
                   else None) all))
                item_locs);
           sc_manifest = Option.map snd prog.Ast.manifest;
           sc_effects  = effects }
  with
  | (Lexer.LexError _ | Parser.ParseError _ | Typechecker.TypeError _
    | Typechecker.TypeErrorAt _ | Module_types.ImportError _
    | Module_types.ImportErrorAt _ | Failure _) as e ->
    Error (diag_of_exn e)

let typecheck_file path : (source_check, Diag.t) result =
  try
    let full = entry_path path in
    let src = In_channel.with_open_text full In_channel.input_all in
    typecheck_source ~path src
  with Sys_error msg ->
    Error (Diag.error ~code:"E-FAIL" ("cannot open file: " ^ msg))

(* Lint a stdlib module's own source. Module bodies are inferred against the
   raw builtins rather than the user-visible globals, so they need the same
   base environment module loading uses -- without it, `toml_parse` and its
   kind read as unbound. *)
let lint_module_source (src : string) : (Lint.finding list, string) result =
  try
    let tokens = Lexer.tokenize src in
    let (prog, item_locs) = Parser.parse_program_with_locs tokens in
    let cache = Hashtbl.create 8 in
    let loading = ref [] in
    let base_dir = Module_types.stdlib_base_dir in
    let (imp, _) = load_imports_for ~item_locs ~base_dir ~cache ~loading ~evaluate:false prog in
    let prog = Typechecker.settle_aliases ~init_tenv:imp.tenv prog in
    match Typechecker.infer_program_env_with_own
            ~init_tenv:(local_tenv_of prog @ imp.tenv)
            ~init_env:imp.type_env ~init_ifaces:imp.ifaces
            ~init_effects:imp.load_effects
            ~type_names:imp.type_names prog with
    | Error msg -> Error ("type error: " ^ msg)
    | Ok (_, own_type_env, _) -> Ok (Lint.check prog item_locs own_type_env)
  with
  | (Lexer.LexError _ | Parser.ParseError _ | Typechecker.TypeError _
    | Typechecker.TypeErrorAt _ | Module_types.ImportError _
    | Module_types.ImportErrorAt _ | Failure _) as e ->
    Error (legacy_of_exn e)

(* Lints share the typecheck's parse and inference rather than repeating
   them: they are reported by `wand t`, so they must cost it almost nothing. *)
let lint_session (sess : session) (src : string) : (Lint.finding list, string) result =
  try
    let tokens = Lexer.tokenize src in
    let (prog, item_locs) = Parser.parse_program_with_locs tokens in
    let loading = ref [] in
    let (imp, _) = load_imports_for ~item_locs ~base_dir:sess.s_base_dir ~cache:sess.s_cache ~loading ~evaluate:false prog in
    let prog =
      Typechecker.settle_aliases ~init_tenv:(imp.tenv @ sess.s_tenv) prog in
    let merged_tenv     = local_tenv_of prog @ imp.tenv @ sess.s_tenv in
    let merged_type_env = imp.type_env @ sess.s_type_env in
    let merged_type_names = imp.type_names @ sess.s_type_names in
    let merged_ifaces = imp.ifaces @ sess.s_ifaces in
    match Typechecker.infer_program_full_with_own
            ~init_tenv:merged_tenv ~init_env:merged_type_env
            ~init_ifaces:merged_ifaces ~init_effects:imp.load_effects
            ~type_names:merged_type_names prog with
    | Error (loc, msg, _) -> Error (Diag.legacy (Diag.error ~code:"E-TYPE" ?loc msg))
    | Ok (_, own_type_env, _, _) ->
      Ok (Lint.check prog item_locs own_type_env)
  with
  | (Lexer.LexError _ | Parser.ParseError _ | Typechecker.TypeError _
    | Typechecker.TypeErrorAt _ | Module_types.ImportError _
    | Module_types.ImportErrorAt _ | Failure _) as e ->
    Error (legacy_of_exn e)

let typecheck_session (sess : session) (src : string) : (repl_result, Diag.t) result =
  try
    let tokens = Lexer.tokenize src in
    let prog   = Parser.parse_program tokens in
    let loading = ref [] in
    let (imp, _) = load_imports_for ~base_dir:sess.s_base_dir ~cache:sess.s_cache ~loading ~evaluate:false prog in
    let prog =
      Typechecker.settle_aliases ~init_tenv:(imp.tenv @ sess.s_tenv) prog in
    let merged_tenv     = local_tenv_of prog @ imp.tenv @ sess.s_tenv in
    let merged_type_env = imp.type_env @ sess.s_type_env in
    let merged_type_names = imp.type_names @ sess.s_type_names in
    let merged_ifaces = imp.ifaces @ sess.s_ifaces in
    match Typechecker.infer_program_full_with_own
            ~init_tenv:merged_tenv ~init_env:merged_type_env
            ~init_ifaces:merged_ifaces ~init_effects:imp.load_effects
            ~type_names:merged_type_names prog with
    | Error (loc, msg, fix) -> Error (Diag.error ~code:"E-TYPE" ?loc ?fix msg)
    | Ok (full_type_env, _, last_t, hole_types) ->
      if hole_types <> [] then
        Ok (RHoles (List.map Typechecker.string_of_typ hole_types))
      else
        let display = match last_non_import prog with
          | None -> RSilent
          | Some (Ast.TLLet (name, _, _)) ->
            (match List.assoc_opt name full_type_env with
             | Some s -> RBind (name, Typechecker.string_of_scheme s)
             | None   -> RBind (name, "?"))
          | Some (Ast.TLType (Ast.Variants (name, _, _), _))
          | Some (Ast.TLType (Ast.Alias (name, _, _), _)) -> RType name
          | Some (Ast.TLExpr _) -> RTypeExpr (Typechecker.string_of_typ last_t)
          | Some _ -> RSilent
        in
        Ok display
  with
  | (Lexer.LexError _ | Parser.ParseError _ | Module_types.ImportError _
    | Module_types.ImportErrorAt _ | Failure _) as e ->
    Error (diag_of_exn e)
