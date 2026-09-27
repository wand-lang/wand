(* `1 file`, `2 files`. Every word this counts takes a plain `s`. *)
let count_of n word =
  Printf.sprintf "%d %s%s" n word (if n = 1 then "" else "s")

let usage () =
  print_endline "wand — a typed language for human/AI pairing";
  print_endline "";
  print_endline "Usage: wand <command> [options] [args]";
  print_endline "       wand [--dry-run|--trace] <file.wand> [--lint|--strict] [args]";
  print_endline "       wand <file.wand> [args]";
  print_endline "       wand <file.wand> -- [args]   (everything after -- is the script's)";
  print_endline "       wand -e <expr>               Evaluate an expression and exit";
  print_endline "";
  print_endline "A file is named directly. An expression is given with -e/--expr.";
  print_endline "";
  print_endline "Commands:";
  print_endline "  d   doc [name]              What a name is: its doc, a module's members,";
  print_endline "                              or everything in scope";
  print_endline "  f   fmt <file>...           Format .wand files in place";
  print_endline "  h   help [cmd]              Show this help, or help for a command";
  print_endline "  i   interactive             Start an interactive session";
  print_endline "  l   lsp                     Start the language server (LSP over stdio)";
  print_endline "  p   package <cmd>           Manage this package's wand.mod and wand.sum";
  print_endline "  s   test [<file>|<dir>]...  Run test_*.wand files (default: search from here)";
  print_endline "  t   type [<file>|<dir>]...  Typecheck files without running them";
  print_endline "  v   version                 Print the version and exit";
  print_endline "";
  print_endline "Evaluating an expression:";
  print_endline "  -e, --expr <expr>  Evaluate it and print the result";
  print_endline "  --load <file>      Load a .wand file first (repeatable)";
  print_endline "";
  print_endline "Running a script:";
  print_endline "  --dry-run        Report what the script would change, without doing it";
  print_endline "  --trace          Run it, reporting each effect as it happens";
  print_endline "  --lint           Report the lint findings first, then run it";
  print_endline "  --strict         Report them, and refuse to run if any is a violation";
  print_endline "  --               End wand's arguments: the rest are the script's";
  print_endline "";
  print_endline "Run 'wand h <command>' for command-specific help."

let usage_for sub =
  match sub with
  | "v" | "version" ->
    print_endline "Usage: wand v";
    print_endline "";
    print_endline "Print the version and exit. Written bare, as `wand 0.1.0`,";
    print_endline "so an installer can compare it to what it meant to install."
  | "p" | "package" ->
    print_endline "Usage: wand p <command> [args]";
    print_endline "";
    print_endline "Manage the package this directory is in: the tree under";
    print_endline "the nearest wand.mod.";
    print_endline "";
    print_endline "Commands:";
    print_endline "  i   init <url>              Start a package here: write wand.mod";
    print_endline "  t   tidy                    Make wand.mod and wand.sum match the imports,";
    print_endline "                              and fetch what they need";
    print_endline "  u   upgrade [url[@version]] Move dependencies to their newest version";
    print_endline "                              within their major, or one to a version";
    print_endline "";
    print_endline "Only these commands change wand.mod. A script or `wand t`";
    print_endline "fetches a version wand.mod names and the cache lacks, and";
    print_endline "checks it against wand.sum."
  | "l" | "lsp" ->
    print_endline "Usage: wand l";
    print_endline "";
    print_endline "Start the language server, speaking the Language Server";
    print_endline "Protocol over stdin/stdout. Meant to be spawned by an";
    print_endline "editor, not run by hand."
  | "i" | "interactive" ->
    print_endline "Usage: wand i [--load <file>]...";
    print_endline "";
    print_endline "Start an interactive wand session.";
    print_endline "";
    print_endline "Options:";
    print_endline "  --load <file>   Load a .wand file before starting (repeatable)"
  | "t" | "type" ->
    print_endline "Usage: wand t [<file>|<dir>]...";
    print_endline "       wand t [--load <file>]... -e <expr>";
    print_endline "";
    print_endline "Typecheck wand files without running them.";
    print_endline "";
    print_endline "A file is named directly, as it is everywhere else. An";
    print_endline "expression is given with -e/--expr: `deploy.wand` is itself";
    print_endline "a valid path expression, so the two cannot be told apart";
    print_endline "by shape and one of them has to say which it is.";
    print_endline "";
    print_endline "One file named on its own reports what it checks out as. A";
    print_endline "directory, or more than one path, is checked file by file:";
    print_endline "each finding carries its path, a failure in one file does";
    print_endline "not stop the rest, and the last line counts what was";
    print_endline "covered. The exit code is 1 if any file has an error, which";
    print_endline "is what a gate reads. A directory is searched all the way";
    print_endline "down for .wand files.";
    print_endline "";
    print_endline "Options:";
    print_endline "  -e, --expr <expr>  Typecheck an expression instead of a file";
    print_endline "  --effects          Report what each file reaches outside itself,";
    print_endline "                     as `<file> ! {IO, Shell(df)}`, and nothing else";
    print_endline "  --fix              Apply the fixes the findings carry, in place";
    print_endline "  --load <file>      Load a .wand file first (with -e; repeatable)";
    print_endline "  --strict           Treat violation lint findings as errors";
    print_endline "  --json             Emit lint findings as JSON instead of text";
    print_endline "";
    print_endline "--effects reports the set wand inferred, never the `uses`";
    print_endline "line: a file with no manifest is unbounded rather than";
    print_endline "sealed. It names the labels a manifest would, so `Raise` is";
    print_endline "not among them, and it narrows Shell to the binaries the";
    print_endline "file runs where every command word in it is literal. A file";
    print_endline "that reaches nothing reports `! {}`.";
    print_endline "";
    print_endline "Lint findings are reported as warnings. Rule IDs carry";
    print_endline "what they do to a build: V- rules report a violation and";
    print_endline "--strict promotes them to errors; A- rules are advisory and";
    print_endline "always stay warnings."
  | "d" | "doc" ->
    print_endline "Usage: wand d [-x|-t|--index] [--load <file>]... [name]";
    print_endline "";
    print_endline "What a name is. A function or a value gives its type and";
    print_endline "doc; a module gives every name in it with its signature;";
    print_endline "no name at all gives what is in scope -- the modules by";
    print_endline "name, and any bindings --load brought. --index gives every";
    print_endline "name inside those modules as well.";
    print_endline "";
    print_endline "`--load` puts a file's own names in scope first, so";
    print_endline "`wand d --load mine.wand` says what that file defines.";
    print_endline "";
    print_endline "Options:";
    print_endline "  -x, --execute   Print the doc with its examples run where they stand,";
    print_endline "                  showing what each one produces now";
    print_endline "  -t, --test      Run the examples and report only what does not produce";
    print_endline "                  what it says. Silent and 0 when they all hold, 1 if any";
    print_endline "                  does not, so it can gate a build";
    print_endline "  --index         Every module's members with their signatures, in";
    print_endline "                  one listing. Takes no name";
    print_endline "  --json          Emit the name, type, and doc as JSON";
    print_endline "  --load <file>   Load a .wand file before looking up the name (repeatable)"
  | "f" | "fmt" ->
    print_endline "Usage: wand f <file.wand>...";
    print_endline "";
    print_endline "Format one or more .wand files in place (each file is";
    print_endline "overwritten with its formatted contents).";
    print_endline "Comments are preserved. An item with a comment inside it is";
    print_endline "left exactly as written, since moving a comment to the wrong";
    print_endline "expression is worse than leaving it where its author put it."
  | "s" | "test" ->
    print_endline "Usage: wand s [--json] [<file.wand>|<dir>]...";
    print_endline "";
    print_endline "Run .wand test files (let {test} = import Test;";
    print_endline "test \"label\" (fn t -> t.ok/t.eq/t.raises ...)) and report";
    print_endline "pass/fail.";
    print_endline "";
    print_endline "With no argument, searches the current directory and everything";
    print_endline "below it for files named test_*.wand — so a script's tests are";
    print_endline "found beside the script. A directory argument is searched the";
    print_endline "same way; a named file is run whatever it is called.";
    print_endline "_build, _opam, .git and node_modules are not searched.";
    print_endline "Exits nonzero if any test failed or any file errored.";
    print_endline "";
    print_endline "Options:";
    print_endline "  --json          Emit per-test results as JSON, printed when the run completes"
  | "h" | "help" ->
    print_endline "Usage: wand h [command]";
    print_endline "";
    print_endline "Show help for all commands, or detailed help for a specific command."
  | _ ->
    Printf.eprintf "Unknown command: %s\nRun 'wand h' for usage.\n" sub;
    exit 1

(* The query commands (`d`, `v`) and the test runner (`s`) take --json
   alone; `t` has its own richer flag set in parse_lint_flags below. *)
let parse_json_flag args =
  (List.mem "--json" args, List.filter (fun a -> a <> "--json") args)

let parse_execute_flag args =
  (List.mem "--execute" args || List.mem "-x" args,
   List.filter (fun a -> a <> "--execute" && a <> "-x") args)

let parse_index_flag args =
  (List.mem "--index" args,
   List.filter (fun a -> a <> "--index") args)

let parse_test_flag args =
  (List.mem "--test" args || List.mem "-t" args,
   List.filter (fun a -> a <> "--test" && a <> "-t") args)

let parse_loads args =
  let rec go loads rest = function
    | "--load" :: file :: tail -> go (loads @ [file]) rest tail
    | arg :: tail              -> go loads (rest @ [arg]) tail
    | []                       -> (loads, rest)
  in
  go [] [] args

(* --strict promotes violations to errors; --json emits findings for a
   tool to read rather than a person; --fix applies what the findings
   carry. *)
let parse_lint_flags args =
  let strict = ref false and json = ref false and fix = ref false
  and effects = ref false in
  let rest = List.filter (fun a ->
    match a with
    | "--strict"  -> strict := true; false
    | "--json"    -> json := true; false
    | "--fix"     -> fix := true; false
    | "--effects" -> effects := true; false
    | _ -> true) args
  in
  (!strict, !json, !fix, !effects, rest)

(* A check answers with one diagnostic, and an unbound name is the one error
   it carries on past, so several may travel together. Everything that
   reports a file's errors reports all of them. *)
let report_diags ~json ~path d =
  let ds = Wand.Diag.all d in
  if json then print_endline (Wand.Diag.to_json_array ~file:path ds)
  else List.iter (fun x ->
    Printf.eprintf "Error: %s\n" (Wand.Diag.legacy x)) ds

(* The same, under the path, for a run over a tree. *)
let report_diags_under ~path d =
  List.iter (fun x ->
    Printf.eprintf "%s: %s\n" path (Wand.Diag.legacy x)) (Wand.Diag.all d);
  flush stderr

(* `Shell(df)` -- the manifest's own spelling, so what a file reaches is
   written the way the line bounding it would be. *)
let effect_label (name, allows) = Wand.Shell_scan.render_label (name, allows)

(* `! {IO, Shell(df)}`, and `! {}` for a file that reaches nothing. Both are
   spellings wand already has: this is the effect set of a signature, over a
   file instead of a function. *)
let effect_set effects =
  " ! {" ^ String.concat ", " (List.map effect_label effects) ^ "}"

let effect_json (name, allows) =
  match allows with
  | None -> Printf.sprintf "{\"label\":\"%s\"}" name
  | Some ws ->
    Printf.sprintf "{\"label\":\"%s\",\"allows\":[%s]}" name
      (String.concat "," (List.map (fun w ->
         "\"" ^ Wand.Diag.escape_json w ^ "\"") ws))

let effects_json_entry path effects =
  Printf.sprintf "{\"file\":\"%s\",\"effects\":[%s]}"
    (Wand.Diag.escape_json path)
    (String.concat "," (List.map effect_json effects))

(* Returns the exit code: lints are warnings unless --strict promotes a
   violation. *)
let report_lints ~strict ~json ?(holes = []) sess src =
  match Wand.Runner.lint_session sess src with
  | Error _ ->
    (* the typecheck itself already reported this *)
    if json then print_endline "[]"; 0
  | Ok findings ->
    if json then
      print_endline (Wand.Lint.diagnostics_json ~strict ~holes findings)
    else
      List.iter (fun f ->
        Printf.eprintf "warning: %s\n" (Wand.Lint.to_text f)) findings;
    (* The exit code says the same thing either way: under `--strict` a
       violation is a failure, and a caller that only reads the code is
       still told so. *)
    if strict && List.exists Wand.Lint.fails_strict findings then 1 else 0

(* `wand a.wand --lint` asks for the verdict on the way to running. A lint is
   not a type error and not a compiler error -- a file that earns one still
   runs correctly by the language's own rules -- so it is not a condition of
   running and the plain run says nothing. Asked for, the findings go to
   stderr, which leaves stdout the script's, and the script still runs.
   `--strict` is the promise `wand t --strict` makes: a violation is a
   failure, and a failure does not run.

   The check costs a parse and an inference that the run then does again.
   That is the price of the flag and it is not paid without it. *)
let lint_before_running ~strict path =
  match Wand.Runner.typecheck_file path with
  | Error d ->
    (* The run would refuse for the same reason; saying it once is enough. *)
    Printf.eprintf "Error: %s\n" (Wand.Diag.legacy d); exit 1
  | Ok sc ->
    List.iter (fun f ->
      Printf.eprintf "warning: %s\n" (Wand.Lint.to_text f))
      sc.Wand.Runner.sc_findings;
    (* Both streams are buffered and both are flushed at exit, so a verdict
       written before the run still reads after its output. Flushed here so
       the findings stand where they happened: before the script ran. *)
    flush stderr;
    if strict && List.exists Wand.Lint.fails_strict sc.Wand.Runner.sc_findings
    then exit 1

(* One-shot commands let an expression name a stdlib module without importing
   it. Importing all of them to provide that costs a disk read, lex, parse,
   full inference and eval per module, on every invocation -- paid in full by
   `wand t "1 + 2"`, the command the generate/typecheck/fix loop runs most.
   Import only the modules the input actually mentions instead. *)
let stdlib_prelude_for sources =
  let mentioned = Hashtbl.create 8 in
  List.iter (fun src ->
    match (try Some (Wand.Lexer.tokenize src) with _ -> None) with
    | None -> ()
    | Some toks ->
      List.iter (fun (tok, _) ->
        match tok with
        | Wand.Token.Upper name
          when List.mem name Wand.Typechecker.stdlib_module_names ->
          Hashtbl.replace mentioned name ()
        | _ -> ()) toks
  ) sources;
  Hashtbl.fold (fun name () acc -> ("import " ^ name) :: acc) mentioned []
  |> String.concat "\n"

(* Source text mentioning every stdlib module, for the commands that must
   load all of them. *)
let all_stdlib_imports =
  String.concat "\n"
    (List.map (fun n -> "import " ^ n) Wand.Typechecker.stdlib_module_names)

(* What a doc example prints is part of what it claims, so everything the
   run writes is caught, in the order it was written. Both streams: a
   session shows them together, and `IO.println_err` writing nothing a
   reader could see would make its example a claim about nothing.

   The value line goes through the REPL's own printer, so what is compared
   is what a reader would have seen. *)
let capturing f =
  let tmp = Filename.temp_file "wand_doc_" ".out" in
  let saved_out = Unix.dup Unix.stdout in
  let saved_err = Unix.dup Unix.stderr in
  let fd = Unix.openfile tmp [Unix.O_WRONLY; Unix.O_TRUNC] 0o600 in
  flush stdout; flush stderr;
  Unix.dup2 fd Unix.stdout;
  Unix.dup2 fd Unix.stderr;
  let result =
    Fun.protect
      ~finally:(fun () ->
        flush stdout; flush stderr;
        Unix.dup2 saved_out Unix.stdout;
        Unix.dup2 saved_err Unix.stderr;
        Unix.close saved_out;
        Unix.close saved_err;
        Unix.close fd)
      f
  in
  let out = In_channel.with_open_text tmp In_channel.input_all in
  (try Sys.remove tmp with Sys_error _ -> ());
  (result, out)

let lines_of s =
  String.split_on_char '\n' s
  |> List.filter (fun l -> String.trim l <> "")

(* Running the examples in a doc string.

   The examples of one doc string are one session, read in order, because
   that is what the prompt in front of them says: a name bound by one is
   there for the next. A prompt with nothing under it claims nothing -- it
   is a step, not an example -- so it is run and not compared.

   Between doc strings nothing is shared, so an example cannot come to
   depend on a name that a neighbour happened to bind. *)
let run_example sess expr =
  let (result, out) =
    capturing (fun () ->
      match Wand.Runner.run_session sess expr with
      | Ok (s, r) -> Wand.Repl.print_result r; Ok s
      | Error msg -> Error msg)
  in
  match result with
  | Ok s    -> (s, lines_of out)
  | Error m -> (sess, ["Error: " ^ m])

(* `-x`: the doc as written, with its examples run where they stand. What
   the example produced this time takes the place of what it says it
   produces, so the two can be compared by eye and a stale one is visible. *)
let show_doc_executed sess name doc =
  (match Wand.Runner.lookup_type sess name with
   | Some t -> Printf.printf "%s : %s\n" name t
   | None   -> ());
  let sess = ref sess in
  List.iter (function
    | Wand.Runner.Prose l -> print_endline l
    | Wand.Runner.Example (expr, _) ->
      (* Echoed as it was written: the first line under the prompt, the rest
         under the continuation prompt. *)
      List.iteri (fun i l ->
        Printf.printf "%s %s\n" (if i = 0 then ">>" else "..") l)
        (String.split_on_char '\n' expr);
      let (s, actual) = run_example !sess expr in
      sess := s;
      List.iter print_endline actual)
    (Wand.Runner.doc_blocks doc)

(* `-t`: nothing to say unless an example does not hold. Every example runs
   even after one has failed -- reporting the first and stopping turns a
   morning's fixing into a morning of runs. *)
let test_doc_examples sess name doc =
  let examples = Wand.Runner.doc_examples doc in
  let failures = ref 0 in
  let sess = ref sess in
  (* The name is printed above its first wrong example and not otherwise. *)
  let said_name = ref false in
  let name_once () =
    if not !said_name then begin said_name := true; Printf.printf "%s\n" name end
  in
  List.iter (fun (expr, expected) ->
    let (s, actual) = run_example !sess expr in
    sess := s;
    if expected = [] || actual = expected then ()
    else begin
      incr failures;
      name_once ();
      Printf.printf "  >> %s\n" expr;
      List.iter (Printf.printf "     says:  %s\n") expected;
      List.iter (Printf.printf "     does:  %s\n") actual
    end
  ) examples;
  (List.length (List.filter (fun (_, e) -> e <> []) examples), !failures)

let load_files ?(sources = []) loads =
  let sess = Wand.Runner.make_session () in
  let file_sources = List.filter_map (fun path ->
    try Some (In_channel.with_open_text path In_channel.input_all)
    with Sys_error _ -> None) loads
  in
  let prelude = stdlib_prelude_for (sources @ file_sources) in
  let sess =
    if prelude = "" then sess
    else match Wand.Runner.run_session sess prelude with
      | Ok (s, _) -> s
      | Error msg ->
        (* The prelude is nothing but `import X` lines for modules the input
           mentions, so a failure here is the standard library failing to
           load. Carrying on without it produced "did you forget to import
           List?" -- an error about the caller's code, for a problem in the
           installation. *)
        Printf.eprintf "Error: %s\n" msg; exit 1
  in
  List.fold_left (fun s path ->
    match (try Ok (In_channel.with_open_text path In_channel.input_all)
           with Sys_error m -> Error m) with
    | Error m ->
      Printf.eprintf "Error loading '%s': %s\n" path m; exit 1
    | Ok src ->
      match Wand.Runner.run_session s src with
      | Ok (s', _) -> s'
      | Error m ->
        Printf.eprintf "Error loading '%s': %s\n" path m; exit 1
  ) sess loads

(* `--` ends wand's own arguments: everything after it belongs to the
   script, whatever it looks like. Without it a script called with its own
   `--dry-run` had the flag read as wand's -- the run became a rehearsal that
   changed nothing, said nothing about why, and the script never saw the
   argument it was passed. *)
(* wand's own flags on a script run. Each is taken wherever it appears
   before `--`, because getting one of them wrong runs a deploy for real. *)
let own_flags = ["--dry-run"; "--trace"; "--lint"; "--strict"]

let split_own args =
  let rec go acc = function
    | "--" :: after -> (List.rev acc, after)
    | a :: tl       -> go (a :: acc) tl
    | []            -> (List.rev acc, [])
  in
  go [] args

(* A path that is not there and reads like source rather than a file name.

   `.wand` is the whole test. A command that takes a file was handed
   something not named like a wand file, and the likeliest reason is that it
   is an expression -- `1 + 2`, `List.map`, or the `e` that used to be a
   subcommand. A name that does end in `.wand` gets no hint: it is a file
   that is not there, and offering `--expr` on a typo is noise. *)
let reads_as_an_expression s = not (Filename.check_suffix s ".wand")

(* The argument written the way it would have to be written again. A hint
   that cannot be pasted is half a hint. *)
let requote s =
  let b = Buffer.create (String.length s + 2) in
  Buffer.add_char b '"';
  String.iter (fun c ->
    if c = '"' || c = '\\' then Buffer.add_char b '\\';
    Buffer.add_char b c) s;
  Buffer.add_char b '"';
  Buffer.contents b

(* What is wrong, then the command that works. Nothing about what the
   spelling used to be: whoever reads this needs the right command, not its
   history. *)
let no_such_file ?hint path =
  Printf.eprintf "Error: no such file: %s\n" path;
  (match hint with
   | Some cmd -> Printf.eprintf "       did you mean: %s\n" cmd
   | None -> ());
  exit 1

(* Which words are commands. The file-running path takes anything else, and
   a flag after a script belongs to the script -- so `--help` is wand's only
   when a command was actually named. *)
let is_a_command = function
  | "h" | "help" | "i" | "interactive" | "l" | "lsp" | "t" | "type"
  | "p" | "package"
  | "d" | "doc" | "f" | "fmt" | "s" | "test"
  | "v" | "version" -> true
  | _ -> false

(* Whatever is left after a command has taken its own flags. Anything still
   opening with `-` is one this command does not have -- taken for a file or
   a name instead, it was reported as a missing path, an unknown module, or
   (`wand d --nope`) as a name with no documentation, at exit 0. *)
let reject_unknown_options cmd args =
  List.iter (fun a ->
    if String.length a > 1 && a.[0] = '-' then begin
      Printf.eprintf "Error: unknown option: %s\n" a;
      (if cmd = "t" && a = "--file" then
         Printf.eprintf "       did you mean: wand t <file>\n");
      Printf.eprintf "Run 'wand h %s' for usage.\n" cmd;
      exit 1
    end) args

(* A flag that takes a value and did not get one. It is a flag the command
   has, so saying it is unknown names the wrong problem. *)
let missing_value cmd flag what =
  Printf.eprintf "Error: expected %s after %s\n" what flag;
  Printf.eprintf "Run 'wand h %s' for usage.\n" cmd;
  exit 1

(* `--help` anywhere among a command's own arguments, except where it is the
   value of a flag that takes one: `wand t -e "--help"` is asking about an
   expression that happens to be spelled like a flag. *)
let wants_help args =
  let rec go prev = function
    | [] -> false
    | a :: tl ->
      let is_a_value =
        match prev with Some ("-e" | "--expr" | "--load") -> true | _ -> false in
      if (a = "--help" || a = "-h") && not is_a_value then true else go (Some a) tl
  in
  go None args

(* One script run, however the line was written. The flags are wand's
   wherever they appear before `--`: a mode written first reads better
   (`wand --dry-run deploy.wand`) and has to mean the same thing, or one
   spelling is a gate and the other is a deploy.

   `--strict` asks for the findings and refuses to run on a violation, so it
   implies `--lint` rather than needing it. A script with a `--strict` of its
   own is given it after `--`. *)
let run_script path args =
  let (before, after) = split_own args in
  let has f = List.mem f before in
  let strict = has "--strict" in
  let lint = has "--lint" || strict in
  let mode =
    if has "--dry-run" then Wand.Runner.DryRun
    else if has "--trace" then Wand.Runner.Trace
    else Wand.Runner.Normal
  in
  let strip =
    List.filter (fun a -> not (List.mem a own_flags)) before @ after
  in
  Wand.Evaluator.exe_args_ref := strip;
  if lint then lint_before_running ~strict path;
  (* A running script can be stopped by a signal or by `exit`. Both unwind,
     so whatever the script is holding is released first, and the code it
     stops with is the one the caller expects. *)
  Wand.Runner.install_signal_handlers ();
  (match Wand.Runner.run_file ~mode path with
   | Ok v    -> if v <> "()" then print_endline v
   | Error e -> Printf.eprintf "Error: %s\n" e; exit 1)

let main () =
  let args = Array.to_list Sys.argv |> List.tl in
  (* `--load` may come either side of the expression, so the loads come out
     of the whole line before asking whether this is the expression form. The
     result is used only when it is; otherwise `args` is matched as it
     arrived and a subcommand's own `--load` reaches the subcommand. *)
  let (top_loads, top_rest) = parse_loads args in
  match args with
  | [] | ["--help"] | ["-h"] -> usage ()
  | _ when (match top_rest with ("-e" | "--expr") :: _ -> true | _ -> false) ->
    (match top_rest with
     | flag :: [] ->
       Printf.eprintf
         "Error: expected an expression after %s\nRun 'wand h' for usage.\n" flag;
       exit 1
     | _ :: [expr] ->
       let sess = load_files ~sources:[expr] top_loads in
       (* An expression runs the same code a script does, so it is stopped
          the same way: a `with` releases, and the exit code is the one the
          caller expects. *)
       Wand.Runner.install_signal_handlers ();
       (match Wand.Runner.run_session sess expr with
        | Error msg -> Printf.eprintf "Error: %s\n" msg; exit 1
        | Ok (_, r) -> Wand.Repl.print_result r)
     | _ ->
       Printf.eprintf "Error: too many arguments\nRun 'wand h' for usage.\n";
       exit 1)
  (* Printed bare, as `wand 0.1.0`, so an installer can compare it to what it
     meant to install without parsing prose. *)
  | ["v"] | ["version"] ->
    print_endline ("wand " ^ Wand.Version.value)
  | sub :: rest when sub = "--dry-run" || sub = "--trace" ->
    (* The mode can come first, which reads better: wand --dry-run
       deploy.wand. The path is lifted out and everything else, this flag
       included, goes to the one parser -- so `--strict` is a gate on either
       side of the path rather than an argument the script is handed. *)
    (match rest with
     | ("-e" | "--expr") :: _ ->
       (* Rehearsing and tracing are built around a script's effects and
          `run_session` has no mode to give them. Said plainly rather than
          accepted and ignored: a `--dry-run` that quietly ran for real is
          the one mistake this flag exists to prevent. *)
       Printf.eprintf "Error: %s applies to a script, not to an expression\n" sub;
       exit 1
     | _ ->
       let (before, after) = split_own rest in
       let rec lift flags = function
         | a :: tl when List.mem a own_flags -> lift (a :: flags) tl
         | path :: tl -> Some (path, List.rev flags @ tl)
         | [] -> None
       in
       (match lift [] before with
        | None ->
          Printf.eprintf "Error: expected a script after %s\n" sub; exit 1
        | Some (path, rest) ->
          if not (Sys.file_exists path) then no_such_file path;
          let tail = match after with [] -> [] | _ -> "--" :: after in
          run_script path ((sub :: rest) @ tail)))
  (* Asked for before anything is done with it. `wand i --help` started a
     session and `wand lsp --help` started a server, both of which hang
     rather than answer; `wand d --help` looked up a doc for `--help` and
     exited 0. Every command has usage text already -- this is only about
     reaching it. *)
  | sub :: rest when is_a_command sub && wants_help rest -> usage_for sub
  | sub :: rest ->
    match sub with
    | "h" | "help" ->
      (match rest with
       | []    -> usage ()
       | [cmd] -> usage_for cmd
       | _     -> usage ())
    | "i" | "interactive" ->
      let (loads, _) = parse_loads rest in
      Wand.Repl.run ~base_dir:(Sys.getcwd ()) ~loads ()
    | "l" | "lsp" ->
      exit (Wand.Lsp.serve stdin stdout)
    | "p" | "package" ->
      let dir = Sys.getcwd () in
      let run f =
        try f () with
        | Wand.Package_cmd.Failed msg | Wand.Package.Unresolved msg ->
          Printf.eprintf "Error: %s\n" msg; exit 1
        | Wand.Package.Error (loc, msg) ->
          Printf.eprintf "Error: %s%s\n"
            (match loc with
             | Some l -> Printf.sprintf "%s:%d:%d: " l.Wand.Token.file l.line l.col
             | None -> "")
            msg;
          exit 1
      in
      (match rest with
       | [] -> usage_for "p"
       | ("i" | "init") :: [url] -> run (fun () -> Wand.Package_cmd.init ~dir url)
       | ("i" | "init") :: _ ->
         Printf.eprintf "Error: expected the module's URL: wand p init <url>\n"; exit 1
       | ["t"] | ["tidy"] -> run (fun () -> Wand.Package_cmd.tidy ~dir)
       | ["u"] | ["upgrade"] -> run (fun () -> Wand.Package_cmd.upgrade ~dir None)
       | ("u" | "upgrade") :: [target] ->
         run (fun () -> Wand.Package_cmd.upgrade ~dir (Some target))
       | cmd :: _ ->
         Printf.eprintf "Error: no package command %s\nRun 'wand h p' for usage.\n" cmd;
         exit 1)
    | "t" | "type" ->
      let (strict, json, fix, effects, rest) = parse_lint_flags rest in
      (* `--effects` answers one question about a file, so it does not also
         rewrite it or report lints. Refused rather than ignored, because a
         flag quietly dropped is work that did not happen. *)
      if effects && fix then begin
        Printf.eprintf
          "Error: --effects reports what a file reaches; it does not rewrite one\n\
           Run 'wand h t' for usage.\n";
        exit 1
      end;
      if effects && strict then begin
        Printf.eprintf
          "Error: --effects reports no lint findings, so --strict has nothing \
           to promote\n\
           Run 'wand h t' for usage.\n";
        exit 1
      end;
      (* A file is named directly, as it is everywhere else in the CLI; an
         expression is given with `--expr`. Which one it is has to be said
         rather than guessed from the argument's shape, because `deploy.wand`
         is itself a valid path expression -- the two cannot be told apart,
         so one of them carries a flag, and it is the rarer one. *)
      let rec take_expr acc = function
        | ("--expr" | "-e") :: e :: tl -> (Some e, List.rev_append acc tl)
        | [("--expr" | "-e") as flag] -> missing_value "t" flag "an expression"
        | ["--load"] -> missing_value "t" "--load" "a file"
        | x :: tl -> take_expr (x :: acc) tl
        | [] -> (None, List.rev acc)
      in
      (match take_expr [] rest with
       | Some _, _ when fix ->
         Printf.eprintf
           "Error: --fix rewrites a file, so it cannot be used with --expr\n\
            Run 'wand h t' for usage.\n";
         exit 1
       | Some _, _ when effects ->
         (* A manifest bounds a file. An expression is not one, so there is
            nothing for the answer to be about. *)
         Printf.eprintf
           "Error: --effects reports what a file reaches, so it cannot be \
            used with --expr\n\
            Run 'wand h t' for usage.\n";
         exit 1
       | Some expr, rest ->
         let (loads, rest') = parse_loads rest in
         (match rest' with
          | [] ->
            let sess = load_files ~sources:[expr] loads in
            (match Wand.Runner.typecheck_session sess expr with
             | Error d ->
               if json then (print_endline (Wand.Diag.to_json_array [d]); exit 1)
               else (Printf.eprintf "Error: %s\n" (Wand.Diag.legacy d); exit 1)
             | Ok r ->
               if not json then Wand.Repl.print_result r;
               let holes = match r with Wand.Runner.RHoles hs -> hs | _ -> [] in
               let code = report_lints ~strict ~json ~holes sess expr in
               if code <> 0 then exit code)
          | _ ->
            Printf.eprintf "Error: too many arguments\nRun 'wand h t' for usage.\n";
            exit 1)
       | None, rest ->
         let (loads, rest') = parse_loads rest in
         (* `--load` seeds a session and a file is checked on its own, so the
            two do not combine. Refused rather than dropped: a flag quietly
            ignored is a check that did not happen. *)
         if loads <> [] then begin
           Printf.eprintf
             "Error: --load applies to --expr, not to a file\n\
              Run 'wand h t' for usage.\n";
           exit 1
         end;
         (* An argument that opens with `-` is a flag this command does not
            have. Taken as the file it would be reported as a missing path,
            or -- with an expression after it -- as too many arguments, and
            neither names the thing that is actually wrong. Found by asking
            what `wand t -e "1 + 2"` does. *)
         reject_unknown_options "t" rest';
         let roots =
           match rest' with
           | [] ->
             Printf.eprintf
               "Error: expected a file or a directory\nRun 'wand h t' for usage.\n";
             exit 1
           | paths -> paths
         in
         List.iter (fun p ->
           if not (Sys.file_exists p) then
             no_such_file p
               ?hint:(if List.length roots = 1 && reads_as_an_expression p
                      then Some ("wand t --expr " ^ requote p) else None)) roots;
         (* One file named directly answers about that file, and says what it
            checks out as. A directory, or more than one path, is a question
            about a tree: the answer is per file, and a failure in one does
            not stop the rest, because a gate wants the whole list. *)
         match roots with
         | [path] when not (Wand.Runner.is_dir path) ->
         if effects then
           (match Wand.Runner.typecheck_file path with
            | Error d ->
              report_diags ~json ~path d; exit 1
            | Ok sc ->
              let es = sc.Wand.Runner.sc_effects in
              if json then print_endline ("[" ^ effects_json_entry path es ^ "]")
              (* The file, then what it performs -- the shape `wand d` gives
                 a name, with the file standing where the name would. *)
              else print_endline (path ^ effect_set es))
         else if fix then
           (match Wand.Fix.fix_file path with
            | Error d ->
              (* An error with no applicable fix: nothing was written. *)
              if json then
                (print_endline (Wand.Diag.to_json_array ~file:path [d]); exit 1)
              else (Printf.eprintf "Error: %s\nnothing fixed\n" (Wand.Diag.legacy d); exit 1)
            | Ok applied ->
              if json then
                print_endline
                  (Wand.Diag.to_json_array ~file:path
                     (List.map (fun a -> a.Wand.Fix.diag) applied))
              else if applied = [] then
                (* A command that rewrites a file says whether it did. It
                   printed nothing and exited 0, which reads the same as a
                   file that was fixed and is not the same thing at all. *)
                Printf.printf "nothing to fix in %s\n" path
              else
                List.iter (fun a ->
                  Printf.printf "%s: %d — %s\n"
                    a.Wand.Fix.code a.Wand.Fix.line a.Wand.Fix.note) applied)
         else
           (match Wand.Runner.typecheck_file path with
            | Error d ->
              report_diags ~json ~path d; exit 1
            | Ok sc ->
              let holes    = sc.Wand.Runner.sc_holes in
              let findings = sc.Wand.Runner.sc_findings in
              if not json then begin
                if holes <> [] then
                  List.iter (fun h -> Printf.printf "Hole: %s\n" h) holes
                else if sc.Wand.Runner.sc_type <> "Unit" then
                  print_endline sc.Wand.Runner.sc_type
              end;
              if json then
                print_endline
                  (Wand.Lint.diagnostics_json ~strict ~file:path ~holes findings)
              else List.iter (fun f ->
                Printf.eprintf "warning: %s\n" (Wand.Lint.to_text f)) findings;
              (* Whatever the output looks like, `--strict` means a violation
                 ends the command in failure. Reporting it as an error inside
                 the JSON and then exiting 0 told the CI step that read the
                 code -- which is most of them -- that the file was clean. *)
              if strict && List.exists Wand.Lint.fails_strict findings
              then exit 1)
         | _ ->
           let files = List.concat_map Wand.Runner.find_wand_files roots in
           if files = [] then begin
             Printf.eprintf "No .wand files found in %s\n"
               (String.concat ", " roots);
             exit 1
           end;
           if effects then begin
             (* Aligned on the `!`, the column set by the longest path here,
                so a reader scans one column instead of ragged text. *)
             let width =
               List.fold_left (fun w p -> max w (String.length p)) 0 files in
             let failed = ref false and items = ref [] in
             List.iter (fun path ->
               match Wand.Runner.typecheck_file path with
               | Error d ->
                 failed := true;
                 if json then
                   items := !items
                     @ List.map (Wand.Diag.to_json ~file:path) (Wand.Diag.all d)
                 else report_diags_under ~path d
               | Ok sc ->
                 let es = sc.Wand.Runner.sc_effects in
                 if json then items := !items @ [effects_json_entry path es]
                 else begin
                   Printf.printf "%-*s%s\n" width path (effect_set es);
                   flush stdout
                 end)
               files;
             if json then print_endline ("[" ^ String.concat "," !items ^ "]")
             else begin
               flush stdout; flush stderr;
               print_endline (count_of (List.length files) "file")
             end;
             if !failed then exit 1
           end else if fix then begin
             let failed = ref false and fixed = ref 0 and items = ref [] in
             List.iter (fun path ->
               match Wand.Fix.fix_file path with
               | Error d ->
                 failed := true;
                 if json then
                   items := !items
                     @ List.map (Wand.Diag.to_json ~file:path) (Wand.Diag.all d)
                 else report_diags_under ~path d
               | Ok applied ->
                 fixed := !fixed + List.length applied;
                 if json then
                   items := !items
                     @ List.map (fun a -> Wand.Diag.to_json ~file:path a.Wand.Fix.diag)
                         applied
                 else
                   List.iter (fun a ->
                     Printf.printf "%s: %s: %d — %s\n"
                       path a.Wand.Fix.code a.Wand.Fix.line a.Wand.Fix.note) applied)
               files;
             if json then print_endline ("[" ^ String.concat "," !items ^ "]")
             else begin
               (* Both streams are buffered, and the summary belongs under
                  what it summarises. Without this the count came out first. *)
               flush stdout; flush stderr;
               Printf.printf "%s, %s\n"
                 (count_of (List.length files) "file")
                 (if !fixed = 0 then "nothing to fix"
                  else Printf.sprintf "%d fixed" !fixed)
             end;
             if !failed then exit 1
           end else begin
             let errors = ref 0 and warnings = ref 0 and strict_hit = ref false
             and items = ref [] in
             List.iter (fun path ->
               match Wand.Runner.typecheck_file path with
               | Error d ->
                 incr errors;
                 if json then
                   items := !items
                     @ List.map (Wand.Diag.to_json ~file:path) (Wand.Diag.all d)
                 else report_diags_under ~path d
               | Ok sc ->
                 let holes    = sc.Wand.Runner.sc_holes in
                 let findings = sc.Wand.Runner.sc_findings in
                 warnings := !warnings + List.length findings;
                 if List.exists Wand.Lint.fails_strict findings then strict_hit := true;
                 if json then
                   items := !items
                     @ Wand.Lint.diagnostics_json_items ~strict ~file:path ~holes findings
                 else begin
                   List.iter (fun h -> Printf.printf "%s: hole: %s\n" path h) holes;
                   flush stdout;
                   List.iter (fun f ->
                     Printf.eprintf "%s: warning: %s\n" path (Wand.Lint.to_text f))
                     findings;
                   flush stderr
                 end)
               files;
             if json then print_endline ("[" ^ String.concat "," !items ^ "]")
             else begin
               (* A run over a tree says what it covered even when it found
                  nothing, because silence reads the same as finding no files
                  to check. It goes under what it summarises, which is why
                  both streams are flushed first. *)
               flush stdout; flush stderr;
               let found =
                 match (!errors, !warnings) with
                 | (0, 0) -> "nothing to report"
                 | (e, 0) -> count_of e "error"
                 | (0, w) -> count_of w "warning"
                 | (e, w) -> count_of e "error" ^ ", " ^ count_of w "warning"
               in
               Printf.printf "%s, %s\n" (count_of (List.length files) "file") found
             end;
             if !errors > 0 || (strict && !strict_hit) then exit 1
           end)
    | "d" | "doc" ->
      let (json, rest) = parse_json_flag rest in
      let (execute, rest) = parse_execute_flag rest in
      let (test, rest) = parse_test_flag rest in
      let (index, rest) = parse_index_flag rest in
      reject_unknown_options "d" (snd (parse_loads rest));
      if index && (execute || test) then begin
        Printf.eprintf
          "Error: --index lists every name; -x and -t run one name's \
           examples. Pick one.\n";
        exit 1
      end;
      if execute && test then begin
        Printf.eprintf
          "Error: -x runs the examples and shows what they do; -t runs them \
           and reports\n       only what does not hold. Pick one.\n";
        exit 1
      end;
      let (loads, rest') = parse_loads rest in
      if index && rest' <> [] then begin
        Printf.eprintf
          "Error: --index lists every name, so it takes no name\n\
           Run 'wand h d' for usage.\n";
        exit 1
      end;
      (match rest' with
       | [] when index ->
         (* Every module's members at once. The same listing `wand d <module>`
            gives, for each module in turn, so what a tool reads is what the
            command already answers one module at a time. *)
         let sess = load_files ~sources:[all_stdlib_imports] loads in
         if json then print_endline (Wand.Runner.index_json sess)
         else List.iter print_endline (Wand.Runner.index_lines sess)
       | [] when execute || test ->
         Printf.eprintf
           "Error: -x and -t run one name's examples, so they need a name\n\
            Run 'wand h d' for usage.\n";
         exit 1
       | [] ->
         (* No name: what is in scope. This was a command of its own, and
            it is the same question with the argument left off -- a name has
            a doc, a module has the names in it, and no name at all has the
            modules. *)
         let sess = load_files ~sources:[all_stdlib_imports] loads in
         if json then print_endline (Wand.Runner.scope_json sess)
         else begin
           let entries =
             List.sort (fun (a, _) (b, _) -> String.compare a b)
               sess.Wand.Runner.s_type_env in
           if entries = [] then print_endline "(empty)"
           else
             List.iter (fun (name, sc) ->
               match sc with
               | Wand.Typechecker.Namespace _ -> print_endline name
               | _ ->
                 Printf.printf "%s : %s\n" name
                   (Wand.Typechecker.string_of_scheme sc)) entries
         end
       | [name] when execute || test ->
         (* A module runs every example it documents; a single name runs its
            own. Under `-t` the exit code is the answer, so this can gate a
            build. *)
         let sess = load_files ~sources:[name; all_stdlib_imports] loads in
         let is_module = Wand.Runner.module_members sess name <> None in
         let names = match Wand.Runner.module_members sess name with
           | Some members -> List.map (fun m -> name ^ "." ^ m) members
           | None         -> [name]
         in
         let documented =
           List.filter_map (fun n ->
             match List.assoc_opt n sess.Wand.Runner.s_docs with
             | Some doc when Wand.Runner.doc_examples doc <> [] -> Some (n, doc)
             | _ -> None) names
         in
         (* A module with nothing to run is a module nobody has written
            examples for yet, which is a state a gate has to pass through.
            A name asked for by itself is a question, and the answer is that
            it has none. *)
         if documented = [] then begin
           if is_module then begin
             if execute then Printf.printf "%s: no examples\n" name;
             exit 0
           end else begin
             Printf.eprintf "Error: no examples to run for '%s'\n" name; exit 1
           end
         end;
         if execute then begin
           List.iteri (fun i (n, doc) ->
             if i > 0 then print_newline ();
             show_doc_executed sess n doc) documented
         end else begin
           let failed =
             List.fold_left (fun failed (n, doc) ->
               let (_, f) = test_doc_examples sess n doc in
               failed + f) 0 documented
           in
           if failed > 0 then exit 1
         end
       | [name] ->
         let sess = load_files ~sources:[name] loads in
         (* A module takes every name in it, which the usage has always
            said and this path did not do: it looked for a doc on the
            namespace, found none, and said so. Listing the members is the
            question that was asked. *)
         (match List.assoc_opt name sess.Wand.Runner.s_type_env with
          | Some (Wand.Typechecker.Namespace (members, _)) ->
            if json then
              (match Wand.Runner.module_json sess name with
               | Ok out    -> print_endline out
               | Error msg -> Printf.eprintf "%s\n" msg; exit 1)
            else
              (ignore members;
               match Wand.Runner.module_lines sess name with
               | Some lines -> List.iter print_endline lines
               | None -> ())
          | _ ->
            if json then
              (* An array whether one name was asked about or a whole
                 module, so the shape belongs to the command rather than to
                 the argument and nothing downstream has to branch on it. *)
              print_endline ("[" ^ Wand.Runner.doc_json sess name ^ "]")
            else begin
              (match Wand.Runner.lookup_type sess name with
               (* The interfaces go on the signature, where `--index` puts
                  them too, rather than in a slot of their own. *)
               | Some t ->
                 Printf.printf "%s : %s%s\n" name t
                   (Wand.Runner.iface_suffix (Wand.Runner.name_ifaces sess name))
               | None   -> ());
              (* What a handler intercepts. The signature carries the label
                 and a label covers several operations, so without this the
                 only way from `! {FS.Read}` to `FS!read_file` was the
                 reference. *)
              (match Wand.Typechecker.operations_performed_by name with
               | []  -> ()
               | ops -> Printf.printf "performs %s\n" (String.concat ", " ops));
              (match List.assoc_opt name sess.Wand.Runner.s_docs with
               | Some doc -> print_endline doc
               | None     -> Printf.printf "%s: no doc\n" name)
            end)
       | _ ->
         Printf.eprintf "Error: too many arguments\nRun 'wand h d' for usage.\n"; exit 1)
    | "f" | "fmt" ->
      reject_unknown_options "f" rest;
      (match rest with
       | [] ->
         Printf.eprintf "Error: expected one or more files\nRun 'wand h f' for usage.\n"; exit 1
       | paths ->
         let had_error = ref false in
         List.iter (fun path ->
           match (try Ok (In_channel.with_open_text path In_channel.input_all)
                  with Sys_error m -> Error m) with
           | Error m -> had_error := true; Printf.eprintf "Error loading '%s': %s\n" path m
           | Ok src ->
             (try
                let formatted = Wand.Formatter.format_source src in
                (* Written beside and renamed into place, the way every
                   other write wand makes is. Truncating the file the
                   reader is editing and then filling it means a crash or a
                   full disk part way through leaves half a source file and
                   no copy of the other half. *)
                Wand.Runner.write_atomic path formatted;
                Printf.printf "formatted %s\n" path
              with
              | (Wand.Lexer.LexError _ | Wand.Parser.ParseError _) as e ->
                had_error := true;
                Printf.eprintf "Error: %s: %s\n" path (Wand.Runner.legacy_of_exn e)
              | Unix.Unix_error (e, f, _) ->
                had_error := true;
                Printf.eprintf "Error: %s: %s: %s\n" path f (Unix.error_message e)
              | Failure m ->
                had_error := true;
                Printf.eprintf "Error: %s: %s\n" path m)
         ) paths;
         if !had_error then exit 1)
    | "s" | "test" ->
      let (json, rest) = parse_json_flag rest in
      reject_unknown_options "s" rest;
      (* No argument means the directory you are standing in, which is what
         you want after editing a script: run its tests without naming them.
         A directory argument searches it the same way; a file is run as
         given, whatever it is called. *)
      let roots = match rest with [] -> ["."] | paths -> paths in
      let missing = List.filter (fun p -> not (Sys.file_exists p)) roots in
      if missing <> [] then begin
        List.iter (fun p -> Printf.eprintf "Error: no such file or directory: %s\n" p) missing;
        exit 1
      end;
      (* A test that holds a resource should give it back when the run is
         interrupted, the same as a script. *)
      Wand.Runner.install_signal_handlers ();
      let paths = List.concat_map Wand.Runner.find_test_files roots in
      (match paths with
       | [] ->
         (match rest with
          | [] ->
            Printf.eprintf
              "No test files found under '.' — a test file is named test_*.wand.\n";
          | _ ->
            Printf.eprintf "No test files found in %s — a test file is named test_*.wand.\n"
              (String.concat ", " roots));
         exit 1
       | paths when json ->
         let results =
           Wand.Runner.with_stdout_to_stderr (fun () ->
             List.map (fun path -> (path, Wand.Runner.run_test_file path)) paths)
         in
         print_endline (Wand.Runner.test_results_json results);
         let clean = List.for_all (fun (_, r) ->
           match r with
           | Error _ -> false
           | Ok outcomes ->
             List.for_all
               (function Wand.Runner.TPass _ -> true | _ -> false) outcomes
         ) results in
         if not clean then exit 1
       | paths ->
         let multi = List.length paths > 1 in
         let had_error = ref false in
         let passed = ref 0 and failed = ref 0 in
         List.iter (fun path ->
           if multi then Printf.printf "=== %s ===\n" path;
           match Wand.Runner.run_test_file path with
           | Error m -> had_error := true; Printf.eprintf "Error loading '%s': %s\n" path m
           | Ok outcomes ->
             List.iter (fun outcome ->
               match outcome with
               | Wand.Runner.TPass label -> incr passed; Printf.printf "ok   %s\n" label
               | Wand.Runner.TFail msg   -> incr failed; Printf.printf "FAIL %s\n" msg
               | Wand.Runner.TError msg  -> incr failed; Printf.printf "FAIL %s\n" msg
             ) outcomes
         ) paths;
         Printf.printf "%d passed, %d failed\n" !passed !failed;
         if !had_error || !failed > 0 then exit 1)
    | path ->
      (* An unknown word reaches here too, since anything that is not a
         command is taken as a script to run. `wand e "1 + 2"` is the one
         worth naming: it is a hyphen away from right, and the argument it
         was given is the expression to put in the hint. *)
      (* `wand deploy.wand` and `wand deploy` name the same script when only
         one of the two is on disk, which is what the runner resolves to. *)
      if not (Sys.file_exists path || Sys.file_exists (path ^ ".wand")) then begin
        let hint =
          match path, rest with
          | ("e" | "eval"), [expr] -> Some ("wand -e " ^ requote expr)
          | "V", [] -> Some "wand v"
          | "env", [] -> Some "wand d"
          | "env", [m] -> Some ("wand d " ^ m)
          | _, [] when reads_as_an_expression path ->
            Some ("wand -e " ^ requote path)
          | _ -> None
        in
        no_such_file ?hint path
      end;
      run_script path rest

(* The last write of a run can be the one that finds the reader gone -- the
   verdict line of `wand s | head`. It is not the script's failure to report,
   and it is not worth a stack trace: 141 is what the shell reports for a
   command that ended on a closed pipe. *)
let () =
  try main () with
  | Sys_error m when Wand.Runner.broken_pipe m -> exit 141
  (* `Proc.exit` and a signal both unwind to here. Every command can reach
     one: a test file that exits deliberately, a doc example, an expression
     under `-e`. The code they carry is the code to leave with, rather than
     OCaml's 2 for an exception nobody caught. *)
  | Wand.Evaluator.Interrupted code -> exit code
