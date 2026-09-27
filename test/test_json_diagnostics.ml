open Wand

(* `wand t --json` is consumed by machines, so the schema is locked here as
   exact strings: severity, code, file, line, col, message, and a `fix`
   payload wherever the compiler already computes the correction. Breaking
   one of these strings means breaking every consumer. *)

let regression = Alcotest.(check string)

let findings src =
  let sess = Runner.make_session () in
  match Runner.lint_session sess src with
  | Ok fs -> fs
  | Error m -> Alcotest.failf "lint failed: %s\nsource:\n%s" m src

(* ── Findings ─────────────────────────────────────────────────────────────── *)

let test_manifest_fix () =
  regression "V-USES2 carries insert_line"
    "[{\"severity\":\"warning\",\"code\":\"V-USES2\",\"line\":1,\"col\":1,\
      \"message\":\"this file performs FS.Write and does not say so; it \
      could declare \\\"uses {FS.Write}\\\"\",\
      \"fix\":{\"insert_line\":\"uses {FS.Write}\"}}]"
    (Lint.diagnostics_json ~strict:false ~holes:[]
       (findings "import FS\nlet f p = FS.write_file p \"x\"\nf /tmp/y"))

let test_uses1_replace_line () =
  let fs =
    findings "uses {Shell, FS.Write}\nimport FS\nlet f p = FS.write_file p \"x\"\nf /tmp/y"
  in
  let json = Lint.diagnostics_json ~strict:false ~holes:[] fs in
  if not (Lint.contains json "\"fix\":{\"replace_line\":\"uses {FS.Write}\"}") then
    Alcotest.failf "A-USES1 lacks replace_line fix:\n%s" json

let test_strict_severity () =
  let fs =
    findings
      "uses {FS.Write, IO}\nimport FS\nimport IO\n\
       FS.write_file /tmp/x.txt \"hi\"\nIO.println \"done\""
  in
  let strict = Lint.diagnostics_json ~strict:true ~holes:[] fs in
  let lax = Lint.diagnostics_json ~strict:false ~holes:[] fs in
  if not (Lint.contains strict "\"severity\":\"error\",\"code\":\"V-DROP1\"") then
    Alcotest.failf "V-DROP1 not an error under --strict:\n%s" strict;
  if Lint.contains lax "\"severity\":\"error\"" then
    Alcotest.failf "errors reported without --strict:\n%s" lax

let test_file_field () =
  let json =
    Lint.diagnostics_json ~strict:false ~file:"deploy.wand" ~holes:[]
      (findings "import FS\nlet f p = FS.write_file p \"x\"\nf /tmp/y")
  in
  if not (Lint.contains json "\"code\":\"V-USES2\",\"file\":\"deploy.wand\",") then
    Alcotest.failf "file field missing or misplaced:\n%s" json

(* A finding marks the whole item it is about, and the range rides along
   as end_line/end_col. A point diagnostic (like V-USES2's line 1) keeps
   the original object shape -- the check above locks that. *)
let test_finding_range () =
  let json =
    Lint.diagnostics_json ~strict:false ~holes:[]
      (findings "let is_ready? x = x > 1\nis_ready? 2")
  in
  if not (Lint.contains json
            "\"line\":1,\"col\":1,\"end_line\":1,\"end_col\":24,") then
    Alcotest.failf "V-PRED2 does not span its item:\n%s" json

(* ── Holes ────────────────────────────────────────────────────────────────── *)

let test_hole_shape () =
  regression "a typed hole is its own shape"
    "[{\"kind\":\"hole\",\"type\":\"Int -> Int -> Int ! 'e\"}]"
    (Lint.diagnostics_json ~strict:false ~holes:["Int -> Int -> Int ! 'e"] [])

(* ── Query commands (`wand d --json`) ────────────────────────────────────── *)

let query_sess src =
  let sess = Runner.make_session () in
  match Runner.run_session sess src with
  | Ok (s, _) -> s
  | Error m -> Alcotest.failf "session load failed: %s\nsource:\n%s" m src

let test_doc_json () =
  let sess =
    query_sess "-- Doubles a number.\nlet double x = x * 2"
  in
  regression "doc as one object"
    "{\"name\":\"double\",\"type\":\"Int -> Int\",\
      \"doc\":\"Doubles a number.\"}"
    (Runner.doc_json sess "double")

let test_doc_json_absent () =
  let sess = query_sess "let x = 1" in
  regression "a missing doc and type are null, not omitted"
    "{\"name\":\"nope\",\"type\":null,\"doc\":null}"
    (Runner.doc_json sess "nope")

let test_scope_json () =
  let sess = query_sess "import List\nlet greet name = \"hi %{name}\"" in
  let json = Runner.scope_json sess in
  if not (Lint.contains json "{\"name\":\"List\",\"module\":true}") then
    Alcotest.failf "module entry missing:\n%s" json;
  if not (Lint.contains json
            "{\"name\":\"greet\",\"type\":\"'a -> String\"}") then
    Alcotest.failf "binding entry missing:\n%s" json

let test_scope_json_empty () =
  regression "an empty scope is an empty array"
    "[]" (Runner.scope_json (Runner.make_session ()))

let test_module_json () =
  let sess = query_sess "import List" in
  (match Runner.module_json sess "List" with
   | Ok json ->
     if not (Lint.contains json "{\"name\":\"List.length\",\"type\":") then
       Alcotest.failf "qualified member missing:\n%s" json
   | Error m -> Alcotest.failf "module_json List failed: %s" m);
  (match Runner.module_json sess "Nope" with
   | Error "Unknown module 'Nope'" -> ()
   | Error m -> Alcotest.failf "unexpected message: %s" m
   | Ok _ -> Alcotest.fail "expected an error for an unknown module");
  let sess = query_sess "let x = 1" in
  match Runner.module_json sess "x" with
  | Error "x is a binding, not a module" -> ()
  | Error m -> Alcotest.failf "unexpected message: %s" m
  | Ok _ -> Alcotest.fail "expected an error for a binding"

(* ── Test runs (`wand s --json`) ──────────────────────────────────────────── *)

let test_run_json () =
  regression "a test run is one object; error status still counts as failed"
    "{\"tests\":[\
       {\"file\":\"test_a.wand\",\"status\":\"pass\",\"label\":\"it adds\"},\
       {\"file\":\"test_a.wand\",\"status\":\"fail\",\
        \"message\":\"it fails: expected 4, got 3\"},\
       {\"file\":\"test_a.wand\",\"status\":\"error\",\
        \"message\":\"pattern match failure\"}],\
      \"errors\":[{\"file\":\"test_b.wand\",\
        \"message\":\"parse error: 2:1: unexpected token: EOF\"}],\
      \"passed\":1,\"failed\":2}"
    (Runner.test_results_json
       [("test_a.wand",
         Ok [Runner.TPass "it adds";
             Runner.TFail "it fails: expected 4, got 3";
             Runner.TError "pattern match failure"]);
        ("test_b.wand", Error "parse error: 2:1: unexpected token: EOF")])

let test_run_json_empty () =
  regression "no tests, no errors"
    "{\"tests\":[],\"errors\":[],\"passed\":0,\"failed\":0}"
    (Runner.test_results_json [])

(* ── Errors ───────────────────────────────────────────────────────────────── *)

(* Errors reach the JSON as `Diag.t` values whose position travelled from
   the raise site as data. Nothing here (or anywhere) recovers a position
   by parsing a message string. *)

let check_error src =
  match Runner.typecheck_source ~path:"wand_json_err.wand" src with
  | Error d -> d
  | Ok _ -> Alcotest.failf "expected an error from:\n%s" src

let test_type_error () =
  regression "type error with its position carried as data"
    "[{\"severity\":\"error\",\"code\":\"E-TYPE\",\"line\":1,\"col\":5,\
      \"message\":\"expected String, got Int\"}]"
    (Diag.to_json_array
       [Diag.error ~code:"E-TYPE" ~loc:(Token.point 1 5 4)
          "expected String, got Int"])

let test_error_without_position () =
  regression "an error with no position reports 1:1, drift fix carried"
    "[{\"severity\":\"error\",\"code\":\"E-LEX\",\"file\":\"x.wand\",\
      \"line\":1,\"col\":1,\
      \"message\":\"a comment is '-- ...' to the end of the line, not '//'\",\
      \"fix\":{\"replace\":{\"from\":\"//\",\"to\":\"--\"}}}]"
    (Diag.to_json_array ~file:"x.wand"
       [Diag.error ~code:"E-LEX"
          "a comment is '-- ...' to the end of the line, not '//'"])

(* End to end: the checker's answer carries the real position. *)

let test_lex_error_position () =
  let d = check_error "let x =\n  1 // 2" in
  Alcotest.(check string) "code" "E-LEX" d.Diag.code;
  (match d.Diag.loc with
   | Some l -> Alcotest.(check (pair int int)) "line/col of the '//'"
                 (2, 5) (l.Token.line, l.Token.col)
   | None -> Alcotest.fail "lex error lost its position");
  (match d.Diag.fix with
   | Some (Lint.Replace { from_ = "//"; to_ = "--" }) -> ()
   | _ -> Alcotest.fail "drift fix not carried")

let test_parse_error_position () =
  let d = check_error "let x = (1\n" in
  Alcotest.(check string) "code" "E-PARSE" d.Diag.code;
  if d.Diag.loc = None then Alcotest.fail "parse error lost its position"

let test_type_error_position () =
  let d = check_error "let x = 1\nlet y = x ++ \"s\"" in
  Alcotest.(check string) "code" "E-TYPE" d.Diag.code;
  (match d.Diag.loc with
   | Some l -> Alcotest.(check int) "points into line 2" 2 l.Token.line
   | None -> Alcotest.fail "type error lost its position")

(* A declaration is checked after the whole file is read, so there is no
   expression to blame and these used to land on 1:1 -- which reads as "the
   first declaration" to anything that edits by location, and the first is
   exactly the one a repeat is not about. *)

let position_of src =
  let d = check_error src in
  Alcotest.(check string) "code" "E-TYPE" d.Diag.code;
  match d.Diag.loc with
  | Some l -> (l.Token.line, l.Token.col)
  | None -> Alcotest.failf "declaration error lost its position: %s" d.Diag.message

let at = Alcotest.(check (pair int int))

let test_declaration_error_positions () =
  (* The type's own name, in the declaration the repeat is about. *)
  at "a type declared twice points at the second"
    (3, 6) (position_of "type A = Foo\n\ntype A = Bar\n\nBar\n");
  at "a declaration over a built-in name"
    (3, 6) (position_of "let x = 1\n\ntype List \'a = Nil\n\nNil\n");
  (* The constructor, where the constructor is what is repeated. *)
  (* Two types may share a constructor name, so what is refused is the bare
     use that cannot say which, and that is where it points. *)
  at "a bare use of a name two types share points at the use"
    (5, 1) (position_of "type A = Foo | Bar\n\ntype B = Foo\n\nFoo\n");
  at "a constructor repeated in one type"
    (1, 16) (position_of "type A = Foo | Foo\n\nFoo\n");
  at "a field repeated in one constructor"
    (3, 6) (position_of "let x = 1\n\ntype M(a: Int, a: Int)\n\nM(a = 1)\n");
  (* The default itself, which the parser already wraps in `Located`. *)
  at "a default of the wrong type points at the default"
    (3, 21) (position_of "let x = 1\n\ntype C(port: Port = 30s)\n\nC()\n");
  at "a default that is not a written value"
    (3, 20)
    (position_of "uses {Shell(hostname)}\n\ntype C(h: String = $(hostname))\n\nC()\n")

(* The loc of a type error spans the whole expression the nearest `Located`
   wraps, not just its first token -- `x ++ "s"` is columns 9 through 16. *)
let test_type_error_range () =
  let d = check_error "let x = 1\nlet y = x ++ \"s\"" in
  match d.Diag.loc with
  | Some l ->
    Alcotest.(check (pair int int)) "start" (2, 9) (l.Token.line, l.Token.col);
    Alcotest.(check (pair int int)) "end (exclusive)"
      (2, 17) (l.Token.end_line, l.Token.end_col)
  | None -> Alcotest.fail "type error lost its position"

let test_parse_error_drift_fix () =
  let d = check_error "1\nx and y" in
  let json = Diag.to_json_array [d] in
  if not (Lint.contains json "\"code\":\"E-PARSE\"") then
    Alcotest.failf "expected E-PARSE:\n%s" json;
  if not (Lint.contains json
            "\"fix\":{\"replace\":{\"from\":\"and\",\"to\":\"&&\"}}") then
    Alcotest.failf "drift fix missing:\n%s" json


(* ── The exit code is part of the contract ──────────────────────────────── *)

(* `--strict` says a violation ends the command in failure, and a CI step
   reads that from the exit code. Under `--json` the code stayed 0 while the
   JSON itself called the finding an error, so the step passed on a file the
   same command had just failed. These run the real binary, since the exit
   code is the CLI's answer and nothing below it can be asked. *)

let wand_binary =
  let dir = Filename.dirname (Filename.dirname Sys.executable_name) in
  Filename.concat (Filename.concat dir "bin") "wand.exe"

let run args =
  let cmd = String.concat " " (List.map Filename.quote (wand_binary :: args)) in
  let ic = Unix.open_process_in (cmd ^ " 2>/dev/null") in
  let out = In_channel.input_all ic in
  let code = match Unix.close_process_in ic with
    | Unix.WEXITED n -> n
    | Unix.WSIGNALED n | Unix.WSTOPPED n -> 128 + n
  in
  (code, String.trim out)

(* V-SHELL1: a command word decided at run time under a narrowed manifest. *)
let violating_file () =
  let path = Filename.temp_file "wand_strict" ".wand" in
  Out_channel.with_open_text path (fun oc ->
    Out_channel.output_string oc "uses {Shell(git)}\n\nlet run! cmd = $(git %!{cmd})\n");
  path

let with_violating_file f =
  let path = violating_file () in
  Fun.protect ~finally:(fun () -> try Sys.remove path with Sys_error _ -> ())
    (fun () -> f path)

let test_strict_json_exit_code () =
  with_violating_file (fun path ->
    let (code, out) = run ["t"; "--strict"; "--json"; path] in
    if not (Lint.contains out "\"severity\":\"error\"") then
      Alcotest.failf "the finding was not reported as an error:\n%s" out;
    Alcotest.(check int) "--strict --json fails on a violation" 1 code)

let test_strict_text_exit_code () =
  with_violating_file (fun path ->
    let (code, _) = run ["t"; "--strict"; path] in
    Alcotest.(check int) "and says the same without --json" 1 code)

let test_json_without_strict_is_a_warning () =
  with_violating_file (fun path ->
    let (code, out) = run ["t"; "--json"; path] in
    if not (Lint.contains out "\"severity\":\"warning\"") then
      Alcotest.failf "expected a warning without --strict:\n%s" out;
    Alcotest.(check int) "a warning is not a failure" 0 code)

let test_strict_json_expression () =
  (* The same rule for an expression, which takes a different path through
     the CLI than a file does. *)
  let (code, out) =
    run ["t"; "--strict"; "--json"; "--expr"; "let big? n = n + 1"] in
  if not (Lint.contains out "V-PRED1") then
    Alcotest.failf "expected V-PRED1:\n%s" out;
  Alcotest.(check int) "--strict --json fails on an expression too" 1 code

let test_strict_json_clean_file () =
  let path = Filename.temp_file "wand_clean" ".wand" in
  Out_channel.with_open_text path (fun oc ->
    Out_channel.output_string oc "let double n = n * 2\n");
  Fun.protect ~finally:(fun () -> try Sys.remove path with Sys_error _ -> ())
    (fun () ->
       let (code, _) = run ["t"; "--strict"; "--json"; path] in
       Alcotest.(check int) "a clean file still passes" 0 code)

(* Module loading refuses in several ways, and every one of them used to
   arrive as `E-FAIL`: a `Failure` from inside a stage, with no code of its
   own and no position. A mistyped import is something a person writes, so
   `wand t` has to be able to point at the line and the editor has to be
   able to underline it. All of these were found by test/fuzz. *)

let import_error src =
  let d = check_error src in
  Alcotest.(check string) "code" "E-IMPORT" d.Diag.code;
  match d.Diag.loc with
  | Some l -> l
  | None -> Alcotest.failf "import error lost its position:\n%s" src

let test_import_unknown_module () =
  let l = import_error "let x = 1\nimport NoSuchModule" in
  Alcotest.(check (pair int int)) "the import's line" (2, 1)
    (l.Token.line, l.Token.col)

let test_import_unknown_symbol () =
  ignore (import_error "let {nope} = import Test")

let test_import_unknown_constructor () =
  ignore (import_error "let {A} = import Test")

let test_import_bare_path () =
  ignore (import_error "import ./thing")

let test_import_bad_pattern () =
  ignore (import_error "let () = import Test")

let test_import_missing_file () =
  ignore (import_error "let m = import ./no-such-module-anywhere")

(* The position is the import that the reader wrote, not 1:1 -- which is
   what anything editing by location would otherwise rewrite. *)
let test_import_error_is_not_the_first_line () =
  let l = import_error "let a = 1\nlet b = 2\nlet {nope} = import Test" in
  Alcotest.(check int) "the third line" 3 l.Token.line

(* ── Which argument is a file and which is an expression ─────────────────── *)

(* `wand t` takes a file, as every other command that takes one does; an
   expression is given with `--expr`. The two cannot be told apart by shape --
   `deploy.wand` is a valid path expression -- so the rarer one carries the
   flag.

   What this pins is the failure. Before it, `wand t ./x.wand` typechecked the
   *path literal*, answered `Path`, and exited 0: a checking tool reporting
   success for a file it never opened. *)

(* `run` sends stderr to /dev/null, and everything this section is about --
   the warnings, the errors and the hints -- is written there. *)
let run_all args =
  let cmd = String.concat " " (List.map Filename.quote (wand_binary :: args)) in
  let ic = Unix.open_process_in (cmd ^ " 2>&1") in
  let out = In_channel.input_all ic in
  let code = match Unix.close_process_in ic with
    | Unix.WEXITED n -> n
    | Unix.WSIGNALED n | Unix.WSTOPPED n -> 128 + n
  in
  (code, String.trim out)

let says out needle =
  if not (Lint.contains out needle) then
    Alcotest.failf "expected %S in:\n%s" needle out

let test_file_is_the_default () =
  with_violating_file (fun path ->
    let (code, out) = run_all ["t"; path] in
    (* The file was read: its finding is reported. *)
    says out "V-SHELL1";
    Alcotest.(check int) "a warning alone is not a failure" 0 code)

let test_a_path_is_not_typechecked_as_a_literal () =
  (* The shape that used to answer `Path` and exit 0. *)
  with_violating_file (fun path ->
    let (_, out) = run_all ["t"; "./" ^ Filename.basename path] in
    if Lint.contains out "Path" then
      Alcotest.failf "the path was checked as a literal:\n%s" out)

let test_expression_needs_the_flag () =
  let (code, out) = run ["t"; "--expr"; "1 + 2"] in
  says out "Int";
  Alcotest.(check int) "an expression still checks" 0 code

let test_missing_file_hints_at_expr () =
  let (code, out) = run_all ["t"; "1 + 2"] in
  says out "no such file";
  says out "wand t --expr";
  Alcotest.(check int) "and fails" 1 code

let test_a_missing_wand_file_gets_no_hint () =
  (* A name that is spelled like a file is a file that is not there. Offering
     `--expr` on a typo is noise. *)
  let (_, out) = run_all ["t"; "no-such-thing.wand"] in
  says out "no such file";
  if Lint.contains out "--expr" then
    Alcotest.failf "a .wand name should not be offered --expr:\n%s" out

(* `-e` and `--expr` mean the same thing in both places they appear. The
   short spelling used to work at the top level and not in `wand t`, where
   `wand t -e "1 + 2"` took `-e` for the file name and reported too many
   arguments -- a flag this command does not have, described as an argument
   count. *)
let test_short_and_long_agree () =
  let (c1, o1) = run_all ["t"; "-e"; "1 + 2"] in
  let (c2, o2) = run_all ["t"; "--expr"; "1 + 2"] in
  Alcotest.(check string) "wand t -e and --expr agree" o2 o1;
  Alcotest.(check int) "and both succeed" c2 c1;
  let (c3, o3) = run_all ["-e"; "1 + 2"] in
  let (c4, o4) = run_all ["--expr"; "1 + 2"] in
  Alcotest.(check string) "wand -e and --expr agree" o4 o3;
  Alcotest.(check int) "and both succeed" c4 c3

let test_unknown_option_is_named () =
  (* Not taken for the file, which would report a missing path or a wrong
     argument count instead of the flag. *)
  let (code, out) = run_all ["t"; "--nope"; "examples/party.wand"] in
  says out "unknown option: --nope";
  Alcotest.(check int) "and fails" 1 code

let test_the_old_file_flag_is_named () =
  let (code, out) = run_all ["t"; "--file"; "examples/party.wand"] in
  says out "unknown option: --file";
  says out "wand t <file>";
  Alcotest.(check int) "and fails" 1 code

(* Every command answers `--help` with its own usage, and answers it before
   doing anything. `wand i --help` started a session and `wand lsp --help`
   started a server -- both hang rather than answer -- and `wand d --help`
   looked up a doc for `--help` and exited 0. *)
let test_every_command_answers_help () =
  List.iter (fun cmd ->
    List.iter (fun flag ->
      let (code, out) = run_all [cmd; flag] in
      if not (Lint.contains out "Usage: wand") then
        Alcotest.failf "wand %s %s did not print usage:\n%s" cmd flag out;
      Alcotest.(check int)
        (Printf.sprintf "wand %s %s exits 0" cmd flag) 0 code)
      ["--help"; "-h"])
    ["t"; "f"; "s"; "d"; "i"; "l"; "lsp"; "h"; "v"]

let test_help_is_not_taken_from_a_script () =
  (* A flag after a script belongs to the script, which is what makes
     `wand deploy.wand --help` the script's business and not wand's. *)
  let path = Filename.temp_file "wand_argv" ".wand" in
  Out_channel.with_open_text path (fun oc ->
    Out_channel.output_string oc
      "uses {IO}\n\nimport IO\nimport Proc\n\nIO.println \"%{Proc.args ()}\"\n");
  Fun.protect ~finally:(fun () -> Sys.remove path) (fun () ->
    let (_, out) = run_all [path; "--help"] in
    says out "[\"--help\"]")

let test_help_is_not_taken_from_an_expression () =
  (* `--help` after `-e` is the expression, not a request for usage. In wand
     `--` opens a comment, so this one checks as an empty program. *)
  let (code, out) = run_all ["t"; "-e"; "--help"] in
  if Lint.contains out "Usage: wand" then
    Alcotest.failf "the expression was read as a request for help:\n%s" out;
  Alcotest.(check int) "and checks clean" 0 code

(* A flag the command has, with its value missing, is not an unknown flag.
   `wand t -e` said "unknown option: -e", which names the wrong problem. *)
let test_a_flag_missing_its_value () =
  let (code, out) = run_all ["t"; "-e"] in
  says out "expected an expression after -e";
  Alcotest.(check int) "and fails" 1 code;
  let (_, out) = run_all ["t"; "--load"] in
  says out "expected a file after --load"

(* Every command that takes an argument used to read a stray flag as one:
   `wand f --nope` looked for a file, `wand d --nope` for a name, and
   `wand d --nope` reported no documentation and exited 0. *)
let test_every_command_names_an_unknown_option () =
  List.iter (fun cmd ->
    let (code, out) = run_all [cmd; "--nope"] in
    says out "unknown option: --nope";
    Alcotest.(check int) (cmd ^ " fails") 1 code)
    ["t"; "f"; "s"; "d"]

(* `--fix` rewrites a file, so it says whether it did. Printing nothing and
   exiting 0 reads exactly like a file that was fixed. *)
let test_fix_says_when_it_changed_nothing () =
  let path = Filename.temp_file "wand_clean" ".wand" in
  Out_channel.with_open_text path (fun oc ->
    Out_channel.output_string oc "let x = 1\n");
  Fun.protect ~finally:(fun () -> Sys.remove path) (fun () ->
    let (code, out) = run_all ["t"; "--fix"; path] in
    says out "nothing to fix";
    Alcotest.(check int) "and succeeds" 0 code)

let test_eval_is_a_top_level_flag () =
  let (code, out) = run ["-e"; "1 + 2"] in
  says out "3";
  Alcotest.(check int) "evaluates" 0 code

let test_eval_subcommand_hints () =
  let (code, out) = run_all ["e"; "1 + 2"] in
  says out "no such file: e";
  says out "wand -e";
  Alcotest.(check int) "and fails" 1 code

let test_mode_flags_refuse_an_expression () =
  (* Accepting `--dry-run` and ignoring it would run for real, which is the
     one mistake that flag exists to prevent. *)
  let (code, out) = run_all ["--dry-run"; "-e"; "1 + 2"] in
  says out "applies to a script";
  Alcotest.(check int) "and fails" 1 code

let () =
  Alcotest.run "json diagnostics" [
    "findings", [
      Alcotest.test_case "manifest insert_line"  `Quick test_manifest_fix;
      Alcotest.test_case "manifest replace_line" `Quick test_uses1_replace_line;
      Alcotest.test_case "strict severity"       `Quick test_strict_severity;
      Alcotest.test_case "file field"            `Quick test_file_field;
      Alcotest.test_case "item range"            `Quick test_finding_range;
      Alcotest.test_case "declaration positions"  `Quick
        test_declaration_error_positions;
    ];
    "file or expression", [
      Alcotest.test_case "a file is the default"      `Quick test_file_is_the_default;
      Alcotest.test_case "a path is not a literal"    `Quick
        test_a_path_is_not_typechecked_as_a_literal;
      Alcotest.test_case "--expr checks an expression" `Quick test_expression_needs_the_flag;
      Alcotest.test_case "missing file hints --expr"  `Quick test_missing_file_hints_at_expr;
      Alcotest.test_case "a .wand name gets no hint"  `Quick
        test_a_missing_wand_file_gets_no_hint;
      Alcotest.test_case "-e and --expr agree"        `Quick test_short_and_long_agree;
      Alcotest.test_case "unknown option is named"    `Quick test_unknown_option_is_named;
      Alcotest.test_case "--file is named"            `Quick test_the_old_file_flag_is_named;
      Alcotest.test_case "every command has --help"   `Quick test_every_command_answers_help;
      Alcotest.test_case "a script keeps --help"      `Quick test_help_is_not_taken_from_a_script;
      Alcotest.test_case "-e keeps --help"            `Quick
        test_help_is_not_taken_from_an_expression;
      Alcotest.test_case "a flag missing its value"   `Quick test_a_flag_missing_its_value;
      Alcotest.test_case "unknown option, every cmd"  `Quick
        test_every_command_names_an_unknown_option;
      Alcotest.test_case "--fix says it did nothing"  `Quick
        test_fix_says_when_it_changed_nothing;
      Alcotest.test_case "-e evaluates"               `Quick test_eval_is_a_top_level_flag;
      Alcotest.test_case "`wand e` hints at -e"       `Quick test_eval_subcommand_hints;
      Alcotest.test_case "--dry-run refuses -e"       `Quick
        test_mode_flags_refuse_an_expression;
    ];
    "imports", [
      Alcotest.test_case "unknown module"      `Quick test_import_unknown_module;
      Alcotest.test_case "unknown symbol"      `Quick test_import_unknown_symbol;
      Alcotest.test_case "unknown constructor" `Quick test_import_unknown_constructor;
      Alcotest.test_case "bare path"           `Quick test_import_bare_path;
      Alcotest.test_case "bad pattern"         `Quick test_import_bad_pattern;
      Alcotest.test_case "missing file"        `Quick test_import_missing_file;
      Alcotest.test_case "not the first line"  `Quick
        test_import_error_is_not_the_first_line;
    ];
    "holes", [
      Alcotest.test_case "shape" `Quick test_hole_shape;
    ];
    "queries", [
      Alcotest.test_case "doc"         `Quick test_doc_json;
      Alcotest.test_case "doc absent"  `Quick test_doc_json_absent;
      Alcotest.test_case "scope"       `Quick test_scope_json;
      Alcotest.test_case "scope empty" `Quick test_scope_json_empty;
      Alcotest.test_case "module"      `Quick test_module_json;
    ];
    "test runs", [
      Alcotest.test_case "run"   `Quick test_run_json;
      Alcotest.test_case "empty" `Quick test_run_json_empty;
    ];
    "exit codes", [
      Alcotest.test_case "--strict --json"       `Quick test_strict_json_exit_code;
      Alcotest.test_case "--strict"              `Quick test_strict_text_exit_code;
      Alcotest.test_case "no --strict"           `Quick test_json_without_strict_is_a_warning;
      Alcotest.test_case "an expression"         `Quick test_strict_json_expression;
      Alcotest.test_case "a clean file"          `Quick test_strict_json_clean_file;
    ];
    "errors", [
      Alcotest.test_case "type"            `Quick test_type_error;
      Alcotest.test_case "no position"     `Quick test_error_without_position;
      Alcotest.test_case "lex position"    `Quick test_lex_error_position;
      Alcotest.test_case "parse position"  `Quick test_parse_error_position;
      Alcotest.test_case "type position"   `Quick test_type_error_position;
      Alcotest.test_case "type range"      `Quick test_type_error_range;
      Alcotest.test_case "parse + fix"     `Quick test_parse_error_drift_fix;
    ];
  ]
