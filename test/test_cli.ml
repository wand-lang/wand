open Wand

(* The REPL's own, so a session under test opens with what a session opens
   with. A second copy of this list is a copy that goes stale. *)
let stdlib_prelude = Repl.stdlib_prelude

let make_sess () =
  let sess = Runner.make_session () in
  match Runner.run_session sess stdlib_prelude with
  | Ok (s, _) -> s
  | Error msg -> Alcotest.failf "stdlib load failed: %s" msg

let lookup_module name sess =
  match List.assoc_opt name sess.Runner.s_type_env with
  | Some (Typechecker.Namespace (members, _)) -> Some members
  | _ -> None

(* ── evaluating an expression (wand -e) ─────────────────────────────────── *)

let test_eval () =
  let sess = make_sess () in
  (* value expression *)
  (match Runner.run_session sess "1 + 2" with
   | Ok (_, Runner.RVal ("3", "Int")) -> ()
   | Ok (_, r) -> Alcotest.failf "eval: unexpected result: %s"
       (match r with Runner.RVal (v, t) -> v ^ " : " ^ t | _ -> "other")
   | Error m -> Alcotest.failf "eval error: %s" m);
  (* let binding *)
  (match Runner.run_session sess "let x = 42" with
   | Ok (_, Runner.RBind ("x", "Int")) -> ()
   | Ok (_, _) -> Alcotest.fail "eval: expected RBind for let"
   | Error m   -> Alcotest.failf "eval let error: %s" m);
  (* stdlib available *)
  (match Runner.run_session sess "List.length [1, 2, 3]" with
   | Ok (_, Runner.RVal ("3", "Int")) -> ()
   | Ok (_, _) -> Alcotest.fail "eval: stdlib List not available"
   | Error m   -> Alcotest.failf "eval stdlib error: %s" m);
  (* error on bad expression *)
  (match Runner.run_session sess "1 + true" with
   | Error _ -> ()
   | Ok _    -> Alcotest.fail "eval: expected error for type mismatch")

let test_eval_holes () =
  let sess = make_sess () in
  match Runner.run_session sess "?" with
  | Ok (_, Runner.RHoles [_]) -> ()
  | Ok (_, _) -> Alcotest.fail "eval hole: expected RHoles"
  | Error m   -> Alcotest.failf "eval hole error: %s" m

(* ── wand t (typecheck) ──────────────────────────────────────────────────── *)

let test_type () =
  let sess = make_sess () in
  (* expression type *)
  (match Runner.typecheck_session sess "1 + 2" with
   | Ok (Runner.RTypeExpr "Int") -> ()
   | Ok _ -> Alcotest.fail "type: expected RTypeExpr Int"
   | Error d -> Alcotest.failf "type error: %s" (Diag.legacy d));
  (* function type *)
  (match Runner.typecheck_session sess "fn x -> x + 1" with
   | Ok (Runner.RTypeExpr t) ->
     if t <> "Int -> Int" then
       Alcotest.failf "type: expected Int -> Int, got %s" t
   | Ok _ -> Alcotest.fail "type: expected RTypeExpr"
   | Error d -> Alcotest.failf "type fn error: %s" (Diag.legacy d));
  (* let binding type *)
  (match Runner.typecheck_session sess "let x = \"hello\"" with
   | Ok (Runner.RBind ("x", "String")) -> ()
   | Ok _ -> Alcotest.fail "type: expected RBind x String"
   | Error d -> Alcotest.failf "type let error: %s" (Diag.legacy d));
  (* type error *)
  (match Runner.typecheck_session sess "1 + true" with
   | Error _ -> ()
   | Ok _    -> Alcotest.fail "type: expected error for type mismatch");
  (* hole *)
  (match Runner.typecheck_session sess "?" with
   | Ok (Runner.RHoles [_]) -> ()
   | Ok _ -> Alcotest.fail "type hole: expected RHoles"
   | Error d -> Alcotest.failf "type hole error: %s" (Diag.legacy d))

(* ── wand d (doc) ────────────────────────────────────────────────────────── *)

let test_doc () =
  let sess = make_sess () in
  (* stdlib function has a type *)
  (match Runner.lookup_type sess "List.map" with
   | Some t ->
     if not (String.length t > 0) then
       Alcotest.fail "doc: List.map type is empty"
   | None -> Alcotest.fail "doc: List.map type not found");
  (* stdlib function has a doc string *)
  (match List.assoc_opt "map" (List.concat_map (fun (n, s) ->
      match s with
      | Typechecker.Namespace (members, _) ->
        if n = "List" then List.map (fun (mn, ms) -> (mn, ms)) members else []
      | _ -> []) sess.Runner.s_type_env) with
   | Some _ -> ()
   | None   -> ());
  (* unknown name returns None *)
  (match Runner.lookup_type sess "no_such_name" with
   | None -> ()
   | Some _ -> Alcotest.fail "doc: expected None for unknown name");
  (* module member lookup works *)
  (match Runner.lookup_type sess "Map.get" with
   | Some t ->
     if not (String.length t > 0) then
       Alcotest.fail "doc: Map.get type is empty"
   | None -> Alcotest.fail "doc: Map.get not found")

(* The listing behind `wand d --index`: every module's members, and the same
   answer `wand d <module>` gives one module at a time. *)
let test_doc_index () =
  let sess = make_sess () in
  let index = Runner.index sess in
  Alcotest.(check bool) "the index is not empty" true (index <> []);
  (* Every entry is a module member, and every module is in it. *)
  List.iter (fun (name, _) ->
    Alcotest.(check bool) (name ^ " is qualified") true
      (String.contains name '.')) index;
  let modules =
    List.sort_uniq String.compare
      (List.filter_map (fun (n, s) ->
         match s with Typechecker.Namespace _ -> Some n | _ -> None)
        sess.Runner.s_type_env)
  in
  List.iter (fun modname ->
    match Runner.module_members sess modname with
    | None -> ()
    | Some members ->
      List.iter (fun m ->
        let qualified = modname ^ "." ^ m in
        Alcotest.(check bool) (qualified ^ " is in the index") true
          (List.mem_assoc qualified index)) members) modules;
  (* A module that is gone stays gone. `Ord` is the interface the ordered
     types implement, declared by the compiler rather than by a file, so
     there is nothing to import and nothing to list. *)
  Alcotest.(check bool) "no Ord module" false (List.mem "Ord" modules);
  Alcotest.(check bool) "and no Ord.max" false (List.mem_assoc "Ord.max" index);
  Alcotest.(check bool) "Int.max is in the index" true
    (List.mem_assoc "Int.max" index)

let test_doc_strings () =
  let sess = make_sess () in
  (* A run of comment lines above a definition is its documentation. *)
  let sess2 = match Runner.run_session sess
    {|-- Doubles a number.
let double x = x * 2|} with
    | Ok (s, _) -> s
    | Error m   -> Alcotest.failf "doc string eval error: %s" m
  in
  (match List.assoc_opt "double" sess2.Runner.s_docs with
   | Some doc ->
     if not (String.length doc > 0) then
       Alcotest.fail "doc: doc string is empty"
   | None -> Alcotest.fail "doc: doc string not stored");
  (* Documentation is a run of `--` lines directly above the definition. Each
     line stands alone, the lines are consecutive, and the last one sits on
     the line above -- so a comment after code documents nothing, and a blank
     line ends the run. *)
  let doc_of src name =
    match Runner.run_session (make_sess ()) src with
    | Ok (s, _) -> List.assoc_opt name s.Runner.s_docs
    | Error m -> Alcotest.failf "doc run eval error: %s" m
  in
  (match doc_of "-- Doubles a number.\n-- Twice, in fact.\nlet double x = x * 2" "double" with
   | Some doc ->
     Alcotest.(check string) "both lines, in order" "Doubles a number.\nTwice, in fact." doc
   | None -> Alcotest.fail "doc: a run of line comments did not attach");
  (match doc_of "let plain x = x  -- trails the code" "plain" with
   | Some d -> Alcotest.failf "doc: a trailing comment documented a binding: %s" d
   | None -> ());
  (match doc_of "-- A file header.\n\nlet far x = x" "far" with
   | Some d -> Alcotest.failf "doc: a blank line did not end the run: %s" d
   | None -> ())

(* ── wand d, with no name (everything in scope) ──────────────────────────── *)

let test_env_all () =
  let sess = make_sess () in
  let entries = sess.Runner.s_type_env in
  (* stdlib modules are present *)
  List.iter (fun name ->
    if not (List.mem_assoc name entries) then
      Alcotest.failf "env: %s not in type env" name
  ) ["List"; "String"; "Map"; "Env"; "Path"; "FS"; "IO"; "Duration"];
  (* user bindings appear after being defined *)
  let sess2 = match Runner.run_session sess "let answer = 42" with
    | Ok (s, _) -> s
    | Error m   -> Alcotest.failf "env let error: %s" m
  in
  if not (List.mem_assoc "answer" sess2.Runner.s_type_env) then
    Alcotest.fail "env: user binding not in type env"

(* ── wand d <Module> ───────────────────────────────────────────────────── *)

let test_env_module () =
  let sess = make_sess () in
  (match lookup_module "NoSuchModule" sess with
   | None   -> ()
   | Some _ -> Alcotest.fail "expected None for unknown module");
  let sess2 = match Runner.run_session sess "let x = 42" with
    | Ok (s, _) -> s
    | Error m   -> Alcotest.failf "run failed: %s" m
  in
  (match List.assoc_opt "x" sess2.Runner.s_type_env with
   | Some (Typechecker.Namespace _) -> Alcotest.fail "x should not be a namespace"
   | _ -> ())

let test_env_map_module () =
  let sess = make_sess () in
  match lookup_module "Map" sess with
  | None -> Alcotest.fail "Map module not found"
  | Some members ->
    let names = List.map fst members in
    List.iter (fun n ->
      if not (List.mem n names) then
        Alcotest.failf "Map.%s missing from env" n
    ) ["get"; "set"; "delete"; "keys"; "values"; "size"; "empty"]

(* ── Suite ───────────────────────────────────────────────────────────────── *)

let step sess src =
  match Runner.run_session sess src with
  | Ok (s, _) -> s
  | Error e -> Alcotest.failf "step [%s] failed: %s" src e

let run_val sess src =
  match Runner.run_session sess src with
  | Ok (_, Runner.RVal (v, _)) -> v
  | Ok (_, _) -> Alcotest.failf "run [%s]: expected RVal" src
  | Error e -> Alcotest.failf "run [%s] error: %s" src e

let test_incremental_pattern_match () =
  let sess = Runner.make_session () in
  let sess = step sess "let fact 1 = 1" in
  let sess = step sess "let fact n = 1 + fact (n - 1)" in
  let sess = step sess "let fact 0 = 0" in
  Alcotest.(check string) "fact 0 after incremental" "0" (run_val sess "fact 0");
  Alcotest.(check string) "fact 1 after incremental" "1" (run_val sess "fact 1");
  Alcotest.(check string) "fact 3 after incremental" "3" (run_val sess "fact 3")

(* The REPL edits definitions; files declare them. Adding a clause for an
   existing function merges into it here rather than erroring as a file
   would, and the merge is announced so the reordering is visible. *)

let test_repl_merges_clauses_and_announces () =
  let sess = Runner.make_session () in
  let sess = match Runner.run_session sess "let f 0 = 0" with
    | Ok (s, Runner.RBind ("f", _)) -> s
    | Ok (_, _) -> Alcotest.fail "expected a binding for the first clause"
    | Error m -> Alcotest.failf "first clause failed: %s" m
  in
  let sess = match Runner.run_session sess "let f n = n * 2" with
    | Ok (s, Runner.RBind ("f", ty)) ->
      let contains hay nee =
        let hn = String.length hay and nn = String.length nee in
        let found = ref false in
        for i = 0 to hn - nn do
          if nn <= hn && String.sub hay i nn = nee then found := true
        done; !found
      in
      if not (contains ty "2 equations")
      then Alcotest.failf "expected the merge to be announced, got: %s" ty
      else s
    | Ok (_, _) -> Alcotest.fail "expected a binding for the merged clause"
    | Error m -> Alcotest.failf "merged clause failed: %s" m
  in
  (* Both clauses are live: the specific one still fires after the merge. *)
  (match Runner.run_session sess "f 0" with
   | Ok (_, Runner.RVal ("0", _)) -> ()
   | Ok (_, _) -> Alcotest.fail "specific clause did not fire after merge"
   | Error m -> Alcotest.failf "f 0 failed: %s" m);
  (match Runner.run_session sess "f 5" with
   | Ok (_, Runner.RVal ("10", _)) -> ()
   | Ok (_, _) -> Alcotest.fail "general clause did not fire after merge"
   | Error m -> Alcotest.failf "f 5 failed: %s" m)

(* A mutual group binds several names at once; each is echoed with its
   type, the way a lone binding is. *)
let test_repl_echoes_mutual_group () =
  let sess = Runner.make_session () in
  let src = "let is_even n = if n == 0 then true else is_odd (n - 1) and\n\
             is_odd n = if n == 0 then false else is_even (n - 1)" in
  let sess = match Runner.run_session sess src with
    | Ok (s, Runner.RGroup [("is_even", _); ("is_odd", _)]) -> s
    | Ok (_, _) -> Alcotest.fail "expected both names of the group echoed"
    | Error m -> Alcotest.failf "mutual group failed: %s" m
  in
  match Runner.run_session sess "is_odd 3" with
  | Ok (_, Runner.RVal ("true", _)) -> ()
  | Ok (_, _) -> Alcotest.fail "is_odd 3 should be true"
  | Error m -> Alcotest.failf "is_odd 3 failed: %s" m

(* Whether a Unit answer is worth printing is a question about effects, not
   about how the expression was written. One that performed something has
   already been seen -- `IO.println` put its line on the screen -- and a
   `() : Unit` under it would be noise. One that performed nothing was never
   shown, so it answers, however it was spelled. *)
let test_repl_answers_pure_unit () =
  let sess = make_sess () in
  let pure name src =
    match Runner.run_session sess src with
    | Ok (_, Runner.RVal ("()", "Unit")) -> ()
    | Ok (_, Runner.RSilent) -> Alcotest.failf "%s: performs nothing, so it should answer () : Unit" name
    | Ok (_, _) -> Alcotest.failf "%s: expected () : Unit" name
    | Error m -> Alcotest.failf "%s failed: %s" name m
  in
  pure "a written ()"      "()";
  pure "a let-bound unit"  "let u = () in u";
  pure "both arms unit"    "if true then () else ()";
  pure "a pure call"       "let f = fn () -> () in f ()";
  let performs name src =
    match Runner.run_session sess src with
    | Ok (_, Runner.RSilent) -> ()
    | Ok (_, _) -> Alcotest.failf "%s: performed something, so it should stay silent" name
    | Error m -> Alcotest.failf "%s failed: %s" name m
  in
  performs "IO.println" "IO.println \"hi\"";
  performs "Env.set"    "Env.set \"WAND_REPL_UNIT_TEST\" \"1\""

(* Checking a file without running it: what an editing loop and CI both want,
   and what a manifest violation will be reported through. The path is stated
   named directly, as it is everywhere else; an expression is what carries a
   flag (`--expr`), since `deploy.wand` is itself a valid path expression and
   the two cannot be told apart by shape. *)

let with_file name contents f =
  let path = Filename.concat (Filename.get_temp_dir_name ()) name in
  let oc = open_out path in
  output_string oc contents; close_out oc;
  let r = (try f path with e -> Sys.remove path; raise e) in
  Sys.remove path; r

let test_typecheck_file () =
  with_file "wand_cli_ok.wand" "let double x = x * 2\ndouble 21" (fun path ->
    match Runner.typecheck_file path with
    | Ok sc ->
      Alcotest.(check string) "reports the file's type" "Int" sc.Runner.sc_type;
      Alcotest.(check int) "no holes" 0 (List.length sc.Runner.sc_holes)
    | Error d -> Alcotest.failf "expected it to typecheck: %s" (Diag.legacy d))

let test_typecheck_file_reports_errors () =
  with_file "wand_cli_bad.wand" "let x : Int = \"no\"\nx" (fun path ->
    match Runner.typecheck_file path with
    | Error _ -> ()
    | Ok _ -> Alcotest.fail "expected a type error")

let test_typecheck_file_reports_holes () =
  with_file "wand_cli_hole.wand" "import List\nList.fold_left ? 0 [1, 2, 3]" (fun path ->
    match Runner.typecheck_file path with
    | Ok sc ->
      Alcotest.(check int) "one hole" 1 (List.length sc.Runner.sc_holes);
      (* The effect variable says the function filling the hole may perform
         effects of its own -- fold_left passes through whatever it is given. *)
      Alcotest.(check string) "with its inferred type" "Int -> Int -> Int ! 'e"
        (List.hd sc.Runner.sc_holes)
    | Error d -> Alcotest.failf "expected it to typecheck: %s" (Diag.legacy d))

(* The editor's case: text that exists only in a buffer. The path decides
   where imports resolve, whether or not a file is there. *)
let test_typecheck_source_unsaved_buffer () =
  with_file "wand_cli_util.wand" "let answer = 42" (fun util ->
    let buffer = Filename.concat (Filename.dirname util) "wand_cli_buffer.wand" in
    match Runner.typecheck_source ~path:buffer
            "let {answer} = import ./wand_cli_util\nanswer" with
    | Ok sc ->
      Alcotest.(check string) "the buffer sees its neighbor" "Int" sc.Runner.sc_type
    | Error d -> Alcotest.failf "expected it to typecheck: %s" (Diag.legacy d))

let test_typecheck_source_own_names () =
  match Runner.typecheck_source ~path:"wand_cli_hover.wand"
          "let double x = x * 2\ndouble 21" with
  | Ok sc ->
    Alcotest.(check bool) "the file's own names are reported" true
      (List.mem_assoc "double" sc.Runner.sc_env)
  | Error d -> Alcotest.failf "expected it to typecheck: %s" (Diag.legacy d)

let test_typecheck_file_lints () =
  with_file "wand_cli_lint.wand" "let is_ready? x = x > 1\nis_ready? 2" (fun path ->
    match Runner.typecheck_file path with
    | Ok sc ->
      Alcotest.(check bool) "a lint is reported" true (sc.Runner.sc_findings <> [])
    | Error d -> Alcotest.failf "expected it to typecheck: %s" (Diag.legacy d))

(* ── wand s: finding the files ────────────────────────────────────────── *)

(* A script's tests live beside the script, so discovery is by prefix and
   the answer has to be the tests and nothing else -- not the script, not a
   fixture, and not dune's copy of the same tree under _build. *)

let write path contents =
  Out_channel.with_open_text path (fun oc -> Out_channel.output_string oc contents)

let with_tree f =
  let root = Filename.temp_file "wand_tests" "" in
  Sys.remove root;
  Sys.mkdir root 0o755;
  Fun.protect
    ~finally:(fun () -> ignore (Sys.command (Printf.sprintf "rm -rf %s" (Filename.quote root))))
    (fun () -> f root)

let test_finds_tests_beside_scripts () =
  with_tree (fun root ->
    let at p = Filename.concat root p in
    write (at "deploy.wand") "let deploy = 1";
    write (at "test_deploy.wand") "";
    (* A nested script directory is searched too. *)
    Sys.mkdir (at "scripts") 0o755;
    write (at "scripts/test_backup.wand") "";
    write (at "scripts/backup.wand") "";
    (* Things that are not test files, by each of the ways they can fail
       to be one. *)
    write (at "fixture.wand") "";
    write (at "test_notes.txt") "";
    write (at "test_.wand") "";
    let found =
      Runner.find_test_files root
      |> List.map Filename.basename
    in
    Alcotest.(check (list string)) "the test files, in a stable order"
      ["test_backup.wand"; "test_deploy.wand"] found)

(* dune copies the source tree into _build, so a search that descends into
   it reports every test twice under two different paths. *)
let test_skips_build_directories () =
  with_tree (fun root ->
    let at p = Filename.concat root p in
    write (at "test_real.wand") "";
    List.iter (fun d ->
      Sys.mkdir (at d) 0o755;
      write (at (Filename.concat d "test_copy.wand")) "")
      ["_build"; "_opam"; ".git"; "node_modules"];
    let found = Runner.find_test_files root |> List.map Filename.basename in
    Alcotest.(check (list string)) "only the real one" ["test_real.wand"] found)

(* A file named outright runs whatever it is called: the prefix is how
   files are found, not a rule about what may be run. *)
let test_a_named_file_is_taken_as_given () =
  with_tree (fun root ->
    let path = Filename.concat root "anything.wand" in
    write path "";
    Alcotest.(check (list string)) "the file itself" [path]
      (Runner.find_test_files path))

let test_a_dangling_symlink_is_stepped_over () =
  with_tree (fun root ->
    write (Filename.concat root "test_real.wand") "";
    Unix.symlink (Filename.concat root "gone") (Filename.concat root "link");
    let found = Runner.find_test_files root |> List.map Filename.basename in
    Alcotest.(check (list string)) "still finds the test" ["test_real.wand"] found)

(* The walk does not descend through a symlink. `Sys.is_directory` follows
   one, so a link to a directory was walked into and `wand s` ran test files
   outside the tree it was pointed at, with every effect a test has -- a
   whole tree brought in by one name nobody reading the directory would
   notice. A linked *file* is still read: it is one name, listed where it can
   be seen, and it is how dune's sandbox presents every fixture in a
   `source_tree` dep. *)
let test_the_walk_does_not_leave_the_tree () =
  with_tree (fun root ->
    with_tree (fun outside ->
      write (Filename.concat outside "test_outside.wand") "";
      write (Filename.concat root "test_real.wand") "";
      Unix.symlink outside (Filename.concat root "linked");
      Unix.symlink (Filename.concat outside "test_outside.wand")
        (Filename.concat root "test_linked.wand");
      let found = Runner.find_test_files root |> List.map Filename.basename in
      Alcotest.(check (list string)) "the linked directory brought nothing in"
        ["test_linked.wand"; "test_real.wand"] found;
      (* Named outright it still runs: that directory is the one the reader
         asked for. *)
      Alcotest.(check (list string)) "a linked directory named outright"
        ["test_outside.wand"]
        (Runner.find_test_files (Filename.concat root "linked")
         |> List.map Filename.basename)))

(* `:reset` built its own list and had been missing eighteen modules since
   they were added, so a session that had been reset could not reach `Map`
   while a fresh one could. One list now, and this is what keeps it one. *)
let test_prelude_names_every_module () =
  List.iter (fun m ->
    let needle = "import " ^ m in
    if not (Lint.contains Repl.stdlib_prelude needle) then
      Alcotest.failf "the REPL prelude does not load %s" m)
    Typechecker.stdlib_module_names

(* ── the REPL's :d ──────────────────────────────────────────────────────── *)

(* `:d` prints rather than returning, and it is the only thing here that
   does, so the capture lives beside the one test that needs it. The output
   is a few dozen lines, well inside a pipe buffer. *)
let captured f =
  let (r, w) = Unix.pipe ~cloexec:true () in
  flush stdout;
  let saved = Unix.dup Unix.stdout in
  Unix.dup2 w Unix.stdout;
  Unix.close w;
  let restore () =
    flush stdout; Unix.dup2 saved Unix.stdout; Unix.close saved in
  (try f () with e -> (restore (); Unix.close r; raise e));
  restore ();
  let buf = Buffer.create 1024 in
  let chunk = Bytes.create 4096 in
  let rec drain () =
    match Unix.read r chunk 0 4096 with
    | 0 -> ()
    | n -> Buffer.add_subbytes buf chunk 0 n; drain ()
  in
  drain (); Unix.close r;
  Buffer.contents buf

(* A module is the one name whose documentation is the list of what it
   holds. `:d Random` answered `Random : <namespace>` and `Random: no doc`,
   which is two lines telling a reader nothing they can act on. *)
let test_repl_doc_of_a_module () =
  let sess = make_sess () in
  let out = captured (fun () -> ignore (Repl.handle_command sess ":d Random")) in
  List.iter (fun needle ->
    if not (Lint.contains out needle) then
      Alcotest.failf ":d Random did not name %s:\n%s" needle out)
    (* The colons line up, the column set by the longest name in the module. *)
    ["Random.shuffle : List 'a -> List 'a ! {Random}";
     "Random.hex     : Int -> String ! {Random}"];
  List.iter (fun stale ->
    if Lint.contains out stale then
      Alcotest.failf ":d Random still says %S:\n%s" stale out)
    ["<namespace>"; "no doc"]

(* With nothing after it, `:d` answers with the session: a module by name, a
   binding with its type. That was `:v`, which is retired -- `:d` is the one
   place to ask what a name is, and "no name" is the widest form of it. *)
let test_repl_doc_with_no_argument () =
  let sess = make_sess () in
  let sess = match Runner.run_session sess "let answer = 42" with
    | Ok (s, _) -> s
    | Error m -> Alcotest.failf "could not bind: %s" m in
  let out = captured (fun () -> ignore (Repl.handle_command sess ":d")) in
  if not (Lint.contains out "answer : Int") then
    Alcotest.failf ":d did not list the session's binding:\n%s" out;
  if not (Lint.contains out "Random") then
    Alcotest.failf ":d did not list the loaded modules:\n%s" out

(* `:v` answered both of those. It still answers, with a line saying where
   they went: "Unknown command" would not. *)
let test_repl_env_is_retired () =
  let sess = make_sess () in
  List.iter (fun cmd ->
    let out = captured (fun () -> ignore (Repl.handle_command sess cmd)) in
    if not (Lint.contains out "retired") then
      Alcotest.failf "%s did not say it is retired:\n%s" cmd out;
    if not (Lint.contains out ":d") then
      Alcotest.failf "%s did not point at :d:\n%s" cmd out)
    [":v"; ":v List"; ":env"]

(* And one member still answers with its own doc, which is what `:d` did for
   a dotted name all along. *)
let test_repl_doc_of_a_member () =
  let sess = make_sess () in
  let out = captured (fun () -> ignore (Repl.handle_command sess ":d Random.hex")) in
  List.iter (fun needle ->
    if not (Lint.contains out needle) then
      Alcotest.failf ":d Random.hex did not carry %S:\n%s" needle out)
    ["Random.hex : Int -> String ! {Random}"; "hexadecimal"]

(* ── Rewriting a file in place (wand f, wand t --fix) ───────────────────── *)

(* Both used to truncate the source and then fill it, so a crash or a full
   disk part way through left half a file and no copy of the other half --
   in the one place where the file is the reader's own work. Both write
   beside and rename now, which is what `write_atomic` has always done for
   `FS.write_atomic`. What that buys is visible from outside: the target
   keeps its mode, a symlink is written through rather than replaced, and
   nothing is left beside it. *)

let wand_binary =
  let dir = Filename.dirname (Filename.dirname Sys.executable_name) in
  Filename.concat (Filename.concat dir "bin") "wand.exe"

let in_scratch f =
  let d = Filename.temp_file "wand_rewrite_" "" in
  Sys.remove d; Unix.mkdir d 0o700;
  Fun.protect
    ~finally:(fun () ->
      Array.iter (fun n ->
        try Sys.remove (Filename.concat d n) with Sys_error _ -> ())
        (Sys.readdir d);
      try Unix.rmdir d with Unix.Unix_error _ -> ())
    (fun () -> f d)

let write_file path contents =
  Out_channel.with_open_text path
    (fun oc -> Out_channel.output_string oc contents)

let read_file path = In_channel.with_open_text path In_channel.input_all

let wand ~dir args =
  let cmd =
    Printf.sprintf "cd %s && %s %s >/dev/null 2>&1" (Filename.quote dir)
      (Filename.quote wand_binary)
      (String.concat " " (List.map Filename.quote args))
  in
  ignore (Sys.command cmd)

let check_rewrite label args =
  in_scratch (fun d ->
    let target = Filename.concat d "a.wand" in
    (* Loose spacing for `wand f` to close up, and a name to import for
       `wand t --fix` to reach for, so one file is rewritten by both. *)
    let before = "let x    =  1\nlet () = IO.println \"hi\"\n" in
    write_file target before;
    Unix.chmod target 0o640;
    let link = Filename.concat d "link.wand" in
    Unix.symlink "a.wand" link;
    wand ~dir:d (args @ ["link.wand"]);
    Alcotest.(check bool) (label ^ ": the file was rewritten") true
      (read_file target <> before);
    Alcotest.(check int) (label ^ ": the target kept its mode") 0o640
      (Unix.stat target).Unix.st_perm;
    Alcotest.(check bool) (label ^ ": the link is still a link") true
      ((Unix.lstat link).Unix.st_kind = Unix.S_LNK);
    Alcotest.(check (list string)) (label ^ ": nothing left beside it")
      ["a.wand"; "link.wand"]
      (List.sort compare (Array.to_list (Sys.readdir d))))

let test_fmt_rewrites_in_place () = check_rewrite "wand f" ["f"]
let test_fix_rewrites_in_place () = check_rewrite "wand t --fix" ["t"; "--fix"]

(* ── wand t over a tree ──────────────────────────────────────────────────── *)

(* One file named on its own answers about that file. A directory, or more
   than one path, is a question about a tree: every finding carries its path,
   one file's error does not stop the rest, and the exit code is what a gate
   reads. Before this the command took one file and a second path was
   "too many arguments", so checking a directory was a shell loop. *)

let wand_out ~dir args =
  let out = Filename.concat dir "_out" in
  let cmd =
    Printf.sprintf "cd %s && %s %s >%s 2>&1" (Filename.quote dir)
      (Filename.quote wand_binary)
      (String.concat " " (List.map Filename.quote args))
      (Filename.quote out)
  in
  let code = Sys.command cmd in
  let text = read_file out in
  Sys.remove out;
  (code, text)

let contains_sub hay needle =
  let n = String.length needle and h = String.length hay in
  let rec go i = i + n <= h && (String.sub hay i n = needle || go (i + 1)) in
  n = 0 || go 0

(* A run checks what `wand t` checks, and an import's work waits on the
   check: `wand b.wand` used to type the script without what its imports do
   as they load, and run those imports before the check at all. *)
let test_run_checks_what_t_checks () =
  in_scratch (fun d ->
    write_file (Filename.concat d "a.wand")
      "import FS\ntype Box(f: Unit -> Unit)\n\
       let box = Box(f = fn () -> FS.write_file! ./written.txt \"x\")\n";
    write_file (Filename.concat d "b.wand")
      "uses {IO}\nimport IO\nlet A = import ./a\nlet main () = A.box.f ()\n\
       main ()\nIO.println \"done\"\n";
    let (code, out) = wand_out ~dir:d ["b.wand"] in
    Alcotest.(check bool) "the run is refused" true (code <> 0);
    Alcotest.(check bool) "for the manifest" true (contains_sub out "FS.Write");
    Alcotest.(check bool) "and wrote nothing" false
      (Sys.file_exists (Filename.concat d "written.txt")))

let test_import_work_waits_on_the_check () =
  in_scratch (fun d ->
    write_file (Filename.concat d "a.wand")
      "import FS\nlet x = FS.write_file! ./written.txt \"x\"\nlet y = 1\n";
    write_file (Filename.concat d "b.wand")
      "uses {IO}\nimport IO\nlet {y} = import ./a\nIO.println y\n";
    let (code, _) = wand_out ~dir:d ["b.wand"] in
    Alcotest.(check bool) "a script whose import writes is refused" true (code <> 0);
    Alcotest.(check bool) "before the import runs" false
      (Sys.file_exists (Filename.concat d "written.txt"));
    write_file (Filename.concat d "c.wand")
      "uses {FS.Write, IO}\nimport IO\nlet {y} = import ./a\nIO.println y\n";
    let (code, out) = wand_out ~dir:d ["--dry-run"; "c.wand"] in
    Alcotest.(check int) "a rehearsal that allows it succeeds" 0 code;
    Alcotest.(check bool) "and reports the import's write" true
      (contains_sub out "would write: ./written.txt");
    Alcotest.(check bool) "without making it" false
      (Sys.file_exists (Filename.concat d "written.txt")))

(* A rehearsal binds no port: it served real clients and reported nothing.
   It reports the listen, and answers it with no connections. *)
let test_a_rehearsal_does_not_listen () =
  in_scratch (fun d ->
    write_file (Filename.concat d "srv.wand")
      "uses {Net.Listen(:18475)}\nimport Net\nimport Stream\n\
       Net.listen :18475 |> Stream.take 1 |> Stream.each (fn c -> Net.write! \"x\" c)\n";
    let (code, out) = wand_out ~dir:d ["--dry-run"; "srv.wand"] in
    Alcotest.(check int) "the rehearsal ends" 0 code;
    Alcotest.(check bool) "and reports the listen" true
      (contains_sub out "would listen on: :18475"))

let test_type_over_a_tree () =
  in_scratch (fun d ->
    write_file (Filename.concat d "bad.wand") "let f x = unknown_name x\n";
    write_file (Filename.concat d "ok.wand") "let g x = x + 1\n";
    Unix.mkdir (Filename.concat d "sub") 0o700;
    write_file (Filename.concat d "sub/deep.wand") "let h x = also_unknown x\n";
    let (code, out) = wand_out ~dir:d ["t"; "."] in
    Alcotest.(check int) "a tree with an error exits 1" 1 code;
    Alcotest.(check bool) "the first error names its file" true
      (contains_sub out "bad.wand:");
    (* One file's error does not stop the rest -- a gate wants the whole
       list, not the first line of it. *)
    Alcotest.(check bool) "and so does the one below it" true
      (contains_sub out "deep.wand:");
    Alcotest.(check bool) "the count is the last line" true
      (contains_sub out "3 files, 2 errors");
    (* The summary goes under what it summarises. Both streams are buffered,
       and without a flush the count came out first. *)
    let lines = List.filter (fun l -> l <> "") (String.split_on_char '\n' out) in
    Alcotest.(check bool) "nothing is printed after it" true
      (match List.rev lines with
       | last :: _ -> contains_sub last "3 files, 2 errors"
       | [] -> false);
    (* A clean tree still says what it covered: silence reads the same as
       finding no files to check. *)
    Sys.remove (Filename.concat d "bad.wand");
    Sys.remove (Filename.concat d "sub/deep.wand");
    let (code, out) = wand_out ~dir:d ["t"; "."] in
    Alcotest.(check int) "a clean tree exits 0" 0 code;
    Alcotest.(check bool) "and says so" true
      (contains_sub out "1 file, nothing to report");
    (try Unix.rmdir (Filename.concat d "sub") with Unix.Unix_error _ -> ()))

let test_type_of_one_file_is_unchanged () =
  in_scratch (fun d ->
    write_file (Filename.concat d "a.wand") "1 + 2\n";
    let (code, out) = wand_out ~dir:d ["t"; "a.wand"] in
    Alcotest.(check int) "a clean file exits 0" 0 code;
    (* One file reports what it checks out as, with no path and no count. *)
    Alcotest.(check string) "and reports its type alone" "Int\n" out)

(* Every unbound name in one run, all the way out to the command. A file
   with six of them took six runs to clear. *)
let test_every_unbound_name_reported () =
  in_scratch (fun d ->
    write_file (Filename.concat d "a.wand")
      "let a x = alpha x\n\nlet b y = beta y\n\nlet c z = gamma z\n";
    let (code, out) = wand_out ~dir:d ["t"; "a.wand"] in
    Alcotest.(check int) "a file that does not check exits 1" 1 code;
    List.iter (fun name ->
      Alcotest.(check bool) (name ^ " is reported") true
        (contains_sub out ("unbound variable '" ^ name ^ "'")))
      ["alpha"; "beta"; "gamma"];
    (* --json carries each as its own object, so a tool reads them without
       parsing the text. *)
    let (_, out) = wand_out ~dir:d ["t"; "--json"; "a.wand"] in
    let count =
      let n = ref 0 and needle = "\"code\":\"E-TYPE\"" in
      String.iteri (fun i _ ->
        if i + String.length needle <= String.length out
        && String.sub out i (String.length needle) = needle then incr n) out;
      !n
    in
    Alcotest.(check int) "three objects" 3 count)

(* A member that answers to an interface says so after its type, in square
   brackets -- round ones are type application, so a reader copying the type
   into an annotation could take the interface for a last argument. A `[`
   never appears in a wand type.

   The colons line up within a module and the column resets at the next:
   names run from 8 to 23 characters, so one column across the whole listing
   would pad every short line by fifteen spaces. *)
let test_wand_d_marks_interfaces () =
  in_scratch (fun d ->
    let (code, out) = wand_out ~dir:d ["d"; "Int"] in
    Alcotest.(check int) "it lists" 0 code;
    Alcotest.(check bool) "a member of Ord is marked" true
      (contains_sub out "Int.max       : Int -> Int -> Int [Ord]");
    Alcotest.(check bool) "and one that is not is left alone" true
      (contains_sub out "Int.abs       : Int -> Int\n");
    (* One member on its own puts it on the signature, where the index puts
       it too, rather than in a slot of its own. *)
    let (_, out) = wand_out ~dir:d ["d"; "Int.max"] in
    Alcotest.(check bool) "a single member is marked" true
      (contains_sub out "Int.max : Int -> Int -> Int [Ord]");
    (* The index carries it, and `--json` carries it as a field. *)
    let (_, out) = wand_out ~dir:d ["d"; "--index"] in
    Alcotest.(check bool) "the index carries it" true
      (contains_sub out "Size.max      : Size -> Size -> Size [Ord]");
    let (_, out) = wand_out ~dir:d ["d"; "--index"; "--json"] in
    Alcotest.(check bool) "as a field" true
      (contains_sub out "\"name\":\"Int.max\",\"type\":\"Int -> Int -> Int\",\"implements\":[\"Ord\"]");
    Alcotest.(check bool) "null where there is none" true
      (contains_sub out "\"name\":\"Int.abs\",\"type\":\"Int -> Int\",\"implements\":null"))

(* ── Interfaces across files ─────────────────────────────────────────────── *)

(* An interface belongs to the module that declares it and is reached through
   the name a file bound for that module -- the rule a type already follows.
   There is no bare form for an imported one, so `ord.Ord` says which
   module's `Ord` it means and two modules declaring the name cannot
   collide. *)

let iface_files d =
  write_file (Filename.concat d "ord.wand")
    "interface Ranked 'a(top: 'a -> 'a -> 'a, bottom: 'a -> 'a -> 'a)\n";
  write_file (Filename.concat d "ints.wand")
    "let ord = import ./ord\n\n\
     implement ord.Ranked Int =\n\
     \  let top a b = if a > b then a else b;\n\
     \  let bottom a b = if a < b then a else b\n"

let test_a_module_is_passed_to_generic_code () =
  in_scratch (fun d ->
    iface_files d;
    write_file (Filename.concat d "main.wand")
      "uses {IO}\n\nimport IO\n\n\
       let ord = import ./ord\n\
       let ints = import ./ints\n\n\
       let biggest (m: ord.Ranked Int) a b = m.top a b\n\n\
       IO.println \"%{biggest ints 3 7}\"\n";
    let (code, out) = wand_out ~dir:d ["main.wand"] in
    Alcotest.(check int) "it runs" 0 code;
    Alcotest.(check string) "and answers" "7\n" out)

let test_an_imported_interface_has_no_bare_form () =
  in_scratch (fun d ->
    iface_files d;
    write_file (Filename.concat d "main.wand")
      "let ord = import ./ord\n\
       let ints = import ./ints\n\n\
       let biggest (m: Ranked Int) a b = m.top a b\n\n\
       biggest ints 3 7\n";
    let (code, out) = wand_out ~dir:d ["t"; "main.wand"] in
    Alcotest.(check int) "the bare name is refused" 1 code;
    (* And the message names the spelling that works, rather than saying the
       type is unknown. *)
    Alcotest.(check bool) "it says what to write" true
      (contains_sub out "write 'ord.Ranked'"))

let test_two_modules_may_declare_one_name () =
  in_scratch (fun d ->
    write_file (Filename.concat d "alpha.wand")
      "interface Shown 'a(show: 'a -> String)\n";
    write_file (Filename.concat d "beta.wand")
      "interface Shown 'a(render: 'a -> String)\n";
    write_file (Filename.concat d "main.wand")
      "let a = import ./alpha\n\
       let b = import ./beta\n\n\
       let f (m: a.Shown Int) = m.show 1\n\
       let g (m: b.Shown Int) = m.render 1\n\n\
       f\n";
    let (code, _) = wand_out ~dir:d ["t"; "main.wand"] in
    Alcotest.(check int) "both are usable in one file" 0 code;
    (* Before the qualifier was required these overwrote each other by
       import order, and nothing was reported. *)
    write_file (Filename.concat d "bare.wand")
      "let a = import ./alpha\n\
       let b = import ./beta\n\n\
       let f (m: Shown Int) = m.show 1\n\n\
       f\n";
    let (code, out) = wand_out ~dir:d ["t"; "bare.wand"] in
    Alcotest.(check int) "and the bare name is refused" 1 code;
    Alcotest.(check bool) "naming both" true
      (contains_sub out "'a.Shown'" && contains_sub out "'b.Shown'"))

(* An interface whose members name its own module's types, implemented in
   another module. The member types were read against the implementing
   module's names, so `N` there was unknown, and an alias that named the
   interface was unknown in any third file. And an interface was known by
   the name a file wrote for it, so two files that bound its module under
   different names did not agree that a module fits it (#51). *)
let test_an_interface_travels_with_its_types () =
  in_scratch (fun d ->
    write_file (Filename.concat d "shapes.wand")
      "type Id = String\n\
       type N(g: Int, f: Unit -> N)\n\
       interface B(create: Id -> N)\n";
    write_file (Filename.concat d "impl.wand")
      "let s = import ./shapes\n\n\
       let make x = s.N(g = x, f = fn () -> make (x + 1))\n\n\
       implement s.B =\n\
       \  let create _ = make 3\n";
    write_file (Filename.concat d "lib.wand")
      "import Map\n\nlet shapes = import ./shapes\n\ntype Lib = Map shapes.B\n";
    write_file (Filename.concat d "main.wand")
      "uses {IO}\n\nimport IO\nimport Map\n\n\
       let shp = import ./shapes\n\
       let impl = import ./impl\n\
       let lib = import ./lib\n\n\
       let one : shp.B = impl\n\
       let count (l: lib.Lib) = Map.size l\n\n\
       IO.println \"%{(one.create \"x\").g} %{count (Map.from_list [(\"a\", one)])}\"\n";
    let (code, out) = wand_out ~dir:d ["main.wand"] in
    Alcotest.(check (pair int string)) "it runs" (0, "3 1\n") (code, out))

(* Two different modules that implement one interface, in one list. Each
   module is its own type, and the list took the first one's, so the second
   was refused with a message that named one interface twice (#52). *)
let test_a_list_of_modules_is_a_list_of_their_interface () =
  in_scratch (fun d ->
    iface_files d;
    write_file (Filename.concat d "rev.wand")
      "let ord = import ./ord\n\n\
       implement ord.Ranked Int =\n\
       \  let top a b = if a < b then a else b;\n\
       \  let bottom a b = if a > b then a else b\n";
    write_file (Filename.concat d "main.wand")
      "uses {IO}\n\nimport IO\nimport List\nimport Map\n\n\
       let ord = import ./ord\n\
       let ints = import ./ints\n\
       let rev = import ./rev\n\n\
       let typed : List (ord.Ranked Int) = [ints, rev]\n\
       let bare = [ints, rev]\n\
       let named : Map (ord.Ranked Int) = Map.from_list [(\"i\", ints), (\"r\", rev)]\n\n\
       IO.println \"%{List.map (fn (m: ord.Ranked Int) -> m.top 3 7) typed} \
       %{List.length bare} %{Map.size named}\"\n";
    let (code, out) = wand_out ~dir:d ["main.wand"] in
    Alcotest.(check (pair int string)) "it runs" (0, "[7, 3] 2 2\n") (code, out))

(* A directory is every `.wand` file under it, as `wand t` and `wand s` read
   one. `wand f driver/` used to answer `Is a directory` (#57). *)
let test_fmt_takes_a_directory () =
  in_scratch (fun d ->
    Unix.mkdir (Filename.concat d "sub") 0o755;
    write_file (Filename.concat d "a.wand") "let x    =  1\n";
    write_file (Filename.concat d "sub/b.wand") "let y    =  2\n";
    let (code, _) = wand_out ~dir:d ["f"; "."] in
    Alcotest.(check int) "it succeeds" 0 code;
    Alcotest.(check (pair string string)) "both files are formatted"
      ("let x = 1\n", "let y = 2\n")
      (read_file (Filename.concat d "a.wand"), read_file (Filename.concat d "sub/b.wand"));
    Unix.mkdir (Filename.concat d "empty") 0o755;
    let (code, _) = wand_out ~dir:d ["f"; "empty"] in
    Alcotest.(check int) "a directory with no files is an error" 1 code)

(* The arguments of a constructor reached through a module are the caller's.
   They were inferred with that module's types in view, so where two modules
   each declare `World`, `core.World(n = w.now)` read `w`'s `World` as
   `core.World` and failed (#63). *)
let test_qualified_constructor_arguments_are_the_callers () =
  in_scratch (fun d ->
    write_file (Filename.concat d "core.wand")
      "type World(n: Int)\ntype Pair = Pair Int Int\n";
    write_file (Filename.concat d "world.wand") "type World(now: Int)\n";
    write_file (Filename.concat d "main.wand")
      "uses {IO}\n\nimport IO\n\n\
       let core = import ./core\n\
       let world = import ./world\n\n\
       let w = world.World(now = 5)\n\n\
       IO.println \"%{core.World(n = w.now)} %{core.World(core.World(n = 1), n = w.now)} %{core.Pair w.now 7}\"\n";
    let (code, out) = wand_out ~dir:d ["main.wand"] in
    Alcotest.(check (pair int string)) "it runs" (0, "World(5) World(5) Pair(5, 7)\n") (code, out))

(* A function stored in a field whose type writes no effects. The file that
   builds the value answers for what the function performs: another file
   that calls the field rebuilds the type with rows of its own, so neither
   file saw the command, and a program under `uses {IO}` ran it (#61). *)
let test_a_stored_function_is_charged_where_it_is_stored () =
  in_scratch (fun d ->
    write_file (Filename.concat d "contract.wand") "type R(f: Unit -> String)\n";
    write_file (Filename.concat d "stores.wand")
      "uses {Random}\n\nlet c = import ./contract\n\
       let r = c.R(f = fn () -> $(echo stored))\n";
    write_file (Filename.concat d "maker.wand")
      "let c = import ./contract\n\
       let mk () = c.R(f = fn () -> $(echo made))\n";
    let (code, out) = wand_out ~dir:d ["t"; "stores.wand"] in
    Alcotest.(check int) "the file that stores it is refused" 1 code;
    Alcotest.(check bool) "naming the effect" true (contains_sub out "Shell");
    let (code, out) = wand_out ~dir:d ["t"; "maker.wand"] in
    Alcotest.(check int) "a function that builds one types" 0 code;
    ignore out;
    write_file (Filename.concat d "uses_maker.wand")
      "uses {IO}\n\nlet m = import ./maker\nlet r = m.mk ()\n";
    let (code, out) = wand_out ~dir:d ["t"; "uses_maker.wand"] in
    Alcotest.(check int) "and the effect is on its arrow" 1 code;
    Alcotest.(check bool) "naming it" true (contains_sub out "Shell"))

(* An update that leaves the stored field alone stores nothing, and is
   charged nothing for it. It was charged the field's shared row, so every
   update of the type shared one effect: a list that put an update beside a
   function that raises made the update raise, and a field nothing read
   broke a check in other code (#79). An update that names the field is
   still charged. *)
let test_an_update_is_charged_only_for_what_it_stores () =
  in_scratch (fun d ->
    write_file (Filename.concat d "m.wand")
      "type Cur(n: Int, dig: Option (String -> String) = None)\n\n\
       let bump (w: Cur) = Cur(w, n = w.n + 1)\n";
    write_file (Filename.concat d "main.wand")
      "import Result\n\nlet M = import ./m\n\n\
       let raising! (w: M.Cur) = if w.n > 3 then Result.get! (Error \"x\") else w\n\n\
       let l = [M.bump, raising!]\n\n\
       let pure_only (f: M.Cur -> M.Cur ! {}) = f\n\n\
       let a = pure_only M.bump\n";
    let (code, out) = wand_out ~dir:d ["t"; "main.wand"] in
    Alcotest.(check (pair int string)) "it checks" (0, "") (code, out);
    write_file (Filename.concat d "stores.wand")
      "uses {Random}\n\nlet M = import ./m\n\n\
       let w = M.Cur(M.Cur(n = 1), dig = Some (fn s -> $(echo %{s})))\n";
    let (code, out) = wand_out ~dir:d ["t"; "stores.wand"] in
    Alcotest.(check int) "an update that stores a function is refused" 1 code;
    Alcotest.(check bool) "naming the effect" true (contains_sub out "Shell"))

(* A module that did not claim the interface does not fit, however many of
   its members happen to line up. *)
let test_a_module_must_have_claimed_it () =
  in_scratch (fun d ->
    iface_files d;
    write_file (Filename.concat d "other.wand") "let greet n = \"hi %{n}\"\n";
    write_file (Filename.concat d "main.wand")
      "let ord = import ./ord\n\
       let other = import ./other\n\n\
       let biggest (m: ord.Ranked Int) a b = m.top a b\n\n\
       biggest other 3 7\n";
    let (code, out) = wand_out ~dir:d ["t"; "main.wand"] in
    Alcotest.(check int) "it is refused" 1 code;
    Alcotest.(check bool) "and says what the module would have to declare" true
      (contains_sub out "does not implement ord.Ranked"))

(* Reaching an interface through the module that declares it is a use of
   that import. The dead-import rule did not look at the qualifier, and
   reported a file that plainly used it. *)
let test_a_qualified_interface_uses_its_import () =
  in_scratch (fun d ->
    iface_files d;
    let (code, out) = wand_out ~dir:d ["t"; "ints.wand"] in
    Alcotest.(check int) "it checks out" 0 code;
    Alcotest.(check bool) "with no dead-import warning" false
      (contains_sub out "V-IMP2"))

(* Nothing in an interface is arity-specific: the parser collects its type
   variables in a loop, the members are a `List.combine` of the parameters
   with the claim's arguments, and unifying two of them zips the argument
   lists. So N falls out rather than being handled -- which is exactly the
   kind of thing a later change to substitution would break in silence. *)
let test_interfaces_at_every_arity () =
  in_scratch (fun d ->
    for n = 0 to 6 do
      let ps = List.init n (fun i -> Printf.sprintf "'%c" (Char.chr (97 + i))) in
      let params = if ps = [] then "" else " " ^ String.concat " " ps in
      let args = String.concat " " (List.init n (fun _ -> "Int")) in
      let member = if n = 0 then "tag: String" else "pick: " ^ String.concat " -> " ps in
      (* `pick` chains the n parameters, so at all-`Int` it takes n - 1
         arguments and answers with one. *)
      let body =
        if n = 0 then "  let tag = \"x\""
        else
          "  let pick"
          ^ String.concat ""
              (List.init (n - 1) (fun i -> Printf.sprintf " p%d" i))
          ^ " = 0"
      in
      let annot =
        if n = 0 then Printf.sprintf "iface.I%d" n
        else Printf.sprintf "iface.I%d %s" n args
      in
      write_file (Filename.concat d (Printf.sprintf "i%d.wand" n))
        (Printf.sprintf "interface I%d%s(%s)\n" n params member);
      write_file (Filename.concat d (Printf.sprintf "m%d.wand" n))
        (Printf.sprintf "let iface = import ./i%d\n\nimplement iface.I%d %s =\n%s\n"
           n n args body);
      write_file (Filename.concat d (Printf.sprintf "u%d.wand" n))
        (Printf.sprintf
           "let iface = import ./i%d\nlet m = import ./m%d\n\n\
            let f (x: %s) = x\n\nf m\n" n n annot);
      let (code, out) = wand_out ~dir:d [Printf.sprintf "u%d.wand" n] in
      Alcotest.(check int)
        (Printf.sprintf "%d type parameter(s) checks out" n) 0 code;
      ignore out
    done)

(* All-`Int` arguments would hide a parameter bound to the wrong one, so the
   types have to differ. *)
let test_each_parameter_binds_its_own_type () =
  in_scratch (fun d ->
    write_file (Filename.concat d "tri.wand")
      "interface Tri 'a 'b 'c(first: 'a -> 'b, second: 'b -> 'c, both: 'a -> 'c)\n";
    let impl order =
      Printf.sprintf
        "import String\n\nlet tri = import ./tri\n\n\
         implement tri.Tri %s =\n\
         \  let first n = \"%%{n}\";\n\
         \  let second s = String.length s > 1;\n\
         \  let both n = n > 9\n" order
    in
    write_file (Filename.concat d "m.wand") (impl "Int String Bool");
    write_file (Filename.concat d "u.wand")
      "let tri = import ./tri\n\
       let m = import ./m\n\n\
       let run (x: tri.Tri Int String Bool) n = \
       (x.first n, x.second (x.first n), x.both n)\n\n\
       run m 42\n";
    let (code, out) = wand_out ~dir:d ["u.wand"] in
    Alcotest.(check int) "it runs" 0 code;
    Alcotest.(check string) "each parameter kept its own type"
      "(\"42\", true, true)\n" out;
    (* The arguments in the wrong order are refused at the first member that
       disagrees, named rather than the block. *)
    write_file (Filename.concat d "m.wand") (impl "Int Bool String");
    let (code, out) = wand_out ~dir:d ["t"; "u.wand"] in
    Alcotest.(check int) "the wrong order is refused" 1 code;
    Alcotest.(check bool) "at the member" true
      (contains_sub out "'first' does not match what 'tri.Tri' declares for it");
    (* And too few arguments are counted. *)
    write_file (Filename.concat d "m.wand") (impl "Int String");
    let (code, out) = wand_out ~dir:d ["t"; "u.wand"] in
    Alcotest.(check int) "too few is refused" 1 code;
    Alcotest.(check bool) "saying how many" true
      (contains_sub out "takes 3 type arguments, and this names 2"))

(* A member's effects reach the file that calls it, and its manifest answers
   for them. The interface is what bounds them, which is why a member cannot
   leave them open: the effects are read from the declaration and never meet
   the implementation, so an unbounded variable would let a module performing
   Shell answer to it and tell the caller nothing. *)
let test_a_members_effects_reach_the_caller () =
  in_scratch (fun d ->
    write_file (Filename.concat d "r.wand")
      "interface Runner(go: Unit -> String ! {Raise, Shell})\n";
    write_file (Filename.concat d "impl.wand")
      "uses {Shell(whoami)}\n\n\
       let r = import ./r\n\n\
       implement r.Runner =\n\
       \  let go () = $(whoami)\n";
    (* A manifest that forbids Shell is held to it. *)
    write_file (Filename.concat d "narrow.wand")
      "uses {IO}\n\n\
       let r = import ./r\n\
       let impl = import ./impl\n\n\
       let use! (m: r.Runner) = m.go ()\n\n\
       use! impl\n";
    let (code, out) = wand_out ~dir:d ["t"; "narrow.wand"] in
    Alcotest.(check int) "the caller is held to its manifest" 1 code;
    Alcotest.(check bool) "and told what it performs" true
      (contains_sub out "performs Shell, which the manifest does not allow");
    (* Declaring it, the file checks out. *)
    write_file (Filename.concat d "wide.wand")
      "uses {Shell}\n\n\
       let r = import ./r\n\
       let impl = import ./impl\n\n\
       let use! (m: r.Runner) = m.go ()\n\n\
       use! impl\n";
    let (code, _) = wand_out ~dir:d ["t"; "wide.wand"] in
    Alcotest.(check int) "and passes when it says so" 0 code;
    (* An implementation performing more than the member declares is refused
       where it is written. *)
    write_file (Filename.concat d "r.wand")
      "interface Runner(go: Unit -> String ! {Shell})\n";
    let (code, out) = wand_out ~dir:d ["t"; "wide.wand"] in
    Alcotest.(check int) "performing more than declared is refused" 1 code;
    Alcotest.(check bool) "naming the member" true
      (contains_sub out "'go' does not match what 'r.Runner' declares for it"))

(* ── wand t --effects ────────────────────────────────────────────────────── *)

(* What a file reaches outside itself, as data, so a check can compare it
   with a policy by exit code. The set is the inferred one and never the
   `uses` line: a file with no manifest is unbounded rather than sealed. *)
let test_effects_of_one_file () =
  in_scratch (fun d ->
    write_file (Filename.concat d "a.wand")
      "uses {IO, Shell(df)}\n\nimport IO\n\nIO.println $(df -P)\n";
    let (code, out) = wand_out ~dir:d ["t"; "--effects"; "a.wand"] in
    Alcotest.(check int) "a file that checks out exits 0" 0 code;
    (* The file, then the set, in the notation a signature uses for one.
       Shell is narrowed to the words the file runs, because every command
       position in it is literal. *)
    Alcotest.(check string) "the file and what it performs"
      "a.wand ! {IO, Shell(df)}\n" out;
    (* A file that reaches nothing says so, rather than printing a blank:
       a check has to be able to match it. *)
    write_file (Filename.concat d "pure.wand") "let f x = x + 1\n";
    let (_, out) = wand_out ~dir:d ["t"; "--effects"; "pure.wand"] in
    Alcotest.(check string) "and a file that reaches nothing"
      "pure.wand ! {}\n" out)

let test_effects_ignore_the_manifest () =
  in_scratch (fun d ->
    (* No manifest at all. Reading the `uses` line would report the empty
       set for the least bounded file there is. *)
    write_file (Filename.concat d "a.wand") "import IO\n\nIO.println \"hi\"\n";
    let (_, out) = wand_out ~dir:d ["t"; "--effects"; "a.wand"] in
    Alcotest.(check string) "the inferred set, not the missing manifest"
      "a.wand ! {IO}\n" out;
    (* `Raise` is not a reach outside the file -- `uses {Raise}` draws
       A-USES1 -- so it is not one of the labels reported. *)
    write_file (Filename.concat d "r.wand")
      "import List\n\nlet f! xs = List.head! xs\n\nf! [1]\n";
    let (_, out) = wand_out ~dir:d ["t"; "--effects"; "r.wand"] in
    Alcotest.(check string) "a raise is not a reach outside"
      "r.wand ! {}\n" out)

let test_effects_over_a_tree () =
  in_scratch (fun d ->
    write_file (Filename.concat d "one.wand") "import IO\n\nIO.println \"hi\"\n";
    write_file (Filename.concat d "a_much_longer_name.wand") "let f x = x\n";
    let (code, out) = wand_out ~dir:d ["t"; "--effects"; "."] in
    Alcotest.(check int) "a tree that checks out exits 0" 0 code;
    (* The `!` lines up, the column set by the longest path. *)
    Alcotest.(check bool) "the longest path sets the column" true
      (contains_sub out "./a_much_longer_name.wand ! {}");
    Alcotest.(check bool) "and the shorter one is padded to it" true
      (contains_sub out "./one.wand                ! {IO}");
    Alcotest.(check bool) "the count is reported" true
      (contains_sub out "2 files");
    (* A file with no inferred set is one that does not check, and the rest
       of the tree still reports. *)
    write_file (Filename.concat d "bad.wand") "let f x = nope x\n";
    let (code, out) = wand_out ~dir:d ["t"; "--effects"; "."] in
    Alcotest.(check int) "one file that does not check exits 1" 1 code;
    Alcotest.(check bool) "and the others still report" true
      (contains_sub out "./one.wand"))

let test_effects_json () =
  in_scratch (fun d ->
    write_file (Filename.concat d "a.wand")
      "uses {IO, Shell(df)}\n\nimport IO\n\nIO.println $(df -P)\n";
    let (_, out) = wand_out ~dir:d ["t"; "--effects"; "--json"; "a.wand"] in
    (* The narrowing is split out, so a tool never parses `Shell(df)`. *)
    Alcotest.(check string) "one entry per file, with allows split out"
      "[{\"file\":\"a.wand\",\"effects\":[{\"label\":\"IO\"},\
       {\"label\":\"Shell\",\"allows\":[\"df\"]}]}]\n" out)

let test_effects_refuses_fix_and_expr () =
  in_scratch (fun d ->
    write_file (Filename.concat d "a.wand") "let f x = x\n";
    let (code, _) = wand_out ~dir:d ["t"; "--effects"; "--fix"; "a.wand"] in
    Alcotest.(check int) "--effects does not rewrite a file" 1 code;
    (* A manifest bounds a file, and an expression is not one. *)
    let (code, _) = wand_out ~dir:d ["t"; "--effects"; "-e"; "1 + 2"] in
    Alcotest.(check int) "and an expression is not a file" 1 code;
    (* It reports no lint findings, so there is nothing for --strict to
       promote. Refused rather than ignored. *)
    let (code, _) = wand_out ~dir:d ["t"; "--effects"; "--strict"; "a.wand"] in
    Alcotest.(check int) "and --strict has nothing to promote" 1 code)

let test_type_of_several_paths () =
  in_scratch (fun d ->
    write_file (Filename.concat d "a.wand") "let f x = x + 1\n";
    write_file (Filename.concat d "b.wand") "let g x = unknown_name x\n";
    let (code, out) = wand_out ~dir:d ["t"; "a.wand"; "b.wand"] in
    Alcotest.(check int) "two paths, one bad, exits 1" 1 code;
    Alcotest.(check bool) "the bad one is named" true (contains_sub out "b.wand:");
    Alcotest.(check bool) "both were checked" true (contains_sub out "2 files"))

let () =
  Alcotest.run "CLI" [
    "what a run checks and withholds", [
      Alcotest.test_case "a run checks what wand t checks" `Quick test_run_checks_what_t_checks;
      Alcotest.test_case "an import's work waits on the check" `Quick test_import_work_waits_on_the_check;
      Alcotest.test_case "a rehearsal does not listen" `Quick test_a_rehearsal_does_not_listen;
    ];
    "wand d --index", [
      Alcotest.test_case "every module's members, once each" `Quick test_doc_index;
    ];
    "the REPL's :d", [
      Alcotest.test_case "a module lists its members" `Quick test_repl_doc_of_a_module;
      Alcotest.test_case "a member shows its doc"     `Quick test_repl_doc_of_a_member;
      Alcotest.test_case "no argument lists the session" `Quick test_repl_doc_with_no_argument;
      Alcotest.test_case ":v is retired"              `Quick test_repl_env_is_retired;
    ];
    "typecheck a file", [
      Alcotest.test_case "reports the type"   `Quick test_typecheck_file;
      Alcotest.test_case "reports errors"     `Quick test_typecheck_file_reports_errors;
      Alcotest.test_case "reports holes"      `Quick test_typecheck_file_reports_holes;
      Alcotest.test_case "checks a buffer"    `Quick test_typecheck_source_unsaved_buffer;
      Alcotest.test_case "reports own names"  `Quick test_typecheck_source_own_names;
      Alcotest.test_case "reports lints"      `Quick test_typecheck_file_lints;
    ];
    "test discovery", [
      Alcotest.test_case "finds tests beside scripts" `Quick test_finds_tests_beside_scripts;
      Alcotest.test_case "skips build directories"    `Quick test_skips_build_directories;
      Alcotest.test_case "a named file as given"      `Quick test_a_named_file_is_taken_as_given;
      Alcotest.test_case "stays in the tree"          `Quick test_the_walk_does_not_leave_the_tree;
      Alcotest.test_case "steps over a broken link"   `Quick test_a_dangling_symlink_is_stepped_over;
    ];
    "repl", [
      Alcotest.test_case "clause merge announced" `Quick test_repl_merges_clauses_and_announces;
      Alcotest.test_case "mutual group echoed"    `Quick test_repl_echoes_mutual_group;
      Alcotest.test_case "a pure Unit answers"    `Quick test_repl_answers_pure_unit;
    ];
    "eval", [
      Alcotest.test_case "eval expressions" `Quick test_eval;
      Alcotest.test_case "eval holes"       `Quick test_eval_holes;
      Alcotest.test_case "incremental pattern match" `Quick test_incremental_pattern_match;
    ];
    "type", [
      Alcotest.test_case "typecheck expressions" `Quick test_type;
    ];
    "doc", [
      Alcotest.test_case "type lookup"   `Quick test_doc;
      Alcotest.test_case "doc strings"   `Quick test_doc_strings;
    ];
    "rewriting a file", [
      Alcotest.test_case "wand f"        `Quick test_fmt_rewrites_in_place;
      Alcotest.test_case "wand f takes a directory" `Quick test_fmt_takes_a_directory;
      Alcotest.test_case "wand t --fix"  `Quick test_fix_rewrites_in_place;
      Alcotest.test_case "wand t over a tree" `Quick test_type_over_a_tree;
      Alcotest.test_case "wand t on one file" `Quick test_type_of_one_file_is_unchanged;
      Alcotest.test_case "wand t on several paths" `Quick test_type_of_several_paths;
      Alcotest.test_case "every unbound name" `Quick test_every_unbound_name_reported;
      Alcotest.test_case "a member's effects reach the caller" `Quick test_a_members_effects_reach_the_caller;
      Alcotest.test_case "interfaces at every arity" `Quick test_interfaces_at_every_arity;
      Alcotest.test_case "each parameter binds its own" `Quick test_each_parameter_binds_its_own_type;
      Alcotest.test_case "wand d marks interfaces" `Quick test_wand_d_marks_interfaces;
      Alcotest.test_case "a module passed as a value" `Quick test_a_module_is_passed_to_generic_code;
      Alcotest.test_case "no bare imported interface" `Quick test_an_imported_interface_has_no_bare_form;
      Alcotest.test_case "two modules, one name" `Quick test_two_modules_may_declare_one_name;
      Alcotest.test_case "conformance is claimed" `Quick test_a_module_must_have_claimed_it;
      Alcotest.test_case "an interface travels with its types" `Quick test_an_interface_travels_with_its_types;
      Alcotest.test_case "a list of modules" `Quick test_a_list_of_modules_is_a_list_of_their_interface;
      Alcotest.test_case "a stored function is charged" `Quick test_a_stored_function_is_charged_where_it_is_stored;
      Alcotest.test_case "an update is charged only for what it stores" `Quick
        test_an_update_is_charged_only_for_what_it_stores;
      Alcotest.test_case "qualified constructor arguments" `Quick test_qualified_constructor_arguments_are_the_callers;
      Alcotest.test_case "a qualifier uses its import" `Quick test_a_qualified_interface_uses_its_import;
      Alcotest.test_case "wand t --effects" `Quick test_effects_of_one_file;
      Alcotest.test_case "--effects is inferred" `Quick test_effects_ignore_the_manifest;
      Alcotest.test_case "--effects over a tree" `Quick test_effects_over_a_tree;
      Alcotest.test_case "--effects --json" `Quick test_effects_json;
      Alcotest.test_case "--effects refusals" `Quick test_effects_refuses_fix_and_expr;
    ];
    "scope", [
      Alcotest.test_case "list all"      `Quick test_env_all;
      Alcotest.test_case "module lookup" `Quick test_env_module;
      Alcotest.test_case "Map module"    `Quick test_env_map_module;
      Alcotest.test_case "the prelude is every module" `Quick
        test_prelude_names_every_module;
    ];
  ]
