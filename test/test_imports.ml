open Wand

let run s = Runner.run_string s

let err label input =
  match run input with
  | Error _ -> ()
  | Ok s -> Alcotest.failf "%s: expected error but got: %s" label s

(* ── Helpers ─────────────────────────────────────────────────────────────── *)

let with_named name src f =
  let dir = Filename.get_temp_dir_name () in
  let path = Filename.concat dir (name ^ ".wand") in
  let oc = open_out path in
  output_string oc src; close_out oc;
  let result = (try f path with e -> Sys.remove path; raise e) in
  Sys.remove path; result

(* ── Private symbols (leading _) ─────────────────────────────────────────── *)

let test_private_symbols () =
  with_named "utils" {|let _secret = 42
let public = 1|} (fun path ->
    err "private symbol not accessible"
      (Printf.sprintf {|let utils = import %s
utils._secret|} path))

(* ── Error: missing field in destructure ────────────────────────────────── *)

let test_destructure_missing_field () =
  with_named "utils" {|let foo = 1|} (fun path ->
    err "missing field gives error"
      (Printf.sprintf {|let {bar = x} = import %s
x|} path))

(* ── User-path imports must state their binding ──────────────────────────── *)

(* A bare `import ./utils` used to bind `Utils`, a name derived by
   capitalising the filename. The two explicit forms below say what they
   bind, so they must keep working; the bare form must not. *)

let test_bare_user_import_rejected () =
  with_named "utils" {|let public = 1|} (fun path ->
    err "bare user-path import does not bind"
      (Printf.sprintf {|import %s
Utils.public|} path))

let test_explicit_binding_works () =
  with_named "utils" {|let public = 1|} (fun path ->
    Alcotest.(check (result string string))
      "let-bound import resolves"
      (Ok "1")
      (run (Printf.sprintf {|let utils = import %s
utils.public|} path)))

let test_destructured_binding_works () =
  with_named "utils" {|let public = 1|} (fun path ->
    Alcotest.(check (result string string))
      "destructured import resolves"
      (Ok "1")
      (run (Printf.sprintf {|let {public} = import %s
public|} path)))

(* ── An import brings in what it names, and nothing else ─────────────────── *)

(* Binding a module to a name used to put every name inside it into scope
   unqualified as well, so `let m = import ./utils` quietly made `helper`
   callable as `helper`. That defeats the point of saying what an import
   binds: the name a reader greps for is not the name in the file. *)

let test_module_names_do_not_leak () =
  with_named "utils" {|let public = 1|} (fun path ->
    err "a name behind the module prefix is not in scope bare"
      (Printf.sprintf {|let utils = import %s
public|} path))

let test_destructuring_binds_only_what_it_names () =
  with_named "utils" {|let foo = 1
let bar = 2|} (fun path ->
    err "an unnamed field is not in scope"
      (Printf.sprintf {|let {foo} = import %s
bar|} path))

(* A module's own imports are its business. *)
let test_transitive_imports_do_not_leak () =
  with_named "utils" {|import List
let public = List.length [1, 2]|} (fun path ->
    err "the module's import does not become the importer's"
      (Printf.sprintf {|let utils = import %s
List.length [1]|} path))

(* A constructor is reached through the module that declares it, the way a
   value is. It used to arrive under its bare name, so two modules that each
   declared `Red` collided and no file could say which it meant. *)
let test_imported_constructors_cross () =
  with_named "colors" {|type Color = Red | Green
let pick = Red
let name c = match c with | Red -> "red" | Green -> "green"|} (fun path ->
    Alcotest.(check (result string string))
      "a constructor of an imported type is usable through the module"
      (Ok "green")
      (run (Printf.sprintf {|let colors = import %s
colors.name colors.Green|} path)))

(* And by being named in the import, which is how a value is brought in. *)
let test_imported_constructors_selected () =
  with_named "hues" {|type Hue = Warm | Cool
let name c = match c with | Warm -> "warm" | Cool -> "cool"|} (fun path ->
    Alcotest.(check (result string string))
      "a constructor named in the import is used bare"
      (Ok "warm")
      (run (Printf.sprintf {|let {name, Warm} = import %s
name Warm|} path)))

(* ── A module's alias, inside it and outside it ──────────────────── *)

(* A module's types are keyed by the module, and the alias table was keyed
   the same way while the file that declared it writes the short name. So
   `type Response = HTTPResponse` resolved everywhere except inside the
   module: `(r: Response)` stayed an opaque name and `r.status` had no field
   to read. *)
let test_module_alias_used_inside () =
  with_named "resp" {|type Response = HTTPResponse
let ok? (r: Response) = r.status >= 200|} (fun path ->
    Alcotest.(check (result string string))
      "a module's own alias resolves inside the module"
      (Ok "true")
      (run (Printf.sprintf {|let {ok?} = import %s
ok? HTTPResponse(status = 204, headers = {}, body = "")|} path)))

(* And the qualified spelling matches, not only annotates: the exhaustiveness
   check read the name after the dot without the module's types in front, so
   `M.Response(...)` forwarded to no constructor and covered nothing. *)
let test_module_alias_pattern () =
  with_named "resp2" {|type Response = HTTPResponse
let make () = HTTPResponse(status = 204, headers = {}, body = "")|}
    (fun path ->
    Alcotest.(check (result string string))
      "a module's alias matches under the qualified name"
      (Ok "204")
      (run (Printf.sprintf {|let m = import %s
match m.make () with
  | m.Response(status = s) -> s|} path)))

(* ── Analysis does not run an import ─────────────────────── *)

(* `wand t` used to evaluate every module the file imported, under the
   handler a run uses. Opening a file in an editor ran the code it imported:
   the language server typechecks on every keystroke, and the attacker writes
   the imported module's own manifest, so nothing static stood in the way.
   Checking asks a module for its types; only a run asks for its values. *)

let with_module_dir f =
  let dir = Filename.temp_file "wand-analysis" "" in
  Sys.remove dir; Sys.mkdir dir 0o700;
  Fun.protect ~finally:(fun () ->
    Array.iter (fun n ->
      try Sys.remove (Filename.concat dir n) with Sys_error _ -> ())
      (Sys.readdir dir);
    try Sys.rmdir dir with Sys_error _ -> ())
    (fun () -> f dir)

let write_file path contents =
  Out_channel.with_open_text path
    (fun oc -> Out_channel.output_string oc contents)

let test_typecheck_does_not_run_imports () =
  with_module_dir (fun dir ->
    let marker = Filename.concat dir "ran" in
    write_file (Filename.concat dir "evil.wand")
      (Printf.sprintf "uses {FS.Write}\nimport FS\nlet boom = FS.write_file! %s \"owned\\n\"\n"
         marker);
    let victim = Filename.concat dir "victim.wand" in
    write_file victim "let {boom} = import ./evil\nlet x = boom\n";
    (match Runner.typecheck_file victim with
     | Ok _    -> ()
     | Error d -> Alcotest.failf "typecheck failed: %s" (Diag.legacy d));
    Alcotest.(check bool) "typechecking did not run the import"
      false (Sys.file_exists marker);
    (* And a run still does. *)
    (match Runner.run_file victim with
     | Ok _    -> ()
     | Error m -> Alcotest.failf "run failed: %s" m);
    Alcotest.(check bool) "running did run the import"
      true (Sys.file_exists marker))

(* An import evaluates the module's bindings, so the file that writes the
   import performs what they perform. Defining a function performs nothing:
   its effects sit on the arrow and arrive when something calls it. *)
let test_an_import_performs_the_module_s_load_effects () =
  with_module_dir (fun dir ->
    write_file (Filename.concat dir "loud.wand")
      "uses {Shell(echo)}\nlet greeting = $(echo hi)\n";
    write_file (Filename.concat dir "quiet.wand")
      "let shout! () = $(echo hi)\n";
    let check name src =
      let path = Filename.concat dir name in
      write_file path src;
      Runner.typecheck_file path
    in
    (match check "a.wand" "uses {IO}\nimport IO\nlet {greeting} = import ./loud\nIO.println greeting\n" with
     | Ok _ -> Alcotest.fail "a manifest without Shell was accepted"
     | Error d ->
       let m = Diag.legacy d in
       if not (Lint.contains m "performs Shell") then
         Alcotest.failf "expected Shell in: %s" m);
    (match check "b.wand" "uses {IO, Shell}\nimport IO\nlet {greeting} = import ./loud\nIO.println greeting\n" with
     | Ok _ -> ()
     | Error d -> Alcotest.failf "declaring Shell was refused: %s" (Diag.legacy d));
    (* A module of functions performs nothing when it is imported, so the
       importer's manifest does not have to grow for one it never calls. *)
    (match check "c.wand" "uses {IO}\nimport IO\nlet {shout!} = import ./quiet\nIO.println \"fine\"\n" with
     | Ok _ -> ()
     | Error d ->
       Alcotest.failf "importing a module of functions wanted a manifest: %s"
         (Diag.legacy d)))

(* Loading a file loads what it imports, so the effects travel the whole
   chain rather than one link of it. *)
let test_load_effects_are_transitive () =
  with_module_dir (fun dir ->
    write_file (Filename.concat dir "leaf.wand")
      "uses {Shell(echo)}\nlet deep = $(echo deep)\n";
    write_file (Filename.concat dir "mid.wand")
      "uses {Shell}\nlet {deep} = import ./leaf\nlet passed = deep\n";
    let top = Filename.concat dir "top.wand" in
    write_file top "uses {IO}\nimport IO\nlet {passed} = import ./mid\nIO.println passed\n";
    match Runner.typecheck_file top with
    | Ok _ -> Alcotest.fail "the effect two imports away was not seen"
    | Error d ->
      let m = Diag.legacy d in
      if not (Lint.contains m "performs Shell") then
        Alcotest.failf "expected Shell in: %s" m)

(* ── Suite ───────────────────────────────────────────────────────────────── *)

let () =
  Alcotest.run "Imports" [
    "private", [
      Alcotest.test_case "private symbols" `Quick test_private_symbols;
    ];
    "errors", [
      Alcotest.test_case "missing field"   `Quick test_destructure_missing_field;
    ];
    "user paths", [
      Alcotest.test_case "bare import rejected"   `Quick test_bare_user_import_rejected;
      Alcotest.test_case "let binding works"      `Quick test_explicit_binding_works;
      Alcotest.test_case "destructuring works"    `Quick test_destructured_binding_works;
    ];
    "scope", [
      Alcotest.test_case "module names do not leak" `Quick test_module_names_do_not_leak;
      Alcotest.test_case "only what is named"       `Quick test_destructuring_binds_only_what_it_names;
      Alcotest.test_case "transitive do not leak"   `Quick test_transitive_imports_do_not_leak;
      Alcotest.test_case "constructors cross"       `Quick test_imported_constructors_cross;
      Alcotest.test_case "constructors selected"    `Quick test_imported_constructors_selected;
    ];
    "analysis", [
      Alcotest.test_case "typecheck does not run imports" `Quick
        test_typecheck_does_not_run_imports;
    ];
    "effects", [
      Alcotest.test_case "an import performs the module's load effects" `Quick
        test_an_import_performs_the_module_s_load_effects;
      Alcotest.test_case "and they are transitive" `Quick
        test_load_effects_are_transitive;
    ];
    "aliases", [
      Alcotest.test_case "used inside its module"   `Quick test_module_alias_used_inside;
      Alcotest.test_case "matched from outside"     `Quick test_module_alias_pattern;
    ];
  ]
