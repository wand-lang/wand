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

(* ── A bare import binds the last segment of its path ──────────────────── *)

let contains msg needle =
  let n = String.length needle and m = String.length msg in
  let rec go i = i + n <= m && (String.sub msg i n = needle || go (i + 1)) in
  go 0

let err_says label needle input =
  match run input with
  | Error e ->
    if not (contains e needle) then
      Alcotest.failf "%s: expected %S in: %s" label needle e
  | Ok s -> Alcotest.failf "%s: expected error but got: %s" label s

let test_bare_user_import_binds () =
  with_named "utils" {|let public = 1|} (fun path ->
    Alcotest.(check (result string string))
      "bare import binds utils"
      (Ok "1")
      (run (Printf.sprintf "import %s\nutils.public" path)));
  with_named "helpers" {|let public = 2|} (fun path ->
    Alcotest.(check (result string string))
      ".wand is dropped"
      (Ok "2")
      (run (Printf.sprintf "import %s.wand\nhelpers.public"
              (Filename.chop_suffix path ".wand"))))

let test_bare_import_needs_a_name () =
  with_named "json-parser" {|let public = 1|} (fun path ->
    Alcotest.(check (result string string)) "a hyphen becomes _" (Ok "1")
      (run (Printf.sprintf "import %s\njson_parser.public" path)));
  with_named "2fast" {|let public = 1|} (fun path ->
    err_says "not a name" "Write `let fast = import"
      (Printf.sprintf "import %s\n1" path))

let test_two_imports_one_name () =
  err_says "stdlib then path" "`List` is already bound by `import List` (standard library) on line 1"
    "import List\nimport ./List\n1";
  err_says "the fix" "Rename this one: `let my_list = import ./List`"
    "import List\nimport ./List\n1";
  err_says "destructured" "Rename this one: `{parse = my_parse}`"
    "let {parse} = import JSON\nlet {parse} = import TOML\n1";
  err_says "an import and a value" "`List` is already bound by `import List`"
    "import List\nlet List = 3\n1";
  err_says "a value, then an import" "is already bound by `let parse`"
    "let parse = 3\nlet {parse} = import JSON\n1"

let test_two_values_one_name_still_run () =
  Alcotest.(check (result string string)) "values may rebind" (Ok "2")
    (run "let a = 1\nlet a = 2\na")

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

(* A record update names its type by the short name, as a construction does,
   and means the type of its own module. It looked the name up in one table
   for the whole program, where the module loaded last wins, so with two
   modules that each declare `type State` an update in one built the
   other's: a wrong value, or a failure when the fields did not match. *)
let test_update_uses_its_own_module () =
  with_named "upd_a" {|type State(pulls: Int = 0)
let bump (s: State) = State(s, pulls = s.pulls + 1)
let pulls (s: State) = s.pulls
let start () = State()|} (fun a ->
  with_named "upd_b" {|type State(name: String = "someone")
let rename (s: State) = State(s, name = "Bo")
let name (s: State) = s.name
let start () = State()|} (fun b ->
    let prog first second =
      Printf.sprintf {|let %s = import %s
let %s = import %s
(upd_a.pulls (upd_a.bump (upd_a.start ())), upd_b.name (upd_b.rename (upd_b.start ())))|}
        (Filename.remove_extension (Filename.basename first)) (Filename.remove_extension first)
        (Filename.remove_extension (Filename.basename second)) (Filename.remove_extension second)
    in
    Alcotest.(check (result string string))
      "a imported first" (Ok "(1, \"Bo\")") (run (prog a b));
    Alcotest.(check (result string string))
      "b imported first" (Ok "(1, \"Bo\")") (run (prog b a))))

(* `State.decoder` and `State.encoder` in the module that declares `State`
   are that module's. They were looked up by the bare name, where the module
   that declared it last wins, so with two modules that each declare
   `type State` one decoded into the other's (#64). *)
let test_derived_members_use_their_own_module () =
  with_named "dec_a" {|import JSON
type State(pulls: Int = 0)
let load j = JSON.decode State.decoder j
let dump s = JSON.stringify (State.encoder s)
let two = State(pulls = 2)|} (fun a ->
  with_named "dec_b" {|import JSON
type State(name: String = "x")
let load j = JSON.decode State.decoder j
let dump s = JSON.stringify (State.encoder s)|} (fun b ->
    let prog first second =
      Printf.sprintf {|import JSON
let %s = import %s
let %s = import %s
(dec_a.load (JSON.parse! "{\"pulls\": 2}"), dec_a.dump dec_a.two)|}
        (Filename.remove_extension (Filename.basename first)) (Filename.remove_extension first)
        (Filename.remove_extension (Filename.basename second)) (Filename.remove_extension second)
    in
    let want = Ok {|(Ok(State(2)), "{\"pulls\":2}")|} in
    Alcotest.(check (result string string)) "a imported first" want (run (prog a b));
    Alcotest.(check (result string string)) "b imported first" want (run (prog b a))))

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

(* ── A declaration means what its own module meant ─────────────── *)

(* A module's type declarations travel to the importer, and a field that
   named a type through one of the module's own imports was read with the
   importer's names instead. `core` writes `metadata : M.Meta` with
   `let M = import ./meta`; a file that bound the same module as `X` got
   "unknown type 'M.Meta'", and it worked only when both files happened to
   use one alias. Kubernetes types generated one module per group-version
   refer across modules like this in nearly every kind. *)
let with_meta_core core f =
  with_module_dir (fun dir ->
    write_file (Filename.concat dir "meta.wand") "type Meta(name: String)\n";
    write_file (Filename.concat dir "core.wand") core;
    f (Filename.concat dir "meta") (Filename.concat dir "core"))

let test_qualified_field_type_resolves_in_its_module () =
  with_meta_core
    "let M = import ./meta\ntype Pod(metadata: M.Meta, image: String)\n"
    (fun meta core ->
      Alcotest.(check (result string string))
        "the importer's alias for the module does not matter"
        (Ok "web")
        (run (Printf.sprintf {|let X = import %s
let Core = import %s
let p = Core.Pod(metadata = X.Meta(name = "web"), image = "nginx")
p.metadata.name|} meta core));
      Alcotest.(check (result string string))
        "and its derived decoder reads the field as the module's own type"
        (Ok "true")
        (run (Printf.sprintf {|import JSON
let X = import %s
let Core = import %s
let doc = JSON.parse! `{"metadata":{"name":"db"},"image":"pg"}`
match JSON.decode Core.Pod.decoder doc with
  | Ok p -> p == Core.Pod(metadata = X.Meta(name = "db"), image = "pg")
  | Error _ -> false|} meta core)))

(* The same for a name the module selected with a destructuring import: the
   importer has no `Meta` of its own, so the bare name meant nothing. *)
let test_selected_field_type_resolves_in_its_module () =
  with_meta_core
    "let {Meta} = import ./meta\ntype Pod(metadata: Meta, image: String)\n"
    (fun meta core ->
      Alcotest.(check (result string string))
        "a type the module selected is the module's, not the importer's"
        (Ok "web")
        (run (Printf.sprintf {|let X = import %s
let Core = import %s
let p = Core.Pod(metadata = X.Meta(name = "web"), image = "nginx")
p.metadata.name|} meta core));
      Alcotest.(check (result string string))
        "and a file that never imports the module can still decode it"
        (Ok "db")
        (run (Printf.sprintf {|import JSON
let Core = import %s
let doc = JSON.parse! `{"metadata":{"name":"db"},"image":"pg"}`
match JSON.decode Core.Pod.decoder doc with
  | Ok p -> p.metadata.name
  | Error e -> e|} core)))

(* A sum and an alias declared in one module, used by a record in another
   through a qualified import, by a file that imports only the second. This
   is how plimsoll's generated modules use `IntOrString` and `Quantity`. *)
let test_sum_and_alias_fields_across_modules () =
  with_module_dir (fun dir ->
    write_file (Filename.concat dir "p.wand")
      "type IntOrString = I Int | S String\ntype Quantity = String\n";
    write_file (Filename.concat dir "apps.wand")
      "let P = import ./p\n\
       type Pull = Always | Never\n\
       type Strategy(maxSurge: P.IntOrString, cpu: P.Quantity, pull: Pull)\n";
    Alcotest.(check (result string string))
      "decoded and encoded again"
      (Ok {|{"maxSurge":"25%","cpu":"1","pull":"Never"}|})
      (run (Printf.sprintf {|import JSON
let Apps = import %s
let doc = JSON.parse! `{"maxSurge":"25%%","cpu":"1","pull":"Never"}`
match JSON.decode Apps.Strategy.decoder doc with
  | Ok s -> JSON.stringify (Apps.Strategy.encoder s)
  | Error e -> e|} (Filename.concat dir "apps"))))

(* A module whose types share constructor names, used through it:
   `m.T.Ctor` in construction and in patterns, a name only one of its types
   has reached as `m.Ctor`, and a derived encoder and decoder that write the
   bare word. *)
let test_constructors_qualified_through_a_module () =
  with_module_dir (fun dir ->
    write_file (Filename.concat dir "apps.wand")
      "type PullPolicy = Always | Never | IfNotPresent\n\
       type RestartPolicy = Always | OnFailure | Never\n\
       type Container(pull: PullPolicy, restart: RestartPolicy)\n";
    let apps = Filename.concat dir "apps" in
    Alcotest.(check (result string string))
      "built, matched, encoded and decoded"
      (Ok {|always never {"pull":"Always","restart":"Never"} true|})
      (run (Printf.sprintf {|import JSON
let apps = import %s
let c = apps.Container(pull = apps.PullPolicy.Always, restart = apps.RestartPolicy.Never)
let pw p = match p with
  | apps.PullPolicy.Always -> "always"
  | apps.PullPolicy.Never -> "never"
  | apps.IfNotPresent -> "if-not-present"
let rw r = match r with
  | apps.RestartPolicy.Always -> "always"
  | apps.RestartPolicy.OnFailure -> "on-failure"
  | apps.RestartPolicy.Never -> "never"
let out = JSON.stringify (apps.Container.encoder c)
let back = match JSON.decode apps.Container.decoder (JSON.parse! out) with
  | Ok d -> d == c
  | Error _ -> false
"%%{pw c.pull} %%{rw c.restart} %%{out} %%{back}"|} apps));
    match run (Printf.sprintf "let apps = import %s\napps.Always" apps) with
    | Error e ->
      if not (contains e "'Always' is a constructor of both") then
        Alcotest.failf "expected the ambiguity error, got: %s" e
    | Ok v -> Alcotest.failf "a bare shared name through a module built %s" v)

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
      Alcotest.test_case "bare import binds"      `Quick test_bare_user_import_binds;
      Alcotest.test_case "bare import needs a name" `Quick test_bare_import_needs_a_name;
      Alcotest.test_case "two imports, one name"  `Quick test_two_imports_one_name;
      Alcotest.test_case "two values, one name"   `Quick test_two_values_one_name_still_run;
      Alcotest.test_case "let binding works"      `Quick test_explicit_binding_works;
      Alcotest.test_case "destructuring works"    `Quick test_destructured_binding_works;
    ];
    "scope", [
      Alcotest.test_case "module names do not leak" `Quick test_module_names_do_not_leak;
      Alcotest.test_case "only what is named"       `Quick test_destructuring_binds_only_what_it_names;
      Alcotest.test_case "transitive do not leak"   `Quick test_transitive_imports_do_not_leak;
      Alcotest.test_case "constructors cross"       `Quick test_imported_constructors_cross;
      Alcotest.test_case "constructors selected"    `Quick test_imported_constructors_selected;
      Alcotest.test_case "an update uses its own module" `Quick test_update_uses_its_own_module;
      Alcotest.test_case "derived members use their own module" `Quick test_derived_members_use_their_own_module;
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
    "declarations", [
      Alcotest.test_case "a qualified field type is its module's" `Quick
        test_qualified_field_type_resolves_in_its_module;
      Alcotest.test_case "a selected field type is its module's" `Quick
        test_selected_field_type_resolves_in_its_module;
      Alcotest.test_case "sum and alias fields across modules" `Quick
        test_sum_and_alias_fields_across_modules;
      Alcotest.test_case "constructors qualified through a module" `Quick
        test_constructors_qualified_through_a_module;
    ];
  ]
