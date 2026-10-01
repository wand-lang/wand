open Wand

(* `wand t --fix` at the engine level: source in, fixed source and the
   applied set out. The same fixes feed the editor's code actions, so what
   these tests lock is the one behavior both consumers share. *)

let fix src =
  match Fix.fix_source ~path:"wand_fix_test.wand" src with
  | Ok (fixed, applied) -> (fixed, applied)
  | Error d -> Alcotest.failf "expected fixes, got refusal: %s" (Diag.legacy d)

let refuse src =
  match Fix.fix_source ~path:"wand_fix_test.wand" src with
  | Error d -> d
  | Ok (fixed, _) -> Alcotest.failf "expected a refusal, got:\n%s" fixed

let codes applied = List.map (fun a -> a.Fix.code) applied

(* The blank line is part of the fix. Every file in the tree stands its
   manifest off from what follows, and so does the formatter, so a manifest
   written against the first import leaves a file that is correct and reads
   as hand-patched. *)
let test_manifest_created () =
  let (fixed, applied) =
    fix "import FS\nlet f p = FS.write_file p \"x\"\nf /tmp/y\n" in
  Alcotest.(check string) "manifest inserted first, with a blank line under it"
    "uses {FS.Write}\n\nimport FS\nlet f p = FS.write_file p \"x\"\nf /tmp/y\n"
    fixed;
  Alcotest.(check (list string)) "one V-USES2" ["V-USES2"] (codes applied)

let test_manifest_after_shebang () =
  let (fixed, _) =
    fix "#!/usr/bin/env wand\nimport FS\nFS.write_file /tmp/x.txt \"hi\"\n" in
  Alcotest.(check string) "shebang stays first"
    "#!/usr/bin/env wand\nuses {FS.Write}\n\nimport FS\nFS.write_file /tmp/x.txt \"hi\"\n"
    fixed

let test_manifest_widened () =
  (* The manifest type error's suggestion, applied from its structured
     fix rather than its prose. *)
  let (fixed, applied) =
    fix "uses {FS.Read}\nimport FS\nFS.write_file /tmp/x.txt \"hi\"\n" in
  Alcotest.(check string) "manifest replaced"
    "uses {FS.Write}\nimport FS\nFS.write_file /tmp/x.txt \"hi\"\n" fixed;
  Alcotest.(check (list string)) "via the E-TYPE fix" ["E-TYPE"] (codes applied)

let test_manifest_narrowed () =
  let (fixed, applied) =
    fix "uses {Shell, FS.Write}\nimport FS\nlet f p = FS.write_file p \"x\"\nf /tmp/y\n" in
  Alcotest.(check string) "unused label dropped"
    "uses {FS.Write}\nimport FS\nlet f p = FS.write_file p \"x\"\nf /tmp/y\n" fixed;
  Alcotest.(check (list string)) "via A-USES1" ["A-USES1"] (codes applied)

(* Widening the Shell list to admit the new word unlocks A-USES1 for the
   binary the file never runs -- the fixed point takes two passes, and the
   engine must find the manifest line even though the shell-word error
   points at the $() that tripped it. *)
let test_shell_fixed_point () =
  let (fixed, applied) =
    fix "uses {Shell(git)}\nlet v = $(curl -s https://x.dev)\nv\n" in
  Alcotest.(check string) "widened, then narrowed"
    "uses {Shell(curl)}\nlet v = $(curl -s https://x.dev)\nv\n" fixed;
  Alcotest.(check (list string)) "both passes reported"
    ["E-TYPE"; "A-USES1"] (codes applied)

let test_dead_import_deleted () =
  let (fixed, applied) =
    fix "import CSV\nlet {parse} = import TOML\nparse \"x = 1\"\n" in
  Alcotest.(check string) "unused import gone"
    "let {parse} = import TOML\nparse \"x = 1\"\n" fixed;
  Alcotest.(check bool) "V-IMP2 among the fixes" true
    (List.mem "V-IMP2" (codes applied))

let test_nothing_to_fix () =
  let src = "let double x = x * 2\ndouble 21\n" in
  let (fixed, applied) = fix src in
  Alcotest.(check string) "unchanged" src fixed;
  Alcotest.(check int) "nothing applied" 0 (List.length applied)

(* A finding without a machine-applicable fix is reported by `wand t` but
   is none of --fix's business. *)
let test_unfixable_finding_left_alone () =
  let src = "let ready? x = x + 1\nready? 2\n" in
  let (fixed, applied) = fix src in
  Alcotest.(check string) "unchanged" src fixed;
  Alcotest.(check int) "nothing applied" 0 (List.length applied)

let test_refuses_parse_error () =
  let d = refuse "let x = (1\n" in
  Alcotest.(check string) "the parse error is reported" "E-PARSE" d.Diag.code

(* A missing import is a correction the checker already knows: the error
   names the module, so the fix is the line the file lacks. *)

let test_import_inserted () =
  let (fixed, applied) = fix "uses {IO}\n\nlet () = IO.println \"hi\"\n" in
  Alcotest.(check string) "under the manifest, with the blank line"
    "uses {IO}\n\nimport IO\n\nlet () = IO.println \"hi\"\n" fixed;
  Alcotest.(check (list string)) "one E-TYPE" ["E-TYPE"] (codes applied)

let test_import_joins_the_run () =
  let src =
    "uses {IO}\nimport IO\nimport Path\n\n     let n = List.length [Path.of_string \"a\"]\nIO.println \"%{n}\"\n" in
  let (fixed, _) = fix src in
  Alcotest.(check string) "in the order the run is kept"
    "uses {IO}\nimport IO\nimport List\nimport Path\n\n     let n = List.length [Path.of_string \"a\"]\nIO.println \"%{n}\"\n"
    fixed

let test_import_before_destructured () =
  let (fixed, _) =
    fix "let {test} = import Test\n\ntest \"x\" (fn t -> t.eq 1 (List.length [1]))\n" in
  Alcotest.(check string) "a plain import goes above a destructured one"
    "import List\n\nlet {test} = import Test\n\ntest \"x\" (fn t -> t.eq 1 (List.length [1]))\n"
    fixed

let test_imports_to_a_fixed_point () =
  let (_, applied) =
    fix "uses {IO}\n\nlet () = IO.println \"%{List.length (Map.keys {a = 1})}\"\n" in
  Alcotest.(check int) "one per pass, three passes" 3 (List.length applied)

let test_refuses_unfixable_type_error () =
  let d = refuse "let x = 1 + true\nx\n" in
  Alcotest.(check string) "the type error is reported" "E-TYPE" d.Diag.code

(* ── A constructor that swallowed an argument ────────────────────────────── *)

(* Parentheses after a constructor are its payload, so `f Nothing (1)` parsed
   as `f (Nothing 1)`. A nullary constructor cannot own the bracket, so the
   checker hands it back to the call around it: the file is what it looks
   like and there is nothing to correct. It used to be an error carrying a
   correction that bracketed the constructor. *)
let test_bare_constructor_needs_no_fix () =
  let (fixed, applied) =
    fix "type Opt = Nothing | Just Int\nlet f a b = b\nlet r = f Nothing (1)\n" in
  Alcotest.(check string) "left exactly as written"
    "type Opt = Nothing | Just Int\nlet f a b = b\nlet r = f Nothing (1)\n" fixed;
  Alcotest.(check (list string)) "nothing reported" [] (codes applied)

(* The qualified spelling too, which is the one a stdlib type gets. The
   arity is declared in the constructor's own module, so the walk has to
   look there. *)
let test_qualified_constructor_needs_no_fix () =
  let src =
    "import Digest\nimport Hash\nlet r = Hash.string Digest.Sha256 (\"a\" ++ \"b\")\n" in
  let (fixed, applied) = fix src in
  Alcotest.(check string) "left exactly as written" src fixed;
  Alcotest.(check (list string)) "nothing reported" [] (codes applied)

(* With no call to hand the argument to there is still an error, and
   bracketing does not answer it -- `(Nothing) (1)` applies it just the same --
   so no correction is carried. *)
let test_payload_with_no_call () =
  let d = refuse "type Opt = Nothing | Just Int\nlet r = Nothing (1)\n" in
  Alcotest.(check bool) "says the one thing there is to say" true
    (Lint.contains (Diag.legacy d) "with nothing after it")

(* In brackets of its own, `f (Nothing (1))` is one argument, and a
   constructor with no payload cannot take the bracket after it there. The
   correction moves the outer bracket onto the payload, which is what
   `wand f` wrote `f Nothing (1)` as before it kept the spelling (#78). *)
let test_bracketed_payload_is_one_argument () =
  let (fixed, applied) =
    fix "type Opt = Nothing | Just Int\nlet f a b = b\nlet r = f (Nothing (1))\n" in
  Alcotest.(check string) "the bracket moves onto the payload"
    "type Opt = Nothing | Just Int\nlet f a b = b\nlet r = f Nothing ((1))\n" fixed;
  Alcotest.(check (list string)) "reported as a type error" ["E-TYPE"] (codes applied)

(* A drift correction names its substitution in prose rather than spanning
   it, so it declines the same test and nothing is written on its behalf. *)
let test_drift_still_declines () =
  let d = refuse "let b = not true\n" in
  Alcotest.(check bool) "refused rather than rewritten" true
    (Lint.contains (Diag.legacy d) "boolean not is")

(* `Map.empty` and `List.empty` as a field default are the literals under
   another spelling, so the literal is written in their place. Anything
   else is a guess about the value, and is refused. *)
let test_empty_default_becomes_a_literal () =
  let (fixed, _) =
    fix "type App(name: String, env: Map String = Map.empty)\nApp(name = \"a\")\n" in
  Alcotest.(check string) "a map"
    "type App(name: String, env: Map String = {})\nApp(name = \"a\")\n" fixed;
  let (fixed, _) =
    fix "type App(name: String, xs: List Int = List.empty)\nApp(name = \"a\")\n" in
  Alcotest.(check string) "a list"
    "type App(name: String, xs: List Int = [])\nApp(name = \"a\")\n" fixed;
  ignore (refuse
    "import List\ntype App(name: String, xs: List Int = List.reverse [])\n1\n")

(* A value piped into `inspect!` has nowhere to go, and `inspect_with!` is
   the same call with stdin. The module is renamed as it was written. *)
let test_piped_inspect_takes_input () =
  let (fixed, applied) =
    fix "uses {Shell(cat)}\n\nimport Shell\n\nlet main! () = \"x\" |> Shell.inspect! $*(cat)\n" in
  Alcotest.(check string) "renamed"
    "uses {Shell(cat)}\n\nimport Shell\n\nlet main! () = \"x\" |> Shell.inspect_with! $*(cat)\n"
    fixed;
  Alcotest.(check (list string)) "via the E-TYPE fix" ["E-TYPE"] (codes applied);
  let (fixed, _) =
    fix "uses {Shell(cat)}\n\nlet S = import Shell\n\nlet main! () = \"x\" |> S.inspect! $*(cat)\n" in
  Alcotest.(check string) "under an alias"
    "uses {Shell(cat)}\n\nlet S = import Shell\n\nlet main! () = \"x\" |> S.inspect_with! $*(cat)\n"
    fixed

(* A script that ends with `main!` and not `main! ()` runs nothing. The fix
   is the `()`, where the name is alone on its line and takes Unit. *)
let test_bare_main_is_called () =
  let (fixed, applied) =
    fix "uses {IO}\n\nimport IO\n\nlet main! () = IO.println \"hi\"\n\nmain!\n" in
  Alcotest.(check string) "the call is written"
    "uses {IO}\n\nimport IO\n\nlet main! () = IO.println \"hi\"\n\nmain! ()\n" fixed;
  Alcotest.(check bool) "by V-DROP3" true (List.mem "V-DROP3" (codes applied))

(* A function from another module runs its own commands, so a script's
   `Shell(...)` list has to hold the words of what it calls -- and only
   those: a function it does not call adds nothing (issue #42). *)
let kube = {|uses {Shell(git, kubectl)}

import Shell

let apply! (s: String) = s |> Shell.inspect_with! $*(kubectl apply -f -)
let version! () = Shell.inspect! $*(git describe)
let both! () = (apply! "x"; version! ())
|}

let dyn = {|uses {Shell}

import Shell

let run! (tool: String) = Shell.inspect! $*(%{tool} --version)
|}

let with_modules f =
  let dir = Filename.temp_dir "wand_words" "" in
  let write name src =
    Out_channel.with_open_text (Filename.concat dir name) (fun oc ->
      output_string oc src)
  in
  write "kube.wand" kube;
  write "dyn.wand" dyn;
  f (Filename.concat dir "script.wand")

let script manifest import call =
  Printf.sprintf "%s\n\n%s\n\nlet main! () = %s\n" manifest import call

let check path src =
  match Runner.typecheck_source ~path src with
  | Ok _ -> None
  | Error d -> Some d.Diag.message

let fix_at path src =
  match Fix.fix_source ~path src with
  | Ok (fixed, _) -> fixed
  | Error d -> Alcotest.failf "expected fixes, got refusal: %s" (Diag.legacy d)

let first_line s = List.hd (String.split_on_char '\n' s)

let test_called_words_count () =
  with_modules @@ fun path ->
  let k = "let K = import ./kube.wand" in
  Alcotest.(check (option string)) "a word the callee runs is allowed" None
    (check path (script "uses {Shell(kubectl)}" k "K.apply! \"x\""));
  Alcotest.(check (option string)) "and through a destructuring" None
    (check path (script "uses {Shell(kubectl)}"
                   "let {apply!} = import ./kube.wand" "apply! \"x\""));
  (match check path (script "uses {Shell(kubectl)}" k "K.both! ()") with
   | Some m when Lint.contains m "'K.both!' runs 'git'" -> ()
   | other ->
     Alcotest.failf "a word the callee's callee runs is required: %s"
       (Option.value other ~default:"no error"));
  (match check path (script "uses {Shell(git)}" "let D = import ./dyn.wand"
                       "D.run! \"git\"") with
   | Some m when Lint.contains m "name is not written out" -> ()
   | other ->
     Alcotest.failf "a command no list bounds cannot be narrowed: %s"
       (Option.value other ~default:"no error"))

let test_called_words_fix () =
  with_modules @@ fun path ->
  let k = "let K = import ./kube.wand" in
  let fixed manifest call = first_line (fix_at path (script manifest k call)) in
  Alcotest.(check string) "a missing word is added"
    "uses {Shell(git, kubectl)}" (fixed "uses {Shell(kubectl)}" "K.both! ()");
  Alcotest.(check string) "a word nothing called runs is removed"
    "uses {Shell(kubectl)}" (fixed "uses {Shell(git, kubectl)}" "K.apply! \"x\"");
  Alcotest.(check string) "a new manifest names the words"
    "uses {Shell(kubectl)}" (first_line (fix_at path
      (Printf.sprintf "%s\n\nlet main! () = K.apply! \"x\"\n" k)));
  Alcotest.(check string) "an unbounded callee needs bare Shell"
    "uses {Shell}"
    (first_line (fix_at path (script "uses {Shell(git)}"
       "let D = import ./dyn.wand" "D.run! \"git\"")))

let () =
  Alcotest.run "fix" [
    "manifest", [
      Alcotest.test_case "created"        `Quick test_manifest_created;
      Alcotest.test_case "after shebang"  `Quick test_manifest_after_shebang;
      Alcotest.test_case "widened"        `Quick test_manifest_widened;
      Alcotest.test_case "narrowed"       `Quick test_manifest_narrowed;
      Alcotest.test_case "shell, 2 passes" `Quick test_shell_fixed_point;
    ];
    "findings", [
      Alcotest.test_case "dead import"    `Quick test_dead_import_deleted;
      Alcotest.test_case "clean file"     `Quick test_nothing_to_fix;
      Alcotest.test_case "no fix carried" `Quick test_unfixable_finding_left_alone;
    ];
    "imports", [
      Alcotest.test_case "inserted"       `Quick test_import_inserted;
      Alcotest.test_case "joins the run"  `Quick test_import_joins_the_run;
      Alcotest.test_case "above destructured" `Quick test_import_before_destructured;
      Alcotest.test_case "to a fixed point" `Quick test_imports_to_a_fixed_point;
    ];
    "a constructor that swallowed an argument", [
      Alcotest.test_case "handed back"    `Quick test_bare_constructor_needs_no_fix;
      Alcotest.test_case "qualified too"  `Quick
        test_qualified_constructor_needs_no_fix;
      Alcotest.test_case "no call to take it" `Quick test_payload_with_no_call;
      Alcotest.test_case "in brackets of its own" `Quick
        test_bracketed_payload_is_one_argument;
      Alcotest.test_case "drift declines" `Quick test_drift_still_declines;
    ];
    "refusals", [
      Alcotest.test_case "parse error"    `Quick test_refuses_parse_error;
      Alcotest.test_case "type error"     `Quick test_refuses_unfixable_type_error;
    ];
    "errors that say the fix", [
      Alcotest.test_case "an empty default" `Quick
        test_empty_default_becomes_a_literal;
      Alcotest.test_case "a piped inspect!" `Quick test_piped_inspect_takes_input;
    ];
    "a function nothing calls", [
      Alcotest.test_case "a bare main! is called" `Quick test_bare_main_is_called;
    ];
    "the words of what a file calls", [
      Alcotest.test_case "count toward its manifest" `Quick test_called_words_count;
      Alcotest.test_case "and its fixes" `Quick test_called_words_fix;
    ];
  ]
