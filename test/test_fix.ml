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

(* A drift correction names its substitution in prose rather than spanning
   it, so it declines the same test and nothing is written on its behalf. *)
let test_drift_still_declines () =
  let d = refuse "let b = not true\n" in
  Alcotest.(check bool) "refused rather than rewritten" true
    (Lint.contains (Diag.legacy d) "boolean not is")

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
      Alcotest.test_case "drift declines" `Quick test_drift_still_declines;
    ];
    "refusals", [
      Alcotest.test_case "parse error"    `Quick test_refuses_parse_error;
      Alcotest.test_case "type error"     `Quick test_refuses_unfixable_type_error;
    ];
  ]
