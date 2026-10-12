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

(* A `:` written for cons is changed to `::`, one at a time, though the
   file does not parse until the last is changed (#117). *)
let test_cons_colon_fixed () =
  let (fixed, applied) =
    fix "let f xs =\n  match xs with\n  | [a : b : t] -> a\n  | (x : y) -> x\n  | [] -> 0\n" in
  Alcotest.(check string) "every ':' is '::'"
    "let f xs =\n  match xs with\n  | [a :: b :: t] -> a\n  | (x :: y) -> x\n  | [] -> 0\n"
    fixed;
  Alcotest.(check (list string)) "three parse fixes"
    ["E-PARSE"; "E-PARSE"; "E-PARSE"] (codes applied)

(* A `:` before a type name may be an annotation in the wrong place, so it
   is refused, not changed. *)
let test_cons_colon_before_a_type_refused () =
  let d = refuse "let f xs =\n  match xs with\n  | [x : Int] -> x\n  | _ -> 0\n" in
  Alcotest.(check bool) "no fix" true (d.Diag.fix = None)

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

(* ── Errors that name one certain change (#139, #140) ───────────────────── *)

(* The fixed text, and that it now typechecks. *)
let fix_clean src =
  let (fixed, applied) = fix src in
  (match Runner.typecheck_source ~path:"wand_fix_test.wand" fixed with
   | Ok _ -> ()
   | Error d ->
     Alcotest.failf "the fixed text does not typecheck: %s\n%s"
       (Diag.legacy d) fixed);
  (fixed, applied)

(* A name from another language's standard library, where the hint names
   the one wand name with the same arguments. *)
let test_foreign_member_renamed () =
  let (fixed, applied) =
    fix_clean "import List\nlet xs = [1, 2]\nlet () = List.iter (fn x -> ()) xs\n" in
  Alcotest.(check string) "List.iter becomes List.each"
    "import List\nlet xs = [1, 2]\nlet () = List.each (fn x -> ()) xs\n" fixed;
  Alcotest.(check (list string)) "by the E-TYPE fix" ["E-TYPE"] (codes applied);
  let (fixed, _) =
    fix_clean "import List\nimport Option\nlet n = Option.unwrap_or 0 (List.head [1])\n" in
  Alcotest.(check string) "Option.unwrap_or becomes Option.default"
    "import List\nimport Option\nlet n = Option.default 0 (List.head [1])\n" fixed;
  (* Into another module: its import is the next fix, and the import that
     nothing uses now goes. *)
  let (fixed, _) = fix_clean "import String\nlet n = String.to_int \"3\"\n" in
  Alcotest.(check string) "String.to_int becomes Int.of_string"
    "import Int\nlet n = Int.of_string \"3\"\n" fixed

(* A hint that names two functions, or a form to write, is a choice for the
   author, and the error keeps only its text. *)
let test_foreign_member_with_a_choice () =
  let d = refuse "import List\nlet x = List.nth [1] 0\n" in
  Alcotest.(check bool) "List.nth names two" true
    (Lint.contains d.Diag.message "List.get (an Option) or List.get!");
  Alcotest.(check bool) "and carries no fix" true (d.Diag.fix = None);
  let d = refuse "import List\nlet x = List.mapi (fn i x -> x) [1]\n" in
  Alcotest.(check bool) "List.mapi takes a function List.indexed does not" true
    (d.Diag.fix = None)

(* A member that differs from the written name only by its last `?` or
   `!` is certain; a guess by spelling is not. *)
let test_suffix_member () =
  let (fixed, _) = fix_clean "import List\nlet b = List.contains 1 [1]\n" in
  Alcotest.(check string) "contains becomes contains?"
    "import List\nlet b = List.contains? 1 [1]\n" fixed;
  let d = refuse "import List\nlet n = List.lenght [1]\n" in
  Alcotest.(check bool) "a guess by spelling is still said" true
    (Lint.contains d.Diag.message "did you mean 'length'?");
  Alcotest.(check bool) "and carries no fix" true (d.Diag.fix = None)

(* A constructor spelled as another language spells it, where one
   constructor is the answer: in an expression and in a pattern. *)
let test_foreign_constructor_renamed () =
  let (fixed, applied) =
    fix_clean "let f x = match x with\n  | Err e -> 1\n  | Ok _ -> 2\nlet y = f (Err 3)\n" in
  Alcotest.(check string) "Err becomes Error, each in its own pass"
    "let f x = match x with\n  | Error e -> 1\n  | Ok _ -> 2\nlet y = f (Error 3)\n" fixed;
  Alcotest.(check (list string)) "two fixes" ["E-TYPE"; "E-TYPE"] (codes applied);
  let (fixed, _) =
    fix_clean "let m = match Just 1 with\n  | Just x -> x\n  | Nothing -> 0\n" in
  Alcotest.(check string) "Just and Nothing"
    "let m = match Some 1 with\n  | Some x -> x\n  | None -> 0\n" fixed;
  let (fixed, _) = fix_clean "let l = match [1] with\n  | Nil -> 0\n  | _ -> 1\n" in
  Alcotest.(check string) "Nil becomes []"
    "let l = match [1] with\n  | [] -> 0\n  | _ -> 1\n" fixed;
  let d = refuse "let r = Left 1\n" in
  Alcotest.(check bool) "Left has two answers, and no fix" true (d.Diag.fix = None)

(* A parameter read only through a field gets its type written where it
   is bound, when one type has the field (#139). *)
let test_parameter_annotated () =
  let (fixed, applied) =
    fix_clean "import IO\nlet succeed r = IO.print r.stdout\nlet () = succeed $?(echo hi)\n" in
  Alcotest.(check string) "on a function's parameter"
    "uses {IO, Shell(echo)}\n\nimport IO\nlet succeed (r: ShellResult) = IO.print r.stdout\nlet () = succeed $?(echo hi)\n"
    fixed;
  Alcotest.(check (list string)) "then the manifest"
    ["E-TYPE"; "V-USES2"] (codes applied);
  let pod = "type Pod(name: String, restarts: Int)\n" in
  let (fixed, _) =
    fix_clean (pod ^ "let show =\n  fn p ->\n    p.restarts\nlet n = show (Pod(name = \"a\", restarts = 1))\n") in
  Alcotest.(check string) "on a lambda's, for a type the file declares"
    (pod ^ "let show =\n  fn (p: Pod) ->\n    p.restarts\nlet n = show (Pod(name = \"a\", restarts = 1))\n")
    fixed;
  let (fixed, _) =
    fix_clean (pod ^ "let total a p b = p.restarts + a + b\nlet n = total 1 (Pod(name = \"a\", restarts = 1)) 2\n") in
  Alcotest.(check string) "among other parameters"
    (pod ^ "let total a (p: Pod) b = p.restarts + a + b\nlet n = total 1 (Pod(name = \"a\", restarts = 1)) 2\n")
    fixed

(* Two types with the field, or a subject that is not a parameter's name,
   is not certain. *)
let test_parameter_not_certain () =
  let d =
    refuse "type A(n: Int)\ntype B(n: Int)\nlet get x = x.n\nlet v = get (A(n = 1))\n" in
  Alcotest.(check bool) "two types have the field" true (d.Diag.fix = None);
  let d = refuse "let get (x, y) = x.stdout\n" in
  Alcotest.(check bool) "a parameter that is a pattern" true (d.Diag.fix = None);
  (* A tuple read by position does not say its size or the types of its
     elements, so no annotation is certain (#144). *)
  let d = refuse "let second p = p.1\n" in
  Alcotest.(check bool) "a tuple index says what to write" true
    (Lint.contains d.Diag.message
       "'p' needs its type before '.1' can be read: write its tuple type, \
        such as '(p: (A, B))'");
  Alcotest.(check bool) "a tuple index carries no fix" true (d.Diag.fix = None)

(* Lines read as one expression get the brackets and the `;` that the
   message shows. *)
let test_lines_bracketed () =
  let (fixed, _) =
    fix_clean "uses {IO}\n\nimport IO\nlet f x = (\n  IO.println \"a\"\n  IO.println \"b\"\n)\nlet () = f 1\n" in
  Alcotest.(check string) "inside brackets"
    "uses {IO}\n\nimport IO\nlet f x = ( IO.println \"a\"; IO.println \"b\" )\nlet () = f 1\n"
    fixed;
  let (fixed, _) =
    fix_clean "uses {IO}\n\nimport IO\nlet () = match Some 1 with\n  | Some n -> IO.println \"a\"\n      IO.println \"b\"\n  | None -> ()\n" in
  Alcotest.(check string) "a body on the arrow's line"
    "uses {IO}\n\nimport IO\nlet () = match Some 1 with\n  | Some n -> ( IO.println \"a\"; IO.println \"b\" )\n  | None -> ()\n"
    fixed;
  (* A comment on one of the lines would hide the rest of the line once the
     lines are one. *)
  let d =
    refuse "uses {IO}\n\nimport IO\nlet () = match Some 1 with\n  | Some n -> IO.println \"a\" -- one\n      IO.println \"b\"\n  | None -> ()\n" in
  Alcotest.(check bool) "a comment among the lines: no fix" true (d.Diag.fix = None)

(* The error a check gives, fix or no fix. *)
let type_error src =
  match Runner.typecheck_source ~path:"wand_fix_test.wand" src with
  | Error d -> d
  | Ok _ -> Alcotest.failf "expected a type error:\n%s" src

(* `String.of_int` is gone (#143). Its hint names `Int.to_string`, and the
   fix changes it; the import of `Int` is the next fix. *)
let test_string_of_int_renamed () =
  let d = type_error "import String\nlet s = String.of_int 3\n" in
  Alcotest.(check bool) "the hint names Int.to_string" true
    (Lint.contains d.Diag.message "use Int.to_string (import Int)");
  let (fixed, _) = fix_clean "import String\nlet s = String.of_int 3\n" in
  Alcotest.(check string) "String.of_int becomes Int.to_string"
    "import Int\nlet s = Int.to_string 3\n" fixed

(* A value given where a String is expected, of a type whose module has
   `to_string` (#143): the error names it, and the fix puts it around the
   value. *)
let test_to_string_around_a_value () =
  let d =
    type_error "import Path\nimport Regex\nlet b = Regex.match? r/^b/ (Path.basename ./a/b.tar)\n" in
  Alcotest.(check bool) "the error names Path.to_string" true
    (Lint.contains d.Diag.message
       "expected String, got Path; Path.to_string gives its text");
  let (fixed, applied) =
    fix_clean "import Path\nimport Regex\nlet b = Regex.match? r/^b/ (Path.basename ./a/b.tar)\n" in
  Alcotest.(check string) "inside the brackets that are there"
    "import Path\nimport Regex\nlet b = Regex.match? r/^b/ (Path.to_string (Path.basename ./a/b.tar))\n"
    fixed;
  Alcotest.(check (list string)) "by the E-TYPE fix" ["E-TYPE"] (codes applied);
  let (fixed, _) =
    fix_clean "import Path\nimport Regex\nlet p = ./a\nlet b = Regex.match? r/^b/ p\n" in
  Alcotest.(check string) "an argument that is a name gets brackets"
    "import Path\nimport Regex\nlet p = ./a\nlet b = Regex.match? r/^b/ (Path.to_string p)\n"
    fixed;
  let (fixed, _) = fix_clean "let n = 3\nlet s = \"n: \" ++ n\n" in
  Alcotest.(check string) "an operand of ++, and then the import"
    "import Int\n\nlet n = 3\nlet s = \"n: \" ++ Int.to_string n\n" fixed;
  let (fixed, _) = fix_clean "import String\nlet n = String.length (-3)\n" in
  Alcotest.(check string) "a value that starts with - keeps its brackets"
    "import Int\nimport String\nlet n = String.length (Int.to_string (-3))\n"
    fixed

(* Not when the extent is not certain, not the other way round, and not
   for a type with no `to_string`. *)
let test_to_string_not_offered () =
  let d =
    refuse "import String\nlet f a b = a ++ b\nlet p = ./x\nlet y = f p p\n" in
  Alcotest.(check bool) "the value stands twice: the hint" true
    (Lint.contains d.Diag.message "Path.to_string gives its text");
  Alcotest.(check bool) "and no fix" true (d.Diag.fix = None);
  let d = refuse "import Path\nlet x = Path.basename \"a\"\n" in
  Alcotest.(check bool) "a String where a Path is expected: no hint" false
    (Lint.contains d.Diag.message "to_string");
  Alcotest.(check bool) "and no fix" true (d.Diag.fix = None);
  let d = refuse "import String\nlet n = String.length r/x/\n" in
  Alcotest.(check bool) "Regex has no to_string: no hint" false
    (Lint.contains d.Diag.message "to_string");
  Alcotest.(check bool) "and no fix" true (d.Diag.fix = None)

(* `List.sum`, `List.max` and `List.min` give an Option (#127). When the
   value at fault is a call to one of them, the error says why. *)
let test_empty_list_hint () =
  let d = type_error "import Int\nimport List\nlet n = Int.abs (List.sum [1, 2])\n" in
  Alcotest.(check bool) "List.sum names its zero" true
    (Lint.contains d.Diag.message
       "expected Int, got Option Int; List.sum gives None for an empty \
        list: add |> Option.default 0");
  let d = type_error "import Float\nimport List\nlet n = Float.abs (List.sum [1.5])\n" in
  Alcotest.(check bool) "the zero of a Float" true
    (Lint.contains d.Diag.message "add |> Option.default 0.0");
  let d = type_error "import List\nimport Size\nlet n = Size.to_string (List.sum [1KB])\n" in
  Alcotest.(check bool) "the zero of a Size" true
    (Lint.contains d.Diag.message "add |> Option.default 0B");
  let d = type_error "import Duration\nimport List\nlet n = Duration.to_string (List.sum [1s])\n" in
  Alcotest.(check bool) "the zero of a Duration" true
    (Lint.contains d.Diag.message "add |> Option.default 0s");
  let d = type_error "import List\nlet b = List.sum [1, 2] > 3\n" in
  Alcotest.(check bool) "an Option is not ordered" true
    (Lint.contains d.Diag.message
       "Option Int is not ordered, so it cannot be compared with < > <= >=; \
        List.sum gives None");
  let d = type_error "import Int\nimport List\nlet n = Int.abs (List.max [1, 2])\n" in
  Alcotest.(check bool) "List.max" true
    (Lint.contains d.Diag.message
       "expected Int, got Option Int; List.max gives None for an empty \
        list: use Option.default or match on it");
  let d = type_error "import Int\nimport List\nlet n = Int.abs (List.min [1, 2])\n" in
  Alcotest.(check bool) "List.min" true
    (Lint.contains d.Diag.message
       "List.min gives None for an empty list: use Option.default or match \
        on it")

(* The fix for `List.sum` adds `|> Option.default <zero>` after the call,
   and then `import Option`. *)
let test_empty_list_sum_fix () =
  let (fixed, applied) =
    fix_clean "import Int\nimport List\nlet n = Int.abs (List.sum [1, 2])\n" in
  Alcotest.(check string) "an argument, inside the brackets that are there"
    "import Int\nimport List\nimport Option\n\nlet n = Int.abs (List.sum [1, 2] |> Option.default 0)\n"
    fixed;
  Alcotest.(check bool) "by the E-TYPE fix" true
    (List.mem "E-TYPE" (codes applied));
  let (fixed, _) =
    fix_clean "import List\ntype Pod(key: String, restarts: Int)\nlet cs = [1, 2]\nlet p = Pod(key = \"a\", restarts = List.sum cs)\n" in
  Alcotest.(check string) "a field of a record gets brackets"
    "import List\nimport Option\n\ntype Pod(key: String, restarts: Int)\nlet cs = [1, 2]\nlet p = Pod(key = \"a\", restarts = (List.sum cs |> Option.default 0))\n"
    fixed;
  let (fixed, _) =
    fix_clean "import List\nlet cs = [1, 2]\nlet b = (cs |> List.sum) > 3\n" in
  Alcotest.(check string) "a pipe into the call"
    "import List\nimport Option\n\nlet cs = [1, 2]\nlet b = (cs |> List.sum |> Option.default 0) > 3\n"
    fixed;
  let (fixed, _) = fix_clean "import List\nlet n = List.sum [1.5] + 1.0\n" in
  Alcotest.(check string) "an operand of +, with the zero of a Float"
    "import List\nimport Option\n\nlet n = (List.sum [1.5] |> Option.default 0.0) + 1.0\n"
    fixed

(* A list that is a parameter: the element type is not known at the error,
   so the type on the other side of the error decides the zero (#127). *)
let test_empty_list_sum_of_a_parameter () =
  let src =
    "import List\ntype Pod(key: String, restarts: Int)\nlet p cs = Pod(key = \"a\", restarts = List.sum cs)\n" in
  let d = type_error src in
  Alcotest.(check bool) "a field of a record: the zero of the field" true
    (Lint.contains d.Diag.message
       "Option ('a: Add) and Int are not the same type; List.sum gives None \
        for an empty list: add |> Option.default 0");
  let (fixed, _) = fix_clean src in
  Alcotest.(check string) "and the fix"
    "import List\nimport Option\n\ntype Pod(key: String, restarts: Int)\nlet p cs = Pod(key = \"a\", restarts = (List.sum cs |> Option.default 0))\n"
    fixed;
  let (fixed, _) =
    fix_clean "import Int\nimport List\nlet f cs = Int.abs (List.sum cs)\n" in
  Alcotest.(check string) "an argument: the type of the parameter"
    "import Int\nimport List\nimport Option\n\nlet f cs = Int.abs (List.sum cs |> Option.default 0)\n"
    fixed;
  let (fixed, _) =
    fix_clean "import List\nlet f cs (thr: Int) = List.sum cs > thr\n" in
  Alcotest.(check string) "a comparison with an Int"
    "import List\nimport Option\n\nlet f cs (thr: Int) = (List.sum cs |> Option.default 0) > thr\n"
    fixed;
  let (fixed, _) = fix_clean "import List\nlet f xs = 1.5 * List.sum xs\n" in
  Alcotest.(check string) "a Float on the other side"
    "import List\nimport Option\n\nlet f xs = 1.5 * (List.sum xs |> Option.default 0.0)\n"
    fixed;
  let d = refuse "import List\nlet f xs y = List.sum xs > y\n" in
  Alcotest.(check bool) "no type on either side: the hint" true
    (Lint.contains d.Diag.message
       "List.sum gives None for an empty list: use Option.default or match \
        on it");
  Alcotest.(check bool) "and no fix" true (d.Diag.fix = None)

(* No fix for `List.max` and `List.min`, no fix where the call stands twice,
   and no hint for an Option that comes from somewhere else. *)
let test_empty_list_not_offered () =
  let d = refuse "import Int\nimport List\nlet n = Int.abs (List.max [1, 2])\n" in
  Alcotest.(check bool) "List.max: no fix" true (d.Diag.fix = None);
  let d = refuse "import Int\nimport List\nlet n = Int.abs (List.min [1, 2])\n" in
  Alcotest.(check bool) "List.min: no fix" true (d.Diag.fix = None);
  let d =
    refuse "import Int\nimport List\nlet xs = [1]\nlet n = Int.max (List.sum xs) (List.sum xs)\n" in
  Alcotest.(check bool) "the call stands twice: the hint" true
    (Lint.contains d.Diag.message "List.sum gives None");
  Alcotest.(check bool) "and no fix" true (d.Diag.fix = None);
  let d = refuse "import Int\nimport List\nlet r = List.sum [1]\nlet n = Int.abs r\n" in
  Alcotest.(check bool) "through a name: no hint" false
    (Lint.contains d.Diag.message "gives None");
  Alcotest.(check bool) "and no fix" true (d.Diag.fix = None);
  let d = refuse "import Int\nlet n = Int.abs (Some 3)\n" in
  Alcotest.(check bool) "an Option of another source: no hint" false
    (Lint.contains d.Diag.message "gives None");
  Alcotest.(check bool) "and no fix" true (d.Diag.fix = None)

(* The engine's test for a substitution at an extent the fix carries: the
   text there must be what the fix says, and the result must parse. *)
let test_replace_at_is_checked () =
  let loc = { Token.file = ""; line = 1; col = 5; offset = 4;
              end_line = 1; end_col = 6; end_offset = 5 } in
  let diag fix = Diag.error ~code:"E-TYPE" ~loc ~fix "x" in
  let lines = ["let x = 1"] in
  let apply from_ to_ =
    Option.map (fun (l, _) -> String.concat "\n" l)
      (Fix.apply_fix lines (diag (Diag.ReplaceAt { at = loc; from_; to_ })))
  in
  Alcotest.(check (option string)) "applied" (Some "let y = 1") (apply "x" "y");
  Alcotest.(check (option string)) "other text there: declined" None (apply "z" "y");
  Alcotest.(check (option string)) "a result that does not parse: declined"
    None (apply "x" "(")

(* `Digest.Sha256` with no `import Digest` (#148). The error is the missing
   import, it carries the line, and nothing in it is the parser's form of
   the expression or the `|>` hint, which is about something else. *)
let test_qualified_constructor_import () =
  let src =
    "import Hash\nimport Path\nimport List\n\n\
     let keyed d rels = List.map (fn r -> (Hash.file! Digest.Sha256 \
     (Path.join d r) |> Digest.hex, Path.to_string r)) rels\n" in
  let d = type_error src in
  let msg = Diag.legacy d in
  Alcotest.(check bool) "names the import" true
    (Lint.contains msg "did you forget to import the standard library Digest?");
  Alcotest.(check bool) "no internal form" false (Lint.contains msg "(@");
  Alcotest.(check bool) "no pipe hint" false (Lint.contains msg "whole call");
  let (fixed, _) = fix_clean src in
  Alcotest.(check bool) "the import is written" true
    (Lint.contains fixed "import Digest\n")

(* The same in a pattern. *)
let test_qualified_pattern_import () =
  let (fixed, _) =
    fix_clean "let name a =\n  match a with\n  | Digest.Sha256 -> \"sha256\"\n  | _ -> \"other\"\n\nname\n" in
  Alcotest.(check bool) "the import is written" true
    (Lint.contains fixed "import Digest\n")

(* `let {Sha256} = Digest` (#149): the module where its import belongs. The
   error has a position and names the import, and the fix writes the
   import on the right side, which is the one correct change. *)
let test_destructured_module_import () =
  let src =
    "import Hash\nimport IO\nimport Digest\nlet {Sha256} = Digest\n\n\
     let main () = IO.println (Hash.string Sha256 \"x\" |> Digest.hex)\n" in
  let missing =
    "import Hash\nimport IO\nlet {Sha256} = Digest\n\n\
     let main () = IO.println (Hash.string Sha256 \"x\")\n" in
  let d = type_error missing in
  Alcotest.(check bool) "names the import" true
    (Lint.contains (Diag.legacy d)
       "did you forget to import the standard library Digest?");
  Alcotest.(check bool) "at a position" true (d.Diag.loc <> None);
  let (fixed, _) = fix_clean missing in
  Alcotest.(check bool) "the right side imports" true
    (Lint.contains fixed "let {Sha256} = import Digest\n");
  (* With the module imported too, the right side is still the import. *)
  let d = type_error src in
  Alcotest.(check bool) "says how names are taken" true
    (Lint.contains (Diag.legacy d) "`let {Sha256} = import Digest`");
  let (fixed, _) = fix_clean src in
  Alcotest.(check bool) "the right side imports" true
    (Lint.contains fixed "let {Sha256} = import Digest\n")

(* Inside a function an import cannot stand, so nothing is certain and no
   fix is carried; the error still names the cause, at a position. *)
let test_destructured_module_local () =
  let d = type_error "import IO\n\nlet main () =\n  let {Sha256} = Digest\n  IO.println \"x\"\n" in
  Alcotest.(check bool) "names the import" true
    (Lint.contains (Diag.legacy d)
       "did you forget to import the standard library Digest?");
  Alcotest.(check bool) "at a position" true (d.Diag.loc <> None);
  Alcotest.(check bool) "no fix" true (d.Diag.fix = None)

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
      Alcotest.test_case "':' before a type" `Quick
        test_cons_colon_before_a_type_refused;
    ];
    "errors that say the fix", [
      Alcotest.test_case "a ':' for cons" `Quick test_cons_colon_fixed;
      Alcotest.test_case "an empty default" `Quick
        test_empty_default_becomes_a_literal;
      Alcotest.test_case "a piped inspect!" `Quick test_piped_inspect_takes_input;
      Alcotest.test_case "a foreign member" `Quick test_foreign_member_renamed;
      Alcotest.test_case "a member with a choice" `Quick
        test_foreign_member_with_a_choice;
      Alcotest.test_case "a member's last ? or !" `Quick test_suffix_member;
      Alcotest.test_case "a foreign constructor" `Quick
        test_foreign_constructor_renamed;
      Alcotest.test_case "a parameter's type" `Quick test_parameter_annotated;
      Alcotest.test_case "a parameter, not certain" `Quick
        test_parameter_not_certain;
      Alcotest.test_case "lines read as one" `Quick test_lines_bracketed;
      Alcotest.test_case "String.of_int" `Quick test_string_of_int_renamed;
      Alcotest.test_case "a value that has to_string" `Quick
        test_to_string_around_a_value;
      Alcotest.test_case "to_string, not offered" `Quick
        test_to_string_not_offered;
      Alcotest.test_case "List.sum, max, min: the hint" `Quick
        test_empty_list_hint;
      Alcotest.test_case "List.sum: the fix" `Quick test_empty_list_sum_fix;
      Alcotest.test_case "List.sum of a parameter" `Quick
        test_empty_list_sum_of_a_parameter;
      Alcotest.test_case "List.sum, max, min: not offered" `Quick
        test_empty_list_not_offered;
      Alcotest.test_case "a replacement is checked" `Quick
        test_replace_at_is_checked;
      Alcotest.test_case "a qualified constructor's import" `Quick
        test_qualified_constructor_import;
      Alcotest.test_case "a qualified pattern's import" `Quick
        test_qualified_pattern_import;
      Alcotest.test_case "names taken from a module" `Quick
        test_destructured_module_import;
      Alcotest.test_case "names taken in a function" `Quick
        test_destructured_module_local;
    ];
    "a function nothing calls", [
      Alcotest.test_case "a bare main! is called" `Quick test_bare_main_is_called;
    ];
    "the words of what a file calls", [
      Alcotest.test_case "count toward its manifest" `Quick test_called_words_count;
      Alcotest.test_case "and its fixes" `Quick test_called_words_fix;
    ];
  ]
