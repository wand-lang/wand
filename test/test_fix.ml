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
    "import Hash\nlet r = Hash.string Hash.Sha256 (\"a\" ++ \"b\")\n" in
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

(* The names other libraries give a top-n selection (#121). A name that
   says which end it keeps is renamed; 'top', 'min_by' and 'max_by' are
   only said. *)
let test_top_n_members () =
  let renamed src want =
    let (fixed, _) = fix_clean src in
    Alcotest.(check string) want want fixed
  in
  renamed "import List\nlet x = List.nlargest 2 (fn x -> x) [1]\n"
    "import List\nlet x = List.greatest_by 2 (fn x -> x) [1]\n";
  renamed "import List\nlet x = List.largest_by 2 (fn x -> x) [1]\n"
    "import List\nlet x = List.greatest_by 2 (fn x -> x) [1]\n";
  renamed "import List\nlet x = List.k_largest 2 (fn x -> x) [1]\n"
    "import List\nlet x = List.greatest_by 2 (fn x -> x) [1]\n";
  renamed "import List\nlet x = List.nsmallest 2 (fn x -> x) [1]\n"
    "import List\nlet x = List.smallest_by 2 (fn x -> x) [1]\n";
  renamed "import List\nlet x = List.least_by 2 (fn x -> x) [1]\n"
    "import List\nlet x = List.smallest_by 2 (fn x -> x) [1]\n";
  renamed "import List\nlet x = List.k_smallest 2 (fn x -> x) [1]\n"
    "import List\nlet x = List.smallest_by 2 (fn x -> x) [1]\n";
  renamed
    "import Stream\nlet x = Stream.nlargest 2 (fn x -> x) (Stream.of_list [1])\n"
    "import Stream\nlet x = Stream.greatest_by 2 (fn x -> x) (Stream.of_list [1])\n";
  renamed
    "import Stream\nlet x = Stream.k_smallest 2 (fn x -> x) (Stream.of_list [1])\n"
    "import Stream\nlet x = Stream.smallest_by 2 (fn x -> x) (Stream.of_list [1])\n";
  List.iter (fun (src, says) ->
    let d = refuse src in
    Alcotest.(check bool) ("says " ^ says) true (Lint.contains d.Diag.message says);
    Alcotest.(check bool) "and carries no fix" true (d.Diag.fix = None))
    [ "import List\nlet x = List.top 2 (fn x -> x) [1]\n",
      "List.greatest_by n f xs for the n greatest";
      "import List\nlet x = List.min_by (fn x -> x) [1]\n",
      "List.smallest_by 1 f xs";
      "import Stream\nlet x = Stream.max_by (fn x -> x) (Stream.of_list [1])\n",
      "Stream.greatest_by 1 f xs" ]

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
   is bound, when one type has the field and nothing in the file gives the
   type (#139). A use further down that gives it settles the read with no
   annotation at all (#193), so these have none. *)
let test_parameter_annotated () =
  let (fixed, applied) =
    fix_clean "import IO\nlet succeed r = IO.print r.stdout\n" in
  Alcotest.(check string) "on a function's parameter"
    "uses {IO}\n\nimport IO\nlet succeed (r: ShellResult) = IO.print r.stdout\n"
    fixed;
  Alcotest.(check (list string)) "then the manifest"
    ["E-TYPE"; "V-USES2"] (codes applied);
  let pod = "type Pod(name: String, restarts: Int)\n" in
  let (fixed, _) = fix_clean (pod ^ "let show =\n  fn p ->\n    p.restarts\n") in
  Alcotest.(check string) "on a lambda's, for a type the file declares"
    (pod ^ "let show =\n  fn (p: Pod) ->\n    p.restarts\n")
    fixed;
  let (fixed, _) = fix_clean (pod ^ "let total a p b = p.restarts + a + b\n") in
  Alcotest.(check string) "among other parameters"
    (pod ^ "let total a (p: Pod) b = p.restarts + a + b\n")
    fixed

(* Two types with the field, or a subject that is not a parameter's name,
   is not certain. *)
let test_parameter_not_certain () =
  let d = refuse "type A(n: Int)\ntype B(n: Int)\nlet get x = x.n\n" in
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

(* `Regex.capture` gives an Option since #126; it gave `[]` for no match.
   A use written for the List does not typecheck, and the fix adds
   `|> Option.default []`, which keeps what the use meant: a match always
   has the whole match first, so the answer is never `Some []`. *)
let old_captures = {|import List
import Regex
let host line =
  match Regex.capture r/^(\w+)@(\w+)$/ line with
  | [_, _, h] -> h
  | _ -> "none"
let count s = List.length (Regex.capture r/\d+/ s)
let first s = s |> Regex.capture r/(\d+)/ |> List.head
let groups = List.length (Regex.capture r/(a)(b)?/ "a")
let missed = Regex.capture r/x/ "abc" == []
(host "u@h", host "nope", count "abc", count "a1", first "x9", first "x", groups, missed)
|}

let test_capture_hint () =
  let d = type_error old_captures in
  Alcotest.(check bool) "the match names the change" true
    (Lint.contains d.Diag.message
       "expected Option (List String), got List 'a; Regex.capture gives \
        None when the pattern does not match: match on Some and None, or \
        add |> Option.default []")

let test_capture_fix () =
  let (fixed, applied) = fix_clean old_captures in
  Alcotest.(check string) "each old use gets Option.default []"
    {|import List
import Option
import Regex
let host line =
  match (Regex.capture r/^(\w+)@(\w+)$/ line |> Option.default []) with
  | [_, _, h] -> h
  | _ -> "none"
let count s = List.length (Regex.capture r/\d+/ s |> Option.default [])
let first s = (s |> Regex.capture r/(\d+)/ |> Option.default []) |> List.head
let groups = List.length (Regex.capture r/(a)(b)?/ "a" |> Option.default [])
let missed = (Regex.capture r/x/ "abc" |> Option.default []) == []
(host "u@h", host "nope", count "abc", count "a1", first "x9", first "x", groups, missed)
|}
    fixed;
  Alcotest.(check bool) "by the E-TYPE fix" true
    (List.mem "E-TYPE" (codes applied));
  Alcotest.(check (result string string)) "and it runs as the old code did"
    (Ok {|("h", "none", 0, 1, Some("9"), None, 3, true)|})
    (Runner.run_string fixed)

(* A use that typechecks with the Option is left as it is. *)
let test_capture_new_use_left () =
  let src = {|import Regex
let f s =
  match Regex.capture r/(a)/ s with
  | Some [_, a] -> a
  | _ -> ""
let g s = Regex.capture r/b/ s
|} in
  let (fixed, applied) = fix src in
  Alcotest.(check string) "nothing changes" src fixed;
  Alcotest.(check (list string)) "and nothing is applied" [] (codes applied)

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

(* `Hash.Sha256` with no `import Hash` (#148). The error is the missing
   import, it carries the line, and nothing in it is the parser's form of
   the expression or the `|>` hint, which is about something else. *)
let test_qualified_constructor_import () =
  let src =
    "import Path\nimport List\n\n\
     let keyed d rels = List.map (fn r -> (Hash.file! Hash.Sha256 \
     (Path.join d r) |> Hash.hex, Path.to_string r)) rels\n" in
  let d = type_error src in
  let msg = Diag.legacy d in
  Alcotest.(check bool) "names the import" true
    (Lint.contains msg "did you forget to import the standard library Hash?");
  Alcotest.(check bool) "no internal form" false (Lint.contains msg "(@");
  Alcotest.(check bool) "no pipe hint" false (Lint.contains msg "whole call");
  let (fixed, _) = fix_clean src in
  Alcotest.(check bool) "the import is written" true
    (Lint.contains fixed "import Hash\n")

(* The same in a pattern. *)
let test_qualified_pattern_import () =
  let (fixed, _) =
    fix_clean "let name a =\n  match a with\n  | Hash.Sha256 -> \"sha256\"\n  | _ -> \"other\"\n\nname\n" in
  Alcotest.(check bool) "the import is written" true
    (Lint.contains fixed "import Hash\n")

(* `let {Sha256} = Hash` (#149): the module where its import belongs. The
   error has a position and names the import, and the fix writes the
   import on the right side, which is the one correct change. *)
let test_destructured_module_import () =
  let src =
    "import IO\nimport Hash\nlet {Sha256} = Hash\n\n\
     let main () = IO.println \"%{Sha256}\"\n" in
  let missing =
    "import IO\nlet {Sha256} = Hash\n\n\
     let main () = IO.println \"%{Sha256}\"\n" in
  let d = type_error missing in
  Alcotest.(check bool) "names the import" true
    (Lint.contains (Diag.legacy d)
       "did you forget to import the standard library Hash?");
  Alcotest.(check bool) "at a position" true (d.Diag.loc <> None);
  let (fixed, _) = fix_clean missing in
  Alcotest.(check bool) "the right side imports" true
    (Lint.contains fixed "let {Sha256} = import Hash\n");
  (* With the module imported too, the right side is still the import. *)
  let d = type_error src in
  Alcotest.(check bool) "says how names are taken" true
    (Lint.contains (Diag.legacy d) "`let {Sha256} = import Hash`");
  let (fixed, _) = fix_clean src in
  Alcotest.(check bool) "the right side imports" true
    (Lint.contains fixed "let {Sha256} = import Hash\n")

(* Inside a function an import cannot stand, so nothing is certain and no
   fix is carried; the error still names the cause, at a position. *)
let test_destructured_module_local () =
  let d = type_error "import IO\n\nlet main () =\n  let {Sha256} = Hash\n  IO.println \"x\"\n" in
  Alcotest.(check bool) "names the import" true
    (Lint.contains (Diag.legacy d)
       "did you forget to import the standard library Hash?");
  Alcotest.(check bool) "at a position" true (d.Diag.loc <> None);
  Alcotest.(check bool) "no fix" true (d.Diag.fix = None)

(* A bare constructor of a standard library module (#153). One module has
   it, so the module's name is the one correct change; the import is the
   next fix when the file has none. *)
let test_bare_stdlib_constructor () =
  let src = "import Hash\nimport Path\nlet f! p = Hash.file! Sha256 p\n" in
  let d = type_error src in
  Alcotest.(check bool) "names the module" true
    (Lint.contains (Diag.legacy d) "'Sha256' is 'Hash.Sha256'");
  Alcotest.(check (option (pair int int))) "at the constructor" (Some (3, 23))
    (Option.map (fun (l : Token.loc) -> (l.Token.line, l.Token.col)) d.Diag.loc);
  let (fixed, applied) = fix_clean src in
  Alcotest.(check bool) "qualified" true
    (Lint.contains fixed "Hash.file! Hash.Sha256 p\n");
  Alcotest.(check bool) "and imported" true (Lint.contains fixed "import Hash\n");
  Alcotest.(check bool) "the qualification is a fix of its own" true
    (List.exists (fun a -> Lint.contains a.Fix.note "\"Hash.Sha256\"") applied)

(* The same in a pattern, with the module imported already, and for a
   constructor with a payload. *)
let test_bare_stdlib_constructor_pattern () =
  let (fixed, _) =
    fix_clean "import Hash\nlet name a =\n  match a with\n  | Sha256 -> \"s\"\n  | _ -> \"o\"\n\nname\n" in
  Alcotest.(check bool) "the pattern is qualified" true
    (Lint.contains fixed "  | Hash.Sha256 -> \"s\"\n");
  let (fixed, _) = fix_clean "let x = Pass \"a\"\n" in
  Alcotest.(check bool) "with a payload" true
    (Lint.contains fixed "let x = Test.Pass \"a\"\n");
  Alcotest.(check bool) "and imported" true (Lint.contains fixed "import Test\n")

(* A name that no standard library module has as a constructor stays an
   unknown constructor with no fix. *)
let test_bare_unknown_constructor () =
  let d = refuse "let x = Sha257\n" in
  Alcotest.(check bool) "no module named" false
    (Lint.contains (Diag.legacy d) "Hash.");
  Alcotest.(check bool) "no fix" true (d.Diag.fix = None)

(* ── Missing imports, known from the parse (#150) ───────────────────────── *)

let issue_150 =
  "import FS\nimport Path\n\
   let f! p = Int.to_string (FS.size! p) ++ String.upper (Hash.hex (Hash.file! Hash.Sha256 p))\n"

let import_fixes ds =
  List.filter_map (fun (d : Diag.t) ->
    match d.Diag.fix with
    | Some (Diag.InsertLine t) -> Some t
    | _ -> None) ds

let position (d : Diag.t) =
  match d.Diag.loc with
  | Some l -> Printf.sprintf "%d:%d" l.Token.line l.Token.col
  | None -> "none"

(* Every missing import is reported with the first error, once each, at
   its first use, and each carries its line. *)
let test_missing_imports_reported () =
  let ds = Diag.all (type_error issue_150) in
  Alcotest.(check (list string)) "each import, once"
    ["import Int"; "import String"; "import Hash"] (import_fixes ds);
  Alcotest.(check (list string)) "at the first use of each"
    ["3:12"; "3:42"; "3:56"]
    (List.filter_map (fun (d : Diag.t) ->
       match d.Diag.fix with
       | Some (Diag.InsertLine _) -> Some (position d)
       | _ -> None) ds);
  Alcotest.(check int) "the module the check found is not said twice" 1
    (List.length (List.filter (fun (d : Diag.t) ->
       Lint.contains d.Diag.message "standard library Int?") ds))

(* With a type error first, the missing imports come with it. *)
let test_missing_imports_with_type_error () =
  let ds = Diag.all (type_error "let y = 1 + \"a\"\nlet x = List.map\n") in
  Alcotest.(check bool) "the type error first" true
    (Lint.contains (List.hd ds).Diag.message "not the same type");
  Alcotest.(check (list string)) "and the import" ["import List"]
    (import_fixes ds)

(* One run adds them all, though the type error stays. The imports are
   kept and the error comes back with them. *)
let test_missing_imports_fixed_in_one_run () =
  let o = Fix.fix_outcome ~path:"wand_fix_test.wand" issue_150 in
  Alcotest.(check string) "every import, in order"
    "import FS\nimport Hash\nimport Int\nimport Path\nimport String\n\n\
     let f! p = Int.to_string (FS.size! p) ++ String.upper (Hash.hex (Hash.file! Hash.Sha256 p))\n"
    o.Fix.text;
  Alcotest.(check int) "three fixes" 3 (List.length o.Fix.fixes);
  (match o.Fix.refused with
   | Some d ->
     Alcotest.(check bool) "the type error remains" true
       (Lint.contains d.Diag.message "expected Int, got Size")
   | None -> Alcotest.fail "expected the type error");
  (* `fix_source` refuses as it did. *)
  ignore (refuse issue_150)

(* With nothing else wrong, the file ends correct. *)
let test_missing_imports_then_clean () =
  let (fixed, _) =
    fix_clean "let f p = Path.to_string p |> String.upper\n\
               let g xs = List.map (fn x -> Int.to_string x) xs\n" in
  Alcotest.(check string) "all four, then the code"
    "import Int\nimport List\nimport Path\nimport String\n\n\
     let f p = Path.to_string p |> String.upper\n\
     let g xs = List.map (fn x -> Int.to_string x) xs\n" fixed

(* A name of the file's own, a built-in type's constructor and a field
   default are not missing imports. A name taken from a module is not the
   module. *)
let test_missing_imports_not_reported () =
  let names src = import_fixes (Diag.all (type_error src)) in
  Alcotest.(check (list string)) "a type of the file's own" []
    (names "type Digest = A | B\nlet x = Digest.A\nlet y = 1 + \"a\"\n");
  Alcotest.(check (list string)) "a constructor of Option" []
    (names "let x = Option.Some 1\nlet y = 1 + \"a\"\n");
  Alcotest.(check (list string)) "a field default" []
    (names "type App(env: Map String = Map.empty)\n");
  Alcotest.(check (list string)) "a name taken from the module" ["import Hash"]
    (names "let {hex} = import Hash\nlet f x = Hash.hex x\nlet y = 1 + \"a\"\n");
  Alcotest.(check (list string)) "a type and a pattern" ["import Hash"]
    (names "let f (x: Hash.Algorithm) =\n  match x with\n  | Hash.Sha256 -> 1\n  | _ -> 2\n")

(* A list in `%!{...}` would read each element as shell source. The one
   form that gives the elements as arguments is `%{...}`, and the fix
   writes it -- the space inside the brace is kept. *)
let test_list_in_a_raw_splice () =
  let (fixed, applied) =
    fix "uses {Shell(printf)}\nlet xs = [\"a\"]\nlet r = $(printf \"[%s]\" %!{ xs })\n" in
  Alcotest.(check string) "%!{ becomes %{"
    "uses {Shell(printf)}\nlet xs = [\"a\"]\nlet r = $(printf \"[%s]\" %{ xs })\n"
    fixed;
  Alcotest.(check (list string)) "via the E-TYPE fix" ["E-TYPE"] (codes applied)

(* A `%!{...}` over a Path is V-SHELL4, and the fix is the quoted splice,
   blanks inside the braces and all. A String raw splice stays (#123). *)
let test_raw_splice_quoted () =
  let (fixed, applied) =
    fix "uses {Shell}\nlet f! (p: Path) (s: String) = $(ls %!{ p } %!{s})\n\
         f! /tmp \"-l\"\n" in
  Alcotest.(check string) "the Path is quoted"
    "uses {Shell}\nlet f! (p: Path) (s: String) = $(ls %{ p } %!{s})\n\
     f! /tmp \"-l\"\n"
    fixed;
  Alcotest.(check (list string)) "via V-SHELL4" ["V-SHELL4"] (codes applied)

(* A bracket against a constructor is its field list, and only named fields
   have one (#207). Each wrong spelling carries the one form the
   declaration gives, and `--fix` writes it. *)
let test_constructor_brackets () =
  let decls =
    "type Pod(name: String, tries: Int = 3)\ntype W = Wrap Int\n\
     type P = Pair (Int, Int)\ntype R = Rect Int Int\ntype C = Red | Green\n\
     let v = 3\nlet name = \"x\"\n"
  in
  List.iter (fun (label, wrong, right) ->
    let (fixed, _) = fix (decls ^ wrong ^ "\n") in
    Alcotest.(check string) label (decls ^ right ^ "\n") fixed)
    [ ("a payload", "let a = Wrap(1)", "let a = Wrap (1)");
      ("a name", "let a = Wrap(v)", "let a = Wrap v");
      ("Some", "let a = Some(v)", "let a = Some v");
      ("a tuple payload", "let a = Pair(1, 2)", "let a = Pair (1, 2)");
      ("two fields", "let a = Rect(1, v + 1)", "let a = Rect 1 (v + 1)");
      ("two fields, spaced", "let a = Rect (2, 3)", "let a = Rect 2 3");
      ("the unit", "let a = Ok()", "let a = Ok ()");
      ("nothing", "let a = Red()", "let a = Red");
      ("named fields after a space", "let a = Pod (name = \"a\")", "let a = Pod(name = \"a\")");
      ("a pun after a space", "let a = Pod (name)", "let a = Pod(name)");
      ("a pattern", "let f x = match x with | Wrap(n) -> n", "let f x = match x with | Wrap n -> n");
      ("a tuple pattern", "let f x = match x with | Pair(p, q) -> p + q",
       "let f x = match x with | Pair (p, q) -> p + q");
      ("a pattern of two fields", "let f x = match x with | Rect(p, q) -> p + q",
       "let f x = match x with | Rect p q -> p + q");
      ("a nested pattern", "let f x = match x with | Some(Wrap(z)) -> z | _ -> 0",
       "let f x = match x with | Some (Wrap z) -> z | _ -> 0");
      ("a named pattern after a space", "let f x = match x with | Pod (name = n) -> n",
       "let f x = match x with | Pod(name = n) -> n");
      ("a pun pattern after a space", "let f x = match x with | Pod (name) -> name",
       "let f x = match x with | Pod(name) -> name") ]

(* A declaration says by its bracket which kind of field list it has, so
   the parser refuses the wrong spacing, with the fix (#207). *)
let test_declaration_brackets () =
  List.iter (fun (label, wrong, right) ->
    let (fixed, _) = fix (wrong ^ "\n") in
    Alcotest.(check string) label (right ^ "\n") fixed)
    [ ("named fields after a space", "type Pod (name: String)", "type Pod(name: String)");
      ("after a parameter", "type Box 'a (item: 'a)", "type Box 'a(item: 'a)");
      ("in a variant", "type N = Leaf (v: Int) | Branch (l: N, r: N)",
       "type N = Leaf(v: Int) | Branch(l: N, r: N)");
      ("a positional field against the name", "type W = Wrap(List Int)", "type W = Wrap (List Int)");
      ("a tuple against the name", "type P = Pair(Int, Int)", "type P = Pair (Int, Int)");
      ("the shorthand", "type S(Int, Int)", "type S (Int, Int)") ]

(* `fun` and `when` were read silently; they are errors with the fix now,
   one word each (#212). *)
let test_earlier_spellings () =
  List.iter (fun (label, wrong, right) ->
    let (fixed, _) = fix (wrong ^ "\n") in
    Alcotest.(check string) label (right ^ "\n") fixed)
    [ ("fun", "let f = fun x -> x + 1", "let f = fn x -> x + 1");
      ("fun in an argument", "let g xs = [1, 2] |> (fun x -> x)", "let g xs = [1, 2] |> (fn x -> x)");
      ("when", "let f x = match x with | n when n > 0 -> 1 | _ -> 0",
       "let f x = match x with | n if n > 0 -> 1 | _ -> 0");
      ("both, and twice", "let f = fun x -> match x with | n when n > 0 -> fun y -> y | _ -> fun y -> y",
       "let f = fn x -> match x with | n if n > 0 -> fn y -> y | _ -> fn y -> y") ]

(* A `_` in a field list made the bracket a tuple, and the error was about
   how many arguments the constructor takes. A field from another
   language's name for it has the wand name written (#214). *)
let test_field_list_names () =
  List.iter (fun (label, wrong, right) ->
    let (fixed, _) = fix ("uses {Shell}\ntype Pod(name: String, tries: Int)\n" ^ wrong ^ "\n") in
    Alcotest.(check string) label
      ("uses {Shell}\ntype Pod(name: String, tries: Int)\n" ^ right ^ "\n") fixed)
    [ ("a _ is left out", "let f p = (let Pod(name, _) = p in name)",
       "let f p = (let Pod(name) = p in name)");
      ("on a built-in record", "let g () = (let ShellResult(stdout, _, code) = $?(echo hi) in code)",
       "let g () = (let ShellResult(stdout, code) = $?(echo hi) in code)");
      ("exit_code is code", "let h () = (let r = $?(echo hi) in r.exit_code)",
       "let h () = (let r = $?(echo hi) in r.code)");
      ("inside a wider expression", "let h () = (let r = $?(echo hi) in r.exit_code + r.code)",
       "let h () = (let r = $?(echo hi) in r.code + r.code)") ]

(* `Digest` was folded into `Hash` (#215). Every place a file names it is
   found at once, inside `%{...}` too, and one run rewrites them all; an
   import that would bind `Hash` twice goes. *)
let test_digest_is_hash () =
  let src =
    "uses {IO}\n\nimport Digest\nimport Hash\nimport IO\nlet {Sha256} = import Digest\n\n\
     let h = Hash.string Digest.Sha256 \"abc\" |> Digest.hex\n\
     let f (d: Digest.Digest) = d.hex\n\
     let main () = IO.println \"%{Digest.base64 (Hash.string Sha256 h)} %{f (Hash.string Sha256 h)}\"\n" in
  let (fixed, _) = fix_clean src in
  Alcotest.(check string) "every place, in one run"
    "uses {IO}\n\nimport Hash\nimport IO\nlet {Sha256} = import Hash\n\n\
     let h = Hash.string Hash.Sha256 \"abc\" |> Hash.hex\n\
     let f (d: Hash.Hash) = d.hex\n\
     let main () = IO.println \"%{Hash.base64 (Hash.string Sha256 h)} %{f (Hash.string Sha256 h)}\"\n" fixed;
  (* A name of the file's own is left alone. *)
  let own = "type Digest = A | B\nlet x = Digest.A\n" in
  let (fixed, _) = fix own in
  Alcotest.(check string) "a type of the file's own" own fixed

(* `Decode.run` reads text and never parses JSON (#216). A decoder that
   reads an object or a list fails on every text, so where one can be seen
   the call is an error, and the fix puts `JSON.parse` in front of it --
   through a name, a type's own decoder and a pipe. *)
let test_decode_run_on_json () =
  let src =
    "import Decode\n\n\
     type Rec(endpoint: String, ms: Int)\n\n\
     let ms = Decode.field \"ms\" Decode.int\n\
     let pair = Decode.map2 (fn e m -> (e, m)) (Decode.field \"endpoint\" Decode.string) ms\n\
     let one line = Decode.run pair line\n\
     let two line = line |> Decode.run Rec.decoder\n\
     let three line = Decode.run (Decode.list Decode.int) (String.trim line)\n" in
  let (fixed, _) = fix_clean src in
  Alcotest.(check string) "every call, with its imports"
    "import Decode\nimport JSON\nimport Result\nimport String\n\n\
     type Rec(endpoint: String, ms: Int)\n\n\
     let ms = Decode.field \"ms\" Decode.int\n\
     let pair = Decode.map2 (fn e m -> (e, m)) (Decode.field \"endpoint\" Decode.string) ms\n\
     let one line = Result.and_then (JSON.decode pair) (JSON.parse line)\n\
     let two line = line |> JSON.parse |> Result.and_then (JSON.decode Rec.decoder)\n\
     let three line = Result.and_then (JSON.decode (Decode.list Decode.int)) (JSON.parse (String.trim line))\n"
    fixed;
  (* A decoder that reads text is left alone, and so is one the check
     cannot see. *)
  let fine =
    "import Decode\n\n\
     let a = Decode.run (Decode.map (fn n -> n + 1) Decode.int) \"1\"\n\
     let b d = Decode.run d \"1\"\n\
     let pair = Decode.field \"ms\" Decode.int\n\
     let c pair = Decode.run pair \"1\"\n" in
  let (fixed, _) = fix fine in
  Alcotest.(check string) "a text decoder" fine fixed

let () =
  Alcotest.run "fix" [
    "a list in a command", [
      Alcotest.test_case "raw splice" `Quick test_list_in_a_raw_splice;
    ];
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
      Alcotest.test_case "a top-n member" `Quick test_top_n_members;
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
      Alcotest.test_case "Regex.capture: the hint" `Quick test_capture_hint;
      Alcotest.test_case "Regex.capture: the fix" `Quick test_capture_fix;
      Alcotest.test_case "Regex.capture: a new use is left" `Quick
        test_capture_new_use_left;
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
      Alcotest.test_case "a bare stdlib constructor" `Quick
        test_bare_stdlib_constructor;
      Alcotest.test_case "a bare stdlib constructor in a pattern" `Quick
        test_bare_stdlib_constructor_pattern;
      Alcotest.test_case "a constructor of no module" `Quick
        test_bare_unknown_constructor;
    ];
    "missing imports", [
      Alcotest.test_case "all are reported" `Quick test_missing_imports_reported;
      Alcotest.test_case "with a type error" `Quick
        test_missing_imports_with_type_error;
      Alcotest.test_case "one run adds them all" `Quick
        test_missing_imports_fixed_in_one_run;
      Alcotest.test_case "then the file is correct" `Quick
        test_missing_imports_then_clean;
      Alcotest.test_case "what is not one" `Quick
        test_missing_imports_not_reported;
    ];
    "a function nothing calls", [
      Alcotest.test_case "a bare main! is called" `Quick test_bare_main_is_called;
    ];
    "a raw splice of data", [
      Alcotest.test_case "becomes a quoted one" `Quick test_raw_splice_quoted;
    ];
    "the words of what a file calls", [
      Alcotest.test_case "count toward its manifest" `Quick test_called_words_count;
      Alcotest.test_case "and its fixes" `Quick test_called_words_fix;
    ];
    "a bracket against a constructor (#207)", [
      Alcotest.test_case "takes the declaration's form" `Quick test_constructor_brackets;
      Alcotest.test_case "and a declaration takes its own" `Quick test_declaration_brackets;
    ];
    "an earlier spelling of a keyword (#212)", [
      Alcotest.test_case "fun and when" `Quick test_earlier_spellings;
    ];
    "a module folded into another (#215)", [
      Alcotest.test_case "Digest is Hash" `Quick test_digest_is_hash;
    ];
    "a field list names its fields (#214)", [
      Alcotest.test_case "and the fix writes them" `Quick test_field_list_names;
    ];
    "a text decoder given JSON (#216)", [
      Alcotest.test_case "the fix parses it first" `Quick test_decode_run_on_json;
    ];
  ]
