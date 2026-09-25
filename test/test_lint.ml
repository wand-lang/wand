open Wand

let sess () = Runner.make_session ()

let findings src =
  match Runner.lint_session (sess ()) src with
  | Ok fs -> fs
  | Error m -> Alcotest.failf "lint failed: %s\nsource:\n%s" m src

let codes src = List.map (fun (f : Lint.finding) -> Lint_rules.code f.Lint.rule) (findings src)

let fires label src code =
  let got = codes src in
  if not (List.mem code got) then
    Alcotest.failf "%s: expected %s, got [%s]" label code (String.concat "; " got)

let silent label src =
  match codes src with
  | [] -> ()
  | got -> Alcotest.failf "%s: expected no findings, got [%s]" label (String.concat "; " got)

(* `silent` asks for no findings at all, which is the wrong question when the
   source is meant to trip a different rule. *)
let not_fired label src code =
  let got = codes src in
  if List.mem code got then
    Alcotest.failf "%s: expected no %s, got [%s]" label code (String.concat "; " got)

(* ── Individual rules ────────────────────────────────────────────────────── *)

(* Two imports fighting over one name leave the first binding dead and
   misstate where the name comes from. Renaming one binding keeps both, so
   that's what the message suggests. *)
let test_imp1 () =
  fires "the same name from two modules"
    "let {parse} = import JSON\nlet {parse} = import TOML\nparse \"x = 1\""
    "V-IMP1";
  silent "renamed apart"
    "let {parse = jparse} = import JSON\nlet {parse = tparse} = import TOML\n\
     let _ = jparse \"1\"\ntparse \"x = 1\"";
  (* A use between the two imports reads the *second* one, because imports
     bind before the file's own bindings wherever they are written. So the
     first binding is dead there too, and the rule used to stop at the
     first non-import item and miss it. *)
  fires "a use between imports"
    "let {parse} = import JSON\nlet j = parse \"1\"\n\
     let {parse} = import TOML\nparse \"x = 1\""
    "V-IMP1";
  silent "two modules that share no name"
    "let {parse} = import JSON\nlet j = parse \"1\"\n\
     let {upper} = import String\nupper \"a\""

(* An import that binds nothing the file mentions. The fix deletes the line,
   so the rule stays silent whenever it cannot account for every name -- an
   import brings its module's types and constructors as well as the names it
   says, and which module a type came from is not in the file. *)

let test_imp2 () =
  fires "a namespace nothing calls"
    "uses {IO}\nimport IO\nimport List\nIO.println \"hi\""
    "V-IMP2";
  silent "one that is called"
    "uses {IO}\nimport IO\nimport List\nIO.println \"%{List.length [1]}\"";
  fires "a destructured name nothing uses"
    "import List\nlet {test} = import Test\nList.length [1]"
    "V-IMP2";
  silent "one that is used"
    "let {test} = import Test\ntest \"x\" (fn t -> t.eq 1 1)";
  (* Naming a built-in type is reading the language, not the module that
     used to declare it -- which is what four dead `import Option` lines in
     the standard library were hiding behind. *)
  fires "a module named only as a built-in type"
    "import Option\nimport List\nlet f (x: Option Int) = x\nList.length [1]"
    "V-IMP2";
  (* A type this file did not declare could have come from any of its
     imports, so none of them is reported. *)
  silent "a file that names a type it did not declare"
    "import Test\nimport List\ntype Run(outcome: Test.TestOutcome)\n\
     Run(outcome = Test.Pass \"x\")";
  silent "and a constructor it did not declare"
    "import Test\nimport List\nTest.Pass \"x\""

(* The civil clock steps, so the length between two readings of it is wrong
   or zero on the day it does. The rule catches the shape a script is
   written in -- save a reading, work, subtract -- as well as the inline
   one, and leaves alone the subtraction that is sound: an instant that came
   from somewhere else. *)
let test_clock1 () =
  fires "two readings inline"
    "uses {Clock}\nimport Clock\nClock.now () - Clock.now ()"
    "V-CLOCK1";
  fires "a reading saved and subtracted"
    "uses {Clock}\nimport Clock\n\
     let took = (let before = Clock.now () in Clock.now () - before)\ntook"
    "V-CLOCK1";
  silent "an age, which is what subtraction is for"
    "uses {Clock, FS.Read}\nimport Clock\nimport FS\n\
     Clock.now () - FS.mtime! /etc/hosts";
  silent "a name rebound to something else is not a reading"
    "uses {Clock, FS.Read}\nimport Clock\nimport FS\n\
     let t = (let before = Clock.now () in \
     let before = FS.mtime! /etc/hosts in Clock.now () - before)\nt";
  silent "measuring the way that works"
    "uses {Clock}\nimport Clock\nClock.timed (fn () -> 1 + 1)"

let test_pred1 () =
  fires "non-Bool predicate" "let big? n = n * 2\nbig? 3" "V-PRED1";
  silent "Bool predicate" "let big? n = n > 2\nbig? 3";
  (* Both directions, as `!` has had both since a signature could say whether
     a function raises. The rule was one-directional for a while, and the
     asymmetry is what let `List.all` and `List.any` sit in the standard
     library answering questions without saying so. *)
  fires "Bool without ?" "let positive n = n > 0\npositive 1" "V-PRED3";
  silent "Bool with ?" "let positive? n = n > 0\npositive? 1";
  (* A Bool that is not a function is a value, not a question: `let ready =
     False` is a fact the program holds, and naming it `ready?` would promise
     a caller something to call. *)
  silent "a Bool value is not a predicate" "let ready = 1 > 0\nready";
  (* A name carries one ending, and `has?!` does not parse. So a function
     that returns Bool and can raise has no name that answers both rules,
     and V-PRED3 asked for a `?` the author cannot write. `!` wins: V-BANG1
     says as much in its own message, and the risk is what a caller has to
     see. *)
  silent "a raising Bool takes the ! and keeps it"
    "import List\nlet empty! xs = List.head! xs > 0\nempty! [1]";
  fires "and a ? on one that raises is still wrong"
    "import List\nlet empty? xs = List.head! xs > 0\nempty? [1]"
    "V-BANG1"

(* A contract raises where it fails, and `try` catches it, so a function
   carrying one performs Raise and takes the name every raiser takes. *)
let test_bang1_reads_a_contract () =
  fires "a precondition"
    "let half n =\n  requires n % 2 == 0\n  n / 2\nhalf 8" "V-BANG1";
  fires "a postcondition"
    "let inc n =\n  ensures result > n\n  n + 1\ninc 1" "V-BANG1";
  silent "and the ! settles it"
    "let half! n =\n  requires n % 2 == 0\n  n / 2\nhalf! 8";
  (* It reaches the caller, the way any other raise does. *)
  fires "a caller of one"
    "let half! n =\n  requires n % 2 == 0\n  n / 2\nlet quarter n = half! (half! n)\nquarter 8"
    "V-BANG1"

(* A raise the caller may bring is not one the function performs. `try`
   discharges Raise across a function, so a wrapper asks for a thunk that
   may raise and answers a Result -- and reading that argument row told
   `attempt` to call itself `attempt!`, which is the opposite of what it is.
   It is the version that returns a Result. *)
let test_bang1_ignores_a_demanded_raise () =
  not_fired "a try wrapper is not a raiser"
    "let attempt f = try f ()\nattempt" "V-BANG1";
  (* The rule still reads what the function itself performs. *)
  fires "a real raiser is still named" 
    "import List\nlet first xs = List.head! xs\nfirst [1]" "V-BANG1"

(* A name takes one ending. `ok?!` and `ok!?` are both parse errors, so the
   advice for a predicate that raises cannot be to add the `!`, which is
   what it used to be -- a name the reader could not have written. *)
let test_bang1_on_a_predicate () =
  let msg src =
    match findings src with
    | [] -> Alcotest.fail "expected a finding"
    | fs ->
      (match List.find_opt
               (fun (f : Lint.finding) ->
                  Lint_rules.code f.Lint.rule = "V-BANG1") fs with
       | Some f -> f.Lint.text
       | None   -> Alcotest.fail "expected V-BANG1")
  in
  let m = msg "import List\nlet found? xs = List.head! xs\nfound? [1]" in
  if Lint.contains m "found?!" then
    Alcotest.failf "suggested a name that does not parse:\n%s" m;
  if not (Lint.contains m "found!") then
    Alcotest.failf "expected it to name the alternative:\n%s" m;
  (* The ordinary case keeps the ordinary advice. *)
  let plain = msg "import List\nlet first xs = List.head! xs\nfirst [1]" in
  if not (Lint.contains plain "first!") then
    Alcotest.failf "expected the plain suggestion:\n%s" plain

let test_pred2 () =
  fires "is_ prefix on a ?-named function" "let is_ready? x = x > 1\nis_ready? 2"
    "V-PRED2";
  silent "the bare form" "let ready? x = x > 1\nready? 2";
  (* `is_` on a name without `?` is not this rule's business -- V-PRED3 has
     the missing `?`, and reporting one name twice would say the same thing
     in two voices. *)
  not_fired "no ? suffix" "let is_ready x = x > 1\nis_ready 2" "V-PRED2"

let test_or1 () =
  fires "Result with a Unit error" "let f x : Result Unit Int = Ok x\nf 1" "V-OR1";
  silent "Result with a reason" "let f x : Result String Int = Ok x\nf 1"

let test_name1 () =
  fires "trailing-underscore parameter" "let rename old_ new_ = old_ ++ new_\nrename \"a\" \"b\""
    "V-NAME1";
  silent "ordinary parameters" "let rename src dst = src ++ dst\nrename \"a\" \"b\"";
  (* A bare `_` is a wildcard, not an escaped name. *)
  silent "wildcard parameter" "let f _ = 1\nf 2"

(* Permitting more than the file uses is the safe direction, so it is
   advisory: --strict must not fail a build over caution. *)
let test_uses1 () =
  fires "manifest permits an unused effect"
    "uses {Shell, FS.Write}\nlet x = 1\nx" "A-USES1";
  silent "manifest matching what the file does"
    "uses {Shell}\nlet publish! () = $(rsync -a . host:/srv)\npublish!";
  silent "no manifest at all" "let x = 1\nx";
  (* `uses {}` is not advice: a file that reaches outside itself for nothing
     has nothing to declare, so the line should go rather than shrink. *)
  (match findings "uses {Shell}\nlet x = 1\nx" with
   | [f] ->
     Alcotest.(check bool) "suggests removal, not an empty manifest" true
       (let t = f.Lint.text in
        (not (List.exists (fun sub ->
           let n = String.length sub and m = String.length t in
           let rec at i = i + n <= m && (String.sub t i n = sub || at (i + 1)) in
           at 0) ["uses {}"]))
        && (let sub = "removed" and m = String.length t in
            let n = String.length sub in
            let rec at i = i + n <= m && (String.sub t i n = sub || at (i + 1)) in
            at 0))
   | fs -> Alcotest.failf "expected one finding, got %d" (List.length fs));
  let over = findings "uses {Shell}\nlet x = 1\nx" in
  Alcotest.(check bool) "never fails --strict" false
    (List.exists Lint.fails_strict over)

(* A file that reaches outside itself and says nothing about it. Advisory,
   because a file without a manifest is legal -- but a manifest is only
   worth having if it makes code better, so this is where a file is told
   what better looks like. *)
(* A statement whose value is a Result loses the failure it carries. Nothing
   else reports it: the file typechecks, the script exits 0, and the write
   that did not happen is never mentioned. *)
let test_drop1 () =
  fires "a discarded Result"
    "uses {FS.Write, IO}\nimport FS\nimport IO\nFS.write_file /tmp/x.txt \"hi\"\nIO.println \"done\""
    "V-DROP1";
  (* Binding to `_` says the failure does not matter, which is an answer. *)
  silent "discarded on purpose"
    "uses {FS.Write, IO}\nimport FS\nimport IO\nlet _ = FS.write_file /tmp/x.txt \"hi\"\nIO.println \"done\"";
  (* The `!` sibling raises, so the failure is not lost. *)
  silent "the raising sibling"
    "uses {FS.Write, IO}\nimport FS\nimport IO\nFS.write_file! /tmp/x.txt \"hi\"\nIO.println \"done\"";
  (* Discarding a String is what running a command for its effect looks like,
     so only Results are worth a finding. *)
  silent "a discarded String"
    "uses {Shell, IO}\nimport IO\n$(echo hi)\nIO.println \"done\"";
  (* The last item is the file's value, not something thrown away. *)
  silent "a Result as the file's value"
    "uses {FS.Write}\nimport FS\nFS.write_file /tmp/x.txt \"hi\"";
  (* `(e1; e2)` discards e1 the same way a bare statement does, so the same
     rule watches it. *)
  fires "a Result discarded by `;`"
    "uses {FS.Write}\nimport FS\nlet go () = (FS.write_file /tmp/x.txt \"hi\"; ())\ngo ()"
    "V-DROP1";
  silent "a seq whose value is the Result"
    "uses {FS.Write}\nimport FS\nlet go () = ((); FS.write_file /tmp/x.txt \"hi\")\ngo ()"

(* A test block answers with one outcome, so an assertion sequenced before
   another is thrown away and the test reports a pass however it went. The
   framework cannot notice -- the value is gone before it is asked for -- so
   the rule is the only thing between a green run and a lie. *)
let test_drop2 () =
  fires "an assertion discarded by `;`"
    "let {test} = import Test\ntest \"t\" (fn t -> (t.eq 1 2; t.eq 3 3))"
    "V-DROP2";
  (* Three or more: still one finding per discarded assertion, and the last
     one is the block's answer rather than a discard. *)
  fires "several discarded assertions"
    "let {test} = import Test\n\
     test \"t\" (fn t -> (t.ok true; t.ok false; t.eq 1 1))"
    "V-DROP2";
  (* The ordinary shape: one assertion, returned. *)
  silent "a single assertion"
    "let {test} = import Test\ntest \"t\" (fn t -> t.eq 3 (1 + 2))";
  (* A top-level `test` statement is discarded too, but the runner collects
     those -- that is how the framework is used, not a mistake. *)
  silent "top-level test statements"
    "let {test} = import Test\n\
     test \"a\" (fn t -> t.ok true)\ntest \"b\" (fn t -> t.ok true)";
  (* The remedy the message names has to lint clean, or it is not a remedy. *)
  silent "assertions split across a group"
    "let {test, group} = import Test\n\
     group \"g\" (fn () -> let n = 6 * 7 in [\n\
     test \"a\" (fn t -> t.eq 42 n),\n\
     test \"b\" (fn t -> t.ok (n > 0))])";
  (* Setup before the assertion is ordinary sequencing, not a discard: only
     a discarded TestOutcome is worth a finding. *)
  silent "a non-assertion statement before the assertion"
    "uses {IO}\nlet {test} = import Test\nimport IO\n\
     test \"t\" (fn t -> (IO.println \"setting up\"; t.ok true))"

(* A narrowed Shell with a command word only the run decides: legal, said
   out loud, and an error under --strict. *)
let test_shell1_dynamic () =
  fires "interpolated word under a narrowed manifest"
    "uses {Shell(git), IO}\nimport IO\nlet c = \"git\"\nIO.println $(%!{c} status)"
    "V-SHELL1";
  silent "interpolated word under bare Shell"
    "uses {Shell, IO}\nimport IO\nlet c = \"git\"\nIO.println $(%!{c} status)";
  silent "literal words under a narrowed manifest"
    "uses {Shell(git), IO}\nimport IO\nIO.println $(git status)"

(* Shell(...) entries have the same accounting as effect labels: one no
   command position runs is flagged -- but only when every position is
   literal, because an interpolated one may be exactly where the
   unused-looking binary is spawned. *)
let test_uses1_shell_binaries () =
  fires "an allowlisted binary nothing runs"
    "uses {Shell(git, curl)}\nlet b = $(git status)\nb"
    "A-USES1";
  silent "all binaries earn their place"
    "uses {Shell(git)}\nlet b = $(git status)\nb";
  (let got =
     codes "uses {Shell(git, curl)}\nlet b c = $(%!{c} x)\nb \"git\"" in
   if List.mem "A-USES1" got then
     Alcotest.failf
       "a dynamic site must suspend the unused-binary judgment, got [%s]"
       (String.concat "; " got))

let test_uses2 () =
  fires "effects and no manifest"
    "let publish! () = $(rsync -a . host:/srv)\npublish!" "V-USES2";
  (* Saying so is the whole point, so having said it ends the matter. *)
  silent "the same file, declared"
    "uses {Shell}\nlet publish! () = $(rsync -a . host:/srv)\npublish!";
  silent "a file that reaches outside nothing" "let x = 1\nx";
  (* Raise is not a capability and never appears in a manifest, so a file
     that only raises has nothing it could declare. *)
  silent "raising alone"
    "import List\nlet head! xs = List.get! 0 xs\nhead! [1]";
  (* A file that reaches outside itself and says nothing has no line to be
     checked against, which is the thing the manifest exists to stop. A repo
     running --strict can insist on it, so this is a violation and not the
     advisory it used to be. *)
  let undeclared = findings "let publish! () = $(rsync -a . host:/srv)\npublish!" in
  Alcotest.(check bool) "fails --strict" true
    (List.exists Lint.fails_strict undeclared)

let test_shell1 () =
  fires "multi-stage pipeline"
    "let c = $(git log --oneline | grep fix | wc -l | tr -d \" \")\nc" "A-SHELL1";
  silent "single command" "uses {Shell}\nlet c = $(git status)\nc";
  silent "one pipe" "uses {Shell}\nlet c = $(ls | wc -l)\nc"

(* ── V-SHELL2: a command that runs on to a second line ───────────────────── *)

(* A newline inside `$()` separates two commands, as it does in a shell
   script, so the text below the break runs on its own. A line broken for
   width almost never means that, and the half above the break can have done
   its work before the half below fails. *)
let test_shell2 () =
  fires "a bare newline inside $()"
    "uses {Shell}\nlet a = $(echo one\n  two)\na" "V-SHELL2";
  fires "the same in $?()"
    "uses {Shell}\nlet a = $?(echo one\n  two)\na" "V-SHELL2";
  (* The literal parts of an interpolated command are checked too. *)
  fires "with a hole in it"
    "uses {Shell}\nlet n = \"x\"\nlet a = $(echo %{n}\n  two)\na" "V-SHELL2";
  (* `\` is the shell's continuation and wand passes it through, so this is
     one command and says nothing. *)
  silent "continued with a backslash"
    "uses {Shell}\nlet a = $(echo one \\\n  two)\na";
  silent "one line" "uses {Shell}\nlet a = $(echo one two)\na"

(* ── Classification ──────────────────────────────────────────────────────── *)

(* Only must-fix rules may fail a build. An advisory one that could fail it
   would teach its audience to ignore every rule beside it. *)
let test_kinds () =
  Alcotest.(check bool) "V-PRED1 must be fixed" true
    (Lint_rules.kind Lint_rules.V_PRED1 = Lint_rules.Violation);
  Alcotest.(check bool) "A-SHELL1 is advisory" true
    (Lint_rules.kind Lint_rules.A_SHELL1 = Lint_rules.Advisory);
  (* The two halves of the manifest differ. A manifest wider than the file
     is imprecise; a file with no manifest declares nothing at all. *)
  Alcotest.(check bool) "A-USES1 is advisory" true
    (Lint_rules.kind Lint_rules.A_USES1 = Lint_rules.Advisory);
  Alcotest.(check bool) "V-USES2 must be fixed" true
    (Lint_rules.kind Lint_rules.V_USES2 = Lint_rules.Violation);
  (* Declared, so the only finding left is the advisory one. Without the
     manifest this snippet also trips V-USES2, which does fail --strict, and
     the check would be measuring that instead. *)
  let shell = findings "uses {Shell}\nlet c = $(a | b | c | d)\nc" in
  Alcotest.(check bool) "an advisory finding never fails --strict" false
    (List.exists Lint.fails_strict shell)

(* Every rule in the catalog has a distinct code, so a message can always be
   traced back to the reference entry that documents it. *)
let test_registry_codes_unique () =
  let codes = List.map (fun (r : Lint_rules.rule) -> r.Lint_rules.code) Lint_rules.all in
  let sorted = List.sort compare codes in
  let rec dup = function
    | a :: (b :: _ as tl) -> if a = b then Some a else dup tl
    | _ -> None
  in
  (match dup sorted with
   | Some c -> Alcotest.failf "duplicate rule code: %s" c
   | None -> ());
  List.iter (fun c ->
    match Lint_rules.of_code c with
    | Some _ -> ()
    | None -> Alcotest.failf "code %s does not round-trip to a rule" c) codes

(* ── The stdlib is the audience's example ────────────────────────────────── *)

(* Rules that the standard library itself violates are rules nobody will
   believe. This is what caught FS.rename's old_/new_ parameters. *)
let test_stdlib_is_clean () =
  let dir = "../stdlib" in
  if not (Sys.file_exists dir) then
    Alcotest.failf "stdlib not found at %s (relative to test sandbox)" dir
  else
    Array.iter (fun name ->
      if Filename.check_suffix name ".wand" then begin
        let src = In_channel.with_open_text (Filename.concat dir name) In_channel.input_all in
        match Runner.lint_module_source src with
        | Error m -> Alcotest.failf "%s failed to lint: %s" name m
        | Ok [] -> ()
        | Ok fs ->
          Alcotest.failf "%s has lint findings:\n%s" name
            (String.concat "\n" (List.map Lint.to_text fs))
      end
    ) (Sys.readdir dir)

(* Everything else written in wand: the tests, the demos, the examples, and
   wand's own CI script. The stdlib check above covered the library only, so
   a manifest permitting what a test file did not use, or a function that
   could raise without saying so, sat there warning and nothing failed. Eleven
   of them had, across files nobody had linted since writing them.

   The exceptions are demo files that are supposed to be wrong: a demo whose
   point is an error has to contain one, and each of these is asserted by its
   own run.sh. *)
let expected_findings =
  [ ("backup.wand", "V-USES2"); ("backup-phoning-home.wand", "V-USES2");
    (* V-SHADOW1 discourages binding a top-level name twice. What the
       evaluator does when someone binds one anyway is still a fact, and
       `test_script.wand` is where that fact is asserted -- so the file has
       to contain the thing the rule reports. Lint is advice; the nearest
       binding winning is behaviour. *)
    ("test_script.wand", "V-SHADOW1") ]

let expected_type_errors =
  [ (* D1: the same script bash would run, which wand will not. *)
    "unsafe.wand";
    (* D4: a manifest narrower than the code, which is the demo. *)
    "backup-bounded.wand" ]

let rec wand_files dir =
  Sys.readdir dir |> Array.to_list
  |> List.concat_map (fun entry ->
       let path = Filename.concat dir entry in
       if Sys.is_directory path then wand_files path
       else if Filename.check_suffix entry ".wand" then [ path ]
       else [])

let test_corpus_is_clean () =
  let roots = List.filter Sys.file_exists [ "wand"; "../demos"; "../examples"; "../ci" ] in
  let files = List.concat_map wand_files roots in
  Alcotest.(check bool) "found files to lint" true (List.length files > 20);
  List.iter
    (fun path ->
      let name = Filename.basename path in
      match Runner.typecheck_file path with
      | Error _ when List.mem name expected_type_errors -> ()
      | Error d -> Alcotest.failf "%s failed to typecheck: %s" path (Diag.legacy d)
      | Ok sc ->
        let findings = sc.Runner.sc_findings in
        let unexpected =
          List.filter
            (fun (f : Lint.finding) ->
              not (List.mem (name, Lint_rules.code f.Lint.rule) expected_findings))
            findings
        in
        if unexpected <> [] then
          Alcotest.failf "%s has lint findings:\n%s" path
            (String.concat "\n" (List.map Lint.to_text unexpected)))
    files

(* The same modules through the path a person uses. `lint_module_source`
   above is the library call; this is `wand t <file>`, which has to reach
   them too -- a module body calls the raw builtins, and checked as a script
   it fails on the first one. Without this the standard library could only
   be checked by importing it, so a module could go wrong in a way nobody
   would see until something used it. *)
let test_stdlib_typechecks_through_the_tool () =
  let dir = "../stdlib" in
  if not (Sys.file_exists dir) then
    Alcotest.failf "stdlib not found at %s (relative to test sandbox)" dir
  else
    Array.iter (fun name ->
      if Filename.check_suffix name ".wand" then
        match Runner.typecheck_file (Filename.concat dir name) with
        | Error d -> Alcotest.failf "%s does not typecheck as a module: %s" name (Diag.legacy d)
        | Ok { Runner.sc_findings = []; _ } -> ()
        | Ok sc ->
          Alcotest.failf "%s has findings:\n%s" name
            (String.concat "\n" (List.map Lint.to_text sc.Runner.sc_findings))
    ) (Sys.readdir dir)

(* And the boundary the same rule protects: a script cannot reach past a
   module to the builtin underneath it. *)
let test_a_script_cannot_call_builtins () =
  match Runner.run_string "fs_temp_file \"x\" \".txt\"" with
  | Error _ -> ()
  | Ok s -> Alcotest.failf "a script reached a raw builtin, got: %s" s

(* ── The doc/lint bridge ─────────────────────────────────────────────────── *)

(* A rule the reference does not document is a rule its audience cannot look
   up; an ID the reference cites that no longer exists sends them looking for
   something gone. Both directions are checked, because prose and enforcement
   drifting apart is exactly what rule IDs exist to prevent. *)
(* ── The stdlib the tools know about ─────────────────────────────────────── *)

(* `stdlib_module_names` drives which modules `wand d` will import to answer
   about, which `wand d` lists, and which unknown name gets "did you forget
   to import". A module on disk but missing from the list still imports and
   runs -- it just goes invisible to the tools. `Test` sat that way with
   unreachable doc strings until someone asked `wand d` about it. *)
let test_every_stdlib_module_is_listed () =
  let dir = "../stdlib" in
  if not (Sys.file_exists dir) then
    Alcotest.failf "stdlib not found at %s (relative to test sandbox)" dir;
  let on_disk =
    Sys.readdir dir
    |> Array.to_list
    |> List.filter_map (fun f ->
         if Filename.check_suffix f ".wand" then Some (Filename.remove_extension f) else None)
    |> List.sort compare
  in
  let listed = List.sort compare Wand.Typechecker.stdlib_module_names in
  let missing = List.filter (fun m -> not (List.mem m listed)) on_disk in
  let extra   = List.filter (fun m -> not (List.mem m on_disk)) listed in
  if missing <> [] then
    Alcotest.failf "on disk but not in stdlib_module_names: %s" (String.concat ", " missing);
  if extra <> [] then
    Alcotest.failf "in stdlib_module_names but not on disk: %s" (String.concat ", " extra)

let reference_path = "../docs/reference.md"

let reference_text () =
  if not (Sys.file_exists reference_path) then
    Alcotest.failf "reference not found at %s (relative to test sandbox)" reference_path;
  In_channel.with_open_text reference_path In_channel.input_all

let contains hay nee =
  let hn = String.length hay and nn = String.length nee in
  if nn > hn then false
  else begin
    let found = ref false in
    for i = 0 to hn - nn do
      if String.sub hay i nn = nee then found := true
    done; !found
  end

let test_every_rule_is_documented () =
  let text = reference_text () in
  List.iter (fun (r : Lint_rules.rule) ->
    if not (contains text r.Lint_rules.code) then
      Alcotest.failf "rule %s is not documented in %s" r.Lint_rules.code reference_path
  ) Lint_rules.all

let test_every_documented_id_exists () =
  let text = reference_text () in
  (* Every V-/A- token in the reference must name a rule in the catalog. *)
  let n = String.length text in
  let i = ref 0 in
  while !i < n - 1 do
    if (text.[!i] = 'V' || text.[!i] = 'A') && text.[!i + 1] = '-' then begin
      let j = ref (!i + 2) in
      while !j < n && (let c = text.[!j] in
                       (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9')) do incr j done;
      let code = String.sub text !i (!j - !i) in
      (* A rule code ends in a number; prose ranges like `A-Z` do not. *)
      let ends_in_digit =
        String.length code > 0 &&
        (let c = code.[String.length code - 1] in c >= '0' && c <= '9')
      in
      if String.length code > 2 && ends_in_digit
         && Lint_rules.of_code code = None then
        Alcotest.failf "%s cites rule %s, which is not in the catalog"
          reference_path code;
      i := !j
    end else incr i
  done

(* A top-level name bound twice. The finding lands on the second binding --
   the first is not dead, which is the whole reason the rule exists -- and
   the message names the line the first is on so a reader can go and look. *)
let test_shadow1 () =
  (* The middle binding answers an Int, not a Bool: V-PRED3 would fire on a
     `check` that answered one, and a fixture about V-SHADOW1 should carry
     one finding. *)
  let src = "let limit = 100\nlet check = fn n -> n + limit\nlet limit = 500" in
  fires "a second binding of one name" src "V-SHADOW1";
  let f =
    List.find (fun (f : Lint.finding) -> f.Lint.rule = Lint_rules.V_SHADOW1)
      (findings src)
  in
  Alcotest.(check int) "reports the second binding" 3 f.Lint.loc.Token.line;
  if not (Lint.contains f.Lint.text "line 1") then
    Alcotest.failf "the message should name the first binding's line: %s" f.Lint.text;
  (* Renaming is the correction the message asks for, and it silences it. *)
  silent "renamed apart"
    "let limit = 100\nlet check = fn n -> n + limit\nlet ceiling = 500\ncheck ceiling"

(* `let _ =` says a dropped failure does not matter. Where the value is Unit
   there is no failure, so the binder does nothing.

   Every shape is checked here because the rule shipped with none, and the
   one it could not see is the one that mattered: a value on the line below
   the binder. That went unreported on a file's own spine -- and a top-level
   `let _ =` takes the statements under it as its body, so the whole tail of
   a file rode along inside it, unmentioned. *)
let test_bind1 () =
  let at src line =
    let f =
      List.find (fun (f : Lint.finding) -> f.Lint.rule = Lint_rules.A_BIND1)
        (findings src)
    in
    Alcotest.(check int) "reports the binder's own line" line
      f.Lint.loc.Token.line
  in
  let same_line = "import IO

let _ = IO.println \"a\"

IO.println \"b\"" in
  fires "a value on the binder's line" same_line "A-BIND1";
  at same_line 3;
  let next_line = "import IO

let _ =
  IO.println \"a\"

IO.println \"b\"" in
  fires "a value on the line below" next_line "A-BIND1";
  at next_line 3;
  (* Indented past the eight columns `let _ = ` occupies, which is where the
     reconstructed position used to land on a line the binder does not sit
     on at all. *)
  let far_in = "import IO

let _ =
            IO.println \"a\"

IO.println \"b\"" in
  fires "a value indented past the binder" far_in "A-BIND1";
  at far_in 3;
  (* Inside a body, where the statements would run together if the binder
     simply came off. Reported, and the message asks for the `;`. *)
  let in_body =
    "import IO

let go () = (
  let _ = IO.println \"a\";
  IO.println \"b\"
)
go ()"
  in
  fires "a binder inside a block" in_body "A-BIND1";
  (* A `Result` is what the binder is for, and saying so is not a finding. *)
  not_fired "a dropped Result keeps its binder"
    "import FS

let _ = FS.copy /a /b

FS.exists? /a" "A-BIND1"

(* A top-level `let _ =` is a binding like any other: the statements below it
   are items of their own, not its body. Read as a `let ... in`, one file's
   last three statements came back as a single parenthesized block. *)
let test_a_top_level_wildcard_binds_one_value () =
  let src = "import IO

let _ =
  IO.println \"a\"

IO.println \"b\"

IO.println \"c\"" in
  let prog = Lexer.tokenize src |> Parser.parse_program in
  Alcotest.(check int) "four items, not one" 4 (List.length prog.Ast.items);
  (* `let _ = e in body` is still an expression, and still one item. *)
  let inline = Lexer.tokenize "let r = let _ = 1 in 2
r" |> Parser.parse_program in
  Alcotest.(check int) "a let-in is one binding and its use" 2
    (List.length inline.Ast.items)

(* An inner binding shadows freely. It is visible on one screen, which is
   exactly what two top-level bindings cannot be assumed to be. *)
let test_shadow1_is_top_level_only () =
  not_fired "an inner let" "let x = 1\nlet f = fn () -> let x = 2 in x\nf ()" "V-SHADOW1"

(* `_` is the name for a value that is deliberately not read, so a file may
   have as many as it likes. *)
let test_shadow1_ignores_underscore () =
  not_fired "two discards" "let _ = 1\nlet _ = 2\n3" "V-SHADOW1"

(* Two imports binding one name belong to V-IMP1, which reports the earlier
   line because that binding really is dead. Reporting both would put two
   findings on one mistake and point them at different lines. *)
let test_shadow1_leaves_imports_to_imp1 () =
  fires "two imports, one name"
    "let {parse} = import JSON\nlet {parse} = import TOML\nparse \"x = 1\""
    "V-IMP1";
  not_fired "two imports, one name"
    "let {parse} = import JSON\nlet {parse} = import TOML\nparse \"x = 1\""
    "V-SHADOW1"

let () =
  Alcotest.run "Lint" [
    "rules", [
      Alcotest.test_case "V-PRED1"  `Quick test_pred1;
      Alcotest.test_case "V-PRED2"  `Quick test_pred2;
      Alcotest.test_case "V-BANG1 on a predicate" `Quick
        test_bang1_on_a_predicate;
      Alcotest.test_case "V-BANG1 ignores a demanded raise" `Quick
        test_bang1_ignores_a_demanded_raise;
      Alcotest.test_case "V-BANG1 reads a contract" `Quick
        test_bang1_reads_a_contract;
      Alcotest.test_case "V-OR1"    `Quick test_or1;
      Alcotest.test_case "V-NAME1"  `Quick test_name1;
      Alcotest.test_case "V-DROP1"  `Quick test_drop1;
      Alcotest.test_case "V-DROP2"  `Quick test_drop2;
      Alcotest.test_case "V-IMP1"   `Quick test_imp1;
      Alcotest.test_case "V-IMP2"   `Quick test_imp2;
      Alcotest.test_case "V-CLOCK1" `Quick test_clock1;
      Alcotest.test_case "A-SHELL1" `Quick test_shell1;
      Alcotest.test_case "A-USES1"  `Quick test_uses1;
      Alcotest.test_case "A-USES1 binaries" `Quick test_uses1_shell_binaries;
      Alcotest.test_case "V-USES2"  `Quick test_uses2;
      Alcotest.test_case "V-SHELL1" `Quick test_shell1_dynamic;
      Alcotest.test_case "A-BIND1"  `Quick test_bind1;
      Alcotest.test_case "A-BIND1 top-level binder" `Quick
        test_a_top_level_wildcard_binds_one_value;
      Alcotest.test_case "V-SHADOW1" `Quick test_shadow1;
      Alcotest.test_case "V-SHADOW1 top level only" `Quick
        test_shadow1_is_top_level_only;
      Alcotest.test_case "V-SHADOW1 ignores _" `Quick
        test_shadow1_ignores_underscore;
      Alcotest.test_case "V-SHADOW1 leaves imports to V-IMP1" `Quick
        test_shadow1_leaves_imports_to_imp1;
    ];
    "catalog", [
      Alcotest.test_case "V-SHELL2"     `Quick test_shell2;
      Alcotest.test_case "kinds"        `Quick test_kinds;
      Alcotest.test_case "unique codes" `Quick test_registry_codes_unique;
    ];
    "stdlib", [
      Alcotest.test_case "lints clean" `Quick test_stdlib_is_clean;
      Alcotest.test_case "the corpus lints clean" `Quick test_corpus_is_clean;
      Alcotest.test_case "checks through the tool" `Quick test_stdlib_typechecks_through_the_tool;
      Alcotest.test_case "scripts cannot call builtins" `Quick test_a_script_cannot_call_builtins;
    ];
    "reference", [
      Alcotest.test_case "documents every rule" `Quick test_every_rule_is_documented;
      Alcotest.test_case "cites only real rules" `Quick test_every_documented_id_exists;
    ];
    "module list", [
      Alcotest.test_case "matches the stdlib directory" `Quick test_every_stdlib_module_is_listed;
    ];
  ]
