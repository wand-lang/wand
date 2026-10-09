open Wand
open Shell_scan

let word = Alcotest.testable
    (fun fmt -> function
       | Literal w  -> Format.fprintf fmt "Literal %S" w
       | Dynamic    -> Format.fprintf fmt "Dynamic"
       | Compound w -> Format.fprintf fmt "Compound %S" w)
    (=)

let check label text expected =
  Alcotest.(check (list word)) label expected (scan_string text).words

let check_segs label segs expected_words expected_raw =
  let s = scan segs in
  Alcotest.(check (list word)) label expected_words s.words;
  Alcotest.(check bool) (label ^ " raw_tail") expected_raw s.raw_tail

let test_single () =
  check "one word" "git" [Literal "git"];
  check "with args" "git status --short" [Literal "git"];
  check "path-qualified" "/usr/bin/git status" [Literal "/usr/bin/git"];
  check "empty" "" [];
  check "spaces only" "   " []

let test_operators () =
  check "pipeline" "git log | grep fix | wc -l"
    [Literal "git"; Literal "grep"; Literal "wc"];
  check "and chain" "make build && make test"
    [Literal "make"; Literal "make"];
  check "or chain" "test -f x || touch x"
    [Literal "test"; Literal "touch"];
  check "semicolons" "cd /tmp; ls; pwd"
    [Literal "cd"; Literal "ls"; Literal "pwd"];
  check "background" "sleep 5 & echo done"
    [Literal "sleep"; Literal "echo"];
  check "trailing operator" "ls |" [Literal "ls"];
  (* A newline is a command separator, not whitespace. Read as whitespace,
     the word after one was never a command position, so a newline arriving
     through `%!{}` ran a second command the allowlist never saw. *)
  check "newline separates" "echo hi\ntouch x"
    [Literal "echo"; Literal "touch"];
  check "newline after an operator" "echo hi &&\ntouch x"
    [Literal "echo"; Literal "touch"];
  check "newline inside quotes is data" "echo \"hi\ntouch x\""
    [Literal "echo"]

let test_quotes () =
  check "quoted pipe is an argument" "echo \"a | b\" | wc"
    [Literal "echo"; Literal "wc"];
  check "single quotes" "grep 'a && b' log" [Literal "grep"];
  check "escaped space joins a word" "run\\ me now" [Literal "run me"];
  (* A backtick span runs a command of its own, so it is a command position
     and not word text. *)
  check "backtick contents are read" "echo `git rev-parse HEAD`"
    [Literal "echo"; Literal "git"];
  check "a substitution inside double quotes still runs"
    "echo \"today is $(date)\"" [Literal "echo"; Literal "date"];
  check "single quotes stop it" "echo 'not $(date) a command'"
    [Literal "echo"]

let test_assignments () =
  check "prefix assignment" "FOO=1 git status" [Literal "git"];
  check "several prefixes" "A=1 B=2 make" [Literal "make"];
  check "assignment only" "FOO=1" [];
  check "assignment after command is an argument" "env A=1 cmd"
    [Literal "env"];
  (* A substitution in a prefix's value runs, and the command word is still
     to come. Read as a Dynamic command word, `x=$(echo) whoami` hid
     whoami. *)
  check "a substitution in a prefix's value" "X=$(date) git log"
    [Literal "date"; Literal "git"];
  check "the prefix goes on after its substitution" "X=$(date)b git"
    [Literal "date"; Literal "git"];
  (* The shell reads a word as an assignment only when its name is plain. *)
  check "a quoted name is a command" "a'v'=b echo" [Literal "av=b"];
  check "an escaped name is a command" "a\\v=b echo" [Literal "av=b"];
  check_segs "a hole in a name is a command"
    [Lit "a"; QuotedHole; Lit "=b echo"] [Dynamic] false;
  check_segs "a hole in a value is a prefix"
    [Lit "x="; QuotedHole; Lit " git"] [Literal "git"] false

let test_redirections () =
  check "leading redirect" "> /tmp/x echo hi" [Literal "echo"];
  check "fd redirect" "cmd 2> /dev/null | wc" [Literal "cmd"; Literal "wc"];
  check "dup redirect" "cmd 2>&1 | tee log" [Literal "cmd"; Literal "tee"];
  check "append" "echo x >> log" [Literal "echo"]

let test_compounds () =
  (* Precision past the first Compound does not matter -- one is already a
     type error under a narrowed manifest -- so words directly after a
     reserved word read as its arguments, not as commands. *)
  check "for loop" "for f in *.txt; do git add $f; done"
    [Compound "for"; Compound "do"; Compound "done"];
  check "if" "if true; then ls; fi"
    [Compound "if"; Compound "then"; Compound "fi"];
  check "brace group" "{ ls; pwd; }"
    [Compound "{"; Literal "pwd"; Compound "}"];
  (* `time` is a wrapper, not control flow. *)
  check "time wraps" "time git status" [Literal "time"]

(* Everything a line runs is a command position, however deeply the shell
   wraps it. Reading these as opaque was a way past a `Shell(...)` manifest:
   `Shell(echo)` admitted `$(echo $(whoami))`, and whoami ran. *)
let test_subshells () =
  check "an inline substitution is a command"
    "git log $(git merge-base a b)..HEAD" [Literal "git"; Literal "git"];
  check "operators inside a substitution are read there"
    "echo $(ls | wc -l) | cat"
    [Literal "echo"; Literal "ls"; Literal "wc"; Literal "cat"];
  check "a substitution in command position leaves the word dynamic"
    "$(which git) --version" [Dynamic; Literal "which"];
  check "a subshell is a command line of its own"
    "(cd /tmp && ls)" [Literal "cd"; Literal "ls"];
  check "nested substitutions" "echo $(echo $(whoami))"
    [Literal "echo"; Literal "echo"; Literal "whoami"];
  (* `$((...))` is arithmetic: sh evaluates it and runs nothing. *)
  check "arithmetic is not a command" "echo $((1 + 2))" [Literal "echo"];
  check "arithmetic with parens of its own" "echo $(( (3 + 1) / 2 ))"
    [Literal "echo"];
  (* Parentheses that balance before a `))` are not arithmetic to sh: it
     reads `$((whoami) )` as a substitution around a subshell, and runs
     whoami. *)
  check "arithmetic that closes early is a substitution" "echo $((whoami) )"
    [Literal "echo"; Literal "whoami"];
  check "an unclosed arithmetic is a substitution" "echo $((whoami"
    [Literal "echo"; Literal "whoami"];
  (* What a substitution yields is text, so the word it lands in cannot be
     read from the source -- it is checked at spawn instead. *)
  check "a word built from a substitution is dynamic"
    "prefix-$(date) run" [Dynamic; Literal "date"]

let test_holes () =
  check_segs "quoted hole in argument position"
    [Lit "git commit -m "; QuotedHole] [Literal "git"] false;
  check_segs "quoted hole as the command word"
    [QuotedHole; Lit " --version"] [Dynamic] false;
  check_segs "quoted hole fused to a literal"
    [Lit "git-"; QuotedHole; Lit " sub"] [Dynamic] false;
  check_segs "raw hole as the command word"
    [RawHole; Lit " --version"] [Dynamic] true;
  check_segs "raw hole in argument position poisons the tail"
    [Lit "ls "; RawHole; Lit " | rm -rf /"] [Literal "ls"] true;
  check_segs "positions before a raw hole are still read"
    [Lit "git add . && "; RawHole] [Literal "git"; Dynamic] true

let test_render_entry () =
  let bare w = Alcotest.(check string) (w ^ " stays bare") w (render_entry w) in
  let quoted w =
    Alcotest.(check string) (w ^ " needs quotes")
      ("\"" ^ w ^ "\"") (render_entry w)
  in
  bare "git";
  bare "docker-compose";
  bare "node.js";
  bare "g++";
  bare "apt-get";
  quoted "7zip";           (* leading digit lexes as a number *)
  quoted "my tool";        (* whitespace *)
  bare "/opt/bin/deploy";
  bare "demos/08-fan-out/probe.sh";
  bare "./probe.sh";
  quoted "a--b";           (* -- starts a comment *)
  quoted "do";             (* a keyword, not an Ident *)
  quoted "git-do"          (* keyword chunk breaks the chain *)

let test_allowed () =
  let allow = ["git"; "/opt/bin/deploy"] in
  Alcotest.(check bool) "bare entry, bare word" true (allowed ~allow "git");
  Alcotest.(check bool) "bare entry admits a path" true
    (allowed ~allow "/usr/bin/git");
  Alcotest.(check bool) "slash entry is exact" true
    (allowed ~allow "/opt/bin/deploy");
  Alcotest.(check bool) "slash entry rejects other paths" false
    (allowed ~allow "/usr/local/bin/deploy");
  Alcotest.(check bool) "slash entry rejects the basename" false
    (allowed ~allow "deploy");
  Alcotest.(check bool) "unlisted" false (allowed ~allow "curl")

(* ── The spawn-time check, end to end ─────────────────────────────────────
   These live here rather than in test/wand because a dynamic command word
   under a narrowed manifest rightly carries a V-SHELL1 finding, and the
   test/wand corpus must lint clean. *)

let run label src expected =
  match Runner.run_string src with
  | Ok v -> Alcotest.(check string) label expected v
  | Error m -> Alcotest.failf "%s: failed to run: %s" label m

let test_spawn_check () =
  run "an allowed dynamic word spawns"
    "uses {Shell(echo)}\nlet r c = $(%!{c} hi)\nr \"echo\""
    "hi";
  run "a disallowed dynamic word is refused, catchably"
    "uses {Shell(echo)}\nimport String\nlet r c = $(%!{c} hi)\n\
     match try r \"printf\" with\n\
     | Ok _ -> \"ran\"\n\
     | Error why -> if String.contains? \"does not allow\" why then \"refused\" else why"
    "refused";
  run "a word that resolves to control flow is refused"
    "uses {Shell(echo)}\nimport String\nlet r c = $(%!{c} true; echo x; done)\n\
     match try r \"while\" with\n\
     | Ok _ -> \"ran\"\n\
     | Error why -> if String.contains? \"control flow\" why then \"refused\" else why"
    "refused";
  run "a mock intercepts before the spawn check"
    "uses {Shell(git)}\nimport Test\n\
     let go () = let c = \"curl\" in $(%!{c} https://example.com)\n\
     Test.with_shell [(\"curl\", \"mocked\")] go"
    "mocked";
  run "jurisdiction survives Par's forwarding"
    "uses {Shell(echo)}\nimport Par\nimport List\nimport String\n\
     let r c = $(%!{c} hi)\n\
     let outcomes = Par.map 2 r [\"echo\", \"printf\", \"echo\"]\n\
     let show o = match o with | Ok v -> v | Error _ -> \"refused\"\n\
     List.map show outcomes |> (fn ws -> String.join \",\" ws)"
    "hi,refused,hi";
  (* Resolved, the line still has a word that only the shell knows: the
     output of a substitution in command position. *)
  run "a command word made by a substitution is refused"
    "uses {Shell(echo)}\nimport String\nlet r c = $($(echo %{c}))\n\
     match try r \"whoami\" with\n\
     | Ok _ -> \"ran\"\n\
     | Error why -> if String.contains? \"output of a command\" why then \"refused\" else why"
    "refused"

(* The direct-exec fast path: which command lines mean exactly their words,
   so the runner may execvp them instead of paying a shell startup. Every
   refusal is a command the shell must keep -- the classifier can only cost
   speed, never meaning. *)
let check_direct label text expected =
  Alcotest.(check (option (list string))) label expected (direct_words text)

let test_direct_words () =
  check_direct "plain words" "git status --short"
    (Some ["git"; "status"; "--short"]);
  check_direct "path-qualified" "/usr/bin/true" (Some ["/usr/bin/true"]);
  check_direct "tabs separate" "git\tstatus" (Some ["git"; "status"]);
  (* The single quotes %{} interpolation writes: literal spans, adjacent
     segments joining, an empty pair still an argument. *)
  check_direct "quoted argument" "git commit -m 'two words'"
    (Some ["git"; "commit"; "-m"; "two words"]);
  check_direct "quoted metacharacters are literal" "grep '^a|b$' notes.txt"
    (Some ["grep"; "^a|b$"; "notes.txt"]);
  check_direct "adjacent segments join" "tar -C a'b c'd"
    (Some ["tar"; "-C"; "ab cd"]);
  check_direct "an empty quoted argument survives" "run-thing '' x"
    (Some ["run-thing"; ""; "x"]);
  check_direct "equals in an argument is not an assignment"
    "git log --pretty=oneline" (Some ["git"; "log"; "--pretty=oneline"]);
  (* Anything the shell would act on keeps the shell. *)
  check_direct "pipe" "ls | wc" None;
  check_direct "redirect" "ls > out" None;
  check_direct "glob" "ls *.txt" None;
  check_direct "variable" "ls $HOME" None;
  check_direct "substitution" "ls `pwd`" None;
  check_direct "double quotes" "ls \"a b\"" None;
  check_direct "backslash" "ls a\\ b" None;
  check_direct "tilde" "ls ~/x" None;
  check_direct "newline" "ls\nls" None;
  check_direct "unclosed quote" "ls 'a" None;
  check_direct "builtin in command position" "echo hi" None;
  check_direct "reserved word in command position" "if true" None;
  check_direct "assignment prefix" "FOO=1 env" None;
  check_direct "empty" "" None;
  check_direct "spaces only" "   " None

(* Nothing spawns from a String. `$()`, `$?()` and `$*()` are the forms a
   script can write, and each does its quoting where the words were
   written; `Shell.run!` and its kin take the `Command` one of those built,
   which is a type no String can become. The raw process builtins are not in
   a script's scope. If this test fails because a spawn-by-string function
   was added, decide which file's Shell(...) bound governs its commands
   before shipping it -- and note that a String has no bound to carry. *)
let test_no_spawn_by_string () =
  let rejected label src needle =
    match Runner.run_string src with
    | Error m when Lint.contains m needle -> ()
    | Error m -> Alcotest.failf "%s: wrong error: %s" label m
    | Ok v -> Alcotest.failf "%s: expected a rejection, got %s" label v
  in
  (* No builtin takes a command as text. `shell_run` and `shell_query` take
     a `Command`, which only `$(...)` builds, so the quoting is done by the
     literal that wrote it. *)
  rejected "raw builtin" "process_run \"curl x\""
    "unbound variable 'process_run'";
  rejected "Shell module" "import Shell\nShell.run! \"curl x\""
    "expected Command, got String";
  rejected "Proc module" "import Proc\nProc.run \"curl x\""
    "no member 'run'"

(* `$((` is arithmetic only when it closes with `))`. Where the parentheses
   balance first, sh reads a command substitution around a subshell and runs
   what is inside, so the manifest has to see that word. *)
let test_arithmetic_that_closes_early_is_checked () =
  let src =
    "uses {IO, Shell(echo)}\nimport IO\nIO.println $(echo $((whoami) ))" in
  match Runner.run_string src with
  | Error m when Lint.contains m "whoami" -> ()
  | Error m -> Alcotest.failf "wrong error: %s" m
  | Ok v -> Alcotest.failf "expected a refusal, got %s" v

let () =
  Alcotest.run "shell scan" [
    "arithmetic", [
      Alcotest.test_case "closing early is a command" `Quick
        test_arithmetic_that_closes_early_is_checked;
    ];
    "positions", [
      Alcotest.test_case "single"       `Quick test_single;
      Alcotest.test_case "operators"    `Quick test_operators;
      Alcotest.test_case "quotes"       `Quick test_quotes;
      Alcotest.test_case "assignments"  `Quick test_assignments;
      Alcotest.test_case "redirections" `Quick test_redirections;
      Alcotest.test_case "compounds"    `Quick test_compounds;
      Alcotest.test_case "subshells"    `Quick test_subshells;
      Alcotest.test_case "holes"        `Quick test_holes;
    ];
    "allowlist", [
      Alcotest.test_case "matching" `Quick test_allowed;
      Alcotest.test_case "render"   `Quick test_render_entry;
    ];
    "direct exec", [
      Alcotest.test_case "classification" `Quick test_direct_words;
    ];
    "spawn check", [
      Alcotest.test_case "end to end" `Quick test_spawn_check;
      Alcotest.test_case "no spawn by string" `Quick test_no_spawn_by_string;
    ];
  ]
