open Wand

(* The REPL's tab completion, tested as the pure function it now is --
   line in, candidates out. Before the extraction this logic could only be
   exercised through linenoise, which needs a pty. *)

let contains_str l s = List.mem s l

(* A session with the stdlib imported, the way `wand i` starts. *)
let env () =
  let sess = Runner.make_session () in
  match Runner.run_session sess "import List\nlet frobnicate x = x" with
  | Ok (s, _) -> s.Runner.s_type_env
  | Error m -> Alcotest.failf "session setup failed: %s" m

let test_bare_ident () =
  let { Complete.start; candidates } = Complete.ident_at (env ()) "frob" in
  Alcotest.(check int) "starts at the beginning" 0 start;
  Alcotest.(check bool) "finds the binding" true
    (contains_str candidates "frobnicate")

let test_builtins_without_env () =
  let { Complete.candidates; _ } = Complete.ident_at [] "O" in
  Alcotest.(check bool) "Ok" true (contains_str candidates "Ok")

let test_namespace_member () =
  let { Complete.start; candidates } =
    Complete.ident_at (env ()) "let xs = List.fil" in
  Alcotest.(check int) "starts after the space" 9 start;
  Alcotest.(check bool) "List.filter" true
    (contains_str candidates "List.filter")

let test_unknown_namespace () =
  let { Complete.candidates; _ } = Complete.ident_at (env ()) "Nope.x" in
  Alcotest.(check (list string)) "nothing to offer" [] candidates

let test_two_dots () =
  let { Complete.candidates; _ } = Complete.ident_at (env ()) "a.b.c" in
  Alcotest.(check (list string)) "nothing to offer" [] candidates

let test_whole_line_rebuilt () =
  let lines = Complete.line_completions (env ()) "let xs = List.fil" in
  Alcotest.(check bool) "the candidate replaces only the identifier" true
    (contains_str lines "let xs = List.filter")

let test_command_names () =
  let lines = Complete.line_completions [] ":e" in
  Alcotest.(check bool) ":edit" true (contains_str lines ":edit");
  Alcotest.(check bool) "no unrelated command" false (contains_str lines ":load");
  (* `:env` is retired. It still answers with a pointer to `:d`, and it is
     not offered: completing to a command whose whole reply is "use the
     other one" wastes the reader's keystroke. *)
  let lines = Complete.line_completions [] ":" in
  Alcotest.(check bool) ":env is not offered" false (contains_str lines ":env");
  Alcotest.(check bool) ":v is not offered" false (contains_str lines ":v")

let test_command_ident_arg () =
  let lines = Complete.line_completions (env ()) ":t Li" in
  Alcotest.(check bool) "the namespace itself" true
    (contains_str lines ":t List");
  let lines = Complete.line_completions (env ()) ":d List.len" in
  Alcotest.(check bool) "a member after :d" true
    (contains_str lines ":d List.length")

(* Multi-line entry detection -- the other pure REPL function. The local
   multi-equation forms from the reference must stay enterable: an open
   binding line mid-entry keeps gathering until a line supplies the body. *)

let complete = Alcotest.(check bool) "complete" true
let incomplete = Alcotest.(check bool) "incomplete" false

let test_single_lines_complete () =
  complete (Repl.is_complete "let f x = x + 1");
  complete (Repl.is_complete "let fib 0 = 0");
  complete (Repl.is_complete "let one = (let h y = y * 2 in h) 3")

let test_dangling_eq () =
  incomplete (Repl.is_complete "let answer =")

let test_equation_chain_gathers () =
  incomplete (Repl.is_complete "let answer =\n  let fib 0 = 0");
  incomplete (Repl.is_complete
    "let answer =\n  let fib 0 = 0\n  let fib 1 = 1");
  incomplete (Repl.is_complete
    "let answer =\n  let fib 0 = 0\n  fib 1 = 1")

let test_in_line_ends_chain () =
  complete (Repl.is_complete
    "let answer =\n  let fib 0 = 0\n  let fib 1 = 1\n  \
     let fib n = fib (n - 1) + fib (n - 2)\n  in fib 10");
  complete (Repl.is_complete "let v =\n  let k = 2 in k + 1")

let test_plain_body_ends_chain () =
  complete (Repl.is_complete "let go =\n  let helper x = x * 2\n  helper 21")

let test_letters_in_are_not_the_keyword () =
  incomplete (Repl.is_complete "let p =\n  let b = /bin/ls");
  incomplete (Repl.is_complete "let d =\n  let wait = 5min")

let test_trailing_and_gathers () =
  incomplete (Repl.is_complete
    "let is_even n = if n == 0 then true else is_odd (n - 1) and");
  incomplete (Repl.is_complete
    "let is_even n = if n == 0 then true else is_odd (n - 1) and\n\
     is_odd n = if n == 0 then false else is_even (n - 1)");
  complete (Repl.is_complete "operand")

let test_eq_in_string_or_operator () =
  complete (Repl.is_complete "let m =\n  \"a = b\"");
  complete (Repl.is_complete "let ok =\n  1 == 1");
  complete (Repl.is_complete "let ok =\n  1 <= 2")

(* Constructors the scope cannot show, read from a checked buffer as the
   language server reads them: a type's own with its name in front, and a
   module's, which the scope holds none of. *)
let test_constructors_after_a_dot () =
  let dir = Filename.temp_file "wand_complete" "" in
  Sys.remove dir; Sys.mkdir dir 0o700;
  let write name text =
    Out_channel.with_open_text (Filename.concat dir name)
      (fun oc -> output_string oc text) in
  write "apps.wand"
    "type PullPolicy = Always | Never | IfNotPresent\n\
     type RestartPolicy = Always | OnFailure | Never\n";
  let buffer =
    "let apps = import ./apps\n\
     type Color = Red | Green\n\
     let c = Color.Red\n\
     c\n" in
  let ctors =
    match Runner.typecheck_source ~path:(Filename.concat dir "main.wand") buffer with
    | Ok sc -> sc.Runner.sc_ctors
    | Error d -> Alcotest.failf "the buffer did not check: %s" (Diag.legacy d)
  in
  let offered line =
    (Complete.ident_at ~ctors [] line).Complete.candidates in
  Alcotest.(check (list string)) "a type in the file"
    ["Color.Green"; "Color.Red"] (List.sort compare (offered "Color."));
  Alcotest.(check (list string)) "a type in a module"
    ["apps.PullPolicy.Always"; "apps.PullPolicy.IfNotPresent"; "apps.PullPolicy.Never"]
    (List.sort compare (offered "apps.PullPolicy."));
  Alcotest.(check bool) "a name only one type in the module has, bare" true
    (List.mem "apps.IfNotPresent" (offered "apps.I"));
  Alcotest.(check bool) "and not a name two of its types share" false
    (List.mem "apps.Always" (offered "apps.A"));
  Alcotest.(check (list string)) "nothing without a dot" []
    (List.filter (fun c -> String.contains c '.') (offered "Col"))

let () =
  Alcotest.run "completion" [
    "identifiers", [
      Alcotest.test_case "a bare prefix"          `Quick test_bare_ident;
      Alcotest.test_case "builtins, empty env"    `Quick test_builtins_without_env;
      Alcotest.test_case "a namespace member"     `Quick test_namespace_member;
      Alcotest.test_case "an unknown namespace"   `Quick test_unknown_namespace;
      Alcotest.test_case "two dots"               `Quick test_two_dots;
      Alcotest.test_case "constructors after a dot" `Quick test_constructors_after_a_dot;
    ];
    "lines", [
      Alcotest.test_case "whole line rebuilt"     `Quick test_whole_line_rebuilt;
      Alcotest.test_case "command names"          `Quick test_command_names;
      Alcotest.test_case "a command's identifier" `Quick test_command_ident_arg;
    ];
    "multi-line entry", [
      Alcotest.test_case "single lines complete"    `Quick test_single_lines_complete;
      Alcotest.test_case "dangling ="               `Quick test_dangling_eq;
      Alcotest.test_case "equation chain gathers"   `Quick test_equation_chain_gathers;
      Alcotest.test_case "an in-line ends it"       `Quick test_in_line_ends_chain;
      Alcotest.test_case "a plain body ends it"     `Quick test_plain_body_ends_chain;
      Alcotest.test_case "letters 'in', not keyword" `Quick test_letters_in_are_not_the_keyword;
      Alcotest.test_case "trailing and gathers"     `Quick test_trailing_and_gathers;
      Alcotest.test_case "= in string or operator"  `Quick test_eq_in_string_or_operator;
    ];
  ]
