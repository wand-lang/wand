open Wand

(* What happens to something a script is holding when a handler case answers
   without resuming.

   An case that never calls its continuation ends the body it was handling.
   OCaml's own answer is to drop the continuation and collect it, running no
   cleanup whatsoever -- so a mock that swallows an effect would silently
   leak every lock and temp file the mocked code had open. Mocking is the
   flagship, and a rehearsal that leaks is the failure mode the effect work
   exists to prevent, so the runtime unwinds the abandoned region instead.

   wand has no cleanup construct of its own yet, so these tests supply one:
   a builtin `holding` that acquires, calls a function, and releases through
   `Fun.protect` -- the same mechanism a resource bracket will use. What is
   under test is the runtime's behaviour, not the builtin. *)

let released : string list ref = ref []

(* `holding label f` -- acquires `label`, applies `f`, releases on the way
   out however that happens. *)
let holding_builtin =
  Evaluator.VBuiltin (fun label ->
    let label = match label with
      | Evaluator.VString s -> s
      | _ -> failwith "holding: label"
    in
    Evaluator.VBuiltin (fun f ->
      Fun.protect
        ~finally:(fun () -> released := !released @ [label])
        (fun () -> Evaluator.apply f Evaluator.VUnit)))

let eval_wand src =
  released := [];
  let env = ("holding", holding_builtin) :: Evaluator.stdlib_eval_env in
  let prog = Lexer.tokenize src |> Parser.parse_program in
  let last = List.fold_left (fun _ item ->
    match item with
    | Ast.TLExpr e -> Some (Evaluator.eval env e)
    | _ -> None) None prog.Ast.items
  in
  match last with
  | Some v -> Evaluator.to_text v
  | None -> "()"

let check_released label expected =
  Alcotest.(check (list string)) label expected !released

(* An case that resumes is the ordinary case and always released. *)
let test_resuming_releases () =
  let answer =
    eval_wand
      {|handle (holding "r" (fn () -> let x = $(echo hi) in x)) with
        | Shell!command c k -> k c
        | Shell!run _ k -> k "mocked"|}
  in
  Alcotest.(check string) "the body's own value" "mocked" answer;
  check_released "released" ["r"]

(* The case that motivated this: the case answers on its own, so the body
   never continues -- and what the body was holding still comes back. *)
let test_abandoning_releases () =
  let answer =
    eval_wand
      {|handle (holding "r" (fn () -> let x = $(echo hi) in x)) with
        | Shell!command c k -> k c
        | Shell!run _ _ -> "answered without resuming"|}
  in
  Alcotest.(check string) "the case's value, not the body's"
    "answered without resuming" answer;
  check_released "released anyway" ["r"]

(* Cleanup runs innermost-first, the order things were acquired in reverse. *)
let test_nested_release_order () =
  ignore
    (eval_wand
       {|handle (holding "outer" (fn () ->
           holding "inner" (fn () -> $(echo hi)))) with
         | Shell!command c k -> k c
         | Shell!command c k -> k c
        | Shell!run _ _ -> "answered"|});
  check_released "innermost first" ["inner"; "outer"]

(* Cleanup that performs an effect of its own reaches the handlers that were
   in scope when the resource was taken -- it runs inside the case, not after
   the handler has gone. A release deleting a lock file is exactly this. *)
let test_release_can_perform () =
  let answer =
    eval_wand
      {|let log = handle (holding "r" (fn () ->
           let x = $(echo body) in x)) with
         | Shell!command c k -> k c
         | Shell!run cmd k -> k "mocked"
         | return v -> v
         in log|}
  in
  Alcotest.(check string) "unchanged by the release" "mocked" answer;
  check_released "released" ["r"]

(* An case that resumes has consumed its continuation; unwinding it a second
   time would raise. Resuming inside a branch and not in another is the
   shape that catches this. *)
let test_conditional_resume () =
  let answer =
    eval_wand
      {|handle (holding "r" (fn () -> let x = $(echo hi) in x)) with
        | Shell!command c k -> k c
        | Shell!run cmd k -> if cmd == "echo hi" then k "resumed" else "not"|}
  in
  Alcotest.(check string) "resumed branch" "resumed" answer;
  check_released "released once" ["r"];
  let answer =
    eval_wand
      {|handle (holding "r" (fn () -> let x = $(echo hi) in x)) with
        | Shell!command c k -> k c
        | Shell!run cmd k -> if cmd == "nope" then k "resumed" else "not"|}
  in
  Alcotest.(check string) "abandoning branch" "not" answer;
  check_released "released once" ["r"]

(* The unwinding must not be observable as a failure: `try` inside the
   abandoned region cannot catch it and carry on. *)
let test_try_cannot_catch_the_unwind () =
  let answer =
    eval_wand
      {|handle (holding "r" (fn () ->
          let attempt = try $(echo hi) in
          "body continued")) with
        | Shell!command c k -> k c
        | Shell!run _ _ -> "answered"|}
  in
  Alcotest.(check string) "the case's value" "answered" answer;
  check_released "released" ["r"]

(* A continuation used after its case answered, or used twice, is an error
   the script can read. The body behind it was unwound when the case
   answered, and OCaml reports a second resume by stopping the interpreter
   with `Continuation_already_resumed`, which is what a handler that carries
   state in a returned function reached (#53). *)
let stale_resume_is_an_error () =
  let says src needle =
    match eval_wand src with
    | s -> Alcotest.failf "expected an error, got %s" s
    | exception Evaluator.EvalError msg ->
      let n = String.length needle and m = String.length msg in
      let rec has i = i + n <= m && (String.sub msg i n = needle || has (i + 1)) in
      if not (has 0) then Alcotest.failf "expected %S in: %s" needle msg
  in
  says
    {|(handle (holding "r" (fn () -> let x = $(echo hi) in x)) with
        | Shell!command c k -> k c
        | Shell!run _ k -> fn s -> (k s) s
        | return x -> fn _ -> x) "late"|}
    "was called after its case answered";
  check_released "the body was released when the case answered" ["r"];
  says
    {|handle (holding "r" (fn () -> let x = $(echo hi) in x)) with
        | Shell!command c k -> k c
        | Shell!run _ k -> k "a" ++ k "b"|}
    "was called twice"

let () =
  Alcotest.run "Abandonment" [
    "a handler case that does not resume", [
      Alcotest.test_case "resuming releases"        `Quick test_resuming_releases;
      Alcotest.test_case "abandoning releases"      `Quick test_abandoning_releases;
      Alcotest.test_case "innermost first"          `Quick test_nested_release_order;
      Alcotest.test_case "release may perform"      `Quick test_release_can_perform;
      Alcotest.test_case "resume in one branch"     `Quick test_conditional_resume;
      Alcotest.test_case "try cannot catch unwind"  `Quick test_try_cannot_catch_the_unwind;
      Alcotest.test_case "stale resume is an error" `Quick stale_resume_is_an_error;
    ];
  ]
