open Wand

(* Two properties of the evaluator, kept together because one pays for the
   other.

   A wand tail call is an OCaml tail call: a function whose body ends in a
   call to itself runs in a stack that does not grow. That used to be untrue
   for a reason that had nothing to do with calls -- every `Located` node
   installed an exception handler to stamp a position onto an error passing
   through it, and a handler is a frame that stays. A `Located` sits on
   every function body and every match arm, so the stack grew with the call
   chain, and since a minor collection rescans the whole stack as roots, a
   long recursion cost time quadratic in its own depth.

   The position now travels in a cell rather than in a handler, which is
   what makes the tail call a tail call. That trades an exact mechanism for
   a maintained one: a handler could not report the wrong position, whereas
   a cell can if anything that carries on after a subexpression forgets to
   put back what it found. So the second half of this file pins the position
   a runtime error is reported at, in the shapes where forgetting would
   show. *)

let wand_binary =
  let dir = Filename.dirname (Filename.dirname Sys.executable_name) in
  Filename.concat (Filename.concat dir "bin") "wand.exe"

let run ?env src =
  let path = Filename.temp_file "wand_tail" ".wand" in
  Out_channel.with_open_text path (fun oc -> Out_channel.output_string oc src);
  Fun.protect ~finally:(fun () -> try Sys.remove path with Sys_error _ -> ())
    (fun () ->
       let prefix = match env with None -> "" | Some e -> e ^ " " in
       let cmd = prefix ^ Filename.quote wand_binary ^ " " ^ Filename.quote path
                 ^ " 2>&1" in
       let ic = Unix.open_process_in cmd in
       let out = In_channel.input_all ic in
       let code = match Unix.close_process_in ic with
         | Unix.WEXITED n -> n
         | Unix.WSIGNALED n | Unix.WSTOPPED n -> 128 + n
       in
       (code, String.trim out))

(* ── The tail call ───────────────────────────────────────────────────────── *)

(* Deliberately run against a stack far smaller than the recursion is deep,
   so the answer is the property itself rather than how long it took: a
   frame per iteration cannot fit in 4096 words, and before this it did not
   -- the same script died with "Fatal error: exception Stack overflow". *)
let tiny_stack = "OCAMLRUNPARAM=l=4096"

let loop n = Printf.sprintf
  "let go 0 acc = acc\nlet go n acc = go (n - 1) (acc + 1)\ngo %d 0\n" n

let test_tail_recursion_is_flat () =
  let (code, out) = run ~env:tiny_stack (loop 200_000) in
  Alcotest.(check int) "200k tail calls in a 4096-word stack" 0 code;
  Alcotest.(check string) "and the answer is right" "200000" out

let test_tail_call_through_if () =
  (* The tail position of an `if` is each branch, not the `if`. *)
  let (code, out) = run ~env:tiny_stack
    "let go n acc = if n == 0 then acc else go (n - 1) (acc + 1)\ngo 200000 0\n" in
  Alcotest.(check int) "a branch is a tail position" 0 code;
  Alcotest.(check string) "answer" "200000" out

let test_tail_call_through_match () =
  let (code, out) = run ~env:tiny_stack
    "let go n acc = match n with\n  | 0 -> acc\n  | _ -> go (n - 1) (acc + 1)\n\
     go 200000 0\n" in
  Alcotest.(check int) "an arm body is a tail position" 0 code;
  Alcotest.(check string) "answer" "200000" out

let test_tail_call_through_block () =
  (* The last statement of a `;` block, and a `let ... in` body. *)
  let (code, out) = run ~env:tiny_stack
    "let go n acc = (\n  let step = acc + 1;\n  \
     if n == 0 then acc else go (n - 1) step\n)\ngo 200000 0\n" in
  Alcotest.(check int) "the last statement of a block is a tail position" 0 code;
  Alcotest.(check string) "answer" "200000" out

(* ── Where an error says it happened ─────────────────────────────────────── *)

let at src expected name =
  let (code, out) = run src in
  if code = 0 then Alcotest.failf "%s: expected a failure, got: %s" name out;
  match String.index_opt out ':' with
  | None -> Alcotest.failf "%s: no position in: %s" name out
  | Some _ ->
    (* "Error: eval error: 4:11: pattern match failure" -- the position is
       the digits before the message. *)
    let re = Str.regexp "\\([0-9]+\\):\\([0-9]+\\): [a-z]" in
    (try
       ignore (Str.search_forward re out 0);
       Alcotest.(check string) name expected
         (Str.matched_group 1 out ^ ":" ^ Str.matched_group 2 out)
     with Not_found -> Alcotest.failf "%s: no position in: %s" name out)

let test_loc_after_a_call_returns () =
  (* The failing line is the caller's. A call that has already returned must
     not leave its own body's position behind, or this reports line 2 --
     which for a stdlib call is a line in a file the script never opened. *)
  at "import List\nlet helper x = x + 1\nlet main () = helper 1 + List.head! []\nmain ()\n"
    "3:15" "after a call returns"

let test_loc_after_a_builtin_applied_closure () =
  at "import List\nlet each () = (\n  let r = try List.map (fn x -> x + 1) [1, 2];\n  \
      List.head! []\n)\neach ()\n"
    "4:3" "after a builtin ran a closure"

let test_loc_after_a_caught_raise () =
  (* `try` turns a raise into a value and carries on. The position it failed
     at goes with it. *)
  at "import List\nlet bad () = List.head! []\nlet after () = (\n  \
      let r = try bad ();\n  List.head! []\n)\nafter ()\n"
    "5:3" "after a caught raise"

let test_loc_inside_a_deep_tail_recursion () =
  (* The tail path is the one that does not put anything back, so this is
     where a wrong position would be most likely. *)
  at "import List\nlet deep n = if n == 0 then List.head! [] else deep (n - 1)\ndeep 5000\n"
    "2:29" "inside a tail recursion"

(* ── The bound on the other path ─────────────────────────────────────────── *)

(* A call with work waiting on it keeps a frame, so nesting them without end
   used to exhaust the stack and end the run with OCaml's own "Fatal error:
   exception Stack overflow" -- which cannot be caught here: a handler that
   matches it, even one whose guard rejects it, hangs rather than unwinds,
   because the guard runs on the stack that just ran out. So the depth is
   bounded before the stack goes, and the reader gets an error instead.

   The bound is read from the environment, which is what lets these run in
   a hundredth of a second rather than the half-minute the real ceiling
   takes to reach. *)
let says needle hay =
  let n = String.length needle and m = String.length hay in
  let rec at i = i + n <= m && (String.sub hay i n = needle || at (i + 1)) in
  at 0

let low_cap = "WAND_MAX_CALL_DEPTH=1000"

let nests_too_deep = "let f n = if n == 0 then 0 else 1 + f (n - 1)\nf 5000\n"

let test_deep_nesting_is_an_error () =
  let (code, out) = run ~env:low_cap nests_too_deep in
  Alcotest.(check int) "refused rather than fatal" 1 code;
  Alcotest.(check bool) "and says which position runs to any depth" true
    (says "only a call in tail position" out);
  Alcotest.(check bool) "never OCaml's own" false
    (says "Fatal error" out)

(* Neither function calls itself, so a message about self-recursion would be
   about the wrong thing. The bound counts nesting, not who is nested in. *)
let test_mutual_recursion_is_bounded () =
  let (code, out) = run ~env:low_cap
    "let f n = 1 + g (n + 1) and\n    g n = 1 + f (n + 1)\nf 1\n" in
  Alcotest.(check int) "refused rather than fatal" 1 code;
  Alcotest.(check bool) "never OCaml's own" false
    (says "Fatal error" out)

(* The whole point of bounding `apply` and not `apply_tail`: a tail call
   keeps no frame, so no bound applies to it however low the bound is. *)
let test_the_bound_exempts_tail_calls () =
  let (code, out) = run ~env:low_cap (loop 200_000) in
  Alcotest.(check int) "200k tail calls under a bound of 1000" 0 code;
  Alcotest.(check string) "and the answer is right" "200000" out

(* The same script the bound refuses runs when the bound is not in its way,
   so what the tests above pin is the bound and not the script. *)
let test_the_default_bound_allows_it () =
  let (code, out) = run nests_too_deep in
  Alcotest.(check int) "runs under the default bound" 0 code;
  Alcotest.(check string) "and the answer is right" "5000" out

(* ── The List functions that take a function ───────────────────────────── *)

(* `List.map`, `find` and their kin are builtins (#122). Each walks the list
   in a loop of its own, and that loop must be flat as a wand tail call is:
   two million elements, in a stack that holds a few thousand frames. *)
let long_list_script = {|uses {IO}
import IO
import List
let xs = List.range 1 2000000
let ys = List.map (fn x -> x + 1) xs
let evens = List.filter (fn x -> x % 2 == 0) ys
let some = List.filter_map (fn x -> if x % 4 == 0 then Some x else None) xs
let flat = List.flat_map (fn x -> [x]) xs
let total = List.fold_left (fn a x -> a + x) 0 xs
let back = List.fold_right (fn x acc -> x :: acc) xs []
let found = List.find (fn x -> x == 2000000) xs
let all = List.all? (fn x -> x > 0) xs
let any = List.any? (fn x -> x < 0) xs
let kept = List.take_while (fn x -> x > 0) xs
let left = List.drop_while (fn x -> x < 1999999) xs
let pairs = List.zip xs (List.indexed xs)
let has = List.contains? 2000000 xs
List.each (fn _ -> ()) xs
IO.println "%{List.length ys} %{List.length evens} %{List.length some} %{List.length flat} %{total} %{List.length back} %{found} %{all} %{any} %{List.length kept} %{left} %{List.length pairs} %{has}"
|}

let test_long_lists_are_flat () =
  let (code, out) = run ~env:tiny_stack long_list_script in
  Alcotest.(check int) "two million elements in a 4096-word stack" 0 code;
  Alcotest.(check string) "and the answers are right"
    "2000000 1000000 500000 2000000 2000001000000 2000000 Some(2000000) true \
     false 2000000 [1999999, 2000000] 2000000 true"
    out

let test_loc_after_a_list_builtin_raised () =
  (* The raise is inside the function `List.find` was given, so the position
     is in that function, not in the builtin and not at the call. *)
  at "import List\nlet main () =\n  List.find (fn x -> List.head! [] == x) [1]\nmain ()\n"
    "3:22" "inside the function a List builtin called"

let () =
  Alcotest.run "tail calls" [
    "a tail call does not grow the stack", [
      Alcotest.test_case "plain recursion"  `Quick test_tail_recursion_is_flat;
      Alcotest.test_case "through an if"    `Quick test_tail_call_through_if;
      Alcotest.test_case "through a match"  `Quick test_tail_call_through_match;
      Alcotest.test_case "through a block"  `Quick test_tail_call_through_block;
    ];
    "nesting without end is refused, not fatal", [
      Alcotest.test_case "deep nesting is an error" `Quick
        test_deep_nesting_is_an_error;
      Alcotest.test_case "mutual recursion too"     `Quick
        test_mutual_recursion_is_bounded;
      Alcotest.test_case "tail calls are exempt"    `Quick
        test_the_bound_exempts_tail_calls;
      Alcotest.test_case "and the default allows it" `Quick
        test_the_default_bound_allows_it;
    ];
    "an error still says where it happened", [
      Alcotest.test_case "after a call returns" `Quick test_loc_after_a_call_returns;
      Alcotest.test_case "after a builtin ran a closure" `Quick
        test_loc_after_a_builtin_applied_closure;
      Alcotest.test_case "after a caught raise" `Quick test_loc_after_a_caught_raise;
      Alcotest.test_case "inside a tail recursion" `Quick
        test_loc_inside_a_deep_tail_recursion;
      Alcotest.test_case "inside the function a List builtin called" `Quick
        test_loc_after_a_list_builtin_raised;
    ];
    "a List builtin walks a long list in a flat loop", [
      Alcotest.test_case "two million elements" `Quick test_long_lists_are_flat;
    ];
  ]
