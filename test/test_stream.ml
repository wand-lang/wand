open Wand
open Evaluator

(* The one claim no wand-level test can check: that `take` stops the
   pulling. Under open-granularity effects a mock hands its lines over
   wholesale and cannot count pulls, so the proof lives below the effect
   boundary -- an injected puller that counts, over a source that never
   ends. If early exit breaks, this test does not fail; it hangs. *)

let test_take_stops_pulling () =
  let pulls = ref 0 in
  let puller () = incr pulls; Some (VInt !pulls) in
  let even = VBuiltin (function
    | VInt n -> VBool (n mod 2 = 0)
    | _ -> VBool false) in
  let desc = { s_source = SPull puller;
               s_stages = [StFilter even; StTake 3] } in
  let seen = ref [] in
  run_stream_terminal desc ~on_item:(fun v -> seen := v :: !seen);
  Alcotest.(check int) "three delivered" 3 (List.length !seen);
  (* The evens among 1..6 are exactly three, so an early-exiting loop
     pulls six times from the infinite source and not once more. *)
  Alcotest.(check int) "six pulled" 6 !pulls

let test_bare_take_pulls_exactly () =
  let pulls = ref 0 in
  let puller () = incr pulls; Some (VInt !pulls) in
  let desc = { s_source = SPull puller; s_stages = [StTake 4] } in
  let seen = ref 0 in
  run_stream_terminal desc ~on_item:(fun _ -> incr seen);
  Alcotest.(check int) "four delivered" 4 !seen;
  Alcotest.(check int) "four pulled" 4 !pulls

let test_release_on_raise () =
  (* A closure raising mid-stream must not leave the loop running. *)
  let pulls = ref 0 in
  let puller () = incr pulls; Some (VInt !pulls) in
  let boom = VBuiltin (fun v ->
    if !pulls >= 3 then raise (EvalError "boom") else v) in
  let desc = { s_source = SPull puller; s_stages = [StMap boom] } in
  (match run_stream_terminal desc ~on_item:(fun _ -> ()) with
   | () -> Alcotest.fail "expected the raise to escape"
   | exception EvalError m -> Alcotest.(check string) "the raise" "boom" m);
  Alcotest.(check int) "stopped at the raise" 3 !pulls

(* `select_by` behind `List.smallest_by`, `List.greatest_by` and their
   stream forms (#121). The reference is the definition: a stable sort by
   the key -- descending for greatest -- then the first n. Keys are drawn
   from a small range, so ties are common, and each element carries its
   position so a tie that came back in the wrong order shows. *)

let ints vs = List.map (function
  | VTuple [VInt k; VInt i] -> (k, i)
  | _ -> Alcotest.fail "expected (Int, Int)") vs

let select ~greatest n xs =
  let calls = ref 0 in
  let key = VBuiltin (function
    | VTuple [k; _] -> incr calls; k
    | _ -> Alcotest.fail "expected a pair") in
  let vs = List.mapi (fun i k -> VTuple [VInt k; VInt i]) xs in
  match select_by ~greatest n key (fun k -> List.iter k vs) with
  | VList out -> (ints out, !calls)
  | _ -> Alcotest.fail "expected a List"

let reference ~greatest n xs =
  let pairs = List.mapi (fun i k -> (k, i)) xs in
  let sorted = List.stable_sort (fun (a, _) (b, _) ->
    if greatest then compare b a else compare a b) pairs in
  List.filteri (fun i _ -> i < n) sorted

let test_select_matches_sort () =
  Random.init 121;
  for _ = 1 to 500 do
    let len = Random.int 40 in
    let xs = List.init len (fun _ -> Random.int 6) in
    let n = Random.int 45 - 3 in
    List.iter (fun greatest ->
      let (got, calls) = select ~greatest n xs in
      Alcotest.(check (list (pair int int)))
        (Printf.sprintf "n=%d greatest=%b" n greatest)
        (reference ~greatest n xs) got;
      Alcotest.(check int) "the key once per element"
        (if n <= 0 then 0 else len) calls)
      [false; true]
  done

let test_select_reads_once () =
  let pulls = ref 0 in
  let puller () =
    incr pulls; if !pulls > 1000 then None else Some (VInt (!pulls mod 37)) in
  let desc = { s_source = SPull puller; s_stages = [] } in
  let id = VBuiltin (fun v -> v) in
  (match select_by ~greatest:true 3 id
           (fun k -> run_stream_terminal desc ~on_item:k) with
   | VList [VInt 36; VInt 36; VInt 36] -> ()
   | _ -> Alcotest.fail "expected the three greatest");
  Alcotest.(check int) "read once, to the end" 1001 !pulls;
  (* Asked for nothing, a source that never ends is not read. *)
  let pulls = ref 0 in
  let desc = { s_source = SPull (fun () -> incr pulls; Some (VInt 1));
               s_stages = [] } in
  (match select_by ~greatest:false 0 id
           (fun k -> run_stream_terminal desc ~on_item:k) with
   | VList [] -> ()
   | _ -> Alcotest.fail "expected []");
  Alcotest.(check int) "not read" 0 !pulls

let () =
  Alcotest.run "stream runtime" [
    "early exit", [
      Alcotest.test_case "take through a filter" `Quick test_take_stops_pulling;
      Alcotest.test_case "bare take"             `Quick test_bare_take_pulls_exactly;
    ];
    "smallest_by and greatest_by", [
      Alcotest.test_case "equal to a stable sort" `Quick test_select_matches_sort;
      Alcotest.test_case "the stream is read once" `Quick test_select_reads_once;
    ];
    "unwinding", [
      Alcotest.test_case "raise mid-stream" `Quick test_release_on_raise;
    ];
  ]
