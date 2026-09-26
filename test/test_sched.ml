open Wand

let now = Sched.elapsed_ms

let run bodies =
  Sched.run ~save:(fun () -> ()) ~restore:(fun () -> ())
    (Array.map (fun _ -> ()) bodies) bodies

let sleep ms =
  let until = now () + ms in
  while now () < until do Sched.sleep_until until done

let test_sleeps_overlap () =
  let start = now () in
  run (Array.init 5 (fun _ () -> sleep 200));
  let took = now () - start in
  Alcotest.(check bool) (Printf.sprintf "five 200ms sleeps in %dms" took)
    true (took < 600)

let test_pipe_wakes_reader () =
  let r, w = Unix.pipe () in
  let got = ref "" in
  let reader () =
    Sched.wait_readable r;
    let buf = Bytes.create 16 in
    let n = Unix.read r buf 0 16 in
    got := Bytes.sub_string buf 0 n
  in
  let writer () =
    sleep 50;
    ignore (Unix.write_substring w "hello" 0 5)
  in
  run [| reader; writer |];
  Unix.close r; Unix.close w;
  Alcotest.(check string) "reader saw the write" "hello" !got

let test_yield_interleaves () =
  let log = ref [] in
  let body name () =
    for i = 1 to 3 do
      log := Printf.sprintf "%s%d" name i :: !log;
      Sched.yield ()
    done
  in
  run [| body "a"; body "b" |];
  Alcotest.(check (list string)) "round robin"
    ["a1"; "b1"; "a2"; "b2"; "a3"; "b3"] (List.rev !log)

let test_failure_reraised () =
  let finished = ref false in
  let raised =
    try
      run [| (fun () -> failwith "boom");
             (fun () -> sleep 20; finished := true) |];
      false
    with Failure m -> m = "boom"
  in
  Alcotest.(check bool) "raised after the others finished" true
    (raised && !finished)

let test_nested_does_not_block_outer () =
  let order = ref [] in
  let inner () =
    run [| (fun () -> sleep 150; order := "inner" :: !order) |]
  in
  let outer () = sleep 50; order := "outer" :: !order in
  run [| inner; outer |];
  Alcotest.(check (list string)) "outer finished first"
    ["outer"; "inner"] (List.rev !order)

let test_state_is_per_fiber () =
  let key = ref 0 in
  let seen = ref [] in
  let body v () =
    key := v;
    Sched.yield ();
    seen := !key :: !seen
  in
  Sched.run ~save:(fun () -> !key) ~restore:(fun v -> key := v)
    [| 1; 2 |] [| body 10; body 20 |];
  Alcotest.(check (list int)) "each fiber kept its own" [10; 20]
    (List.rev !seen);
  Alcotest.(check int) "the caller's is back" 0 !key

let test_outside_a_scheduler () =
  let start = now () in
  sleep 30;
  Sched.yield ();
  Alcotest.(check bool) "blocks directly" true (now () - start >= 30)

let test_evaluator_fiber_state () =
  let seen = ref [] in
  let body d () =
    Domain.DLS.set Evaluator.shell_deadline (Some d);
    Sched.yield ();
    seen := Domain.DLS.get Evaluator.shell_deadline :: !seen
  in
  Domain.DLS.set Evaluator.shell_deadline (Some 7);
  Evaluator.run_fibers [| body 1; body 2 |];
  Alcotest.(check (list (option int))) "per fiber" [Some 1; Some 2]
    (List.rev !seen);
  Alcotest.(check (option int)) "caller's kept" (Some 7)
    (Domain.DLS.get Evaluator.shell_deadline);
  Domain.DLS.set Evaluator.shell_deadline None

let () =
  Alcotest.run "sched" [
    "sched", [
      Alcotest.test_case "sleeps overlap" `Quick test_sleeps_overlap;
      Alcotest.test_case "a pipe wakes its reader" `Quick test_pipe_wakes_reader;
      Alcotest.test_case "yield interleaves" `Quick test_yield_interleaves;
      Alcotest.test_case "a failure is raised again" `Quick test_failure_reraised;
      Alcotest.test_case "nested does not block the outer" `Quick
        test_nested_does_not_block_outer;
      Alcotest.test_case "state is per fiber" `Quick test_state_is_per_fiber;
      Alcotest.test_case "outside a scheduler" `Quick test_outside_a_scheduler;
      Alcotest.test_case "evaluator fiber state" `Quick
        test_evaluator_fiber_state;
    ];
  ]
