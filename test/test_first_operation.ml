open Wand

(* `Runner.first_operation`: what the fuzzer's effect-gate check runs a
   program with. It has to stop at the first operation the program does not
   handle itself, name it, and carry out nothing -- a mutant that writes a
   file must not write one. *)

let first src =
  match Runner.first_operation src with
  | Ok None -> "none"
  | Ok (Some op) -> op
  | Error m -> "error: " ^ m

let temp name = Filename.concat (Filename.get_temp_dir_name ()) name

let test_a_pure_program_reaches_none () =
  Alcotest.(check string) "pure" "none" (first "1 + 2")

let test_it_names_the_first_operation () =
  Alcotest.(check string) "a print" "IO!println" (first "import IO\nIO.println \"hi\"")

let test_it_carries_out_nothing () =
  let path = temp (Printf.sprintf "wand-first-op-%d" (Unix.getpid ())) in
  (try Sys.remove path with Sys_error _ -> ());
  Alcotest.(check string) "a write" "FS!write_file"
    (first (Printf.sprintf "import FS\nFS.write_file! %s \"x\"" path));
  Alcotest.(check bool) "nothing written" false (Sys.file_exists path)

(* Par sends a worker's operations back to the observer, so a write on
   another domain is stopped too. *)
let test_a_par_worker_is_stopped_too () =
  let path = temp (Printf.sprintf "wand-first-op-par-%d" (Unix.getpid ())) in
  (try Sys.remove path with Sys_error _ -> ());
  Alcotest.(check string) "a write in a worker" "FS!write_file"
    (first (Printf.sprintf
              "import Par\nimport FS\nPar.all! [fn () -> FS.write_file! %s \"x\"]" path));
  Alcotest.(check bool) "nothing written" false (Sys.file_exists path)

(* An operation the program handles itself is not one it performs. *)
let test_a_handled_operation_is_not_reached () =
  Alcotest.(check string) "handled" "none"
    (first "import IO\nhandle IO.println \"hi\" with\n| IO!println _ k -> k ()")

let () =
  Alcotest.run "First operation" [
    "first_operation", [
      Alcotest.test_case "a pure program reaches none" `Quick
        test_a_pure_program_reaches_none;
      Alcotest.test_case "it names the first operation" `Quick
        test_it_names_the_first_operation;
      Alcotest.test_case "it carries out nothing" `Quick test_it_carries_out_nothing;
      Alcotest.test_case "a Par worker is stopped too" `Quick
        test_a_par_worker_is_stopped_too;
      Alcotest.test_case "a handled operation is not reached" `Quick
        test_a_handled_operation_is_not_reached;
    ];
  ]
