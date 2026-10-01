open Wand

(* Every `.wand` test fixture under test/wand, run through
   Runner.run_test_file so `dune test` gates on them like any other suite.

   The list is discovered, not written down. It used to be enumerated here,
   and six fixtures -- including test_operations, the only check that effect
   operations actually perform what they claim -- were never added, so they
   ran only when someone typed `wand s test/wand` by hand. An enumeration
   that has to be kept in step with a directory is a list that will fall out
   of step with it, so this reads the directory instead: a new fixture is
   picked up by existing here, and cannot be forgotten.

   Discovery and the file check both happen lazily, inside the Alcotest
   cases -- Alcotest's working directory at that point is what makes the
   relative paths resolve, matching test_formatter.ml's
   test_idempotent_stdlib. *)

(* `dune test` runs in a sandbox where the fixtures are at `wand/`; `dune exec
   test/test_wand_stdlib.exe` runs from the build root, where they are at
   `test/wand/`. Both are documented ways to run a suite, so the directory is
   found rather than assumed. *)
let candidate_dirs = ["wand"; "test/wand"; "../test/wand"]

let dir () =
  match List.find_opt Sys.file_exists candidate_dirs with
  | Some d -> d
  | None   -> List.hd candidate_dirs

(* `find_test_files` is the same discovery `wand s` does, so the set gated
   here and the set a developer runs by hand cannot disagree. Helpers that
   are not themselves tests (ImportsFixture.wand) do not match `test_*` and
   are correctly left out. *)
let fixtures () =
  Runner.find_test_files (dir ())
  |> List.sort String.compare

let check_fixture path () =
  if not (Sys.file_exists path) then
    Alcotest.failf "fixture not found at %s (relative to test sandbox)" path
  else
    match Runner.run_test_file path with
    | Error msg -> Alcotest.failf "file-level error running %s: %s" path msg
    | Ok outcomes ->
      let failures = List.filter_map (function
        | Runner.TPass _ -> None
        | Runner.TFail msg | Runner.TError msg -> Some msg
      ) outcomes in
      if failures <> [] then
        Alcotest.failf "%d/%d wand tests failed:\n%s"
          (List.length failures) (List.length outcomes) (String.concat "\n" failures)

(* A directory that suddenly discovers nothing would report success while
   testing nothing, which is the failure this suite exists to prevent. *)
let test_discovery_found_fixtures () =
  let n = List.length (fixtures ()) in
  if n = 0 then
    Alcotest.failf
      "no test_*.wand fixtures discovered under %s/ -- either the sandbox is \
       missing the source_tree dep or the fixtures moved" (dir ());
  Alcotest.(check bool) "found a plausible number of fixtures" true (n >= 20)

let () =
  let cases =
    List.map (fun path ->
      let name = Filename.remove_extension (Filename.basename path) in
      Alcotest.test_case name `Quick (check_fixture path))
      (fixtures ())
  in
  Alcotest.run "Stdlib (wand)" [
    "discovery", [
      Alcotest.test_case "fixtures are found" `Quick test_discovery_found_fixtures;
    ];
    "fixtures", cases;
  ]
