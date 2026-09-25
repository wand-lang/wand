(* Every reproducer the fuzzer has filed and that has since been fixed.

   This is what makes a scheduled fuzz run compound. A finding that is only
   fixed can come back; a finding that is fixed and left here as a fixture
   cannot, because the fix is now something `dune test` holds in place on
   every PR. Nothing else in the suite would notice: these are inputs no
   person would write, so no hand-written test covers them.

   The check is `Wand_fuzz.Oracle.check` -- the same code the fuzzer runs.
   A fixture that passed a second, hand-written implementation of the
   property would prove nothing about the property the fuzzer tests. *)

let dir = "fuzz/regressions"

let read path = In_channel.with_open_bin path In_channel.input_all

(* A fixture that has to be typechecked as some other file -- because it
   crashed the compiler through an import, or because the base environment
   differs for a stdlib module -- names that file in a sibling `.under`,
   as a path from the repository root. Most are self-contained and have
   none. *)
let path_for fixture =
  let under = Filename.remove_extension fixture ^ ".under" in
  if Sys.file_exists under then
    let rel = String.trim (read under) in
    (* The suite runs in the `test` directory of the build tree, and what the
       path names is declared as a dep of this stanza, so it sits beside that
       directory -- one level up, not two. Checked rather than assumed: a
       path that is not there leaves the fixture checked as an ordinary
       script, which is not the question it was filed over, and it passes. *)
    let under_root = Filename.concat Filename.parent_dir_name rel in
    if not (Sys.file_exists under_root) then
      Alcotest.failf "%s names %s, and it is not there: the stanza's deps \
                      are wrong" under rel;
    under_root
  else fixture

let fixtures () =
  if not (Sys.file_exists dir && Sys.is_directory dir) then
    Alcotest.failf
      "%s is missing (relative to the test sandbox): the stanza's deps are \
       wrong, and this suite would otherwise pass by finding nothing" dir;
  Sys.readdir dir
  |> Array.to_list
  |> List.filter (fun f -> Filename.check_suffix f ".wand")
  |> List.sort compare
  |> List.map (Filename.concat dir)

let check fixture () =
  let v =
    (* `~eval:true`: a fixture for a formatter bug that changed what a
       program answered is only held in place by running it. *)
    Wand_fuzz.Oracle.check_all ~timeout:30.0 ~eval:true ~width:92
      ~path:(path_for fixture) (read fixture)
  in
  if Wand_fuzz.Oracle.is_finding v then
    Alcotest.failf "%s: %s\n\nsee %s for what this was"
      fixture (Wand_fuzz.Oracle.describe v)
      (Filename.remove_extension fixture ^ ".txt")

let () =
  let cases =
    List.map
      (fun f -> Alcotest.test_case (Filename.basename f) `Quick (check f))
      (fixtures ())
  in
  (* The directory is empty until the fuzzer finds something. An empty run
     is a pass, but the missing-directory check above means it is a pass
     because there is nothing to check, not because nothing was looked at. *)
  Alcotest.run "fuzz regressions" [ ("reproducers", cases) ]
