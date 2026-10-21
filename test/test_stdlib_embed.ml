open Wand

(* The standard library is compiled into the binary. Two things have to hold
   and neither shows up in ordinary use:

   The table must match the files it was generated from. If the dune rule's
   dependencies are wrong, a built binary carries yesterday's standard
   library and every other test still passes -- they exercise the table, and
   the table is self-consistently stale. Comparing it to what is on disk is
   the only check that can see the difference.

   And the search that used to find a `stdlib/` directory must stay gone. It
   made the binary work only inside a tree that happened to contain one, and
   it meant a directory with the right name *was* the standard library. Both
   are checked by running the binary, because both are about where it is
   run from. *)

let stdlib_dir = "../stdlib"

let read path = In_channel.with_open_bin path In_channel.input_all

let disk_modules () =
  Sys.readdir stdlib_dir
  |> Array.to_list
  |> List.filter (fun n -> Filename.check_suffix n ".wand")
  |> List.map Filename.remove_extension
  |> List.sort compare

(* ── The table against the files ──────────────────────────────────────────── *)

let test_same_modules () =
  let embedded = List.map fst Stdlib_embed.table |> List.sort compare in
  Alcotest.(check (list string))
    "embedded modules are the modules on disk" (disk_modules ()) embedded

let test_same_sources () =
  List.iter
    (fun (name, embedded) ->
      let path = Filename.concat stdlib_dir (name ^ ".wand") in
      let on_disk = read path in
      if embedded <> on_disk then
        Alcotest.failf
          "%s.wand differs from the embedded copy (%d bytes on disk, %d \
           embedded) -- the generated table is stale, which means the dune \
           rule is not depending on the stdlib sources"
          name (String.length on_disk) (String.length embedded))
    Stdlib_embed.table

(* A module is not empty and does carry its doc comments: `wand d` reads
   them out of the same source the loader parses, so a table of names
   mapped to empty strings would satisfy everything above. *)
let test_sources_are_whole () =
  match List.assoc_opt "List" Stdlib_embed.table with
  | None -> Alcotest.fail "no List module in the embedded table"
  | Some src ->
    Alcotest.(check bool) "List has a body" true (String.length src > 1000);
    Alcotest.(check bool)
      "List keeps its doc comments" true
      (Option.is_some (String.index_opt src '('))

(* ── Where the binary is run from ─────────────────────────────────────────── *)

let wand_binary =
  let dir = Filename.dirname (Filename.dirname Sys.executable_name) in
  Filename.concat (Filename.concat dir "bin") "wand.exe"

let scratch () =
  let d = Filename.temp_file "wand_embed_" "" in
  Sys.remove d;
  Unix.mkdir d 0o755;
  d

let rec rm path =
  match Sys.is_directory path with
  | true ->
    Array.iter (fun e -> rm (Filename.concat path e)) (Sys.readdir path);
    Unix.rmdir path
  | false -> Sys.remove path
  | exception Sys_error _ -> ()

let run ~dir args =
  let cmd =
    Printf.sprintf "cd %s && %s %s 2>&1" (Filename.quote dir)
      (Filename.quote wand_binary)
      (String.concat " " (List.map Filename.quote args))
  in
  let ic = Unix.open_process_in cmd in
  let out = In_channel.input_all ic in
  ignore (Unix.close_process_in ic);
  String.trim out

let with_scratch f =
  let d = scratch () in
  Fun.protect ~finally:(fun () -> rm d) (fun () -> f d)

let test_runs_outside_any_tree () =
  with_scratch (fun d ->
    Alcotest.(check string)
      "a directory with no stdlib above it" "2 : Int"
      (run ~dir:d [ "-e"; "List.length [1,2]" ]))

let decoy dir =
  let s = Filename.concat dir "stdlib" in
  Unix.mkdir s 0o755;
  Out_channel.with_open_text (Filename.concat s "List.wand") (fun oc ->
    Out_channel.output_string oc "let length x = \"not wand\"\n")

let test_decoy_stdlib_is_ignored () =
  with_scratch (fun d ->
    decoy d;
    Alcotest.(check string)
      "a stdlib/ in the working directory changes nothing" "2 : Int"
      (run ~dir:d [ "-e"; "List.length [1,2]" ]))

let test_decoy_stdlib_above_is_ignored () =
  with_scratch (fun d ->
    decoy d;
    let sub = Filename.concat d "sub" in
    Unix.mkdir sub 0o755;
    Alcotest.(check string)
      "a stdlib/ in a parent directory changes nothing" "2 : Int"
      (run ~dir:sub [ "-e"; "List.length [1,2]" ]))

(* The override is what makes a built binary usable against a working tree,
   so it has to actually replace the library rather than merely be read. *)
let test_override_replaces_the_library () =
  with_scratch (fun d ->
    let alt = Filename.concat d "alt" in
    Unix.mkdir alt 0o755;
    List.iter
      (fun (name, src) ->
        let src = if name = "List" then "let length x = 99\n" else src in
        Out_channel.with_open_text (Filename.concat alt (name ^ ".wand"))
          (fun oc -> Out_channel.output_string oc src))
      Stdlib_embed.table;
    let cmd =
      Printf.sprintf "cd %s && WAND_STDLIB=%s %s -e %s 2>&1" (Filename.quote d)
        (Filename.quote alt) (Filename.quote wand_binary)
        (Filename.quote "List.length [1,2]")
    in
    let ic = Unix.open_process_in cmd in
    let out = String.trim (In_channel.input_all ic) in
    ignore (Unix.close_process_in ic);
    Alcotest.(check string) "WAND_STDLIB wins over the embedded table"
      "99 : Int" out)

(* A bare constructor that two modules declare (#153). The embedded
   library has no such name, so a library with one more type in `Digest`
   makes `Held` a constructor of `Digest` and of `FS`. The error names
   both, and `wand t --fix` changes nothing. *)
let test_constructor_of_two_modules () =
  with_scratch (fun d ->
    let alt = Filename.concat d "alt" in
    Unix.mkdir alt 0o755;
    List.iter
      (fun (name, src) ->
        let src =
          if name = "Hash" then src ^ "\ntype Clash = Held | Other\n" else src
        in
        Out_channel.with_open_text (Filename.concat alt (name ^ ".wand"))
          (fun oc -> Out_channel.output_string oc src))
      Stdlib_embed.table;
    let file = Filename.concat d "clash.wand" in
    let src = "let x = Held\n" in
    Out_channel.with_open_text file (fun oc -> Out_channel.output_string oc src);
    let wand args =
      let cmd =
        Printf.sprintf "cd %s && WAND_STDLIB=%s %s %s 2>&1" (Filename.quote d)
          (Filename.quote alt) (Filename.quote wand_binary) args
      in
      let ic = Unix.open_process_in cmd in
      let out = In_channel.input_all ic in
      ignore (Unix.close_process_in ic);
      out
    in
    let contains s sub =
      let n = String.length sub in
      let rec go i =
        i + n <= String.length s && (String.sub s i n = sub || go (i + 1))
      in
      go 0
    in
    let out = wand "t clash.wand" in
    Alcotest.(check bool) "names both" true
      (contains out "'Held' is a constructor of more than one module: \
                     'FS.Held', 'Hash.Held'");
    ignore (wand "t --fix clash.wand");
    Alcotest.(check string) "the file is as it was" src
      (In_channel.with_open_text file In_channel.input_all))

let () =
  Alcotest.run "stdlib_embed"
    [ ( "table",
        [ Alcotest.test_case "same modules as on disk" `Quick test_same_modules;
          Alcotest.test_case "same sources as on disk" `Quick test_same_sources;
          Alcotest.test_case "sources are whole" `Quick test_sources_are_whole
        ] );
      ( "location",
        [ Alcotest.test_case "runs outside any tree" `Quick
            test_runs_outside_any_tree;
          Alcotest.test_case "decoy stdlib/ ignored" `Quick
            test_decoy_stdlib_is_ignored;
          Alcotest.test_case "decoy stdlib/ above ignored" `Quick
            test_decoy_stdlib_above_is_ignored;
          Alcotest.test_case "WAND_STDLIB replaces the library" `Quick
            test_override_replaces_the_library;
          Alcotest.test_case "a constructor of two modules" `Quick
            test_constructor_of_two_modules
        ] )
    ]
