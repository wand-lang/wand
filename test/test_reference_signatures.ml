(* Every signature the reference states, checked against the binary.

   `Par.each` was documented as `('a -> Unit ! 'e)` for a while after it
   stopped requiring Unit. Nothing caught it: the module-list test compares
   `stdlib_module_names` against the directory, and the lint-rule tests
   check that cited rules exist, but no test read a documented type.

   So this one reads them all. A line of the form `Module.name : type`
   inside the reference is looked up with `wand t --expr` and compared. Lines
   naming something that is not a standard library module are skipped --
   the document defines a few types of its own for examples. *)

let wand_binary =
  let dir = Filename.dirname (Filename.dirname Sys.executable_name) in
  Filename.concat (Filename.concat dir "bin") "wand.exe"

let reference_path = "../docs/reference.md"

let run_type expr =
  let cmd =
    Printf.sprintf "%s t --expr %s 2>&1" (Filename.quote wand_binary)
      (Filename.quote expr)
  in
  let ic = Unix.open_process_in cmd in
  let out = In_channel.input_all ic in
  ignore (Unix.close_process_in ic);
  String.trim out

(* Alignment is not a difference: the reference pads `JSON.decode ` and
   `Args.parse  ` to line up their colons. *)
let squeeze s =
  let buf = Buffer.create (String.length s) in
  let last_space = ref false in
  String.iter
    (fun c ->
      let is_space = c = ' ' || c = '\t' in
      if is_space then (if not !last_space then Buffer.add_char buf ' ')
      else Buffer.add_char buf c;
      last_space := is_space)
    s;
  String.trim (Buffer.contents buf)

let is_name_char c =
  (c >= 'a' && c <= 'z')
  || (c >= 'A' && c <= 'Z')
  || (c >= '0' && c <= '9')
  || c = '_' || c = '?' || c = '!'

(* A `### `Module`` heading. Signatures under one are written without the
   prefix -- `map : ...` inside `### `List`` -- so the heading says which
   module they belong to. *)
let heading_of line =
  let n = String.length line in
  if n > 6 && String.sub line 0 5 = "### `" && line.[n - 1] = '`' then
    let name = String.sub line 5 (n - 6) in
    if List.mem name Wand.Typechecker.stdlib_module_names then Some name else None
  else None

(* `name : type` or `Module.name : type`, and nothing else. A space on the
   left means this is prose or a binding (`let n : Int = ...`). *)
let signature_of ?current_module line =
  match String.index_opt line ':' with
  | None -> None
  | Some i when i + 1 >= String.length line || line.[i + 1] <> ' ' -> None
  | Some i ->
    let lhs = String.trim (String.sub line 0 i) in
    let rhs = String.trim (String.sub line (i + 1) (String.length line - i - 1)) in
    (* `wand d` marks a member that answers to an interface -- `[Ord]` after
       the type -- and the reference shows its real output. The mark is not
       part of the type, so it comes off before the comparison. *)
    let rhs =
      match String.rindex_opt rhs '[' with
      | Some b when String.length rhs > 0 && rhs.[String.length rhs - 1] = ']' ->
        String.trim (String.sub rhs 0 b)
      | _ -> rhs
    in
    if lhs = "" || rhs = "" then None
    else if String.exists (fun c -> c = ' ') lhs then None
    else if not (String.for_all (fun c -> is_name_char c || c = '.') lhs) then None
    else
      (match String.index_opt lhs '.' with
       | Some d ->
         let m = String.sub lhs 0 d in
         let member = String.sub lhs (d + 1) (String.length lhs - d - 1) in
         if member = "" || not (List.mem m Wand.Typechecker.stdlib_module_names)
         then None
         else Some (lhs, rhs)
       | None ->
         (* Unprefixed: a member of whichever module's section it is in. *)
         (match current_module with
          | Some m -> Some (m ^ "." ^ lhs, rhs)
          | None -> None))

let signatures () =
  let ic = open_in reference_path in
  let rec loop current acc =
    match In_channel.input_line ic with
    | None -> List.rev acc
    | Some line ->
      (* Any heading ends the previous module's scope, so prose elsewhere in
         the document is never read as one of its members. *)
      (match (if String.length line > 0 && line.[0] = '#' then Some (heading_of line) else None) with
       | Some m -> loop m acc
       | None ->
         (match signature_of ?current_module:current line with
          | Some s -> loop current (s :: acc)
          | None -> loop current acc))
  in
  let out = loop None [] in
  close_in ic;
  out

let test_documented_signatures_are_real () =
  let sigs = signatures () in
  (* A parser that matches nothing passes every comparison it never makes. *)
  Alcotest.(check bool)
    "found signatures to check (the parser still matches the document)" true
    (List.length sigs >= 100);
  List.iter
    (fun (name, documented) ->
      let actual = run_type name in
      if squeeze documented <> squeeze actual then
        Alcotest.failf
          "the reference documents %s as:\n  %s\nbut wand reports:\n  %s" name
          documented actual)
    sigs

(* The other direction. The test above proves that what the reference says is
   true; it says nothing about what the reference leaves out, and a function
   documented nowhere is as good as absent to a reader. `Env.read!`,
   `Env.load!`, `IO.read_line!` and `IO.read_all!` were exported and named
   nowhere in the document, and nothing noticed. *)

let members_of m =
  let open Wand in
  let sess = Runner.make_session () in
  match Runner.run_session sess ("import " ^ m) with
  | Ok (sess, _) ->
    (match List.assoc_opt m sess.Runner.s_type_env with
     | Some (Wand.Typechecker.Namespace (members, _)) -> List.map fst members
     | _ -> Alcotest.failf "%s did not import as a namespace" m)
  | Error e -> Alcotest.failf "could not import %s: %s" m e

let test_every_export_is_documented () =
  let documented = List.map fst (signatures ()) in
  let missing =
    List.concat_map
      (fun m ->
        List.filter_map
          (fun f ->
            let qualified = m ^ "." ^ f in
            if List.mem qualified documented then None else Some qualified)
          (members_of m))
      Wand.Typechecker.stdlib_module_names
  in
  if missing <> [] then
    Alcotest.failf
      "the standard library exports %d name(s) the reference gives no \
       signature for:\n  %s"
      (List.length missing)
      (String.concat "\n  " missing)

let contains haystack needle =
  let hn = String.length haystack and nn = String.length needle in
  nn <= hn
  && (let found = ref false in
      for i = 0 to hn - nn do
        if String.sub haystack i nn = needle then found := true
      done;
      !found)

(* A section nothing links to is a section nobody finds. `Args` had a full
   page of its own and was the one module the contents list left out, so it
   read as missing however complete it was. *)

let test_every_module_is_listed_in_contents () =
  let doc =
    let ic = open_in reference_path in
    let s = In_channel.input_all ic in
    close_in ic; s
  in
  (* The contents links each module as `[Name](#name)`. *)
  let missing =
    List.filter
      (fun m ->
        let link = Printf.sprintf "[%s](#%s)" m (String.lowercase_ascii m) in
        not (contains doc link))
      Wand.Typechecker.stdlib_module_names
  in
  if missing <> [] then
    Alcotest.failf
      "the contents list does not link %d standard library module(s):\n  %s"
      (List.length missing)
      (String.concat "\n  " missing)

(* The reference's table of interceptable operations, which had fallen four
   behind the binary before anything enumerated them. Each line reads
   `| `FS` | `read_file`, ... |`, so the family and the backticked verbs on
   that line rebuild the `Family!verb` names to compare. *)
let documented_operations () =
  let doc =
    let ic = open_in reference_path in
    let s = In_channel.input_all ic in
    close_in ic; s
  in
  let names = ref [] in
  List.iter (fun line ->
    match String.index_opt line '|' with
    | None -> ()
    | Some _ ->
      let cells = String.split_on_char '|' line in
      (match List.filter (fun c -> String.trim c <> "") cells with
       | family :: rest when List.length rest >= 1 ->
         let unquote s =
           let s = String.trim s in
           let n = String.length s in
           if n >= 2 && s.[0] = '`' && s.[n-1] = '`' then Some (String.sub s 1 (n-2))
           else None
         in
         (match unquote family with
          | Some fam
            when List.mem fam
                   ["Shell"; "Net"; "FS"; "Hash"; "Env"; "IO"; "Proc"; "Clock";
                    "Random"; "Shared"] ->
            List.iter (fun cell ->
              List.iter (fun verb ->
                match unquote verb with
                | Some v -> names := (fam ^ "!" ^ v) :: !names
                | None -> ())
                (String.split_on_char ',' cell))
              rest
          | _ -> ())
       | _ -> ())
  ) (String.split_on_char '\n' doc);
  !names

let test_operations_table_matches_the_binary () =
  let documented = documented_operations () in
  Alcotest.(check bool)
    "found the operations table (the parser still matches the document)" true
    (List.length documented >= 30);
  let real = Wand.Typechecker.operation_names () in
  let missing = List.filter (fun o -> not (List.mem o documented)) real in
  let extra = List.filter (fun o -> not (List.mem o real)) documented in
  if missing <> [] || extra <> [] then
    Alcotest.failf
      "the reference's operations table disagrees with the binary:\n\
      \  undocumented: %s\n\
      \  documented but not real: %s"
      (if missing = [] then "(none)" else String.concat ", " missing)
      (if extra = [] then "(none)" else String.concat ", " extra)

let () =
  Alcotest.run "reference signatures"
    [ ( "stdlib",
        [ Alcotest.test_case "match the binary" `Quick
            test_documented_signatures_are_real;
          Alcotest.test_case "cover every export" `Quick
            test_every_export_is_documented;
          Alcotest.test_case "list every module" `Quick
            test_every_module_is_listed_in_contents;
          Alcotest.test_case "operations table matches" `Quick
            test_operations_table_matches_the_binary
        ] )
    ]
