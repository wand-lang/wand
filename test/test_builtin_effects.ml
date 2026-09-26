open Wand

(* What each primitive says it performs, checked against what it performs.

   A primitive's effects in `Typechecker.stdlib_type_env` are written by
   hand, and they are the only thing standing between a manifest and the
   truth: inference believes them, so a primitive that understates what it
   does makes the whole manifest wrong for every file downstream.
   `env_load_file` said `{Env, Raise}` while performing a real
   `FS!read_file`, so a file whose entire manifest was `uses {Env}` could
   read any path on disk and typecheck -- and A-USES1 advised trimming the
   honest manifest back to the lie. Nothing caught it, because nothing
   compared the two halves.

   This compares them. The declared half is read from the typechecker's own
   table rather than parsed, so it cannot be misread. The performed half has
   to come from the source: the effects a builtin performs are inside its
   closure, and there is no argument you can pass a closure to be told what
   it would do. So this reads `lib/evaluator.ml` and takes every operation
   named in each builtin's body.

   That makes it a text scan, which can silently match nothing when the file
   is reshaped -- so `test_scan_found_builtins` fails if the shape stops
   parsing, and the whole suite is worthless without it. *)

let evaluator_source = "../lib/evaluator.ml"

let read_source () =
  if not (Sys.file_exists evaluator_source) then
    Alcotest.failf "evaluator source not found at %s (relative to the test sandbox)"
      evaluator_source
  else In_channel.with_open_text evaluator_source In_channel.input_all

(* A top-level entry in the builtin table: two spaces, then ("name",. Nested
   tuples sit deeper than that, so the anchor picks out exactly the entries. *)
let entry_re  = Re.Pcre.re ~flags:[`MULTILINE] {|^  \("([a-z_0-9]+)",|}
let perform_re = Re.Pcre.re {|(?:WandEffect|perform_wand) \("([^"]+)"|}
let performing_re = Re.Pcre.re {|performing "([^"]+)"|}

let compile = Re.compile

(* name -> the operation names its body mentions. A builtin that performs
   nothing does not appear. *)
let performed_ops src =
  let starts =
    Re.all (compile entry_re) src
    |> List.map (fun g -> (Re.Group.get g 1, fst (Re.Group.offset g 0)))
  in
  let rec bodies acc = function
    | [] -> acc
    | (name, start) :: rest ->
      let stop = match rest with (_, next) :: _ -> next | [] -> String.length src in
      let body = String.sub src start (stop - start) in
      let ops =
        (Re.all (compile perform_re) body
         |> List.map (fun g -> Re.Group.get g 1))
        @ (Re.all (compile performing_re) body
           |> List.map (fun g -> Re.Group.get g 1))
      in
      let ops = List.sort_uniq String.compare ops in
      bodies (if ops = [] then acc else (name, ops) :: acc) rest
  in
  List.rev (bodies [] starts)

let labels_of_ops ops =
  List.fold_left (fun acc op ->
    match Typechecker.effect_of_operation op with
    | Some e -> Effect_set.EffSet.add e acc
    | None   -> acc) Effect_set.EffSet.empty ops

let declared_labels name =
  match List.assoc_opt name Typechecker.stdlib_type_env with
  | Some scheme -> Some (Typechecker.manifest_labels_of_scheme scheme)
  | None -> None

let show set =
  Effect_set.EffSet.elements set
  |> List.map Effect_set.name_of
  |> String.concat ", "

(* Without this the suite passes by finding nothing to check. *)
let test_scan_found_builtins () =
  let found = performed_ops (read_source ()) in
  if List.length found < 30 then
    Alcotest.failf
      "only %d effect-performing builtins found in %s -- the table's shape \
       changed and this suite is no longer reading it"
      (List.length found) evaluator_source

(* An operation named in a body that the operations table does not know is
   either a typo in the evaluator or a table that lost an entry. Either way
   its effect is invisible to the check above, so it cannot pass quietly. *)
let test_every_operation_is_known () =
  performed_ops (read_source ())
  |> List.iter (fun (name, ops) ->
    List.iter (fun op ->
      if Typechecker.effect_of_operation op = None then
        Alcotest.failf
          "%s performs %S, which the operations table does not define"
          name op) ops)

let test_declared_covers_performed () =
  performed_ops (read_source ())
  |> List.iter (fun (name, ops) ->
    match declared_labels name with
    | None -> ()  (* not a primitive with hand-written effects; nothing to check *)
    | Some declared ->
      let performed = labels_of_ops ops in
      let missing = Effect_set.EffSet.diff performed declared in
      if not (Effect_set.EffSet.is_empty missing) then
        Alcotest.failf
          "%s performs %s but declares only %s -- missing %s.\n\
           A file calling it would typecheck without declaring %s."
          name (show performed) (show declared) (show missing) (show missing))

(* The bug this suite was written for, kept as its own case so a regression
   names it rather than appearing as one line of a sweep. *)
let test_env_load_file_declares_fs_read () =
  match declared_labels "env_load_file" with
  | None -> Alcotest.fail "env_load_file is missing from stdlib_type_env"
  | Some declared ->
    Alcotest.(check bool)
      "env_load_file reads a file, so it declares FS.Read" true
      (Effect_set.EffSet.mem Effect_set.FsRead declared)

let () =
  Alcotest.run "Builtin effects" [
    "scan", [
      Alcotest.test_case "finds the builtins"  `Quick test_scan_found_builtins;
      Alcotest.test_case "operations are known" `Quick test_every_operation_is_known;
    ];
    "declared", [
      Alcotest.test_case "covers what is performed" `Quick test_declared_covers_performed;
      Alcotest.test_case "env_load_file reads"       `Quick test_env_load_file_declares_fs_read;
    ];
  ]
