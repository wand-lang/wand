(* The commands each function runs, by the word in command position.

   A manifest's `Shell(kubectl)` names the binaries a file runs. The file's
   own `$(...)` sites say which those are, and so do the functions it calls
   from other modules: a script that calls `plimsoll.apply!` runs kubectl
   as surely as one that writes `$(kubectl ...)`. So each module answers,
   for each of its top-level bindings, the words that binding can run --
   its own sites, the bindings of its own that it names, and the imported
   functions it names -- and a file that imports it counts the words of
   what it names.

   Read off the syntax, not the types. A binding counts as run when it is
   named, called or not: passed to `List.map`, it runs there. That can say
   a word runs where it does not -- a local that shadows the name, a branch
   never taken -- and never the other way round, which is the direction a
   manifest has to err in. It needs only the parse, so a module whose types
   come from the compile cache still answers. *)

type t = {
  words : string list;  (* sorted, each once *)
  (* A command whose word is not written out, where nothing bounds it: an
     interpolated word, a shell compound, or a `%!{...}`, in a module with
     no `Shell(...)` list. What it runs is not known, so no list of words
     can promise to hold it. *)
  dynamic : bool;
}

let empty = { words = []; dynamic = false }

let union a b =
  { words = List.sort_uniq compare (a.words @ b.words);
    dynamic = a.dynamic || b.dynamic }

let is_empty t = t.words = [] && not t.dynamic

(* The expressions directly under `e`. *)
let children (e : Ast.expr) : Ast.expr list =
  match e with
  | App (a, b) | BinOp (_, a, b) | Seq (a, b) | Let (_, a, b, _) | With (a, _, b) ->
    [a; b]
  | Fn (_, a) | UnOp (_, a) | Annot (_, a) | Field (a, _) | Try a
  | Located (_, a) | Qualified (_, a)
  | RunCmd (a, _) | RunQuery (a, _) | MkCommand (a, _) -> [a]
  | LetRec (bs, b, _) -> List.map (fun (_, _, e) -> e) bs @ [b]
  | If (c, t, e) -> [c; t; e]
  | Match (s, cases) ->
    s :: List.concat_map (fun (_, g, b) -> Option.to_list g @ [b]) cases
  | Tuple es | List es -> es
  | ConstrApp (_, fs, _) -> List.map snd fs
  | ConstrUpdate (_, b, fs, _) -> b :: List.map snd fs
  | MapLit kvs -> List.map snd kvs
  | Contract (rs, es, b) -> rs @ es @ [b]
  | Interp (parts, _) | RawInterp (parts, _) -> List.map snd parts
  | CmdInterp (parts, _) -> List.map (fun (_, e, _) -> e) parts
  | Handle (b, cases) ->
    b :: List.map (function
      | Ast.EffectCase (_, _, _, x) | Ast.ReturnCase (_, x) -> x) cases
  | _ -> []

(* The name `e` refers to, as the file writes it: `f`, or `K.f`. *)
let name_of (e : Ast.expr) =
  match e with
  | Var n -> Some n
  | Field (m, n) ->
    (match Ast.strip_located m with
     | Var m | Constr m -> Some (m ^ "." ^ n)
     | _ -> None)
  | Qualified (m, inner) ->
    (match Ast.strip_located inner with
     | Var n -> Some (m ^ "." ^ n)
     | _ -> None)
  | _ -> None

(* Every node under `e`, with the nearest location above it. *)
let rec walk (loc : Token.loc) (e : Ast.expr) f =
  f loc e;
  let loc = match e with Located (l, _) -> l | _ -> loc in
  List.iter (fun c -> walk loc c f) (children e)

(* What the command sites under `e` run. `bound` is the file's own
   `Shell(...)` list: a word that is not written out can still only be one
   of those, because the site carries the list and is checked against it
   when it runs. *)
let of_sites ?bound (e : Ast.expr) =
  let acc = ref empty in
  let unknown () =
    acc := union !acc
        (match bound with
         | Some ws -> { words = ws; dynamic = false }
         | None -> { empty with dynamic = true })
  in
  walk (Token.point 1 1 0) e (fun _ e ->
    match e with
    | RunCmd (payload, _) | RunQuery (payload, _) | MkCommand (payload, _) ->
      let s = Shell_scan.scan (Shell_scan.segs_of_cmd payload) in
      if s.Shell_scan.raw_tail then unknown ();
      List.iter (function
        | Shell_scan.Literal w -> acc := union !acc { words = [w]; dynamic = false }
        | Shell_scan.Dynamic | Shell_scan.Compound _ -> unknown ())
        s.Shell_scan.words
    | _ -> ());
  !acc

(* Each name under `e` that the table knows, with where it is written. *)
let calls (table : (string * t) list) (e : Ast.expr) =
  let found = ref [] in
  walk (Token.point 1 1 0) e (fun loc e ->
    match name_of e with
    | Some n ->
      (match List.assoc_opt n table with
       | Some w when not (is_empty w) -> found := (loc, n, w) :: !found
       | _ -> ())
    | None -> ());
  List.rev !found

(* Whether `src` has a command in it at all: `$(`, `$?(` or `$*(`. A
   search of the text costs a small part of a walk of the tree, and most
   modules have none. A comment can hold one, so a yes means only "read
   the tree". *)
let may_run (src : string) =
  let n = String.length src in
  let rec go i =
    match String.index_from_opt src i '$' with
    | None -> false
    | Some j when j + 1 < n ->
      let c = src.[j + 1] in
      if c = '(' then true
      else if (c = '?' || c = '*') && j + 2 < n && src.[j + 2] = '(' then true
      else go (j + 1)
    | Some _ -> false
  in
  go 0

(* The file's `Shell(...)` list, when its manifest has one. *)
let bound_of (prog : Ast.program) =
  match prog.manifest with
  | Some (labels, _) -> Option.join (List.assoc_opt "Shell" labels)
  | None -> None

let bindings_of (prog : Ast.program) =
  List.concat_map (fun (item : Ast.top_item) ->
    match item with
    | TLLet (n, _, b) -> [(n, b)]
    | TLLetRec bs -> List.map (fun (n, _, b) -> (n, b)) bs
    | TLLetPat (p, b) -> List.map (fun n -> (n, b)) (Ast.pat_names p)
    | _ -> []) prog.items

(* What each top-level binding of `prog` runs, by its bare name. `imported`
   is what the file's imports run, under the names the file writes them.

   A binding runs what it names, and what those name in turn, so the own
   bindings are read to a fixed point: each pass can only add words, and
   there are finitely many to add. *)
let of_program ~(imported : (string * t) list) (prog : Ast.program) =
  let bound = bound_of prog in
  let bindings = bindings_of prog in
  let own_names = Hashtbl.create 64 in
  List.iter (fun (n, _) -> Hashtbl.replace own_names n ()) bindings;
  let start =
    List.map (fun (n, b) ->
      let from_imports =
        List.fold_left (fun acc (_, _, w) -> union acc w) empty
          (calls imported b)
      in
      (n, union (of_sites ?bound b) from_imports)) bindings
  in
  (* Most modules run nothing, the standard library among them, and every
     run loads some. Such a module answers at once. *)
  if List.for_all (fun (_, w) -> is_empty w) start then [] else
  (* The own names each binding mentions, found once. *)
  let mentions =
    List.map (fun (n, b) ->
      let names = ref [] in
      walk (Token.point 1 1 0) b (fun _ e ->
        match name_of e with
        | Some m when Hashtbl.mem own_names m && not (List.mem m !names) ->
          names := m :: !names
        | _ -> ());
      (n, !names)) bindings
  in
  let table = Hashtbl.create 16 in
  List.iter (fun (n, w) ->
    let prev = Option.value (Hashtbl.find_opt table n) ~default:empty in
    Hashtbl.replace table n (union prev w)) start;
  let changed = ref true in
  while !changed do
    changed := false;
    List.iter (fun (n, ms) ->
      let now = Hashtbl.find table n in
      let next =
        List.fold_left (fun acc m -> union acc (Hashtbl.find table m)) now ms in
      if next <> now then begin
        Hashtbl.replace table n next;
        changed := true
      end) mentions
  done;
  List.filter_map (fun n ->
    let w = Hashtbl.find table n in
    if is_empty w then None else Some (n, w))
    (List.sort_uniq compare (List.map fst bindings))

(* Every place `prog` names an imported function that runs a command. *)
let imported_calls ~(imported : (string * t) list) (prog : Ast.program) =
  List.concat_map (fun (item : Ast.top_item) ->
    let bodies = match item with
      | TLLet (_, _, b) | TLLetPat (_, b) | TLExpr b -> [b]
      | TLLetRec bs -> List.map (fun (_, _, b) -> b) bs
      | TLImplement (im, _) -> List.map (fun (_, _, b, _) -> b) im.Ast.im_binds
      | TLImport _ | TLType _ | TLInterface _ -> []
    in
    List.concat_map (calls imported) bodies) prog.items
