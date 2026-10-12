(* The standard library imports a file is missing, read from its parse
   alone (#150). `Digest.hex` where `Digest` is a standard library module,
   the file does not import it, and no name of the file's own is `Digest`
   needs `import Digest`, and nothing about types has to be known to say
   so. The typechecker stops at its first error, so a file with a type
   error and three missing imports was told about one thing at a time, and
   `wand t --fix` added nothing while the type error stood. This pass finds
   every one of them, whatever else is wrong with the file, so `wand t`
   reports them all and `wand t --fix` adds them all in one run.

   It finds only what the typechecker would also refuse: a name the file
   could mean as something of its own is left alone. The language does not
   change -- a file still needs its imports. *)

open Ast

let is_upper n = n <> "" && n.[0] >= 'A' && n.[0] <= 'Z'

(* `Option.Some` names a constructor of the built-in type, with no import:
   the typechecker asks for a type of the name before it asks for a
   module. *)
let builtin_ctor_type m =
  match List.assoc_opt m Typechecker.builtin_tenv with
  | Some (Variants (_, _, _ :: _)) -> true
  | _ -> false

(* Every name in a pattern: its variables, the keys of a map pattern and
   the constructors it names. A destructured import binds the names in it,
   and any of them can be a type that a `Name.Ctor` reaches. *)
let rec pat_names acc (p : pat) =
  match p with
  | PVar n -> n :: acc
  | PTuple ps | PList ps -> List.fold_left pat_names acc ps
  | PCons (a, b) -> pat_names (pat_names acc a) b
  | PConstr (n, ps) -> List.fold_left pat_names (n :: acc) ps
  | PConstrNamed (n, fs) ->
    List.fold_left (fun acc (f, p) -> pat_names (f :: acc) p) (n :: acc) fs
  | PConstrBare (n, ids) -> n :: ids @ acc
  | PQualified (_, p) | PAnnot (p, _) | PLocated (_, p) | PCtor (_, p, _) ->
    pat_names acc p
  | PMap fs -> List.fold_left (fun acc (k, p) -> pat_names (k :: acc) p) acc fs
  | POr ps -> List.fold_left pat_names acc ps
  | Int _ | Float _ | String _ | Bool _ | Unit | Path _ | DateTime _
  | Duration _ | URL _ | IPv4 _ | CIDR _ | Port _ | Version _ | Size _
  | Wild -> acc

(* The names the file gives things of its own at the top level: what its
   imports bind, its types and their constructors, its interfaces, and
   every name a top-level binding binds. Only an uppercase one can stand
   where a module's name does, but all are kept, since that costs
   nothing. *)
let own_names (prog : program) =
  List.concat_map (function
    | TLImport k ->
      (match Parser.import_name k with Some n -> [n] | None -> [])
    | TLLet (n, ps, _) -> n :: List.fold_left pat_names [] ps
    | TLLetRec binds ->
      List.concat_map (fun (n, ps, _) -> n :: List.fold_left pat_names [] ps)
        binds
    | TLLetPat (p, _) -> pat_names [] p
    | TLType (Variants (n, _, ctors), _) ->
      n :: List.map (fun (c : ctor_def) -> c.name) ctors
    | TLType (Alias (n, _, _), _) -> [n]
    | TLInterface (i, _) -> [i.if_name]
    | TLImplement _ | TLExpr _ -> []) prog.items

(* The standard library modules the file imports by name. Only `import X`
   puts `X` in scope: `let {hex} = import Digest` takes `hex` and leaves
   `Digest.hex` unknown, and the typechecker says the same. *)
let imported (prog : program) =
  List.filter_map (function
    | TLImport (StdlibModule m) -> Some m
    | _ -> None) prog.items

(* Each qualifier the file uses, with where it was used: the innermost
   position the parse kept around the use. In the order they are met. *)
let uses (prog : program) item_locs =
  let found = ref [] in
  let note m at =
    if is_upper m then found := (m, at) :: !found
  in
  let rec ty at = function
    | TEName _ | TEVar _ -> ()
    | TEQual (m, _) -> if not (builtin_ctor_type m) then note m at
    | TEApp (a, b) -> ty at a; ty at b
    | TETuple ts -> List.iter (ty at) ts
    | TEFun (a, b, _) -> ty at a; ty at b
  in
  let rec pat at = function
    | PLocated (l, p) -> pat (Some l) p
    | PCtor (_, p, _) -> pat at p
    | PQualified (m, p) ->
      if not (builtin_ctor_type m) then note m at;
      pat at p
    | PAnnot (p, t) -> pat at p; ty at t
    | PTuple ps | PList ps -> List.iter (pat at) ps
    | PCons (a, b) -> pat at a; pat at b
    | PConstr (_, ps) -> List.iter (pat at) ps
    | PConstrNamed (_, fs) -> List.iter (fun (_, p) -> pat at p) fs
    | PMap fs -> List.iter (fun (_, p) -> pat at p) fs
    | POr ps -> List.iter (pat at) ps
    | PConstrBare _ | PVar _ | Wild | Int _ | Float _ | String _ | Bool _
    | Unit | Path _ | DateTime _ | Duration _ | URL _ | IPv4 _ | CIDR _
    | Port _ | Version _ | Size _ -> ()
  in
  let rec ex at (e : expr) =
    let go = ex at in
    match e with
    | Located (l, e) -> ex (Some l) e
    (* `Digest.hex`: a member read through the module's name. *)
    | Field (inner, _) ->
      (match strip_located inner with
       | Constr m -> note m (match inner with Located (l, _) -> Some l | _ -> at)
       | _ -> ());
      go inner
    (* `Digest.Sha256`: a constructor reached through it. *)
    | Qualified (m, inner) ->
      if not (builtin_ctor_type m) then note m at;
      go inner
    | Int _ | Float _ | String _ | Bool _ | Unit | Path _ | Glob _
    | DateTime _ | Duration _ | URL _ | IPv4 _ | CIDR _ | Port _ | Version _
    | Size _ | Var _ | Constr _ | EnvVar _ | Hole | RegexLit _ | ImportExpr _
    | RawString _ | ConstrBare _ -> ()
    | App (a, b) | BinOp (_, a, b) | Seq (a, b) -> go a; go b
    | Fn (ps, b) -> List.iter (pat at) ps; go b
    | Let (p, a, b, _) -> pat at p; go a; go b
    | LetRec (binds, b, _) ->
      List.iter (fun (_, ps, e) -> List.iter (pat at) ps; go e) binds;
      go b
    | If (a, b, c) | Unless (a, b, c) -> go a; go b; go c
    | Match (e, cases) ->
      go e;
      List.iter (fun (p, g, b) -> pat at p; Option.iter go g; go b) cases
    | UnOp (_, a) | RunCmd (a, _) | RunQuery (a, _) | MkCommand (a, _)
    | Try a -> go a
    | Tuple es | List es -> List.iter go es
    | ConstrApp (_, args, _) -> List.iter (fun (_, e) -> go e) args
    | ConstrUpdate (_, r, fs, _) -> go r; List.iter (fun (_, e) -> go e) fs
    | Contract (pre, post, b) -> List.iter go pre; List.iter go post; go b
    | Interp (parts, _) | RawInterp (parts, _) ->
      List.iter (fun (_, e) -> go e) parts
    | CmdInterp (parts, _) -> List.iter (fun (_, e, _) -> go e) parts
    | Handle (e, cases) ->
      go e;
      List.iter (function
        | EffectCase (alts, _, b) -> List.iter (fun (_, p) -> pat at p) alts; go b
        | ReturnCase (p, b) -> pat at p; go b) cases
    | With (r, p, b) -> go r; pat at p; go b
    | Annot (t, e) -> ty at t; go e
    | MapLit fs -> List.iter (fun (_, e) -> go e) fs
    | Slot _ | Global _ | GlobalCtor _ | ModField _ -> ()
  in
  List.iteri (fun i item ->
    let at =
      match List.nth_opt item_locs i with
      | Some (a, b) -> Some (Token.span_to a b)
      | None -> None
    in
    match item with
    | TLLet (_, ps, e) -> List.iter (pat at) ps; ex at e
    | TLLetRec binds ->
      List.iter (fun (_, ps, e) -> List.iter (pat at) ps; ex at e) binds
    | TLLetPat (p, e) -> pat at p; ex at e
    | TLExpr e -> ex at e
    | TLImport _ -> ()
    (* A field default is read with nothing in scope, so an import changes
       nothing about it: `Map.empty` there wants `{}`, not `import Map`. *)
    | TLType (Variants (_, _, ctors), _) ->
      List.iter (fun (c : ctor_def) ->
        List.iter (fun (_, t) -> ty at t) c.fields) ctors
    | TLType (Alias (_, _, t), _) -> ty at t
    | TLInterface (i, _) -> List.iter (fun (_, t) -> ty at t) i.if_members
    | TLImplement (im, _) ->
      List.iter (ty at) im.im_args;
      List.iter (fun (_, ps, e, _) -> List.iter (pat at) ps; ex at e)
        im.im_binds) prog.items;
  List.rev !found

(* Where `m` stands in `l`: the token that spells it, followed by its dot. *)
let name_in (l : Token.loc) m =
  let toks = Typechecker.tokens_of_file l.Token.file in
  let n = Array.length toks in
  let rec go i =
    if i + 1 >= n then None
    else
      let (a, la) = toks.(i) and (d, ld) = toks.(i + 1) in
      if a = Token.Upper m && d = Token.Dot
         && ld.Token.offset = la.Token.end_offset
         && Typechecker.inside l la
      then Some la
      else go (i + 1)
  in
  go 0

let message m =
  Printf.sprintf "did you forget to import the standard library %s?" m

(* The missing imports, one diagnostic each, at the first use of each
   module, in the order of those uses. Each carries the line that adds the
   import, as the typechecker's own finding for one does. *)
let find ?(item_locs = []) (prog : program) : Diag.t list =
  let own = own_names prog in
  let have = imported prog in
  let missing (m, _) =
    List.mem m Typechecker.stdlib_module_names
    && not (List.mem m have) && not (List.mem m own)
  in
  let firsts =
    List.fold_left (fun acc (m, at) ->
      if List.mem_assoc m acc then acc else (m, at) :: acc)
      [] (List.filter missing (uses prog item_locs))
    |> List.rev
  in
  List.map (fun (m, at) ->
    let loc =
      match at with
      | Some l -> (match name_in l m with Some t -> Some t | None -> Some l)
      | None -> None
    in
    Diag.error ~code:"E-TYPE" ?loc ~fix:(Diag.InsertLine ("import " ^ m))
      (message m)) firsts

(* Whether `d` already says that `m` is not imported. The typechecker
   reports one missing import on its own, and the pass must not report it a
   second time. *)
let reports d m = Diag.contains d.Diag.message (message m)

(* The diagnostics of `extra` that none of `ds` already gives. *)
let not_in ds extra =
  List.filter (fun x ->
    let m =
      match x.Diag.fix with
      | Some (Diag.InsertLine t) ->
        String.sub t 7 (String.length t - 7)
      | _ -> ""
    in
    not (List.exists (fun d -> reports d m) ds)) extra
