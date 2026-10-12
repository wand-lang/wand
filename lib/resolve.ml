(* Names resolved before a definition runs.

   The evaluator's environment is a list: binding a name pushes it on the
   front. Looking a name up by its text walks that list and compares
   strings, and past the file's own definitions it hashes the name into an
   index. Most of what a definition names is bound inside the definition
   itself -- a parameter, a `let`, a pattern of a `match` arm -- and for
   those the place in the list is known before the definition runs: it is
   the number of bindings between the name and its binder.

   This pass writes that number down. A `Var` whose binder it can see
   becomes `Slot (binder, d)`, which the evaluator reads by going `d` steps
   down the list with no compare on the way. The binder's own string goes
   with it, and the binding found there must hold that same string, by one
   pointer compare; if it does not, the evaluator looks the name up by its
   text as before. So the pass can make a lookup cheaper and never wrong:
   where the shape of the environment at run time differs from what the
   pass saw -- the REPL merges the equations of a function, say -- the
   lookup only takes the long way.

   A name the pass cannot see bound -- a definition of the file, an import,
   a builtin -- becomes `Global`, which is looked up by its text and keeps
   what an index answered last time. `m.f` on a module becomes `ModField`,
   which keeps the field too.

   The pass models exactly what the evaluator pushes, and nothing else:

   - a pattern pushes its names in the order `pattern_names` gives, which
     is the order `try_match` binds them;
   - `fn p1 p2 -> b` pushes p1's names, then p2's, before `b`;
   - `let f = fn ...` also pushes `f` itself, in front of the parameters
     -- the evaluator makes such a function recursive;
   - `let rec` pushes each name in order, and each body sees them all;
   - a handler's effect case pushes its pattern and then the
     continuation's name.

   Anything else stops the pass from looking outward. A construct it does
   not know is resolved as if nothing were bound outside it, so the names
   inside it that are bound outside stay lookups by text. *)

open Ast

(* The names a pattern binds, in the order `try_match` pushes them. *)
let rec pattern_names (p : pat) =
  match p with
  | PVar n -> [n]
  | PTuple ps | PList ps | PConstr (_, ps) -> List.concat_map pattern_names ps
  | PCons (h, t) -> pattern_names h @ pattern_names t
  | PConstrNamed (_, kvs) | PMap kvs ->
    List.concat_map (fun (_, p) -> pattern_names p) kvs
  | PConstrBare (_, ids) -> ids
  | PQualified (_, p) | PAnnot (p, _) | PLocated (_, p) | PCtor (_, p, _) ->
    pattern_names p
  | Int _ | Float _ | String _ | Bool _ | Unit | Path _ | DateTime _
  | Duration _ | URL _ | IPv4 _ | CIDR _ | Port _ | Version _ | Size _
  | Wild -> []

(* A scope is the front of the environment as the evaluator will build
   it: the most recent binder first. What lies past its end is not known. *)
let push_pat scope p =
  List.fold_left (fun sc n -> n :: sc) scope (pattern_names p)

let push_params scope params = List.fold_left push_pat scope params

let new_cache () = { hit = Obj.repr 0 }

(* A name the scope does not bind: past all of the scope. *)
let global scope x =
  let rec last = function [] -> "" | [n] -> n | _ :: rest -> last rest in
  { g_name = x; g_depth = List.length scope; g_edge = last scope;
    g_cache = new_cache () }

let find scope x =
  let rec go d = function
    | [] -> None
    | n :: rest -> if String.equal n x then Some (n, d) else go (d + 1) rest
  in
  go 0 scope

(* A pattern whose constructors are looked up as a `Global` is: a
   constructor is checked before what is under it binds anything, so its
   scope is what the pattern has bound before it, in the order
   `pattern_names` gives. Answers the pattern and the scope after it. A
   name the definition binds itself, and a qualified constructor, stay as
   they are, and are looked up by name. *)
let rec pat_in scope (p : pat) : pat * string list =
  let ctor name inner =
    match find scope name with
    | Some _ -> inner
    | None -> PCtor (global scope name, inner, new_cache ())
  in
  match p with
  | PVar n -> (p, n :: scope)
  | PConstr (name, ps) ->
    let ps, after = pats_in scope ps in
    (ctor name (PConstr (name, ps)), after)
  | PConstrNamed (name, kvs) ->
    let kvs, after = fields_in scope kvs in
    (ctor name (PConstrNamed (name, kvs)), after)
  | PConstrBare (_, ids) -> (ctor (pat_ctor_name p) p, push_pat scope p)
  | PTuple ps -> let ps, after = pats_in scope ps in (PTuple ps, after)
  | PList ps -> let ps, after = pats_in scope ps in (PList ps, after)
  | PCons (h, t) ->
    let h, sc = pat_in scope h in
    let t, after = pat_in sc t in
    (PCons (h, t), after)
  | PMap kvs -> let kvs, after = fields_in scope kvs in (PMap kvs, after)
  | PAnnot (inner, te) -> let inner, after = pat_in scope inner in (PAnnot (inner, te), after)
  | PLocated (l, inner) -> let inner, after = pat_in scope inner in (PLocated (l, inner), after)
  (* Already resolved: resolved again, for the place it is in now. *)
  | PCtor (_, inner, _) -> pat_in scope inner
  | PQualified _ | Int _ | Float _ | String _ | Bool _ | Unit | Path _
  | DateTime _ | Duration _ | URL _ | IPv4 _ | CIDR _ | Port _ | Version _
  | Size _ | Wild -> (p, push_pat scope p)

and pats_in scope ps =
  let ps, after = List.fold_left (fun (acc, sc) p ->
    let p, sc = pat_in sc p in (p :: acc, sc)) ([], scope) ps in
  (List.rev ps, after)

and fields_in scope kvs =
  let kvs, after = List.fold_left (fun (acc, sc) (k, p) ->
    let p, sc = pat_in sc p in ((k, p) :: acc, sc)) ([], scope) kvs in
  (List.rev kvs, after)

and pat_ctor_name = function
  | PConstr (n, _) | PConstrNamed (n, _) | PConstrBare (n, _) -> n
  | _ -> ""

let pat scope p = fst (pat_in scope p)

(* Parameters, each resolved past the ones before it. *)
let params scope ps = fst (pats_in scope ps)

(* The labels on a type that `Field` answers itself, rather than reading a
   module's member. A field with one of these names stays a `Field`. *)
let special_label = function
  | "decoder" | "encoder" | "usage" | "parser" | "all" | "loader" -> true
  | l -> tuple_index l <> None

(* Whether `let p = e` makes `e` a recursive function: the evaluator does
   so when `p` is a name and `e` is a `fn`, written as one. *)
let rec is_fn = function
  | Fn _ -> true
  | Located (_, e) | Annot (_, e) -> is_fn e
  | _ -> false

let rec expr scope (e : expr) : expr =
  let r = expr scope in
  match e with
  | Int _ | Float _ | String _ | Bool _ | Unit | Path _ | Glob _
  | DateTime _ | Duration _ | URL _ | IPv4 _ | CIDR _ | Port _ | Version _
  | Size _ | EnvVar _ | Hole | RegexLit _ | ImportExpr _
  | RawString _ -> e
  | Constr x -> GlobalCtor (global scope x)
  | GlobalCtor g -> GlobalCtor (global scope g.g_name)
  (* Already resolved: resolved again, so a tree that has been through
     this pass can go through it again for a new place. *)
  | Var x | Slot (x, _) | Global { g_name = x; _ } ->
    (match find scope x with
     | Some (binder, d) -> Slot (binder, d)
     | None -> Global (global scope x))
  | ModField (g, f, _, n) -> ModField (global scope g.g_name, f, new_cache (), n)
  | App (f, x) -> App (r f, r x)
  | Fn (ps, body) -> Fn (params scope ps, expr (push_params scope ps) body)
  | Let (PVar name, e1, e2, style) when is_fn e1 ->
    Let (PVar name, rec_fn name scope e1, expr (name :: scope) e2, style)
  | Let (p, e1, e2, style) ->
    Let (pat scope p, r e1, expr (push_pat scope p) e2, style)
  | LetRec (bindings, e2, style) ->
    let names = List.fold_left (fun sc (n, _, _) -> n :: sc) scope bindings in
    LetRec (List.map (fun (n, ps, b) ->
      (n, params names ps, expr (push_params names ps) b)) bindings,
            expr names e2, style)
  | If (a, b, c) -> If (r a, r b, r c)
  | Unless (a, b, c) -> Unless (r a, r b, r c)
  | Match (s, cases) -> Match (r s, List.map (case scope) cases)
  | BinOp (op, a, b) -> BinOp (op, r a, r b)
  | UnOp (op, a) -> UnOp (op, r a)
  | Tuple es -> Tuple (List.map r es)
  | List es -> List (List.map r es)
  | ConstrApp (n, fields, allow) ->
    ConstrApp (n, List.map (fun (f, e) -> (f, r e)) fields, allow)
  | ConstrUpdate (n, base, fields, allow) ->
    ConstrUpdate (n, r base, List.map (fun (f, e) -> (f, r e)) fields, allow)
  (* Read by its syntax where it is evaluated, so left as written. *)
  | ConstrBare _ | Qualified _ -> e
  | Field (inner, label) ->
    (match strip_located inner with
     | Constr m when not (special_label label) ->
       let rec positions = function Located (_, e) -> 1 + positions e | _ -> 0 in
       ModField (global scope m, label, new_cache (), positions inner)
     | Constr _ | Qualified _ -> e
     | _ -> Field (r inner, label))
  | Seq (a, b) -> Seq (r a, r b)
  | Located (l, a) -> Located (l, r a)
  (* The postconditions see a `result` the evaluator binds, so they are
     resolved as if nothing were bound outside them. *)
  | Contract (reqs, ens, body) ->
    Contract (List.map r reqs, List.map (expr []) ens, r body)
  | RunCmd (a, allow) -> RunCmd (r a, allow)
  | RunQuery (a, allow) -> RunQuery (r a, allow)
  | MkCommand (a, allow) -> MkCommand (r a, allow)
  | Interp (parts, tail) -> Interp (List.map (fun (s, e) -> (s, r e)) parts, tail)
  | RawInterp (parts, tail) ->
    RawInterp (List.map (fun (s, e) -> (s, r e)) parts, tail)
  | CmdInterp (parts, tail) ->
    CmdInterp (List.map (fun (s, e, h) -> (s, r e, h)) parts, tail)
  | Handle (body, cases) ->
    Handle (r body, List.map (function
      | EffectCase (op, p, k, b) -> EffectCase (op, p, k, expr (k :: push_pat scope p) b)
      | ReturnCase (p, b) -> ReturnCase (p, expr (push_pat scope p) b)) cases)
  | Try a -> Try (r a)
  | With (res, p, body) -> With (r res, p, expr (push_pat scope p) body)
  | Annot (t, a) -> Annot (t, r a)
  | MapLit kvs -> MapLit (List.map (fun (k, e) -> (k, r e)) kvs)

(* The right-hand side of `let name = fn ...`, whose body also sees
   `name`. *)
and rec_fn name scope e =
  match e with
  | Fn (ps, body) ->
    let scope = name :: scope in
    Fn (params scope ps, expr (push_params scope ps) body)
  | Located (l, e) -> Located (l, rec_fn name scope e)
  | Annot (t, e) -> Annot (t, rec_fn name scope e)
  | e -> expr scope e

and case scope (p, guard, body) =
  let inner = push_pat scope p in
  (pat scope p, Option.map (expr inner) guard, expr inner body)

(* A top-level item. Past the item, the environment is the file's, which
   changes as the file runs, so nothing past it is resolved. *)
let item (it : top_item) : top_item =
  match it with
  | TLLet (name, [], body) -> TLLet (name, [], expr [] body)
  | TLLet (name, ps, body) ->
    TLLet (name, params [name] ps, expr (push_params [name] ps) body)
  | TLLetRec bindings ->
    let names = List.fold_left (fun sc (n, _, _) -> n :: sc) [] bindings in
    TLLetRec (List.map (fun (n, ps, b) ->
      (n, params names ps, expr (push_params names ps) b)) bindings)
  | TLLetPat (p, e) -> TLLetPat (p, expr [] e)
  | TLImplement (im, loc) ->
    TLImplement ({ im with im_binds = List.map (fun (n, ps, b, l) ->
      match ps with
      | [] -> (n, ps, expr [] b, l)
      | _ -> (n, ps, expr (push_params [n] ps) b, l)) im.im_binds }, loc)
  | TLExpr e -> TLExpr (expr [] e)
  | TLImport _ | TLType _ | TLInterface _ -> it
