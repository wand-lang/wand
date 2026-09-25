(* Lints run inside `wand t` rather than as a separate command: the typecheck
   is the step an editing loop already repeats, and a rule that does not
   surface there is a rule its audience never sees.

   This module only finds violations and locates them. What each rule is
   called, how it is classified, and what it says all live in `Lint_rules`. *)

(* A machine-applicable correction, carried alongside the human text so a
   tool consuming `--json` can fix without re-parsing prose. The type lives
   in `Diag` so findings and errors share one fix representation; it is
   re-exported here because the constructors read as lint vocabulary. *)
type fix = Diag.fix =
  | InsertLine  of string   (* a line the file lacks (the manifest) *)
  | ReplaceLine of string   (* the corrected form of the flagged line *)
  | DeleteLine              (* the flagged line should not exist *)
  | Replace     of { from_ : string; to_ : string }
  | AppendToLine of string  (* put this at the end of the flagged line *)  (* drift errors only *)

type finding = {
  rule : Lint_rules.id;
  loc  : Token.loc;   (* the whole item for the item-level rules *)
  text : string;
  fix  : fix option;
}

let strip_located = Ast.strip_located

let ends_with s c = String.length s > 0 && s.[String.length s - 1] = c

(* Types reach here with inference variables still standing in for what was
   solved, so every read has to go through repr first. *)
let rec result_type (t : Typechecker.typ) =
  match Typechecker.repr t with
  | Typechecker.TFun (_, r, _) -> result_type r
  | t -> t

let type_of_scheme (s : Typechecker.scheme) =
  match s with
  | Typechecker.Mono t -> Some t
  | Typechecker.Poly (_, _, t) -> Some t
  | Typechecker.Namespace _ -> None

(* Any Result in the type whose error side carries nothing. *)
let rec informationless_error (t : Typechecker.typ) =
  match Typechecker.repr t with
  | Typechecker.TResult (e, _) when Typechecker.repr e = Typechecker.TUnit -> true
  | Typechecker.TResult (e, v) -> informationless_error e || informationless_error v
  | Typechecker.TFun (a, b, _) -> informationless_error a || informationless_error b
  | Typechecker.TApp (a, b) -> informationless_error a || informationless_error b
  | Typechecker.TList t | Typechecker.TMap t -> informationless_error t
  | Typechecker.TTuple ts -> List.exists informationless_error ts
  | _ -> false

(* Whether any arrow in the type can raise.

   A raise the caller may bring is not one this function performs. `try`
   discharges Raise across a function boundary, so
   `attempt : (Unit -> 'a ! {Raise | 'e}) -> Result String 'a ! 'e` asks for
   a thunk that may raise and answers a Result. Reading that argument row
   made V-BANG1 tell `attempt` to call itself `attempt!` -- the opposite of
   what it is, since it is the version that returns a Result.

   So argument positions do not count, and positions flip through them: a
   function this one is handed is one it calls, and a function handed to
   *that* one is one this one supplies. It is the rule the manifest uses,
   for the same reason.

   The arrows of a single curried function still share one effect set, so it
   still does not matter which of those is asked. *)
(* `through_resource` is the difference between the two `!` rules, and the
   asymmetry is deliberate. A `Resource` carries what acquiring and
   releasing it perform, so `FS.lock!` raises when the bracket is entered
   rather than when the name is called. That is a real risk, and a `!` on
   such a name is honest -- V-BANG2 passes `true` and does not object.

   V-BANG1 passes `false`, so it does not *demand* the `!`. `FS.temp_file`
   and `FS.temp_dir` have raised from their acquire since they were written,
   and the bracket is where a reader is already looking. A rule that renamed
   both would be re-deciding a convention rather than enforcing one. *)
let rec type_raises ?(demanded = false) ?(through_resource = false)
    (t : Typechecker.typ) =
  let self ?(flip = false) t =
    type_raises ~through_resource
      ~demanded:(if flip then not demanded else demanded) t
  in
  match Typechecker.repr t with
  | Typechecker.TFun (a, b, r) ->
    (not demanded && Effect_set.mem Effect_set.Raise r)
    || self ~flip:true a || self b
  | Typechecker.TTuple ts -> List.exists self ts
  | Typechecker.TList t | Typechecker.TMap t -> self t
  | Typechecker.TResult (e, t) -> self e || self t
  (* A resource carries what acquiring and releasing it perform, so a
     function answering one can raise without raising itself: `FS.lock!`
     returns a value, and entering the bracket is what raises. The `!` is
     the caller's warning either way, which is what this rule is asking
     about. *)
  | Typechecker.TResource (r, t) ->
    (through_resource && not demanded && Effect_set.mem Effect_set.Raise r)
    || self t
  | Typechecker.TApp (f, a) -> self f || self a
  | _ -> false

let is_function (t : Typechecker.typ) =
  match Typechecker.repr t with Typechecker.TFun _ -> true | _ -> false

let rec pat_names (p : Ast.pat) =
  match p with
  | Ast.PVar n -> [n]
  | Ast.PTuple ps | Ast.PList ps -> List.concat_map pat_names ps
  | Ast.PCons (h, t) -> pat_names h @ pat_names t
  | Ast.PConstr (_, ps) -> List.concat_map pat_names ps
  | Ast.PConstrNamed (_, kvs) | Ast.PMap kvs ->
    List.concat_map (fun (_, p) -> pat_names p) kvs
  | Ast.PConstrBare (_, ids) -> ids
  | Ast.PAnnot (p, _) -> pat_names p
  | _ -> []

(* Shell-level structure in a command string. One stage is exactly what $()
   is for; it is the accumulation of stages that hides work from the type
   system and from --trace. *)
(* A URL written out, which is what makes a host readable from the file. A
   located literal is still a literal. *)
let rec is_url_literal (e : Ast.expr) =
  match e with
  | Ast.Located (_, inner) -> is_url_literal inner
  | Ast.URL _ -> true
  | _ -> false

let shell_operators cmd =
  let n = String.length cmd in
  let count = ref 0 and i = ref 0 in
  while !i < n do
    (match cmd.[!i] with
     | '|' when !i + 1 < n && cmd.[!i + 1] = '|' -> incr count; incr i
     | '&' when !i + 1 < n && cmd.[!i + 1] = '&' -> incr count; incr i
     | '|' | '>' | '<' | ';' -> incr count
     | _ -> ());
    incr i
  done;
  !count

let shell_threshold = 3

(* ── Traversal ───────────────────────────────────────────────────────────── *)

let is_clock_now (e : Ast.expr) =
  match strip_located e with
  | Ast.App (f, arg) when strip_located arg = Ast.Unit ->
    (match strip_located f with
     | Ast.Field (m, "now") ->
       (match strip_located m with
        | Ast.Var "Clock" | Ast.Constr "Clock" -> true
        | _ -> false)
     | _ -> false)
  | _ -> false

(* A `let _ =` over a Unit value. The binder is how a file says a dropped
   Result does not matter; over Unit there is nothing to drop.

   Whether the binder can simply be struck is a question about the text
   around it. On a file's own spine the statement stands alone, and inside
   `( ... ; ... )` the `;` is already there. Newline-joined inside a body it
   is neither: the statements below would run together, so the finding says
   what to write and carries no fix. *)
(* Whether a `let _ = ` binds a value of type Unit -- the question the rule
   asks, kept apart from where it is reported. *)
let wild_let_is_unit v =
  match v with
  | Ast.Located (l, _) ->
    (match List.assoc_opt l !Typechecker.wild_let_types with
     | Some t -> Typechecker.repr t = Typechecker.TUnit
     | None -> false)
  | _ -> false

(* The span to report, and whether `let _ = ` sits whole on one line in
   front of the value.

   The binder ends where the value begins, so a value on the binder's own
   line locates it: `let _ = ` is the eight columns in front. That was the
   only case the rule could see. A value on the *next* line is the same
   mistake and was reported not at all below column 9, and at a
   reconstructed position on a line it does not occupy above it -- a
   four-line file put the finding on line 6. So the binder's own position
   is the fallback, and the fix is offered only where the eight columns are
   really there for it to replace. *)
let wild_let_span ~binder v =
  match v with
  | Ast.Located (l, _)
    when l.Token.line = binder.Token.line && l.Token.col > 8 ->
    ({ l with Token.end_line = l.Token.line;
              Token.col = l.Token.col - 8;
              Token.end_col = l.Token.col }, true)
  | _ -> (binder, false)

let walk_expr ?(spine = false) start_loc (e : Ast.expr) : finding list =
  let acc = ref [] in
  let here = ref start_loc in
  (* Names bound to a reading of the clock, so that the shape a script is
     written in is caught as well as the inline one: save a reading, do the
     work, subtract. A name bound to anything else drops out again, which is
     what keeps a rebinding from being read as a reading. *)
  let readings = ref [] in
  let is_reading e =
    is_clock_now e
    || (match strip_located e with
        | Ast.Var n -> List.mem n !readings
        | _ -> false)
  in
  (* A newline this command did not ask for: one with no `\` in front of
     it. The escape is what a shell reads as "the line continues", and it is
     the only spelling that keeps one command one command. *)
  let runs_on text =
    let n = String.length text in
    let rec look i =
      if i >= n then false
      else if text.[i] = '\n' && (i = 0 || text.[i - 1] <> '\\') then true
      else look (i + 1)
    in
    look 0
  in
  let rec go ?(spine = false) (e : Ast.expr) =
    match e with
    | Ast.Located (l, inner) ->
      let saved = !here in
      here := l; go ~spine inner; here := saved
    | Ast.RunCmd (inner, allow) | Ast.RunQuery (inner, allow)
    | Ast.MkCommand (inner, allow) ->
      (* A newline inside `$()` separates two commands, exactly as it does
         in a shell script, so the text below it runs on its own. A line
         broken for width almost never means that, and the half above the
         break can have done its work before the half below fails. `\` is
         the continuation and wand passes it through, so the correction is
         one character at the end of the line. *)
      (let literals = match strip_located inner with
         | Ast.String cmd -> [cmd]
         | Ast.CmdInterp (parts, tail) ->
           List.map (fun (lit, _, _) -> lit) parts @ [tail]
         | _ -> []
       in
       if List.exists runs_on literals then
         acc := { rule = Lint_rules.V_SHELL2;
                  loc = !here;
                  text = Lint_rules.shell2;
                  fix = Some (AppendToLine " \\") } :: !acc);
      (match strip_located inner with
       | Ast.String cmd ->
         let ops = shell_operators cmd in
         if ops >= shell_threshold then
           acc := { rule = Lint_rules.A_SHELL1;
                    loc = !here;
                    text = Lint_rules.shell1 ~stages:ops;
                    fix = None } :: !acc
       | _ -> ());
      (* A narrowed manifest with a command word only the run decides:
         legal, checked at spawn, and said out loud -- under --strict this
         is what holds a file to fully static words. *)
      (if allow <> None then
         let s = Shell_scan.scan (Shell_scan.segs_of_cmd inner) in
         if s.Shell_scan.raw_tail
            || List.exists (fun w -> w = Shell_scan.Dynamic)
                 s.Shell_scan.words
         then
           acc := { rule = Lint_rules.V_SHELL1;
                    loc = !here;
                    text = Lint_rules.shell1_dynamic;
                    fix = None } :: !acc);
      go inner
    (* A request whose host the run decides. The bound is on the node, so a
       file that declared bare `Net` or nothing says nothing here -- the
       rule is about a narrowed manifest, exactly as V-SHELL1 is.

       What is reported is the construction, which is where a URL becomes a
       request. `HTTP.get u` builds its request inside the standard library,
       so it is not a site in this file and does not appear here; the check
       that catches it is the one at the send, which is where the record
       always said a run-decided host is answered for. *)
    | Ast.ConstrApp (name, fields, (Some _ as bound)) ->
      (if name = "HTTPRequest" then
         match List.assoc_opt (Some "url") fields with
         | Some u when not (is_url_literal u) ->
           acc := { rule = Lint_rules.V_NET1;
                    loc = !here;
                    text = Lint_rules.net1_dynamic;
                    fix = None } :: !acc
         | _ -> ());
      ignore bound;
      List.iter (fun (_, v) -> go v) fields
    | Ast.Interp (parts, _) | Ast.RawInterp (parts, _) ->
      List.iter (fun (_, e) -> go e) parts
    (* Measuring by subtracting two readings of the civil clock. Only the
       qualified form is matched: a destructured `now` says nothing about
       which module it came from, and guessing would fire on somebody
       else's function. *)
    | Ast.BinOp ("-", a, b) when is_reading a && is_reading b ->
      acc := { rule = Lint_rules.V_CLOCK1; loc = !here;
               text = Lint_rules.clock1; fix = None } :: !acc;
      go a; go b
    | Ast.App (a, b) | Ast.BinOp (_, a, b) | Ast.Seq (a, b) -> go a; go b
    | Ast.UnOp (_, a) | Ast.Fn (_, a) | Ast.Annot (_, a)
    | Ast.Field (a, _) | Ast.Try a -> go a
    | Ast.Let (p, a, b, style) ->
      (match p with
       | Ast.Wild when wild_let_is_unit a ->
         let standalone = spine || style = Ast.LetBlock in
         let (loc, replaceable) = wild_let_span ~binder:!here a in
         let fix =
           if standalone && replaceable then
             Some (Replace { from_ = "let _ = "; to_ = "" })
           else None
         in
         acc := { rule = Lint_rules.A_BIND1; loc;
                  text = Lint_rules.bind1 ~standalone; fix } :: !acc
       | _ -> ());
      go a;
      (match p with
       | Ast.PVar name ->
         if is_clock_now a then readings := name :: !readings
         else readings := List.filter (fun n -> n <> name) !readings
       | _ -> ());
      go ~spine b
    | Ast.LetRec (bs, b, _) -> List.iter (fun (_, _, x) -> go x) bs; go b
    | Ast.If (c, t, f) -> go c; go t; go f
    | Ast.Match (s, cases) ->
      go s;
      List.iter (fun (_, g, b) ->
        (match g with Some g -> go g | None -> ()); go b) cases
    | Ast.Tuple es | Ast.List es -> List.iter go es
    | Ast.MapLit kvs -> List.iter (fun (_, v) -> go v) kvs
    | Ast.ConstrApp (_, fields, _) -> List.iter (fun (_, v) -> go v) fields
    | Ast.ConstrBare (_, _) -> ()
    | Ast.Qualified (_, e) -> go e
    | Ast.ConstrUpdate (_, base, fields, _) -> go base; List.iter (fun (_, v) -> go v) fields
    | Ast.Handle (b, cases) ->
      go b;
      List.iter (function
        | Ast.ReturnCase (_, x) -> go x
        | Ast.EffectCase (_, _, _, x) -> go x) cases
    | Ast.Contract (reqs, ens, body) ->
      List.iter go reqs; List.iter go ens; go body
    | _ -> ()
  in
  go ~spine e;
  List.rev !acc

(* Every name a file mentions, for the rule that reports an import nothing
   uses. The match is exhaustive on purpose: a node this forgets is a name
   it does not see, and the finding deletes a line, so a miss is a deleted
   import that was needed. Adding a node to the AST without adding it here
   is a compile error. *)
let rec names_of_expr (e : Ast.expr) : string list =
  let of_list es = List.concat_map names_of_expr es in
  match e with
  | Ast.Var n | Ast.Constr n -> [n]
  (* `IO.println`: the head is the namespace, the label is its member. *)
  | Ast.Field (e, label) -> label :: names_of_expr e
  | Ast.Int _ | Ast.Float _ | Ast.String _ | Ast.Bool _ | Ast.Unit
  | Ast.Path _ | Ast.Glob _ | Ast.DateTime _ | Ast.Duration _ | Ast.URL _
  | Ast.IPv4 _ | Ast.CIDR _ | Ast.Port _ | Ast.Version _ | Ast.Size _
  | Ast.EnvVar _ | Ast.Hole | Ast.RegexLit _ | Ast.RawString _ -> []
  | Ast.ImportExpr _ -> []
  | Ast.App (a, b) | Ast.BinOp (_, a, b) | Ast.Seq (a, b) ->
    names_of_expr a @ names_of_expr b
  | Ast.UnOp (_, a) | Ast.Located (_, a) | Ast.Try a
  | Ast.RunCmd (a, _) | Ast.RunQuery (a, _) | Ast.MkCommand (a, _) ->
    names_of_expr a
  | Ast.Annot (te, a) -> names_of_type_expr te @ names_of_expr a
  | Ast.Fn (ps, a) -> List.concat_map names_of_pat ps @ names_of_expr a
  | Ast.Let (p, a, b, _) ->
    names_of_pat p @ names_of_expr a @ names_of_expr b
  | Ast.LetRec (bs, b, _) ->
    List.concat_map (fun (_, ps, x) ->
      List.concat_map names_of_pat ps @ names_of_expr x) bs
    @ names_of_expr b
  | Ast.If (c, t, f) -> of_list [c; t; f]
  | Ast.Match (s, cases) ->
    names_of_expr s
    @ List.concat_map (fun (p, g, b) ->
        names_of_pat p
        @ (match g with Some g -> names_of_expr g | None -> [])
        @ names_of_expr b) cases
  | Ast.Tuple es | Ast.List es -> of_list es
  | Ast.MapLit kvs -> of_list (List.map snd kvs)
  | Ast.ConstrApp (c, kvs, _) -> c :: of_list (List.map snd kvs)
  | Ast.ConstrUpdate (c, base, kvs, _) ->
    c :: names_of_expr base @ of_list (List.map snd kvs)
  | Ast.ConstrBare (c, ids) -> c :: ids
  (* The module is a use of the import that brought it in. *)
  | Ast.Qualified (m, e) -> m :: names_of_expr e
  | Ast.Contract (reqs, ens, body) -> of_list (reqs @ ens @ [body])
  | Ast.Interp (parts, _) | Ast.RawInterp (parts, _) ->
    of_list (List.map snd parts)
  | Ast.CmdInterp (parts, _) -> of_list (List.map (fun (_, e, _) -> e) parts)
  | Ast.Handle (b, cases) ->
    names_of_expr b
    @ List.concat_map (function
        | Ast.ReturnCase (p, x) -> names_of_pat p @ names_of_expr x
        | Ast.EffectCase (op, p, _, x) ->
          op :: names_of_pat p @ names_of_expr x) cases
  | Ast.With (r, p, b) -> names_of_expr r @ names_of_pat p @ names_of_expr b

and names_of_pat (p : Ast.pat) : string list =
  match p with
  | Ast.PConstr (c, ps) -> c :: List.concat_map names_of_pat ps
  | Ast.PConstrNamed (c, kvs) -> c :: List.concat_map (fun (_, p) -> names_of_pat p) kvs
  | Ast.PConstrBare (c, _) -> [c]
  | Ast.PQualified (m, p) -> m :: names_of_pat p
  | Ast.PTuple ps | Ast.PList ps -> List.concat_map names_of_pat ps
  | Ast.PCons (h, t) -> names_of_pat h @ names_of_pat t
  | Ast.PMap kvs -> List.concat_map (fun (_, p) -> names_of_pat p) kvs
  | Ast.PAnnot (p, te) -> names_of_pat p @ names_of_type_expr te
  (* A binder introduces a name rather than using one, so it contributes
     nothing: an import is not kept alive by something else shadowing it. *)
  | _ -> []

and names_of_type_expr (te : Ast.type_expr) : string list =
  match te with
  | Ast.TEName n -> [n]
  (* The module is the name a dead import would be reported for. *)
  | Ast.TEQual (m, n) -> [m; n]
  | Ast.TEVar _ -> []
  | Ast.TEApp (a, b) -> names_of_type_expr a @ names_of_type_expr b
  | Ast.TETuple ts -> List.concat_map names_of_type_expr ts
  | Ast.TEFun (a, b, _) -> names_of_type_expr a @ names_of_type_expr b

(* The module an interface is reached through, where one is written:
   `ord.Ord` mentions `ord`. *)
let iface_qualifier name =
  match String.index_opt name '.' with
  | Some i -> [String.sub name 0 i]
  | None -> []

let names_of_item (item : Ast.top_item) : string list =
  match item with
  | Ast.TLExpr e -> names_of_expr e
  | Ast.TLLet (_, ps, body) ->
    List.concat_map names_of_pat ps @ names_of_expr body
  | Ast.TLLetRec bs ->
    List.concat_map (fun (_, ps, b) ->
      List.concat_map names_of_pat ps @ names_of_expr b) bs
  | Ast.TLLetPat (p, body) -> names_of_pat p @ names_of_expr body
  | Ast.TLType (tdef, _) ->
    (match tdef with
     | Ast.Alias (_, _, te) -> names_of_type_expr te
     | Ast.Variants (_, _, ctors) ->
       List.concat_map (fun c ->
         List.concat_map (fun (_, te) -> names_of_type_expr te) c.Ast.fields
         @ List.concat_map (fun (_, d) -> names_of_expr d) c.Ast.defaults) ctors)
  | Ast.TLInterface (i, _) ->
    List.concat_map (fun (_, te) -> names_of_type_expr te) i.Ast.if_members
  | Ast.TLImplement (im, _) ->
    (* `implement ord.Ord Int` reaches the interface through the module it
       bound, so the qualifier is a use of that import like any other. *)
    iface_qualifier im.Ast.im_iface
    @ List.concat_map names_of_type_expr im.Ast.im_args
    @ List.concat_map (fun (_, ps, b, _) ->
        List.concat_map names_of_pat ps @ names_of_expr b) im.Ast.im_binds
  | Ast.TLImport _ -> []

(* The names an item mentions in type position, which is what tells a type
   named in a signature from a namespace called in an expression. *)
let names_of_item_types (item : Ast.top_item) : string list =
  let rec te_of_expr (e : Ast.expr) =
    match e with
    | Ast.Annot (te, a) -> names_of_type_expr te @ te_of_expr a
    | Ast.Located (_, a) | Ast.UnOp (_, a) | Ast.Try a
    | Ast.RunCmd (a, _) | Ast.RunQuery (a, _) | Ast.MkCommand (a, _) ->
      te_of_expr a
    | Ast.App (a, b) | Ast.BinOp (_, a, b) | Ast.Seq (a, b) -> te_of_expr a @ te_of_expr b
    | Ast.Fn (ps, a) -> List.concat_map te_of_pat ps @ te_of_expr a
    | Ast.Let (p, a, b, _) -> te_of_pat p @ te_of_expr a @ te_of_expr b
    | Ast.LetRec (bs, b, _) ->
      List.concat_map (fun (_, ps, x) -> List.concat_map te_of_pat ps @ te_of_expr x) bs
      @ te_of_expr b
    | Ast.If (c, t, f) -> te_of_expr c @ te_of_expr t @ te_of_expr f
    | Ast.Match (s, cases) ->
      te_of_expr s
      @ List.concat_map (fun (p, g, b) ->
          te_of_pat p @ (match g with Some g -> te_of_expr g | None -> []) @ te_of_expr b)
          cases
    | Ast.Tuple es | Ast.List es -> List.concat_map te_of_expr es
    | Ast.MapLit kvs -> List.concat_map (fun (_, v) -> te_of_expr v) kvs
    | Ast.ConstrApp (_, kvs, _) -> List.concat_map (fun (_, v) -> te_of_expr v) kvs
    | Ast.ConstrUpdate (_, b, kvs, _) ->
      te_of_expr b @ List.concat_map (fun (_, v) -> te_of_expr v) kvs
    | Ast.Contract (reqs, ens, body) ->
      List.concat_map te_of_expr (reqs @ ens @ [body])
    | Ast.Interp (parts, _) | Ast.RawInterp (parts, _) ->
      List.concat_map (fun (_, e) -> te_of_expr e) parts
    | Ast.CmdInterp (parts, _) -> List.concat_map (fun (_, e, _) -> te_of_expr e) parts
    | Ast.Handle (b, cases) ->
      te_of_expr b
      @ List.concat_map (function
          | Ast.ReturnCase (p, x) -> te_of_pat p @ te_of_expr x
          | Ast.EffectCase (_, p, _, x) -> te_of_pat p @ te_of_expr x) cases
    | Ast.With (r, p, b) -> te_of_expr r @ te_of_pat p @ te_of_expr b
    | _ -> []
  and te_of_pat (p : Ast.pat) =
    match p with
    | Ast.PAnnot (p, te) -> te_of_pat p @ names_of_type_expr te
    | Ast.PTuple ps | Ast.PList ps | Ast.PConstr (_, ps) -> List.concat_map te_of_pat ps
    | Ast.PCons (h, t) -> te_of_pat h @ te_of_pat t
    | Ast.PConstrNamed (_, kvs) | Ast.PMap kvs ->
      List.concat_map (fun (_, p) -> te_of_pat p) kvs
    | _ -> []
  in
  match item with
  | Ast.TLExpr e -> te_of_expr e
  | Ast.TLLet (_, ps, body) -> List.concat_map te_of_pat ps @ te_of_expr body
  | Ast.TLLetRec bs ->
    List.concat_map (fun (_, ps, b) -> List.concat_map te_of_pat ps @ te_of_expr b) bs
  | Ast.TLLetPat (p, body) -> te_of_pat p @ te_of_expr body
  | Ast.TLType (tdef, _) ->
    (match tdef with
     | Ast.Alias (_, _, te) -> names_of_type_expr te
     | Ast.Variants (_, _, ctors) ->
       List.concat_map (fun c ->
         List.concat_map (fun (_, te) -> names_of_type_expr te) c.Ast.fields) ctors)
  | Ast.TLInterface (i, _) ->
    List.concat_map (fun (_, te) -> names_of_type_expr te) i.Ast.if_members
  | Ast.TLImplement (im, _) ->
    iface_qualifier im.Ast.im_iface
    @ List.concat_map names_of_type_expr im.Ast.im_args
    @ List.concat_map (fun (_, ps, b, _) ->
        List.concat_map te_of_pat ps @ te_of_expr b) im.Ast.im_binds
  | Ast.TLImport _ -> []

(* `own_env` holds the program's own top-level bindings, already inferred, so
   the type-directed rules read what the checker concluded rather than
   re-deriving it. *)
let check (prog : Ast.program) (item_locs : (Token.loc * Token.loc) list)
    (own_env : Typechecker.env) : finding list =
  let locs = Array.of_list item_locs in
  let no_loc = Token.point 0 0 0 in
  let findings = ref [] in
  let add ?fix rule loc text =
    findings := { rule; loc; text; fix } :: !findings
  in
  (* V-IMP1 watches every import in the file, not only the leading run.
     Imports bind before the file's own bindings, wherever they are
     written, so the last import of a name decides every use of it -- a use
     above the second import line reads the second import. The earlier
     binding is dead however far down the rebinding sits.

     The rule used to stop at the first non-import item, on the reasoning
     that a later rebinding might follow a genuine use. It cannot: the
     "genuine use" reads the later module too, which is the whole reason
     this warns. Found porting a script whose helper collided with an
     earlier port's.

     The pattern can bind another name (`let {parse = csv_parse} = import
     CSV`), so keeping both is spelled by renaming, not by shadowing. *)
  let import_display = function
    | Ast.StdlibModule s -> s
    | Ast.UserPath p     -> p
  in
  let imports_seen : (string * (Token.loc * string)) list ref = ref [] in
  (* Every name the file mentions anywhere below its imports. An import is
     reported only when none of the names it binds is in here. *)
  let mentioned = List.concat_map names_of_item prog.Ast.items in
  (* Naming a type is not using the module it came from when the type is
     built in: `Option String` in a signature reads the language, not
     `import Option`. Kept apart so that import can be reported dead, which
     is what four of them in the standard library became when `Option`
     stopped needing one. *)
  let mentioned_as_value =
    List.concat_map (fun item ->
      let types = names_of_item_types item in
      let all = names_of_item item in
      let count n xs = List.length (List.filter (fun x -> x = n) xs) in
      List.filter (fun n ->
        not (Typechecker.builtin_type_name n)
        || count n all > count n types)
        (List.sort_uniq compare all)) prog.Ast.items
  in
  (* An import brings its module's types and their constructors as well as
     the names it says, and which module a type came from is not in this
     file. So the rule stays silent about the whole file when it mentions a
     type or constructor it did not declare and wand does not build in: that
     name may be the only reason an import is there, and a fix that deletes
     lines cannot guess. *)
  let declared_here =
    List.concat_map (function
      | Ast.TLType (Ast.Variants (n, _, ctors), _) ->
        n :: List.map (fun c -> c.Ast.name) ctors
      | Ast.TLType (Ast.Alias (n, _, _), _) -> [n]
      | _ -> []) prog.Ast.items
  in
  let is_upper n = n <> "" && n.[0] >= 'A' && n.[0] <= 'Z' in
  let namespace_names =
    List.filter_map (function
      | Ast.TLImport k -> Some (Module_types.namespace_name_of k)
      | Ast.TLLet (n, [], b) when Option.is_some (Module_types.import_kind_of b) -> Some n
      | _ -> None) prog.Ast.items
  in
  let unattributable =
    List.exists (fun n ->
      is_upper n
      && not (List.mem n declared_here)
      && not (List.mem n namespace_names)
      && not (Typechecker.builtin_type_name n)
      && not (List.mem n ["Ok"; "Error"; "Some"; "None"; "true"; "false"]))
      mentioned
  in
  (* Every top-level name a `let` binds, and the line it was bound on. The
     imports are not in here: V-IMP1 already reports those, and reports them
     the other way round -- an import that is rebound is dead, so the finding
     lands on the earlier line and offers to delete it. A value's earlier
     binding is not dead, so this reports the second one instead. *)
  let value_bindings_seen = ref [] in
  List.iteri (fun i (item : Ast.top_item) ->
    (* An item-level finding marks the whole item, first token to last. *)
    let loc =
      if i < Array.length locs
      then Token.span_to (fst locs.(i)) (snd locs.(i))
      else no_loc
    in
    begin
      let bound = match item with
        | Ast.TLImport _ -> Some []
        | Ast.TLLet (name, [], body) ->
          Option.map (fun k -> [ (name, k) ]) (Module_types.import_kind_of body)
        | Ast.TLLetPat (pat, body) ->
          Option.map (fun k -> List.map (fun n -> (n, k)) (pat_names pat))
            (Module_types.import_kind_of body)
        | _ -> None
      in
      match bound with
      | None -> ()
      | Some names ->
        List.iter (fun (n, k) ->
          let this = import_display k in
          (match List.assoc_opt n !imports_seen with
           | Some (first_loc, first_mod) ->
             (* The dead binding is the earlier one, so the fix deletes the
                line the finding already points at. *)
             add ~fix:DeleteLine Lint_rules.V_IMP1 first_loc
               (Lint_rules.imp1 ~name:n ~first:first_mod ~second:this
                  ~line:loc.Token.line)
           | None -> ());
          imports_seen := (n, (loc, this)) :: List.remove_assoc n !imports_seen
        ) names
    end;
    (* An import that binds nothing the file mentions. *)
    (match item with
     | _ when unattributable -> ()
     | Ast.TLImport k ->
       let n = Module_types.namespace_name_of k in
       if not (List.mem n mentioned_as_value) then
         add ~fix:DeleteLine Lint_rules.V_IMP2 loc
           (Lint_rules.imp2 ~what:(import_display k) ~names:[n])
     | Ast.TLLet (n, [], body) when Option.is_some (Module_types.import_kind_of body) ->
       if not (List.mem n mentioned) then
         add ~fix:DeleteLine Lint_rules.V_IMP2 loc
           (Lint_rules.imp2
              ~what:(import_display (Option.get (Module_types.import_kind_of body)))
              ~names:[n])
     | Ast.TLLetPat (pat, body) when Option.is_some (Module_types.import_kind_of body) ->
       let names = pat_names pat in
       if names <> [] && not (List.exists (fun n -> List.mem n mentioned) names) then
         add ~fix:DeleteLine Lint_rules.V_IMP2 loc
           (Lint_rules.imp2
              ~what:(import_display (Option.get (Module_types.import_kind_of body)))
              ~names)
     | _ -> ());
    (* A name bound twice at the top level. *)
    (let rebinds =
       match item with
       | Ast.TLLet (name, _, body)
         when Option.is_none (Module_types.import_kind_of body) -> [name]
       | Ast.TLLetPat (pat, body)
         when Option.is_none (Module_types.import_kind_of body) -> pat_names pat
       | _ -> []
     in
     List.iter (fun n ->
       (* `_` is the name for a value that is deliberately not read, so a
          file may have as many as it likes. *)
       if n <> "_" then begin
         (match List.assoc_opt n !value_bindings_seen with
          | Some first_line ->
            add Lint_rules.V_SHADOW1 loc (Lint_rules.shadow1 ~name:n ~line:first_line)
          | None -> ());
         value_bindings_seen :=
           (n, loc.Token.line) :: List.remove_assoc n !value_bindings_seen
       end) rebinds);
    match item with
    | Ast.TLLet (name, params, body) ->
      (match Option.bind (List.assoc_opt name own_env) type_of_scheme with
       | Some t ->
         let res = result_type t in
         (* A name carries one ending, and `has_example?!` does not parse. So
            a function that returns Bool and can raise has no name that
            answers both conventions, and V-PRED3 asking for the `?` asked
            for something the author cannot write. `!` wins: the risk is
            what a caller has to see, and V-BANG1 already says so in as many
            words. *)
         let raises = is_function t && type_raises t in
         if ends_with name '?' && String.length name > 4
            && String.sub name 0 3 = "is_" then
           add Lint_rules.V_PRED2 loc (Lint_rules.pred2 ~name);
         if ends_with name '?' && res <> Typechecker.TBool then
           add Lint_rules.V_PRED1 loc
             (Lint_rules.pred1 ~name ~actual:(Typechecker.string_of_typ res));
         (* And the other way, as `!` is checked both ways below. *)
         if res = Typechecker.TBool && not (ends_with name '?') && is_function t
            && not raises then
           add Lint_rules.V_PRED3 loc (Lint_rules.pred3 ~name);
         if informationless_error t then
           add Lint_rules.V_OR1 loc (Lint_rules.or1 ~name);
         (* The `!` convention, checked in both directions now that a
            signature says whether a function can raise. *)
         if is_function t then begin
           if raises && not (ends_with name '!') then
             add Lint_rules.V_BANG1 loc (Lint_rules.bang1 ~name);
           if (not (type_raises ~through_resource:true t))
              && ends_with name '!' then
             add Lint_rules.V_BANG2 loc (Lint_rules.bang2 ~name)
         end
       | None -> ());
      (match List.concat_map pat_names params
             |> List.filter (fun n -> String.length n > 1 && ends_with n '_') with
       | [] -> ()
       | ps -> add Lint_rules.V_NAME1 loc (Lint_rules.name1 ~name ~params:ps));
      findings := List.rev_append (walk_expr loc body) !findings
    | Ast.TLExpr body ->
      (* A statement whose value is a Result throws away the failure it
         carries, and nothing else reports it: `wand t` is happy, the script
         exits 0, and the write that did not happen is never mentioned.
         Only Results, because discarding a String is what running a command
         for its effect looks like. The last item is the file's value rather
         than a discarded one, so it is left alone.

         `let () = ...` already catches this as a type error, and `let _ =`
         says the failure does not matter. This is for the bare statement,
         which says nothing either way. *)
      (if i < List.length prog.Ast.items - 1 then
         match List.assoc_opt i !Typechecker.expr_item_types with
         | Some t ->
           (match Typechecker.repr t with
            | Typechecker.TResult _ ->
              add Lint_rules.V_DROP1 loc
                (Lint_rules.drop1 ~typ:(Typechecker.string_of_typ t))
            | _ -> ())
         | None -> ());
      findings := List.rev_append (walk_expr ~spine:true loc body) !findings
    | Ast.TLLetPat (pat, body) ->
      (* A top-level `let _ = ...` is an item, so the walk over expressions
         never reaches it. It is the shape the rule was written for -- a
         file's own spine, where the statement stands alone and the binder
         can simply come off. *)
      (match pat with
       | Ast.Wild when wild_let_is_unit body ->
         let (bloc, replaceable) = wild_let_span ~binder:loc body in
         add ?fix:(if replaceable then
                     Some (Replace { from_ = "let _ = "; to_ = "" })
                   else None)
           Lint_rules.A_BIND1 bloc (Lint_rules.bind1 ~standalone:true)
       | _ -> ());
      findings := List.rev_append (walk_expr loc body) !findings
    | Ast.TLLetRec bindings ->
      List.iter (fun (_, _, b) ->
        findings := List.rev_append (walk_expr loc b) !findings) bindings
    | Ast.TLImplement (im, _) ->
      List.iter (fun (_, _, b, _) ->
        findings := List.rev_append (walk_expr loc b) !findings) im.Ast.im_binds
    | Ast.TLImport _ | Ast.TLType _ | Ast.TLInterface _ -> ()
  ) prog.Ast.items;
  (* The same rule for `(e1; e2)` sequences: every expression before the
     last is discarded, and one whose value is a Result throws away the
     failure it carries. Recorded by the typechecker because the rule needs
     the type, exactly like the bare-statement case above.

     A discarded TestOutcome is the same mistake with a worse ending, so it
     is checked here too. Only in a sequence: a test file's top-level
     `test "..."` statements are collected by the runner, so discarding one
     there is how the framework is meant to be used. *)
  List.iter (fun (loc, t) ->
    match Typechecker.repr t with
    | Typechecker.TResult _ ->
      add Lint_rules.V_DROP1 loc
        (Lint_rules.drop1 ~typ:(Typechecker.string_of_typ t))
    (* The type is `Test`'s, so its canonical name says which module it came
       from. What the rule is about is the short name. *)
    | Typechecker.TName n when Typechecker.short_type_name n = "TestOutcome" ->
      add Lint_rules.V_DROP2 loc Lint_rules.drop2
    | _ -> ()) !Typechecker.seq_discard_types;
  (* A manifest that permits more than the file uses. Checked from what
     inference concluded, so the rule cannot disagree with the type error
     that covers the opposite case. *)
  (match !Typechecker.last_manifest with
   | Some (declared, inferred, loc) ->
     let unused = Effect_set.EffSet.diff declared inferred in
     if not (Effect_set.EffSet.is_empty unused) then begin
       let corrected =
         if Effect_set.EffSet.is_empty inferred then None
         else
           Some (Typechecker.render_manifest
                   ?shell:(Typechecker.shell_suggestion ()) inferred)
       in
       add ?fix:(Option.map (fun c -> ReplaceLine c) corrected)
         Lint_rules.A_USES1 loc
         (Lint_rules.uses1
            ~unused:(String.concat ", "
              (List.map Effect_set.name_of (Effect_set.EffSet.elements unused)))
            ~corrected)
     end
     else begin
       (* The labels all earn their place; do the binaries? Only judged
          when every command position is literal -- an interpolated one may
          be exactly where the unused-looking binary is spawned. *)
       match !Typechecker.last_shell_allow with
       | Some allow when !Typechecker.last_shell_static ->
         let used = !Typechecker.last_shell_words in
         let unused_bins =
           List.filter (fun entry ->
             (* A pattern is left alone. It is written on purpose and it
                claims a shape rather than a name, so "this file runs
                nothing matching it today" is not the same finding as an
                unused literal -- and the fix would delete a deliberate
                choice. Without this, adding one command outside the shape
                cost the author their pattern: `--fix` added the literal,
                and this rule then removed the pattern it no longer
                matched. *)
             not (String.contains entry '*')
             && not (List.exists (Shell_scan.allowed ~allow:[entry]) used))
             allow
         in
         if unused_bins <> [] then begin
           let kept =
             List.filter (fun e -> not (List.mem e unused_bins)) allow in
           let corrected =
             Typechecker.render_manifest
               ?shell:(if kept = [] then None else Some kept) inferred
           in
           add ~fix:(ReplaceLine corrected) Lint_rules.A_USES1 loc
             (Lint_rules.uses1_shell
                ~unused:(String.concat ", "
                  (List.map (fun b -> "'" ^ b ^ "'") unused_bins))
                ~corrected)
         end
       | _ -> ()
     end
   | None ->
     (* No manifest at all. A file that reaches outside itself is told what
        it could declare; a file that does not has nothing to say. *)
     let performs = !Typechecker.last_file_effects in
     if not (Effect_set.EffSet.is_empty performs) then
       let corrected =
         Typechecker.render_manifest
           ?shell:(Typechecker.shell_suggestion ()) performs in
       add ~fix:(InsertLine corrected)
         Lint_rules.V_USES2 (Token.point 1 1 0)
         (Lint_rules.uses2
            ~performs:(String.concat ", "
              (List.map Effect_set.name_of (Effect_set.EffSet.elements performs)))
            ~corrected));
  List.stable_sort (fun a b ->
    match compare a.loc.Token.line b.loc.Token.line with
    | 0 -> compare a.loc.Token.col b.loc.Token.col
    | c -> c)
    (List.rev !findings)

(* ── Rendering ───────────────────────────────────────────────────────────── *)

(* Only must-fix rules can fail a build. *)
let fails_strict f = Lint_rules.kind f.rule = Lint_rules.Violation

let to_text f =
  Printf.sprintf "%d:%d: %s: %s" f.loc.Token.line f.loc.Token.col
    (Lint_rules.code f.rule) f.text

let contains = Diag.contains

(* ── Structured diagnostics (`wand t --json`) ────────────────────────────── *)

(* One JSON array on stdout, one object per diagnostic, rendered by `Diag`.
   The schema is part of the CLI's contract (reference.md, "REPL and CLI"):
   every object has `severity`, `code`, `line`, `col`, and `message`; `file`
   when a file was named; a `fix` object when a machine-applicable
   correction exists; and typed holes come as their own `{"kind":"hole"}`
   shape. *)

let to_diag ~strict f : Diag.t =
  { Diag.severity = if strict && fails_strict f then Diag.Error else Diag.Warning;
    code    = Lint_rules.code f.rule;
    loc     = Some f.loc;
    message = f.text;
    fix     = f.fix;
    others  = [] }

let hole_json t =
  Printf.sprintf "{\"kind\":\"hole\",\"type\":\"%s\"}" (Diag.escape_json t)

(* The objects alone, so a run over several files can put them all in one
   array rather than printing an array per file. *)
let diagnostics_json_items ~strict ?file ~holes findings =
  List.map hole_json holes
  @ List.map (fun f -> Diag.to_json ?file (to_diag ~strict f)) findings

let diagnostics_json ~strict ?file ~holes findings =
  "[" ^ String.concat "," (diagnostics_json_items ~strict ?file ~holes findings) ^ "]"

let to_json fs = diagnostics_json ~strict:false ~holes:[] fs
