(* What a written type says an arrow performs: the `! {Shell, IO}` or
   `! 'e` after it. Absent when nothing was written, which leaves it to
   inference -- the effects of a function are read back from its body, and a
   type only has to say so where it is describing a relationship inference
   cannot see for itself.

   `{Shell | 'e}` is both at once: at least Shell, plus whatever 'e stands
   for. The printer emits all four shapes, so the grammar reads all four. *)
type te_effects = {
  te_labels : string list;    (* Shell, FS.Read, ... ; [] for `! 'e` alone *)
  te_var    : string option;  (* Some "e" for 'e *)
}

(* A type or a constructor as it was written: `Status`, or `Foo.Status`.
   The qualifier is the name the file gave the import, not the module's key.
   Resolution turns one into the other; the parser keeps what was written so
   the formatter can give it back. *)
type qname = {
  qual : string option;
  base : string;
}

let bare base = { qual = None; base }

let qualified qual base = { qual = Some qual; base }

let show_qname q =
  match q.qual with
  | None -> q.base
  | Some m -> m ^ "." ^ q.base

type type_expr =
  | TEName  of string
  (* 'a, and 'a: Ord for one that carries a constraint. The constraint is
     on the variable rather than standing in for it, so two variables under
     one constraint stay two. *)
  | TEVar   of string * string option
  (* `Foo.Status`: a type reached through the module that declares it. Kept
     apart from `TEName` so that every match on a type has to say what it
     does with one. *)
  | TEQual  of string * string
  | TEApp   of type_expr * type_expr    (* List Int, Result Int *)
  | TETuple of type_expr list            (* (Int, Int), 2+ elements *)
  (* Int -> Int, and what calling it performs. Only the innermost arrow of a
     curried type carries them, as in an inferred one: supplying one argument
     of several does nothing until the last arrives. *)
  | TEFun   of type_expr * type_expr * te_effects option

type import_kind =
  | StdlibModule of string   (* import List        — resolves to stdlib/List.wand *)
  | UserPath     of string   (* import ./utils     — resolves relative to caller  *)
  | ModuleURL    of string   (* import https://…   — resolves through wand.pkg    *)
  | ModuleAlias  of string   (* import json2       — a `name` in wand.pkg         *)

type pat =
  | Int      of int
  | Float    of float
  | String   of string
  | Bool     of bool
  | Unit
  | Path     of string
  | DateTime of string
  | Duration of string
  | URL      of string
  | IPv4     of string
  | CIDR     of string
  | Port     of int
  | Version  of string
  | Size     of string
  | PVar     of string
  | Wild
  | PTuple   of pat list
  | PList    of pat list
  | PCons    of pat * pat
  | PConstr       of string * pat list
  | PConstrNamed  of string * (string * pat) list
  (* `Pod(name, restarts)` -- bare identifiers inside a constructor's
     parentheses, which two declarations read two ways. A constructor with
     named fields reads each identifier as its own field, the way `{a, b}`
     puns for a map; one whose payload is a tuple reads them as the tuple.
     The parser cannot tell the two apart, because the declaration may be in
     another file, so it keeps what was written and `constr_bare_reading`
     picks once the declaration is in hand. *)
  | PConstrBare   of string * string list
  (* `Foo.Live`, `Foo.Conf(host = h)`: a constructor reached through the
     module that declares it. A wrapper rather than a qualified twin of each
     form, so `Foo.Conf(name, port)` reads by the same rules as `Conf(name,
     port)` and only the reaching differs. *)
  | PQualified    of string * pat
  (* `(p : Pod)`: the type a parameter is given. The annotation is a
     constraint on the pattern under it, not a pattern of its own. *)
  | PAnnot        of pat * type_expr
  | PMap          of (string * pat) list

(* Which of the two spellings a binding comes back as. `let x = 1 in e` and
   the block binding `(let x = 1; e)` bind the same name over the same body,
   so they build the same node, and a third spelling -- the newline that ends
   the right-hand side -- builds it too. The parser picks one of the two by
   where the binding stands, rather than recording which was written, so a
   block is never printed half in one form and half in the other. *)
type let_style = LetIn | LetBlock

type expr =
  | Int      of int
  | Float    of float
  | String   of string
  | Bool     of bool
  | Unit
  | Path     of string
  | Glob     of string
  | DateTime of string
  | Duration of string
  (* A URL literal, and the `Net(...)` bound of the file it was written in.
     `HTTP.get https://api.example.com/x` builds its request inside the
     standard library, so the construction site carries the wrong manifest;
     the literal is the part the caller wrote, and it is where the caller's
     bound can be attached. A URL the run computed has none, which is the
     case `V-NET1` reports. *)
  | URL      of string * string list option
  | IPv4     of string
  | CIDR     of string
  | Port     of int
  | Version  of string
  | Size     of string
  | Var      of string
  | Constr   of string
  | EnvVar   of string
  | Hole
  | App      of expr * expr
  | Fn       of pat list * expr
  | Let      of pat * expr * expr * let_style
  | LetRec   of (string * pat list * expr) list * expr * let_style
      (* mutually-recursive function group: let f ... = ... and g ... = ... *)
  | If       of expr * expr * expr
  | Match    of expr * case list
  | BinOp    of string * expr * expr
  | UnOp     of string * expr
  | Tuple      of expr list
  | List       of expr list
  (* The third part is the manifest's narrowing list for the label this
     construction may be bounded by, taken from the file the construction
     was written in. `$*()` stamps a `Command` the same way and for the same
     reason: the bound belongs to whoever wrote the words, and by the time a
     value is used its file is long out of reach. Only a built-in type whose
     construction is checked reads it; every other constructor carries
     `None`. *)
  | ConstrApp  of string * (string option * expr) list * string list option
  (* `T(r, b = 3)`: the record `r` with the named fields replaced. Kept
     apart from `ConstrApp` because it is checked differently -- a
     construction has to name every field, and an update names only what
     changes. *)
  | ConstrUpdate of string * expr * (string * expr) list * string list option
  (* `Pod(name, restarts)`: bare identifiers where a constructor's arguments
     go, read the way `PConstrBare` is read on the other side -- fields of
     their own name where the constructor names its fields, a tuple where it
     does not. A bare name *before* a named field is not this: `T(r, b = 3)`
     is an update, and that spelling was taken first. *)
  | ConstrBare of string * string list
  (* `Foo.Live`, `Foo.Conf(host = "a")`: the expression side of
     `PQualified`. *)
  | Qualified of string * expr
  | Field      of expr * string
  | Seq      of expr * expr
  | Located  of Token.loc * expr
  | Contract of expr list * expr list * expr
  (* The second component is the Shell allowlist of the file this site was
     written in, when its manifest narrows Shell -- `uses {Shell(git)}`.
     Jurisdiction travels with the site: a closure from a narrowed file
     keeps its own file's bound however far it is passed. None means the
     site is unbounded (bare `Shell`, or no manifest). *)
  | RunCmd    of expr * string list option
  | RunQuery  of expr * string list option
  (* `$*(cmd)`: the command itself, not its output. `$()` and `$?()` are
     this one run and this one queried, so all three carry the same payload
     and the same bound. *)
  | MkCommand of expr * string list option
  | RegexLit  of string * string
  | ImportExpr of import_kind
  | Interp   of (string * expr) list * string
  (* A backtick string: the same value as `String`/`Interp`, kept apart so
     the formatter can give one back as one. *)
  | RawString of string
  | RawInterp of (string * expr) list * string
  (* A command's interpolations, each carrying where it was written and so
     what it has to be quoted for -- see `Token.hole`. Kept apart from
     `Interp` because a string has no argument boundaries at all. *)
  | CmdInterp of (string * expr * Token.hole) list * string
  | Handle   of expr * handle_case list
  | Try      of expr
  (* `with r as p -> body`: acquire, bind, run, release. The resource is a
     description of how to get and give back, so it is an ordinary
     expression here rather than something already open. *)
  | With     of expr * pat * expr
  | Annot    of type_expr * expr
  | MapLit   of (string * expr) list

and case = pat * expr option * expr

and handle_case =
  | EffectCase of string * pat * string * expr  (* op, arg_pat, cont_name, body *)
  | ReturnCase of pat * expr

(* Which reading `PConstrBare` carries, once the declaration says whether the
   constructor names its fields. Both stages that match values ask here, so
   the two cannot drift apart: `Pod(name, restarts)` binds each field to its
   own name where `Pod` has fields, and matches the tuple payload where it
   does not. *)
let constr_bare_reading ~named_fields name ids : pat =
  if named_fields then PConstrNamed (name, List.map (fun i -> (i, PVar i)) ids)
  else match ids with
    (* `Red()` is the constructor that carries nothing, which is what empty
       parentheses mean where there are no fields to name. *)
    | [] -> PConstr (name, [])
    | ids -> PConstr (name, [PTuple (List.map (fun i -> PVar i) ids)])


(* The same question on the expression side, and the same answer: a
   constructor that names its fields takes each identifier as the field of
   that name, and one that does not takes them as the tuple it is applied
   to. *)
let constr_bare_construction ~named_fields name ids : expr =
  if named_fields then
    ConstrApp (name, List.map (fun i -> (Some i, Var i)) ids, None)
  else match ids with
    (* `Some ()` is the constructor applied to unit, which is what an empty
       pair of parentheses means everywhere else. *)
    | [] -> App (Constr name, Unit)
    | ids -> App (Constr name, Tuple (List.map (fun i -> Var i) ids))

(* ── Pretty-print ─────────────────────────────────────────────────────────── *)

(* The expression under any `Located` wrappers. Shared here because nearly
   every stage wants it, and each keeping its own copy is how they drift. *)
let rec pat_names (p : pat) =
  match p with
  | PVar n -> [n]
  | PTuple ps | PList ps -> List.concat_map pat_names ps
  | PCons (h, t) -> pat_names h @ pat_names t
  | PConstr (_, ps) -> List.concat_map pat_names ps
  | PConstrNamed (_, kvs) | PMap kvs ->
    List.concat_map (fun (_, p) -> pat_names p) kvs
  | PConstrBare (_, ids) -> ids
  | PAnnot (p, _) -> pat_names p
  | _ -> []

let rec strip_located = function
  | Located (_, e) -> strip_located e
  | e -> e

let rec show_pat : pat -> string = function
  | Int n      -> string_of_int n
  | Float f    -> string_of_float f
  | String s   -> Printf.sprintf "%S" s
  | Bool b     -> string_of_bool b
  | Unit       -> "()"
  | Path s     -> Printf.sprintf "path:%s" s
  | DateTime s -> Printf.sprintf "datetime:%s" s
  | Duration s -> Printf.sprintf "dur:%s" s
  | URL s      -> Printf.sprintf "url:%s" s
  | IPv4 s     -> Printf.sprintf "ipv4:%s" s
  | CIDR s     -> Printf.sprintf "cidr:%s" s
  | Port n     -> Printf.sprintf "port:%d" n
  | Version s  -> Printf.sprintf "ver:%s" s
  | Size s     -> Printf.sprintf "size:%s" s
  | PVar s         -> s
  | Wild           -> "_"
  | PTuple ps      -> Printf.sprintf "(%s)" (String.concat ", " (List.map show_pat ps))
  | PList ps       -> "[" ^ String.concat ", " (List.map show_pat ps) ^ "]"
  | PCons (h, t)   -> Printf.sprintf "[%s :: %s]" (show_pat h) (show_pat t)
  | PConstr (c,[]) -> c
  | PConstr (c,ps) -> Printf.sprintf "(%s %s)" c (String.concat " " (List.map show_pat ps))
  | PConstrNamed (c, kvs) ->
    Printf.sprintf "(%s %s)" c (String.concat ", "
      (List.map (fun (k, p) -> k ^ "=" ^ show_pat p) kvs))
  | PConstrBare (c, ids) ->
    Printf.sprintf "%s(%s)" c (String.concat ", " ids)
  | PQualified (m, p) -> Printf.sprintf "%s.%s" m (show_pat p)
  | PMap kvs ->
    "{" ^ String.concat ", " (List.map (fun (k, p) -> k ^ " = " ^ show_pat p) kvs) ^ "}"
  | PAnnot (p, _)  -> show_pat p

let rec show : expr -> string = function
  | Int n      -> string_of_int n
  | Float f    -> string_of_float f
  | String s   -> Printf.sprintf "%S" s
  | Bool b     -> string_of_bool b
  | Unit       -> "()"
  | Path s     -> Printf.sprintf "path:%s" s
  | Glob s     -> Printf.sprintf "glob:%s" s
  | DateTime s -> Printf.sprintf "datetime:%s" s
  | Duration s -> Printf.sprintf "dur:%s" s
  | URL (s, _) -> Printf.sprintf "url:%s" s
  | IPv4 s     -> Printf.sprintf "ipv4:%s" s
  | CIDR s     -> Printf.sprintf "cidr:%s" s
  | Port n     -> Printf.sprintf "port:%d" n
  | Version s  -> Printf.sprintf "ver:%s" s
  | Size s     -> Printf.sprintf "size:%s" s
  | Var x      -> x
  | Constr x   -> x
  | EnvVar x   -> Printf.sprintf "$%s" x
  | Hole       -> "?"
  | App (f, x)      -> Printf.sprintf "(@ %s %s)" (show f) (show x)
  | Fn (ps, e)      -> Printf.sprintf "(fn %s -> %s)"
                         (String.concat " " (List.map show_pat ps)) (show e)
  | Let (p, e1, e2, st) ->
    Printf.sprintf "(let %s = %s%s %s)" (show_pat p) (show e1)
      (match st with LetIn -> " in" | LetBlock -> ";") (show e2)
  | LetRec (bindings, e2, st) ->
    Printf.sprintf "(let rec %s%s %s)"
      (String.concat " and " (List.map (fun (n, ps, b) ->
        Printf.sprintf "%s %s = %s" n
          (String.concat " " (List.map show_pat ps)) (show b)) bindings))
      (match st with LetIn -> " in" | LetBlock -> ";")
      (show e2)
  | If (c, t, e)    -> Printf.sprintf "(if %s %s %s)" (show c) (show t) (show e)
  | Match (e, cs)   -> Printf.sprintf "(match %s %s)" (show e) (show_cases cs)
  | BinOp (op,a,b)  -> Printf.sprintf "(%s %s %s)" (show a) op (show b)
  | UnOp (op, e)    -> Printf.sprintf "(%s%s)" op (show e)
  | Tuple es        -> Printf.sprintf "(tuple %s)" (String.concat " " (List.map show es))
  | List es         -> Printf.sprintf "[%s]" (String.concat "; " (List.map show es))
  | ConstrApp (c, kvs, _) ->
    Printf.sprintf "(%s %s)" c (String.concat ", "
      (List.map (fun (k, v) -> (match k with Some n -> n ^ "=" | None -> "") ^ show v) kvs))
  | ConstrUpdate (c, base, kvs, _) ->
    Printf.sprintf "(%s %s with %s)" c (show base) (String.concat ", "
      (List.map (fun (k, v) -> k ^ "=" ^ show v) kvs))
  | ConstrBare (c, ids) -> Printf.sprintf "%s(%s)" c (String.concat ", " ids)
  | Qualified (m, e) -> Printf.sprintf "%s.%s" m (show e)
  | Field (e, l)    -> Printf.sprintf "(. %s %s)" (show e) l
  | Seq (a, b)      -> Printf.sprintf "(seq %s %s)" (show a) (show b)
  | Located (_, e)  -> show e
  | RunCmd   (e, _)   -> Printf.sprintf "$(%s)" (show e)
  | RunQuery (e, _)   -> Printf.sprintf "$?(%s)" (show e)
  | MkCommand (e, _)  -> Printf.sprintf "$*(%s)" (show e)
  | RegexLit (p, f)   -> Printf.sprintf "r/%s/%s" p f
  | ImportExpr (StdlibModule n) -> Printf.sprintf "import %s" n
  | ImportExpr (UserPath p)     -> Printf.sprintf "import %s" p
  | ImportExpr (ModuleURL u)    -> Printf.sprintf "import %s" u
  | ImportExpr (ModuleAlias n)  -> Printf.sprintf "import %s" n
  | RawString s -> Printf.sprintf "`%s`" s
  | RawInterp (parts, tail) ->
    let buf = Buffer.create 32 in
    Buffer.add_char buf '`';
    List.iter (fun (lit, e) ->
      Buffer.add_string buf lit;
      Buffer.add_string buf "%{";
      Buffer.add_string buf (show e);
      Buffer.add_char buf '}'
    ) parts;
    Buffer.add_string buf tail;
    Buffer.add_char buf '`';
    Buffer.contents buf
  | Interp (parts, tail) ->
    let buf = Buffer.create 32 in
    Buffer.add_char buf '"';
    List.iter (fun (lit, e) ->
      Buffer.add_string buf lit;
      Buffer.add_string buf "%{";
      Buffer.add_string buf (show e);
      Buffer.add_char buf '}'
    ) parts;
    Buffer.add_string buf tail;
    Buffer.add_char buf '"';
    Buffer.contents buf
  | CmdInterp (parts, tail) ->
    let buf = Buffer.create 32 in
    List.iter (fun (lit, e, h) ->
      Buffer.add_string buf lit;
      Buffer.add_string buf
        (match (h : Token.hole) with Token.Source -> "%!{" | _ -> "%{");
      Buffer.add_string buf (show e);
      Buffer.add_char buf '}'
    ) parts;
    Buffer.add_string buf tail;
    Buffer.contents buf
  | Handle (body, cases) ->
    let show_handle_case = function
      | EffectCase (op, p, k, b) ->
        Printf.sprintf "(| %s %s %s -> %s)" op (show_pat p) k (show b)
      | ReturnCase (p, b) ->
        Printf.sprintf "(| return %s -> %s)" (show_pat p) (show b)
    in
    Printf.sprintf "(handle %s with %s)" (show body)
      (String.concat " " (List.map show_handle_case cases))
  | Contract (reqs, ens, body) ->
    let clause kw e = Printf.sprintf "(%s %s)" kw (show e) in
    let rs = String.concat " " (List.map (clause "requires") reqs) in
    let es = String.concat " " (List.map (clause "ensures") ens) in
    Printf.sprintf "(contract %s %s %s)" rs es (show body)
  | Try e -> Printf.sprintf "(try %s)" (show e)
  | With (r, p, b) ->
    Printf.sprintf "(with %s as %s -> %s)" (show r) (show_pat p) (show b)
  | Annot (_, e) -> show e
  | MapLit kvs ->
    "{" ^ String.concat ", " (List.map (fun (k, e) -> k ^ " = " ^ show e) kvs) ^ "}"

and show_cases cs = String.concat " " (List.map show_case cs)

and show_case (p, g, e) =
  match g with
  | None   -> Printf.sprintf "(| %s -> %s)" (show_pat p) (show e)
  | Some g -> Printf.sprintf "(| %s when %s -> %s)" (show_pat p) (show g) (show e)

let pp ppf (e : expr) = Format.pp_print_string ppf (show e)
let equal a b = show a = show b

(* What may stand as a field default: a value written out. No variable, no
   call, nothing that reads the world -- so the default needs no environment
   to evaluate in and no effect to declare, and `wand d` can print it back.
   A constructor application counts, which is what makes `None`, `Some 3` and
   a nested record available. *)
let rec is_written_value (e : expr) : bool =
  match e with
  | Int _ | Float _ | String _ | Bool _ | Unit
  | Path _ | Glob _ | DateTime _ | Duration _ | URL _ | IPv4 _ | CIDR _
  | Port _ | Version _ | Size _ | RegexLit (_, _) -> true
  | Constr _ -> true
  | Located (_, e) -> is_written_value e
  | UnOp ("-", e) -> is_written_value e
  | Tuple es | List es -> List.for_all is_written_value es
  | MapLit kvs -> List.for_all (fun (_, v) -> is_written_value v) kvs
  | ConstrApp (_, kvs, _) -> List.for_all (fun (_, v) -> is_written_value v) kvs
  | ConstrBare (_, _) -> true
  | App (f, a) -> is_constr_head f && is_written_value a
  | _ -> false

and is_constr_head (e : expr) : bool =
  match e with
  | Constr _ -> true
  | Located (_, e) -> is_constr_head e
  | App (f, a) -> is_constr_head f && is_written_value a
  | _ -> false

(* ── Top-level program ────────────────────────────────────────────────────── *)

type ctor_def = {
  name   : string;
  (* Where it was written. A declaration is checked after the whole file is
     read, so the error has no expression to blame and would otherwise land
     on line 1 -- which reads as "the first declaration" to anything that
     acts on a location, and the first is exactly the one a repeat is not.
     `None` for the built-in definitions, which are in no file. *)
  loc    : Token.loc option;
  fields : (string option * type_expr) list;
  (* `port : Port = :8080`. A field with one of these may be left out of a
     construction, and a derived decoder reads it from the default when the
     document has nothing under that name. Keyed by field name, so only a
     named field can carry one -- a positional payload has no name to leave
     out. *)
  defaults : (string * expr) list;
}

type type_def =
  | Variants of string * string list * ctor_def list
      (* name, type parameters (e.g. ["a"] for Option 'a), constructors *)
  (* `type Point = (Int, Int)`: another name for a type that already exists,
     rather than a new one. Transparent -- the two are interchangeable --
     so this introduces no constructor and nothing at run time.

     A single name after `=` is ambiguous while parsing, because whether it
     is a type is not known until every declaration has been read:
     `type Colour = Red` is a variant, and `type Point = Pair` an alias.
     The parser leaves those as `Variants` and the typechecker turns the
     ones whose constructor names a type into this. *)
  | Alias of string * string list * type_expr

(* A contract on a module: the functions it must provide. The type parameter
   is instantiation rather than dispatch -- in `implement Ord Int` the `Int`
   is what `'a` is bound to, so `max` in that implementation has to be
   `Int -> Int -> Int`. *)
type interface_def = {
  if_name    : string;
  if_params  : string list;
  if_members : (string * type_expr) list;
}

(* A module's claim to an interface, and the bindings that answer for it.
   The names are the module's own: nothing is hoisted or registered, so an
   implementation cannot be written anywhere but the file it is about. *)
type implement_def = {
  im_iface : string;          (* as written: `Ord`, or `Foo.Ord` *)
  im_args  : type_expr list;  (* the `Int` in `implement Ord Int` *)
  (* Each binding with the position of its own `let`, because a member is a
     definition in its own right: an editor that showed them all at the
     block's position ran four signatures together on one line. *)
  im_binds : (string * pat list * expr * Token.loc) list;
}

type top_item =
  | TLLet    of string * pat list * expr
  | TLLetRec of (string * pat list * expr) list
  | TLLetPat of pat * expr
  | TLImport of import_kind
  (* The declaration, and where it was written. A `type_def` also arrives
     from an import and from the built-ins, where there is no file to point
     into, so the location belongs to the item rather than to the definition
     it carries. *)
  | TLType   of type_def * Token.loc option
  (* Both carry where they were written, for the same reason a `type_def`
     does: one arrives from an import, where there is no file to point
     into. *)
  | TLInterface of interface_def * Token.loc option
  | TLImplement of implement_def * Token.loc option
  | TLExpr   of expr

type program = {
  items : top_item list;
  docs  : (string * string) list;  (* name -> doc string *)
  (* `uses {Shell, FS.Write}`, when the file declares one. Syntactically the
     first item, so a reader knows the bound without searching. Each label
     is its name plus, for `Shell(git, curl)`, the binaries it admits --
     None is the bare label. *)
  manifest : ((string * string list option) list * Token.loc) option;
}
