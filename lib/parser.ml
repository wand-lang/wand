open Ast

(* The position rides beside the message, not baked into it: every consumer
   that wants "3:5: " renders it, and the ones that want the numbers (the
   JSON output, the language server) read them without re-parsing prose.
   `None` is for the errors with no single token to point at. *)
exception ParseError of Token.loc option * string

let fail msg = raise (ParseError (None, msg))
let fail_at loc msg = raise (ParseError (Some loc, msg))

type state = {
  tokens : (Token.t * Token.loc) array;
  mutable pos : int;
  mutable paren_depth : int;
    (* depth of unclosed (/[/{ -- lets newline-significance checks tell "end
       of statement" apart from "still inside an open bracket" *)
  top_fns : (string, int) Hashtbl.t;
    (* function name -> arity, for top-level definitions already completed.
       A function's equations are parsed as one contiguous group, so seeing
       a name here again means a later `let` for it after a gap. *)
  mutable with_owners : int;
    (* how many enclosing constructs are waiting to consume a `with` --
       a `match` scrutinee or `handle` body in progress. `try e` followed
       by `with` is OCaml drift worth naming, but only when no such owner
       is waiting: `match try thunk () with | ...` is idiomatic wand. *)
  mutable clause_name : string option;
    (* the name whose equations are being read, while a clause body is being
       read. A line below that begins another equation for it ends the body,
       however far in it is indented -- see `newline_breaks_expr`. *)
  mutable stmt_depth : int;
    (* the bracket depth the anchor below was set at. Inside a bracket
       opened since then, a newline means nothing again -- which is the half
       of the old rule that was right. *)
  mutable stmt_col : int;
    (* the column the statement being parsed began at. A newline ends that
       statement unless the line after it is indented past this -- which is
       what lets a definition run onto more lines without brackets, and what
       lets a newline end a binding inside a block the way it does at the top
       level. Saved and restored around each nested statement. *)
  mutable shell_allow : string list option;
    (* the manifest's Shell(...) allowlist, once one has been parsed --
       stamped onto every $()/$?() site so the bound travels with the
       site's AST wherever its closure goes. *)
  mutable net_allow : string list option;
    (* the manifest's Net(...) list, stamped onto every construction for the
       same reason and in the same way: a request is checked against the
       bound of the file that built it, and by the time it is sent that file
       is no longer anywhere to be asked. *)
}

let make tokens =
  { tokens = Array.of_list tokens; pos = 0;
    paren_depth = 0; top_fns = Hashtbl.create 16; with_owners = 0;
    clause_name = None; stmt_depth = 0; stmt_col = 1; shell_allow = None;
    net_allow = None }

(* A parser for the source inside an interpolation hole. It is the same
   file, so a URL literal or a `$()` in the hole carries the file's manifest
   bound the way one outside it does. *)
let sub_parser s tokens =
  let s2 = make tokens in
  s2.shell_allow <- s.shell_allow;
  s2.net_allow <- s.net_allow;
  s2

(* Comments are invisible to the real parser, exactly like `Newline`. A run
   of them above a definition is that definition's documentation, which
   `doc_run_before` reads straight from the token array. *)
let is_skippable = function
  | Token.Newline | Token.LineComment _ -> true
  | _ -> false

let skip s =
  while s.pos < Array.length s.tokens
     && is_skippable (fst s.tokens.(s.pos)) do
    s.pos <- s.pos + 1
  done

(* Only a `Newline` token is a line break. A comment never carries one: it
   runs to the end of its line, and the `Newline` that follows supplies the
   break. *)
let has_newline_before_next s =
  let i = ref s.pos in
  let seen_break = ref false in
  let continue_ = ref true in
  while !continue_ && !i < Array.length s.tokens do
    (match fst s.tokens.(!i) with
     | Token.Newline -> seen_break := true; incr i
     | Token.LineComment _ -> incr i
     | _ -> continue_ := false)
  done;
  !seen_break

let peek_col s =
  let i = ref s.pos in
  while !i < Array.length s.tokens && is_skippable (fst s.tokens.(!i)) do incr i done;
  if !i < Array.length s.tokens then (snd s.tokens.(!i)).Token.col else 0

(* Does a newline here end the statement?

   It used to be decided by bracket depth alone: outside every bracket a
   newline ended a statement, inside one it meant nothing. That is two rules
   for one piece of punctuation, and it is why `let a = 1` inside a `( ... )`
   needed a `;` or an `in` when the same line at the top level needed
   neither, and why a definition could not run onto a second line without
   being bracketed first.

   One rule now: indentation. A newline ends the statement unless the line
   after it is indented past the column the statement began at. So a
   continuation says it is one by sitting further in, which is how it would
   be written anyway, and a line that starts back at the statement's own
   column starts a new one.

   Depth still matters, but only to say when the anchor stops applying: a
   bracket opened since the anchor was set puts the statement back inside
   something, and a newline in there is formatting again. That is why
   `let seeded = (List.fold_left\n    ...)` keeps working with its
   continuation lines to the left of the `let` -- they are inside the
   bracket, not after the statement.

   This only ever decides whether two atoms juxtapose into an application.
   An infix operator is never blocked by a newline -- `1\n  + 2` was already
   one expression -- and a construct still owed a body (`fn x ->`, `match ...
   with`) reads that body through a path that never consults this. *)
(* Is what comes next another equation for `name`? `let f 0 = ...` and then
   `f n = ...` on the line below, with or without a repeated `let`.

   A clause's body would otherwise swallow the equation under it whenever
   that equation is indented past the binding's `let` -- which is exactly
   how the aligned spelling is written:

     let fib 0 = 0
         fib 1 = 1

   The scan stops at the `=`, and refuses to cross a line end, so it can
   only ever match something written as one equation. *)
let starts_another_equation s name =
  let n = Array.length s.tokens in
  let i = ref s.pos in
  while !i < n && is_skippable (fst s.tokens.(!i)) do incr i done;
  if !i < n && fst s.tokens.(!i) = Token.Let then begin
    incr i;
    while !i < n && is_skippable (fst s.tokens.(!i)) do incr i done
  end;
  match (if !i < n then Some s.tokens.(!i) else None) with
  | Some (Token.Ident n', loc) when n' = name ->
    let line = loc.Token.line in
    let j = ref (!i + 1) in
    let answer = ref false and go = ref true in
    while !go && !j < n do
      (match s.tokens.(!j) with
       | (Token.Eq, _) -> answer := true; go := false
       | (_, l) when l.Token.line <> line -> go := false
       | (Token.Newline, _) | (Token.LineComment _, _) -> go := false
       | _ -> incr j)
    done;
    !answer
  | _ -> false

let newline_breaks_expr s =
  has_newline_before_next s
  && (match s.clause_name with
      | Some name when starts_another_equation s name -> true
      | _ -> s.paren_depth = s.stmt_depth && peek_col s <= s.stmt_col)

let peek s =
  let i = ref s.pos in
  while !i < Array.length s.tokens && is_skippable (fst s.tokens.(!i)) do incr i done;
  if !i < Array.length s.tokens then fst s.tokens.(!i) else Token.EOF

(* The token after the next one, skipping what `peek` skips. One token of
   lookahead is all that tells `(p : Pod)` from the cons mistake. *)
(* Three tokens ahead, for `Foo.Live` in a pattern: the module, the dot, and
   the constructor. *)
let peek3 s =
  let skip_from j =
    let k = ref j in
    while !k < Array.length s.tokens && is_skippable (fst s.tokens.(!k)) do incr k done;
    !k
  in
  let a = skip_from s.pos in
  if a >= Array.length s.tokens then Token.EOF
  else
    let b = skip_from (a + 1) in
    if b >= Array.length s.tokens then Token.EOF
    else
      let c = skip_from (b + 1) in
      if c < Array.length s.tokens then fst s.tokens.(c) else Token.EOF

let peek2 s =
  let i = ref s.pos in
  let skip_from j =
    let k = ref j in
    while !k < Array.length s.tokens && is_skippable (fst s.tokens.(!k)) do incr k done;
    !k
  in
  i := skip_from !i;
  if !i >= Array.length s.tokens then Token.EOF
  else begin
    let j = skip_from (!i + 1) in
    if j < Array.length s.tokens then fst s.tokens.(j) else Token.EOF
  end

let peek_loc s =
  let i = ref s.pos in
  while !i < Array.length s.tokens && is_skippable (fst s.tokens.(!i)) do incr i done;
  if !i < Array.length s.tokens then snd s.tokens.(!i) else Token.point 0 0 0

(* The token just consumed. `advance` steps past skippables before taking
   one, so `tokens.(pos - 1)` is it -- which is the extent of a name that has
   already been read, as against `peek_loc`, which is the one still ahead. *)
let last_loc s =
  if s.pos = 0 || s.pos > Array.length s.tokens then peek_loc s
  else snd s.tokens.(s.pos - 1)

(* `start` widened to stop at the last token consumed. `peek` never moves
   `pos`, and `advance` steps past skippables before consuming, so
   `tokens.(pos - 1)` is always the last real token of whatever just
   finished parsing -- which makes this the whole-expression extent a
   `Located` wrapper wants, rather than its first token's. *)
let span_to_here s (start : Token.loc) =
  if s.pos = 0 || s.pos > Array.length s.tokens then start
  else Token.span_to start (snd s.tokens.(s.pos - 1))

(* Parse with `f` and wrap what it returns in a `Located` spanning
   everything it consumed. The explicit sequencing matters: the span can
   only be read once `f` has finished moving `pos`. *)
let locate s f =
  let start = peek_loc s in
  let e = f () in
  Ast.Located (span_to_here s start, e)

(* The column of the token just consumed. Used by `let_`, which is entered
   with its `let` already taken and needs that keyword's column. *)
let prev_col s =
  let i = ref (s.pos - 1) in
  while !i > 0 && is_skippable (fst s.tokens.(!i)) do decr i done;
  if !i >= 0 && !i < Array.length s.tokens then (snd s.tokens.(!i)).Token.col else 1

let open_bracket s = s.paren_depth <- s.paren_depth + 1
let close_bracket s = s.paren_depth <- max 0 (s.paren_depth - 1)

let advance s =
  skip s;
  if s.pos >= Array.length s.tokens then Token.EOF
  else begin
    let t = fst s.tokens.(s.pos) in
    s.pos <- s.pos + 1;
    (match t with
     | Token.LParen | Token.LBracket | Token.LBrace -> open_bracket s
     | Token.RParen | Token.RBracket | Token.RBrace -> close_bracket s
     | _ -> ());
    t
  end

let advance_loc s =
  skip s;
  if s.pos >= Array.length s.tokens
  then (Token.EOF, Token.point 0 0 0)
  else begin
    let pair = s.tokens.(s.pos) in
    s.pos <- s.pos + 1;
    (match fst pair with
     | Token.LParen | Token.LBracket | Token.LBrace -> open_bracket s
     | Token.RParen | Token.RBracket | Token.RBrace -> close_bracket s
     | _ -> ());
    pair
  end

(* A trial parse rewinds with these, which carry the bracket depth back as
   well as the position. The depth is what decides whether a newline ends a
   statement, so a rewind that leaves it raised makes every later newline at
   the top level look like it is inside brackets: after `type X (T, U)`,
   whose fields are read by trying the named form first, the definition two
   lines down was read as a continuation of the one above it, and `wand f`
   wrote that reading back. *)
let mark s = (s.pos, s.paren_depth, s.stmt_col, s.stmt_depth, s.clause_name)

let rewind s (pos, depth, col, sdepth, cname) =
  s.pos <- pos; s.paren_depth <- depth; s.stmt_col <- col;
  s.stmt_depth <- sdepth; s.clause_name <- cname

(* A field name sits where no scope reaches it: after a `.`, or before the
   `:` or `=` that follows it inside a constructor's brackets. A word the
   language has taken reads as itself in those three places. `type` is a
   field of half the JSON any script decodes -- every Kubernetes condition,
   every JSON Schema node -- and a name wand refuses is a name the document
   still spells that way.

   A pun is the one field position this does not reach, since `T(type)`
   would have to bind a variable. Those say to write the field out. *)
let field_name_of = function
  | Token.Ident name -> Some name
  | t -> Token.keyword_text t

let is_field_name t = field_name_of t <> None

(* Whether a lowercase module name starts a type after a `:`. A user
   module's namespace is its file name, and only the dot after it tells
   `(c : one.Status)` from `(x : xs)`, the cons mistake. *)
let qualified_type_after_colon s =
  (match peek2 s with Token.Ident _ -> true | _ -> false)
  && peek3 s = Token.Dot

(* Returns true if the upcoming LParen (not yet consumed) is followed by "ident =" *)
let peek_named_args s =
  let arr = s.tokens in
  let n = Array.length arr in
  let i = ref s.pos in
  let skip () = while !i < n && is_skippable (fst arr.(!i)) do incr i done in
  skip ();
  if !i >= n || fst arr.(!i) <> Token.LParen then false
  else begin
    incr i; skip ();
    if !i >= n then false
    else if is_field_name (fst arr.(!i)) then begin
      incr i; skip ();
      !i < n && fst arr.(!i) = Token.Eq
    end else false
  end

(* `T(type, status = s)` -- a keyword standing where a field's short form
   would go. Reading it as a field list is what lets the message name the
   fix, and costs nothing: a keyword alone in brackets is a parse error
   either way. `result` is the exception, being an expression of its own
   inside a contract, so it stays a payload. *)
let peek_keyword_pun s =
  let arr = s.tokens in
  let n = Array.length arr in
  let i = ref s.pos in
  let skip () = while !i < n && is_skippable (fst arr.(!i)) do incr i done in
  skip ();
  if !i >= n || fst arr.(!i) <> Token.LParen then false
  else begin
    incr i; skip ();
    if !i >= n then false
    else match fst arr.(!i) with
    | Token.Result -> false
    | t when Token.keyword_text t <> None ->
      let j = ref (!i + 1) in
      while !j < n && is_skippable (fst arr.(!j)) do incr j done;
      !j < n && (fst arr.(!j) = Token.Comma || fst arr.(!j) = Token.RParen)
    | _ -> false
  end

(* Whether the upcoming LParen (not yet consumed) holds a field list rather
   than a payload, deciding on any `ident =` in the group and not only the
   first entry. A pattern may pun -- `Pod(name, restarts = r)` names one
   field and takes the other under its own name -- so the entry that says
   which list this is need not be the first one. Patterns only: an `=`
   inside a pattern has no other meaning, where an expression's `T(r, b = 3)`
   is an update and is read by `peek_field_after_comma`. *)
let peek_named_pat_args s =
  let arr = s.tokens in
  let n = Array.length arr in
  let i = ref s.pos in
  let skip () = while !i < n && is_skippable (fst arr.(!i)) do incr i done in
  skip ();
  if !i >= n || fst arr.(!i) <> Token.LParen then false
  else begin
    incr i;
    let depth = ref 1 and found = ref false in
    while !depth > 0 && !i < n do
      (match fst arr.(!i) with
       | Token.LParen | Token.LBracket | Token.LBrace -> incr depth
       | Token.RParen | Token.RBracket | Token.RBrace -> decr depth
       | t when !depth = 1 && is_field_name t ->
         let j = ref (!i + 1) in
         while !j < n && is_skippable (fst arr.(!j)) do incr j done;
         if !j < n && fst arr.(!j) = Token.Eq then found := true
       | _ -> ());
      incr i
    done;
    !found
  end

(* Whether the upcoming LParen holds nothing but bare identifiers, two or
   more of them: `Pod(name, restarts)`, the form whose reading the
   declaration decides. One identifier is left alone -- `Wrap(v)` is the
   payload under that name whichever way it is read, so there is nothing for
   a declaration to settle. *)
let peek_bare_args s =
  let arr = s.tokens in
  let n = Array.length arr in
  let i = ref s.pos in
  let skip () = while !i < n && is_skippable (fst arr.(!i)) do incr i done in
  skip ();
  if !i >= n || fst arr.(!i) <> Token.LParen then false
  else begin
    incr i;
    let count = ref 0 and ok = ref true and stop = ref false in
    while not !stop && !i < n do
      skip ();
      (match fst arr.(!i) with
       | Token.Ident _ ->
         incr count; incr i; skip ();
         (match fst arr.(!i) with
          | Token.Comma -> incr i
          | Token.RParen -> stop := true
          | _ -> ok := false; stop := true)
       | _ -> ok := false; stop := true)
    done;
    !ok && !count >= 2
  end

(* From a `,`, whether what follows is a named field rather than another
   element: `T(r, b = 3)` updates `r`, where `T(r, b)` applies `T` to a
   pair. The parser cannot tell at the `(`, because the base may be any
   expression, so it asks here once the first one is read. *)
let peek_field_after_comma s =
  let arr = s.tokens in
  let n = Array.length arr in
  let i = ref s.pos in
  let skip () = while !i < n && is_skippable (fst arr.(!i)) do incr i done in
  if !i >= n || fst arr.(!i) <> Token.Comma then false
  else begin
    incr i; skip ();
    if !i >= n then false
    else if is_field_name (fst arr.(!i)) then begin
      incr i; skip ();
      !i < n && fst arr.(!i) = Token.Eq
    end else false
  end

let keywords = [
  "let"; "in"; "match"; "with"; "if"; "then"; "else"; "fn"; "fun";
  "type"; "import"; "when"; "and"; "or";
  "handle"; "return"
]

let keyword_hint = function
  | Token.Ident "of" ->
    " -- a constructor takes its payload directly: 'Circle Int', \
     not 'Circle of Int'"
  | Token.Ident s -> Util.hint s keywords
  (* Corrections for reserved words a reader of OCaml or Python would
     reach for; naming the wand spelling here is what makes the
     edit-typecheck loop converge instead of circle. *)
  | Token.And ->
    " -- the boolean operator is '&&'; 'and' only joins mutually \
     recursive let bindings"
  | Token.Or -> " -- the boolean operator is '||'"
  | Token.End ->
    " -- expressions group with parentheses, not 'begin ... end'"
  (* `in` is a `let`'s own keyword and means nothing without one. Reached
     here, the binding it would close is missing: `g x in y` is an
     expression, a keyword, and another expression. The message used to name
     the bracket or the token and leave the reader to work out which of the
     three was wrong. *)
  | Token.In ->
    " -- 'in' closes a 'let' and there is none open here; a binding is \
     'let name = value in body', or 'let name = value;' with the body below"
  | _ -> ""

let expect s tok =
  let loc = peek_loc s in
  let t = advance s in
  if not (Token.equal t tok) then
    fail_at loc (Format.asprintf "expected %a, got %a%s"
      Token.pp tok Token.pp t (keyword_hint t))

(* `puns` says whether the short form is available where this is read. It
   is in a construction and in a pattern, where a bare field also binds a
   name -- so a keyword there has to carry its `=`, and the message says
   that rather than refusing the word. *)
let expect_field_name ?(puns = false) s =
  let loc = peek_loc s in
  match advance s with
  | Token.Ident name -> name
  | t ->
    (match Token.keyword_text t with
     | Some name when (not puns) || peek s = Token.Eq -> name
     | Some name ->
       fail_at loc (Printf.sprintf
         "'%s' is a keyword, so this field cannot take the short form: write '%s = %s_'"
         name name name)
     | None ->
       fail_at loc (Format.asprintf "expected a field name, got %a" Token.pp t))

let expect_ident s =
  let loc = peek_loc s in
  match advance s with
  | Token.Ident name -> name
  (* A word the language has taken cannot be a name here, and saying only
     "expected identifier" leaves the reader looking at a word that plainly
     is one. `result` is the one this is written for: it names a contract's
     return value, so `type Run(result : T)` is refused. *)
  | t when Token.is_keyword t ->
    fail_at loc (Format.asprintf
      "'%a' is a keyword, so it cannot be a name here" Token.pp t)
  | t -> fail_at loc (Format.asprintf "expected identifier, got %a" Token.pp t)

(* A handler case's continuation binder. `_` is allowed and means the case
   answers without resuming -- a normal thing to write now that abandoning a
   continuation releases what the abandoned code was holding. It binds a
   name no expression can mention, so the intent is stated rather than
   left to a reader noticing that some `k` is never used. *)
let expect_cont_name s =
  let loc = peek_loc s in
  match advance s with
  | Token.Ident name -> name
  | Token.Underscore -> "_"
  | t -> fail_at loc (Format.asprintf
      "expected a name for the continuation, or _ if it is not resumed, \
       got %a" Token.pp t)

(* ── Binding powers ───────────────────────────────────────────────────────── *)

let lbp = function
  | Token.PipeArrow   -> 10
  (* A `:` binds here so that the message below is reached rather than
     "expected -> , got :" from wherever the expression happened to end. *)
  | Token.Colon | Token.DoubleColon -> 15
  | Token.PipePipe  -> 20
  | Token.AmpAmp    -> 30
  | Token.EqEq | Token.BangEq
  | Token.Lt   | Token.Gt
  | Token.LtEq | Token.GtEq -> 40
  | Token.Plus | Token.Minus | Token.PlusPlus -> 50
  | Token.Star | Token.Slash | Token.Percent -> 60
  | Token.Dot  -> 80
  | _ -> 0

let is_atom_start = function
  | Token.Int _ | Token.Float _ | Token.String _ | Token.Bool _
  | Token.Path _ | Token.Glob _ | Token.DateTime _
  | Token.Duration _ | Token.URL _ | Token.IPv4 _ | Token.CIDR _
  | Token.Port _ | Token.Version _ | Token.Size _
  | Token.Ident _ | Token.Upper _ | Token.Hole
  | Token.LParen | Token.LBracket | Token.LBrace
  | Token.Dollar | Token.InterpStr _ | Token.RunCmdRaw _ | Token.RunQueryRaw _
  | Token.CommandRaw _
  | Token.RawStr _ | Token.RawInterpStr _
  | Token.Regex _ | Token.EnvVar _ | Token.Import
  | Token.Result
  | Token.Handle | Token.Try -> true
  | _ -> false

(* `with` belongs here and not in `is_atom_start`: it starts an expression,
   the way `let` and `match` do, but it is not an argument without brackets.
   Left out, a binding's body could not be a `with` -- `let _ = 1;` followed
   by one fell through to the file, and the formatter wrote that shape from
   a program that meant the other. Found by the daily fuzzer. *)
let is_expr_start = function
  | Token.Let | Token.If | Token.Match | Token.Fn | Token.With
  | Token.Minus | Token.Bang -> true
  | t -> is_atom_start t

(* Whether the `;` just ahead hands the rest to a binding's body rather than
   ending the statement the binding sits in. The token after it has to start
   an expression and to sit at or past `anchor` -- the binding's own column,
   which is what `parse_body` measures the newline against. A statement of a
   chain sits level with the one above it, so the test is `>=`, exactly as
   it is there. *)
let semicolon_joins_the_body s anchor =
  let n = Array.length s.tokens in
  let i = ref s.pos in
  (* Past the `;` itself, then past whatever `peek` steps over. *)
  if !i < n && fst s.tokens.(!i) = Token.Semicolon then incr i;
  while !i < n && is_skippable (fst s.tokens.(!i)) do incr i done;
  !i < n
  && is_expr_start (fst s.tokens.(!i))
  && (snd s.tokens.(!i)).Token.col >= anchor

let is_pat_atom_start = function
  | Token.Int _ | Token.Float _ | Token.String _ | Token.RawStr _ | Token.Bool _
  | Token.Ident _ | Token.Underscore | Token.Upper _
  | Token.LParen | Token.LBracket | Token.LBrace -> true
  | _ -> false

(* A multi-equation definition folded into one binding: synthetic `_p0.._pN`
   parameters and a `Match` over them, one case per equation. Shared by the
   local and top-level binding parsers.

   Each equation arrives with the location of its first pattern. The fold
   loses the equation as a syntactic unit, so that location is wrapped
   around the body: it is the only thing left for the typechecker to blame
   when it finds an equation no value can reach. An error raised inside the
   body carries a Located of its own, further in, and innermost wins. *)
let collapse_multi_equation arity eqs : Ast.pat list * Ast.expr =
  match eqs with
  | [(_, p, b)] -> (p, b)
  | eqs ->
    let fresh = List.init arity (fun i -> Printf.sprintf "_p%d" i) in
    let scrutinee = match fresh with
      | [v] -> Ast.Var v
      | vs  -> Ast.Tuple (List.map (fun v -> Ast.Var v) vs)
    in
    let cases = List.map (fun (loc, pats, body) ->
      let pat = match pats with [p] -> p | ps -> Ast.PTuple ps in
      (pat, None, Ast.Located (loc, body))
    ) eqs in
    (List.map (fun v -> Ast.PVar v) fresh, Ast.Match (scrutinee, cases))

(* ── Pattern parsing ──────────────────────────────────────────────────────── *)

let builtin_types = [
  "Int"; "Float"; "String"; "Bool"; "Unit"; "Path";
  "DateTime"; "Duration"; "URL";
  "IPv4"; "CIDR"; "Port"; "Version"; "Size"
]

let is_type_atom_start = function
  | Token.Upper _ | Token.LParen | Token.TypeVar _ -> true
  | _ -> false

(* A user module is bound to a lowercase name, so `l.S` is a type atom that
   its first token cannot tell from a variable. Three tokens say it. *)
let type_atom_ahead s =
  is_type_atom_start (peek s)
  || ((match peek s with Token.Ident _ -> true | _ -> false)
      && peek2 s = Token.Dot
      && (match peek3 s with Token.Upper _ -> true | _ -> false))

let rec parse_type_atom s =
  let loc = peek_loc s in
  match advance s with
  (* `Foo.Status`: the type reached through the module that declares it. A
     module name and a type name are both `Upper`, so the dot is what tells
     this from a bare name. *)
  | Token.Ident name when peek s = Token.Dot
                       && (match peek2 s with Token.Upper _ -> true | _ -> false) ->
    (* A user module's namespace is its file name, which is lowercase. *)
    ignore (advance s);
    (match advance s with
     | Token.Upper b -> Ast.TEQual (name, b)
     | t -> fail_at loc (Format.asprintf
         "expected a type name after '%s.', got %a" name Token.pp t))
  | Token.Upper name when peek s = Token.Dot ->
    ignore (advance s);
    let base =
      match advance s with
      | Token.Upper b -> b
      | t -> fail_at (peek_loc s) (Format.asprintf
          "expected a type name after '%s.', got %a" name Token.pp t)
    in
    Ast.TEQual (name, base)
  | Token.Upper name -> Ast.TEName name
  (* `'a: Ord` is a variable carrying a constraint. `Ord` written alone is
     the same constraint with no name to tell it from another one. *)
  | Token.TypeVar name ->
    if peek s = Token.Colon then begin
      ignore (advance s);
      match advance s with
      | Token.Upper c -> Ast.TEVar (name, Some c)
      | t -> fail_at (peek_loc s) (Format.asprintf
          "expected a constraint after ''%s:', got %a" name Token.pp t)
    end else Ast.TEVar (name, None)
  | Token.LParen ->
    let first = parse_type_expr s in
    if peek s = Token.Comma then begin
      let rest = ref [] in
      while peek s = Token.Comma do
        ignore (advance s); rest := !rest @ [parse_type_expr s]
      done;
      expect s Token.RParen;
      Ast.TETuple (first :: !rest)
    end else begin
      expect s Token.RParen; first   (* pure grouping *)
    end
  | Token.Ident name ->
    fail_at loc (Printf.sprintf "expected type name, got '%s'%s"
      name (Util.hint name builtin_types))
  (* A field's name is read as a name only when it is one. `result` names a
     contract's return value, so `type Run(result : T)` arrives here as a
     keyword where a type should be, and the message has to say which of the
     two words is the problem. *)
  | t when Token.is_keyword t ->
    fail_at loc (Format.asprintf
      "'%a' is a keyword, so it cannot be a field name" Token.pp t)
  | t -> fail_at loc (Format.asprintf "expected type name, got %a" Token.pp t)

and parse_type_app s =
  let left = ref (parse_type_atom s) in
  while type_atom_ahead s && not (newline_breaks_expr s) do
    left := Ast.TEApp (!left, parse_type_atom s)
  done;
  !left

(* The `! ...` after an arrow. Four shapes, matching what the printer emits
   so a signature `wand t` reports can be pasted back as an annotation:

     ! 'e              a variable, and nothing known
     ! {Shell, IO}     exactly these
     ! {Shell | 'e}    at least Shell, plus whatever 'e stands for
     (absent)          left to inference

   An effect name is the same dotted Upper word a manifest uses, so
   `FS.Read` reads here as it reads there. *)
and parse_te_effect_name s =
  let rec parts acc =
    let part = match advance s with
      | Token.Upper u -> u
      | t -> fail_at (peek_loc s) (Format.asprintf
          "expected an effect name after '!', got %a" Token.pp t)
    in
    let acc = acc @ [part] in
    if peek s = Token.Dot then (ignore (advance s); parts acc) else acc
  in
  String.concat "." (parts [])

and parse_te_effects s =
  ignore (advance s);   (* the ! *)
  match peek s with
  | Token.TypeVar v ->
    ignore (advance s);
    { Ast.te_labels = []; te_var = Some v }
  | Token.LBrace ->
    ignore (advance s);
    let labels = ref [] and var = ref None in
    if peek s <> Token.RBrace then begin
      labels := [parse_te_effect_name s];
      while peek s = Token.Comma do
        ignore (advance s);
        labels := !labels @ [parse_te_effect_name s]
      done;
      if peek s = Token.Pipe then begin
        ignore (advance s);
        match advance s with
        | Token.TypeVar v -> var := Some v
        | t -> fail_at (peek_loc s) (Format.asprintf
            "expected an effect variable after '|', got %a" Token.pp t)
      end
    end;
    expect s Token.RBrace;
    { Ast.te_labels = !labels; te_var = !var }
  | t ->
    fail_at (peek_loc s) (Format.asprintf
      "expected an effect set or variable after '!', got %a" Token.pp t)

and parse_type_expr s =
  let left = parse_type_app s in
  if peek s = Token.Arrow then begin
    ignore (advance s);
    let right = parse_type_expr s in
    (* The effects belong to the innermost arrow. If the right side is
       already an arrow it has taken any `!` for itself, so this one is a
       step in a curried type and carries nothing. *)
    let eff =
      match right with
      | Ast.TEFun _ -> None
      | _ -> if peek s = Token.Bang then Some (parse_te_effects s) else None
    in
    Ast.TEFun (left, right, eff)
  end
  else left

(* A pattern, and a cons written without its brackets. `[h :: t]` is the
   spelling: the brackets say list, exactly as `[a, b, c]` does. `h :: t`
   bare is what an OCaml reader writes, so it is read and `wand f` writes
   the brackets back -- the same treatment `fun` gets for `fn`.

   Right-associative, and the tail is a whole pattern, so `a :: b :: t`
   nests to the right. `Some h :: t` is `(Some h) :: t`: a constructor
   takes its payload by juxtaposition, through `pat_base_`, which stops
   before the `::`. *)
let rec pat_ s =
  let p = pat_base_ s in
  if peek s = Token.DoubleColon then begin
    ignore (advance s);
    PCons (p, pat_ s)
  end else p

and pat_base_ s =
  match peek s with
  | Token.Int n      -> ignore (advance s); (Int n : pat)
  | Token.Float f    -> ignore (advance s); Float f
  | Token.String str -> ignore (advance s); String str
  (* A backtick string is a string; a pattern has no interpolation to keep
     apart, so it needs no separate form. *)
  | Token.RawStr str -> ignore (advance s); String str
  | Token.Bool b     -> ignore (advance s); Bool b
  | Token.Ident name when peek2 s = Token.Dot
                       && (match peek3 s with Token.Upper _ -> true | _ -> false) ->
    ignore (advance s); ignore (advance s);
    (match advance s with
     | Token.Upper base -> PQualified (name, pconstr_body_ s base)
     | t -> fail_at (peek_loc s) (Format.asprintf
         "expected a constructor after '%s.', got %a" name Token.pp t))
  | Token.Ident name -> ignore (advance s); PVar name
  | Token.Underscore -> ignore (advance s); Wild
  | Token.LParen     ->
    ignore (advance s);
    if peek s = Token.RParen then (ignore (advance s); Unit)
    else begin
      let p = pat_ s in
      if peek s = Token.Comma then begin
        let ps = ref [p] in
        while peek s = Token.Comma do
          ignore (advance s); ps := !ps @ [pat_ s]
        done;
        expect s Token.RParen; PTuple !ps
      end else if peek s = Token.Colon
              && (is_type_atom_start (peek2 s) || qualified_type_after_colon s) then begin
        (* `(p : Pod)` gives the parameter a type. One token decides it: a
           type starts with an `Upper` name, a `'a`, or a `(`, and a cons
           pattern -- which is written `[h : t]`, in brackets -- never
           does. So the message below still meets the mistake it was
           written for. *)
        ignore (advance s);
        let te = parse_type_expr s in
        expect s Token.RParen;
        PAnnot (p, te)
      end else if peek s = Token.Colon then
        fail_at (peek_loc s)
          "a cons pattern is written in square brackets: \
           [x :: xs] -- a single ':' gives a name a type"
      else begin
        expect s Token.RParen;
        (* (bare_var) signals single-constructor unwrap; complex patterns stay transparent *)
        match p with PVar _ -> PTuple [p] | _ -> p
      end
    end
  | Token.LBracket ->
    ignore (advance s); list_pat_ s
  | Token.LBrace ->
    ignore (advance s); brace_map_pat_ s
  (* `Foo.Live`, `Foo.Conf(host = h)`: the pattern side of a qualified
     constructor. A lowercase name after the dot is not a constructor, so
     only an `Upper` takes this branch. *)
  | Token.Upper name when peek2 s = Token.Dot
                       && (match peek3 s with Token.Upper _ -> true | _ -> false) ->
    ignore (advance s); ignore (advance s);
    (match advance s with
     | Token.Upper base -> PQualified (name, pconstr_body_ s base)
     | t -> fail_at (peek_loc s) (Format.asprintf
         "expected a constructor after '%s.', got %a" name Token.pp t))
  | Token.Upper name -> ignore (advance s); pconstr_body_ s name
  | t ->
    fail_at (peek_loc s) (Format.asprintf "unexpected token in pattern: %a%s"
      Token.pp t (keyword_hint t))

and pconstr_body_ s name =
  if peek_named_pat_args s || peek_keyword_pun s then begin
    ignore (advance s); (* consume LParen *)
    let fields = ref [] in
    if peek s <> Token.RParen then begin
      (* A bare identifier puns, the way it does in a map pattern: the
         field binds a variable of its own name. *)
      let parse_field () =
        let fname = expect_field_name ~puns:true s in
        if peek s = Token.Eq then begin
          ignore (advance s);
          fields := !fields @ [(fname, pat_ s)]
        end else fields := !fields @ [(fname, (PVar fname : pat))]
      in
      parse_field ();
      while peek s = Token.Comma do ignore (advance s); parse_field () done
    end;
    expect s Token.RParen;
    PConstrNamed (name, !fields)
  end else if peek_bare_args s then begin
    ignore (advance s); (* consume LParen *)
    let ids = ref [expect_ident s] in
    while peek s = Token.Comma do
      ignore (advance s); ids := !ids @ [expect_ident s]
    done;
    expect s Token.RParen;
    PConstrBare (name, !ids)
  end else if peek s = Token.LParen then begin
    ignore (advance s); (* consume LParen *)
    (* Empty parentheses read the way they do in a construction: a pattern
       naming no fields where the constructor has them, the constructor
       that carries nothing where it does not. *)
    if peek s = Token.RParen then (ignore (advance s); PConstrBare (name, []))
    else begin
      (* A payload carries a type the same way anything else does:
         `Ok (v: Pod)`. This branch used to read `(` as the start of an
         argument list and nothing else, so the one place a pattern could
         not be annotated was the one where a decoder's result lands.

         The `:` is a type only when a type follows it, which is the same
         single-token test the grouped pattern uses -- so `Ok (x : xs)`
         still meets the message written for it. *)
      let pat_item () =
        let p = pat_ s in
        if peek s = Token.Colon
              && (is_type_atom_start (peek2 s) || qualified_type_after_colon s) then begin
          ignore (advance s);
          PAnnot (p, parse_type_expr s)
        end else if peek s = Token.Colon then
          fail_at (peek_loc s)
            "a cons pattern is written in square brackets: \
             [x :: xs] -- a single ':' gives a name a type"
        else p
      in
      let pats = ref [pat_item ()] in
      while peek s = Token.Comma do
        ignore (advance s); pats := !pats @ [pat_item ()]
      done;
      expect s Token.RParen;
      (* Parentheses group a tuple; several arguments are written by
         juxtaposition. Mirrors the expression side. *)
      match !pats with
      | (_ :: _ :: _ as ps) -> PConstr (name, [PTuple ps])
      | pats -> PConstr (name, pats)
    end
  end else begin
    let args = ref [] in
    while is_pat_atom_start (peek s) do
      args := !args @ [pat_atom_ s]
    done;
    PConstr (name, !args)
  end

and pat_atom_ s =
  match peek s with
  | Token.Int n      -> ignore (advance s); (Int n : pat)
  | Token.Float f    -> ignore (advance s); Float f
  | Token.String str -> ignore (advance s); String str
  (* A backtick string is a string; a pattern has no interpolation to keep
     apart, so it needs no separate form. *)
  | Token.RawStr str -> ignore (advance s); String str
  | Token.Bool b     -> ignore (advance s); Bool b
  | Token.Ident name -> ignore (advance s); PVar name
  | Token.Underscore -> ignore (advance s); Wild
  | Token.Upper name -> ignore (advance s); PConstr (name, [])
  | Token.LParen     ->
    ignore (advance s);
    if peek s = Token.RParen then (ignore (advance s); Unit)
    else begin
      let p = pat_ s in
      if peek s = Token.Comma then begin
        let ps = ref [p] in
        while peek s = Token.Comma do
          ignore (advance s); ps := !ps @ [pat_ s]
        done;
        expect s Token.RParen; PTuple !ps
      end else if peek s = Token.Colon
              && (is_type_atom_start (peek2 s) || qualified_type_after_colon s) then begin
        (* `(p : Pod)` gives the parameter a type. One token decides it: a
           type starts with an `Upper` name, a `'a`, or a `(`, and a cons
           pattern -- which is written `[h : t]`, in brackets -- never
           does. So the message below still meets the mistake it was
           written for. *)
        ignore (advance s);
        let te = parse_type_expr s in
        expect s Token.RParen;
        PAnnot (p, te)
      end else if peek s = Token.Colon then
        fail_at (peek_loc s)
          "a cons pattern is written in square brackets: \
           [x :: xs] -- a single ':' gives a name a type"
      else (expect s Token.RParen; p)
    end
  | Token.LBracket ->
    ignore (advance s); list_pat_ s
  | Token.LBrace ->
    ignore (advance s); brace_map_pat_ s
  | t ->
    fail_at (peek_loc s) (Format.asprintf "unexpected token in pattern: %a%s"
      Token.pp t (keyword_hint t))

and list_pat_ s =
  (* [ already consumed *)
  if peek s = Token.RBracket then (ignore (advance s); PList [])
  else begin
    (* `[k = pat]` was a map pattern until 0.17; brackets are lists alone
       now. Refused with the correction rather than read on as a list,
       because `k = pat` is no list element and the generic error would
       point nowhere near the mistake. *)
    let is_map =
      match peek s with
      | Token.Ident _ | Token.String _ ->
        let saved = mark s in
        ignore (advance s);
        let result = peek s = Token.Eq in
        rewind s saved;
        result
      | _ -> false
    in
    if is_map then
      fail_at (snd s.tokens.(s.pos - 1))
        "a map pattern is written in braces -- {k = v}, not [k = v]"
    else begin
      (* The elements are read with `pat_base_`: the brackets own the cons
         here, so a `::` inside them is this loop's and not the element's. *)
      let first = pat_base_ s in
      if peek s = Token.Colon then
        fail_at (peek_loc s)
          "cons is '::' -- a single ':' gives a name a type";
      let is_cons () = peek s = Token.DoubleColon in
      if is_cons () then begin
        ignore (advance s);
        (* Chain further cons cells: [a :: b :: c :: t] is PCons(a, PCons(b,
           PCons(c, t))), not just a single cons with a flat tail. *)
        let rec parse_cons_tail () =
          let p = pat_base_ s in
          if is_cons () then begin
            ignore (advance s);
            PCons (p, parse_cons_tail ())
          end else p
        in
        let tl = parse_cons_tail () in
        expect s Token.RBracket;
        PCons (first, tl)
      end else begin
        let pats = ref [first] in
        while peek s = Token.Comma do
          ignore (advance s); pats := !pats @ [pat_base_ s]
        done;
        expect s Token.RBracket;
        PList !pats
      end
    end
  end

(* `{status = s, phase}` -- a map pattern. A bare identifier puns: the key
   binds a variable of its own name. A quoted key has no identifier to pun
   into, so it takes `= pat` like any rename. *)
and brace_map_pat_ s =
  (* { already consumed *)
  if peek s = Token.RBrace then (ignore (advance s); PMap [])
  else begin
    let parse_entry () =
      match advance s with
      | Token.Ident k ->
        if peek s = Token.Eq then (ignore (advance s); (k, pat_ s))
        else (k, (PVar k : pat))
      (* An uppercase key is how an import names a type or a constructor:
         `let {TestOutcome, Pass} = import Test`. A map may have such a key
         too, and reads the same way. *)
      | Token.Upper k ->
        if peek s = Token.Eq then (ignore (advance s); (k, pat_ s))
        else (k, (PVar k : pat))
      | Token.String k ->
        expect s Token.Eq; (k, pat_ s)
      | t -> fail (Format.asprintf "expected map key, got %a" Token.pp t)
    in
    let entries = ref [parse_entry ()] in
    while peek s = Token.Comma do
      ignore (advance s); entries := !entries @ [parse_entry ()]
    done;
    expect s Token.RBrace;
    PMap !entries
  end

(* ── Expression parsing (Pratt) ───────────────────────────────────────────── *)

let rec expr_ bp s =
  let left = ref (atom_ s) in
  let continue_ = ref true in
  while !continue_ do
    let had_newline = newline_breaks_expr s in
    let t = peek s in
    let bp' = lbp t in
    if bp' > bp then begin
      ignore (advance s);
      left := infix_ !left t s
    end else if is_atom_start t && 70 > bp && not had_newline then
      left := App (!left, atom_ s)
    else
      continue_ := false
  done;
  !left

and infix_ left op s =
  match op with
  | Token.PipeArrow  -> BinOp ("|>", left, expr_ 10 s)
  | Token.DoubleColon -> BinOp ("::", left, expr_ 14 s)
  (* `:` was cons until 0.31.0. It is a type now, and a type does not
     belong between two expressions, so the correction is the whole
     answer. Not a `Diag.Replace`: a `:` in this file may also be a type
     or a port, and a substitution cannot tell them apart. *)
  | Token.Colon ->
    fail_at (peek_loc s)
      "cons is '::' -- a single ':' gives a name a type"
  | Token.PipePipe  -> BinOp ("||", left, expr_ 20 s)
  | Token.AmpAmp    -> BinOp ("&&", left, expr_ 30 s)
  | Token.EqEq      -> BinOp ("==", left, expr_ 40 s)
  | Token.BangEq    -> BinOp ("!=", left, expr_ 40 s)
  | Token.Lt        -> BinOp ("<",  left, expr_ 40 s)
  | Token.Gt        -> BinOp (">",  left, expr_ 40 s)
  | Token.LtEq      -> BinOp ("<=", left, expr_ 40 s)
  | Token.GtEq      -> BinOp (">=", left, expr_ 40 s)
  | Token.Plus      -> BinOp ("+",  left, expr_ 50 s)
  | Token.Minus     -> BinOp ("-",  left, expr_ 50 s)
  | Token.PlusPlus  -> BinOp ("++", left, expr_ 50 s)
  | Token.Star      -> BinOp ("*",   left, expr_ 60 s)
  | Token.Slash     -> BinOp ("/",   left, expr_ 60 s)
  | Token.Percent   -> BinOp ("%",   left, expr_ 60 s)
  (* `one.Live`: a constructor reached through a module whose name is
     lowercase, which every user module has. An uppercase member is a
     constructor; a lowercase one is a value. *)
  | Token.Dot when (match peek s with Token.Upper _ -> true | _ -> false) ->
    let m =
      match strip_located left with
      | Var m -> m
      | _ -> fail_at (peek_loc s)
          "a constructor is reached through a module's name"
    in
    Qualified (m, constr_atom_ s)
  | Token.Dot       -> Field (left, expect_field_name s)
  | t -> fail (Format.asprintf "unexpected infix: %a" Token.pp t)

(* An interpolation's body is lexed on its own, so it is told where it sits
   in the file and in which file. Without it a body's positions start at 1:1,
   and an error inside a `%{...}` pointed at line 1 of a file the string is
   nowhere near. *)
and interp_tokens loc src (at : Token.pos) =
  Lexer.tokenize ~file:loc.Token.file ~line:at.Token.p_line
    ~col:at.Token.p_col ~base:at.Token.p_offset src

and atom_base_ s =
  let loc = peek_loc s in
  match advance s with
  | Token.Int n      -> (Int n : expr)
  | Token.Float f    -> Float f
  | Token.String str -> String str
  | Token.Bool b     -> Bool b
  | Token.Path p     -> Path p
  | Token.Glob g     -> Glob g
  | Token.DateTime d -> DateTime d
  | Token.Duration d -> Duration d
  | Token.URL u      -> URL (u, s.net_allow)
  | Token.IPv4 a     -> IPv4 a
  | Token.CIDR c     -> CIDR c
  | Token.Port n     -> Port n
  | Token.Version v  -> Version v
  | Token.Size sz    -> Size sz
  | Token.Ident name  -> Var name
  | Token.Upper name when peek s = Token.Dot && (match peek2 s with
                                                | Token.Upper _ -> true
                                                | _ -> false) ->
    (* `Foo.Live`, `Foo.Conf(host = "a")`: the constructor reached through
       the module that declares it. A lowercase member after the dot is a
       namespace value, and is read elsewhere. *)
    ignore (advance s);
    Ast.Qualified (name, constr_atom_ s)
  | Token.Upper name  -> constr_body_ s name
  | Token.EnvVar name -> EnvVar name
  | Token.Hole        -> Hole
  | Token.Minus      -> UnOp ("-", expr_ 65 s)
  | Token.Bang       -> UnOp ("!", expr_ 65 s)
  | Token.LParen     ->
    if peek s = Token.RParen then (ignore (advance s); Unit)
    else begin
      let e_loc = peek_loc s in
      (* A block that opens with a binding is a sequence whatever follows:
         `(let x = 1; ...)`. Reading it as an expression first would hand
         the binding to `let_` with no way back to the statements after
         it. *)
      if peek s = Token.Let then begin
        let e = paren_seq s in expect s Token.RParen; e
      end else begin
      let e = expr_ 0 s in
      if peek s = Token.Comma then begin
        let es = ref [e] in
        while peek s = Token.Comma do
          ignore (advance s); es := !es @ [expr_ 0 s]
        done;
        expect s Token.RParen; Tuple !es
      end else if peek s = Token.Semicolon then begin
        (* `(e1; e2; e3)` -- statements in sequence, valuing the last. The
           parentheses are what make it unambiguous: outside a bracket a
           newline already separates statements, and juxtaposition means a
           bare `;` could not tell "next statement" from "next argument"
           anywhere an application may continue. A trailing `;` before the
           `)` is allowed. Nested to the right so every discarded statement
           is a Seq's own first child, which is where the typechecker
           records its type for the discarded-Result lint. *)
        ignore (advance s);
        let first = Located (span_to_here s e_loc, e) in
        let e =
          if peek s = Token.RParen then first else Seq (first, paren_seq s)
        in
        expect s Token.RParen; e
      end else if is_expr_start (peek s) then begin
        (* A second statement with no `;` between: the newline separated
           them, as it does at the top level and inside a block that opens
           with a binding. Reached only from here, because the first
           statement had to be read as an expression before anything could
           say it was a statement. *)
        let first = Located (span_to_here s e_loc, e) in
        let e = Seq (first, paren_seq s) in
        expect s Token.RParen; e
      end else (expect s Token.RParen; e)
      end
    end
  | Token.LBracket -> list_ s
  | Token.LBrace   -> brace_map_ s
  | Token.Let      -> let_ s
  | Token.If       -> if_ s
  | Token.Match    -> match_ s
  | Token.Fn       -> fn_ s
  | Token.Import   ->
    (match advance s with
     | Token.Upper n -> ImportExpr (Ast.StdlibModule n)
     | Token.Path p  -> ImportExpr (Ast.UserPath p)
     | t -> fail_at loc (Format.asprintf "expected module name or path after import, got %a"
                Token.pp t))
  | Token.Result   -> Var "result"
  | Token.Dollar   ->
    expect s Token.LParen;
    let e = expr_ 0 s in
    expect s Token.RParen;
    RunCmd (e, s.shell_allow)
  | Token.RunCmdRaw (parts, tail) ->
    let parse_parts = List.map (fun (lit, src, hole, at) ->
      let toks = interp_tokens loc src at in
      let s2 = sub_parser s toks in
      (lit, expr_ 0 s2, hole)
    ) parts in
    if parse_parts = [] then RunCmd (String tail, s.shell_allow)
    else RunCmd (CmdInterp (parse_parts, tail), s.shell_allow)
  | Token.RunQueryRaw (parts, tail) ->
    let parse_parts = List.map (fun (lit, src, hole, at) ->
      let toks = interp_tokens loc src at in
      let s2 = sub_parser s toks in
      (lit, expr_ 0 s2, hole)
    ) parts in
    if parse_parts = [] then RunQuery (String tail, s.shell_allow)
    else RunQuery (CmdInterp (parse_parts, tail), s.shell_allow)
  | Token.CommandRaw (parts, tail) ->
    let parse_parts = List.map (fun (lit, src, hole, at) ->
      let toks = interp_tokens loc src at in
      let s2 = sub_parser s toks in
      (lit, expr_ 0 s2, hole)
    ) parts in
    if parse_parts = [] then MkCommand (String tail, s.shell_allow)
    else MkCommand (CmdInterp (parse_parts, tail), s.shell_allow)
  | Token.Regex (pat, flags) -> RegexLit (pat, flags)
  | Token.InterpStr (parts, tail) ->
    let parsed = List.map (fun (lit, src, at) ->
      let toks = interp_tokens loc src at in
      let s2 = sub_parser s toks in
      (lit, expr_ 0 s2)
    ) parts in
    Interp (parsed, tail)
  | Token.RawStr str -> RawString str
  | Token.RawInterpStr (parts, tail) ->
    let parsed = List.map (fun (lit, src, at) ->
      let toks = interp_tokens loc src at in
      let s2 = sub_parser s toks in
      (lit, expr_ 0 s2)
    ) parts in
    RawInterp (parsed, tail)
  | Token.Handle -> parse_handle_ s
  | Token.With   -> parse_with_ s
  | Token.Try    ->
    let body = expr_ 0 s in
    (* Only a `with` this statement still reaches. The hint is for someone
       writing OCaml's `try e with cases`, which is one statement -- so it
       is owed only where the `with` is still part of one: on the same line,
       or on a line indented past the `try`. A `with` that opens a statement
       of its own is a `with ... as ... ->` bracket at the top level, which
       is an ordinary thing for a script to do, and the layout rule that
       every other construct obeys says so. Reporting it here made
       `try h` above a top-level `with` unparseable, and left the formatter
       bracketing every top-level `with` to avoid a hazard that need not
       exist. Found by test/fuzz. *)
    if peek s = Token.With && s.with_owners = 0 && not (newline_breaks_expr s) then
      fail_at (peek_loc s)
        "try takes no cases, so there is no 'try ... with': 'try e' \
         yields a Result to match on -- and 'handle ... with' is what \
         intercepts effects."
    else Ast.Try body
  | t -> fail_at loc (Format.asprintf "unexpected token: %a%s"
      Token.pp t (keyword_hint t))

(* `.field` binds tightly to the atom it immediately follows -- applied right
   after every atom_base_ call so `f x.y` parses as `f (x.y)`, not `(f x).y`
   (which is what plain infix-operator precedence would give, since by the
   time the Pratt loop in expr_ reaches `.`, `left` already has the whole
   application chain accumulated). *)
(* The constructor after a module's name: `Foo.Live`, `Foo.Conf(host = "a")`.
   The same forms as an unqualified one, so this is the branch that reads
   them, reached from both places. *)
and constr_atom_ s =
  match advance s with
  | Token.Upper name -> constr_body_ s name
  | t -> fail_at (peek_loc s) (Format.asprintf
      "expected a constructor after the module name, got %a" Token.pp t)
and constr_body_ s name =
  (* The constructor's own extent, taken before anything after it is read.
     A diagnostic about the constructor -- `None` swallowing the argument
     meant for the call -- is reported here rather than at the head of the
     spine, and a correction can only be offered over the span that holds
     exactly the name it replaces. *)
  let ctor_loc = last_loc s in
  let constr = Located (ctor_loc, Constr name) in
  (* The bracket a constructor takes has to be one the statement is still
     open for. `peek` steps over a newline without asking, so a constructor
     alone on its line used to absorb a bracket that opened the *next* item:
     `H` and `("..." H)` were two items, and re-reading them made one. The
     rule is the ordinary one -- a line back at the statement's own column
     starts something new, and a bracket the statement already opened
     suspends it -- so an indented `(` is still a payload. Found by
     test/fuzz.

     All three shapes below need it, and all three want the same `(`, so the
     question is asked once. *)
  let takes_a_bracket = peek s = Token.LParen && not (newline_breaks_expr s) in
  if takes_a_bracket && (peek_named_args s || peek_keyword_pun s) then begin
    ignore (advance s); (* consume LParen *)
    let fields = ref [] in
    if peek s <> Token.RParen then begin
      (* A bare identifier puns: the field takes the value of the name it
         already has. Safe here because the first field carried an `=`, so
         this cannot be the base of an update. *)
      let parse_field () =
        let fname = expect_field_name ~puns:true s in
        if peek s = Token.Eq then begin
          ignore (advance s);
          fields := !fields @ [(Some fname, expr_ 0 s)]
        end else fields := !fields @ [(Some fname, (Var fname : expr))]
      in
      parse_field ();
      while peek s = Token.Comma do ignore (advance s); parse_field () done
    end;
    expect s Token.RParen;
    ConstrApp (name, !fields, s.net_allow)
  end else if takes_a_bracket && peek_bare_args s then begin
    ignore (advance s); (* consume LParen *)
    let ids = ref [expect_ident s] in
    while peek s = Token.Comma do
      ignore (advance s); ids := !ids @ [expect_ident s]
    done;
    expect s Token.RParen;
    ConstrBare (name, !ids)
  end else if takes_a_bracket then begin
    ignore (advance s); (* consume LParen *)
    (* `M()` is a construction naming no fields where `M` has fields, all of
       which must then have defaults, and a constructor applied to unit
       where it does not. The declaration decides, as it does for a list of
       bare names. *)
    if peek s = Token.RParen then (ignore (advance s); ConstrBare (name, []))
    else if peek s = Token.Let then begin
      (* `Ctor (let x = 1; f x)` -- a block, as it is after any other `(`.
         Reading it as an expression first would hand the binding to `let_`
         with no way back to the statements after it, the same reason the
         plain bracket asks this first. *)
      let e = paren_seq s in
      expect s Token.RParen;
      App (constr, e)
    end
    else begin
      let arg_loc = peek_loc s in
      let first = expr_ 0 s in
      if peek s = Token.Semicolon then begin
        (* `Ctor (e1; e2)` -- the constructor applied to a block, which is
           what the bracket means everywhere else. Only `,` makes this
           bracket a field list; a `;` never can, since a construction names
           its fields one per comma. Without this the form was a parse
           error, and `wand f` reached it by writing `(I)(a; b)` back as
           `I (a; b)`. Found by test/fuzz. *)
        ignore (advance s);
        let first = Located (span_to_here s arg_loc, first) in
        let e = if peek s = Token.RParen then first else Seq (first, paren_seq s) in
        expect s Token.RParen;
        App (constr, e)
      end
      else if peek_field_after_comma s then begin
        (* `T(r, b = 3)`: everything not named comes from `r`. *)
        let fields = ref [] in
        while peek s = Token.Comma do
          ignore (advance s);
          let fname = expect_field_name s in
          expect s Token.Eq;
          fields := !fields @ [(fname, expr_ 0 s)]
        done;
        expect s Token.RParen;
        ConstrUpdate (name, first, !fields, s.net_allow)
      end else begin
      let args = ref [first] in
      while peek s = Token.Comma do
        ignore (advance s); args := !args @ [expr_ 0 s]
      done;
      expect s Token.RParen;
      (* `Ctor (a, b)` is the constructor applied to one tuple. Several
         arguments are written by juxtaposition, `Ctor a b`, as everywhere
         else in the language.

         This used to depend on the constructor's declared arity, which the
         parser only knew for types declared in the same file -- so the
         same expression meant different things depending on where its type
         lived, and `Some (a, b)` was read as two arguments in every file
         but Option's own. *)
      match !args with
      | (_ :: _ :: _ as es) -> App (constr, Tuple es)
      | args ->
        List.fold_left (fun acc arg -> App (acc, arg)) constr args
      end
    end
  end else
    constr

and postfix_field_ s e =
  let e = ref e in
  while peek s = Token.Dot do
    ignore (advance s);
    (* An uppercase member is a constructor reached through the module's
       name, not a field of a value. *)
    (match peek s with
     | Token.Upper _ ->
       let m =
         match strip_located !e with
         | Var m -> m
         | _ -> fail_at (peek_loc s)
             "a constructor is reached through a module's name"
       in
       e := Qualified (m, constr_atom_ s)
     | _ -> e := Field (!e, expect_field_name s))
  done;
  !e

and atom_ s = postfix_field_ s (atom_base_ s)

and list_ s =
  (* [ already consumed *)
  if peek s = Token.RBracket then (ignore (advance s); List [])
  else begin
    (* `[k = v]` was a map until 0.17; brackets are lists alone now. Same
       refusal as the pattern side, for the same reason. *)
    let is_map =
      match peek s with
      | Token.Ident _ | Token.String _ ->
        let saved = mark s in
        ignore (advance s);
        let result = peek s = Token.Eq in
        rewind s saved;
        result
      | _ -> false
    in
    if is_map then
      fail_at (snd s.tokens.(s.pos - 1))
        "a map is written in braces -- {k = v}, not [k = v]"
    else begin
      (* Located for the same reason a match arm's body is: a comment
         between two elements needs the boundary between them. *)
      let elems = ref [locate s (fun () -> expr_ 0 s)] in
      while peek s = Token.Comma do
        ignore (advance s); elems := !elems @ [locate s (fun () -> expr_ 0 s)]
      done;
      expect s Token.RBracket;
      List !elems
    end
  end

(* `{x = 1, y = 2}` -- a map literal. `{}` is the empty map, sugar for
   `Map.empty`. Keys are identifiers or quoted strings. *)
and brace_map_ s =
  (* { already consumed *)
  if peek s = Token.RBrace then (ignore (advance s); MapLit [])
  else begin
    (* `{r with b = 3}` is the record update of several other languages, and
       it is the first thing anyone writes here. Braces are a map, and an
       update names its type. The error this used to give was "expected =,
       got with", so the whole form is written out, with the reader's own
       names in it.

       The type is the one thing the braces do not carry, so it stands as
       `T`. The values are in the reader's own line already, and finding
       where each one ends means parsing it, so they stand as `...`. *)
    let update_drift base =
      let arr = s.tokens in
      let n = Array.length arr in
      let names = ref [] in
      let depth = ref 1 in
      let i = ref s.pos in
      while !depth > 0 && !i < n do
        (match fst arr.(!i) with
         | Token.LBrace -> incr depth
         | Token.RBrace -> decr depth
         | Token.Ident f when !depth = 1 ->
           let j = ref (!i + 1) in
           while !j < n && is_skippable (fst arr.(!j)) do incr j done;
           if !j < n && fst arr.(!j) = Token.Eq then names := !names @ [f]
         | _ -> ());
        incr i
      done;
      let fields = match !names with
        | [] -> "field = ..."
        | fs -> String.concat ", " (List.map (fun f -> f ^ " = ...") fs)
      in
      fail_at (peek_loc s) (Printf.sprintf
        "a record update names its type: `T(%s, %s)`. Braces are a map"
        base fields)
    in
    let parse_entry () =
      let key = match advance s with
        | Token.Ident k  ->
          if peek s = Token.With then update_drift k else k
        | Token.String k -> k
        | t -> fail (Format.asprintf "expected map key, got %a" Token.pp t)
      in
      expect s Token.Eq;
      (key, expr_ 0 s)
    in
    let entries = ref [parse_entry ()] in
    while peek s = Token.Comma do
      ignore (advance s); entries := !entries @ [parse_entry ()]
    done;
    expect s Token.RBrace;
    MapLit !entries
  end

(* The statements a binding takes as its body when a newline joined them to
   it, outside any block. `s.stmt_col` is that binding's own `let`, and a
   line starting left of it belongs to whatever encloses the binding, not to
   this body. Without the guard the loop ran to the end of the file: a
   definition whose body was written this way took every definition below it
   inside itself, `wand f` wrote that reading back, and a `main! ()` moved
   into a function nobody calls is a script that prints nothing and exits
   0. *)
and parse_body s =
  let e = ref (locate s (fun () -> expr_ 0 s)) in
  while is_expr_start (peek s) && peek_col s >= s.stmt_col do
    e := Seq (!e, locate s (fun () -> expr_ 0 s))
  done;
  !e

(* Parses `params (: T)? = body`, with optional multi-equation clauses
   repeating `name`, for a binding already-consumed `name`. Returns the
   final (params, body) pair — used both for the first binding in a
   `let ... and ...` group and each subsequent `and name ...` clause.
   Requires at least one parameter: plain-value mutual recursion isn't
   supported in a strict language (self-reference only works through a
   function's laziness), so `and`-bound members must be functions. *)
and parse_fn_binding s name =
  (* Every body read below is a clause of `name`, so a line that begins
     another equation for it ends the body it is under. Restored on the way
     out: a nested binding inside one of these bodies sets its own. *)
  let outer_clause = s.clause_name in
  s.clause_name <- Some name;
  Fun.protect ~finally:(fun () -> s.clause_name <- outer_clause) @@ fun () ->
  let loc = peek_loc s in
  let params = ref [] in
  while is_pat_atom_start (peek s) do
    params := !params @ [pat_atom_ s]
  done;
  if !params = [] then
    fail_at loc (Printf.sprintf
      "'%s' in a mutually-recursive 'and' group must take at least one \
       parameter — plain recursive values aren't supported" name);
  let annot = if peek s = Token.Colon then
    (ignore (advance s); Some (parse_type_expr s))
  else None in
  expect s Token.Eq;
  let body_loc = peek_loc s in
  let body = locate s (fun () -> parse_contract_body s) in
  let body = match annot with
    | Some te -> Located (span_to_here s body_loc, Annot (te, body))
    | None -> body
  in
  let arity = List.length !params in
  let eqs = ref [(loc, !params, body)] in
  let more = ref true in
  while !more do
    let saved = mark s in
    (try
      (* A continuation clause may optionally repeat `let` (matching the
         top-level `let f 0 = .. / let f n = ..` syntax) or omit it (the
         original local shorthand) -- both are accepted. *)
      (match peek s with Token.Let -> ignore (advance s) | _ -> ());
      (match peek s with
       | Token.Ident n when n = name -> ignore (advance s)
       | _ -> raise Exit);
      let eq_loc = peek_loc s in
      let ps = ref [] in
      while is_pat_atom_start (peek s) do ps := !ps @ [pat_atom_ s] done;
      if List.length !ps <> arity then raise Exit;
      (match peek s with Token.Eq -> ignore (advance s) | _ -> raise Exit);
      let b = locate s (fun () -> parse_contract_body s) in
      eqs := !eqs @ [(eq_loc, !ps, b)]
    with Exit -> rewind s saved; more := false)
  done;
  collapse_multi_equation arity !eqs

(* The statements of a `( ... )` block, up to the closing paren, which the
   caller takes. A `let` here binds for the rest of the block: `;` is what
   ends its right-hand side, exactly as a newline does at the top level of
   a file. So a block and a file read the same way.

   A newline ends a statement here too, so `;` is optional between two of
   them. It used to be required, and only for statements: a binding already
   ended at a newline, which made `(let a = 1` and `a` below it legal while
   `(1 + 1` and `2 + 2` below it was a parse error. One rule for the
   bracket, and the separator becomes the formatter's business rather than
   the author's -- `wand f` writes the `;`. *)
and paren_seq s =
  let loc = peek_loc s in
  let e =
    if peek s = Token.Let then begin
      ignore (advance s);
      (* A binding written with `in` names a value for one expression and
         is a statement like any other; one written with `;` has already
         taken the rest of the block as its body. *)
      let_ ~block:true s
    end else Located (span_to_here s loc, expr_ 0 s)
  in
  if peek s = Token.Semicolon then begin
    ignore (advance s);
    if peek s = Token.RParen then e else Seq (e, paren_seq s)
  end
  else if is_expr_start (peek s) then Seq (e, paren_seq s)
  else e

and let_ ?(block = false) s =
  (* let already consumed *)
  (* The `let` keyword's own column anchors this binding. Its value runs on
     to the next line only where that line is indented past the keyword, so
     a newline ends a binding inside a block exactly as it does at the top
     level -- `let a = 1` and then `a`, with no `;` and no `in`. The keyword
     rather than the name: at the top level `let y = f` continues onto an
     indented `1` below it, and the name's column would refuse that.

     Restored on the way out, so the expression this binding sits inside
     goes on measuring against its own anchor. *)
  let outer_col = s.stmt_col and outer_depth = s.stmt_depth in
  s.stmt_col <- prev_col s;
  s.stmt_depth <- s.paren_depth;
  Fun.protect
    ~finally:(fun () -> s.stmt_col <- outer_col; s.stmt_depth <- outer_depth)
  @@ fun () ->
  let p = pat_ s in
  (* The body, and what joined it to the binding: `in`, the `;` of a block,
     or the newline that ended the right-hand side. All three bind the name
     over everything that follows, so what is recorded is not which was
     written but which `wand f` writes -- the `;` where the binding is a
     block's, `in` where it names a value for one expression. *)
  (* Which spelling the binding comes back as, and the only thing that
     decides it is meaning.

     `LetIn` narrows: a `;` waiting after the body says the name belongs to
     that body and to nothing below it. Everything else binds for the rest
     of what encloses it, which is `LetBlock`, whether brackets were written
     round it or not.

     Brackets are deliberately not part of this. They are the formatter's to
     put in and take out -- it drops the ones a construct no longer needs --
     so a style read off them disagrees with itself on the next pass. Read
     the same node either way and the formatter is free to choose. *)
  let style_for ~narrows = if narrows then Ast.LetIn else Ast.LetBlock in
  let consume_rest () =
    if peek s = Token.In then begin
      ignore (advance s);
      let body = locate s (fun () -> expr_ 0 s) in
      (* A `;` narrows only inside a block, where it is the block's own
         separator and the statements after it are the ones the name must
         not reach. At the top level a `;` ends the item, and the formatter
         does not write it back -- so reading it as a narrowing one made the
         binding a `let ... in` whose reprint parsed as something else.
         Found by test/fuzz. *)
      (body, style_for ~narrows:(block && peek s = Token.Semicolon))
    end
    else if block && peek s = Token.Semicolon then begin
      (* The binding's body is everything after the `;`. *)
      ignore (advance s);
      if peek s = Token.RParen then
        fail_at (peek_loc s)
          "this binding has no body: a block cannot end with a `let`, \
           because nothing would read the name"
      else (paren_seq s, Ast.LetBlock)
    end
    else if block && peek s = Token.RParen then
      fail_at (peek_loc s)
        "this binding has no body: a block cannot end with a `let`, \
         because nothing would read the name"
    (* Outside a block, a `;` ends the binding's value exactly as a newline
       does, and what follows is the body. The two spellings were one rule
       everywhere but here: a file is a block and takes the `;`, a `( ... )`
       is a block and takes it, and a bare body took only the newline -- so
       the `;` fell through to the file, ended the whole definition, and the
       indented lines below it became items of their own.

       Indentation decides, the way it decides for a newline: what follows
       the `;` is the body when it sits at or past the binding's column, and
       ends the statement when it falls back inside it. A file's own
       `let a = 1; let b = 2` is untouched -- a top-level binding is read by
       the loop below rather than here, so its `;` never reaches this.

       Sequencing two bare statements is unaffected and still wants
       parentheses: a newline does not join those either, and this is the
       newline's rule, not a new one. *)
    else if (not block) && peek s = Token.Semicolon
            && s.paren_depth = s.stmt_depth
            && semicolon_joins_the_body s s.stmt_col then begin
      ignore (advance s);
      let body = parse_body s in
      (body, style_for ~narrows:false)
    end
    else if is_expr_start (peek s) then begin
      (* Neither `in` nor `;`: the newline ended the right-hand side, and
         what follows is the body. That is a third way to write a binding,
         and it is spelt back as one of the other two. Inside a block it is
         the block's binding and comes back with the `;`. Outside one, a
         body of several statements is a block whether or not it was written
         with parentheses, and comes back with them; a body of one
         expression is what `in` is for. *)
      if block then (paren_seq s, Ast.LetBlock)
      else
        let body = parse_body s in
        (body, style_for ~narrows:false)
    end
    (* A binding with nothing after it. The body is written out as `()`, and
       that is a body like any other: spelt the way the position spells one,
       or the next pass reads back a style other than the one just
       printed. *)
    else (Unit, style_for ~narrows:false)
  in
  match p with
  | PVar "rec" when is_pat_atom_start (peek s) ->
    fail_at (peek_loc s)
      "a let is already recursive -- drop the 'rec' (mutual \
       recursion is 'let f ... and g ...')"
  | PVar name when peek s <> Token.Eq && is_pat_atom_start (peek s) ->
    (* function shorthand: let f params = body, with optional multi-equation
       and optional mutually-recursive `and` group *)
    let (params, body) = parse_fn_binding s name in
    if peek s = Token.And then begin
      let bindings = ref [(name, params, body)] in
      while peek s = Token.And do
        ignore (advance s);
        let and_loc = peek_loc s in
        let name2 = match advance s with
          | Token.Ident n -> n
          | t -> fail_at and_loc (Format.asprintf
              "expected identifier after 'and', got %a" Token.pp t)
        in
        let (params2, body2) = parse_fn_binding s name2 in
        bindings := !bindings @ [(name2, params2, body2)]
      done;
      let (rest, style) = consume_rest () in
      LetRec (!bindings, rest, style)
    end else begin
      let (rest, style) = consume_rest () in
      Let (PVar name, Fn (params, body), rest, style)
    end
  | PVar name when peek s <> Token.Eq ->
    (* annotated value binding: let x : T = e *)
    let annot = if peek s = Token.Colon then
      (ignore (advance s); Some (parse_type_expr s))
    else None in
    expect s Token.Eq;
    let body = locate s (fun () -> parse_contract_body s) in
    let e = match annot with Some te -> Annot (te, body) | None -> body in
    let (rest, style) = consume_rest () in
    Let (PVar name, e, rest, style)
  | _ ->
    expect s Token.Eq;
    let e1 = locate s (fun () -> expr_ 0 s) in
    let (e2, style) = consume_rest () in
    Let (p, e1, e2, style)

and if_ s =
  (* if already consumed *)
  let cond  = locate s (fun () -> expr_ 0 s) in
  expect s Token.Then;
  let then_ = locate s (fun () -> expr_ 0 s) in
  (* A one-armed `if` is `else ()`. Scripting is full of conditionals that
     do something or nothing -- reporting a count only when there is one --
     and writing the empty branch out adds a line that says nothing. The
     branch still has to be `Unit`, because the two arms of an `if` are one
     expression and the missing one can only be `()`. *)
  if peek s <> Token.Else then If (cond, then_, Unit)
  else begin
    ignore (advance s);
    let else_ = locate s (fun () -> expr_ 0 s) in
    If (cond, then_, else_)
  end

and match_ s =
  (* match already consumed *)
  s.with_owners <- s.with_owners + 1;
  (* Located so the formatter knows where the scrutinee ends: a comment
     above the first arm sits between that point and the arm, and without
     the bound there is nothing to tell it from a comment above the whole
     `match`. *)
  let scrutinee = locate s (fun () -> expr_ 0 s) in
  expect s Token.With;
  s.with_owners <- s.with_owners - 1;
  let arms_loc = peek_loc s in
  let cases = ref [] in
  let continue_ = ref true in
  while !continue_ do
    if peek s = Token.Pipe then begin
      ignore (advance s);
      let p = pat_ s in
      let guard =
        if peek s = Token.When then begin
          ignore (advance s);
          Some (locate s (fun () -> expr_ 0 s))
        end else None
      in
      expect s Token.Arrow;
      let body = locate s (fun () -> expr_ 0 s) in
      cases := !cases @ [(p, guard, body)]
    end else
      continue_ := false
  done;
  (* A match with no arms has no value for any input, so nothing it could
     mean is worth inferring. Left alone it typechecked as 'a -> 'b and
     bound a name whose body had silently gone missing. *)
  if !cases = [] then
    fail_at arms_loc
      "match has no cases; each begins with '|', as in `| Some x -> x`";
  Match (scrutinee, !cases)

and contract_expr_ ~col s =
  let outer = s.stmt_col and outer_depth = s.stmt_depth in
  s.stmt_col <- col;
  s.stmt_depth <- s.paren_depth;
  Fun.protect
    ~finally:(fun () -> s.stmt_col <- outer; s.stmt_depth <- outer_depth)
    (fun () -> expr_ 0 s)

and parse_contract_body s =
  let reqs = ref [] in
  let ens  = ref [] in
  let continue_ = ref true in
  while !continue_ do
    match peek s with
    | Token.Requires ->
      let col = peek_col s in
      ignore (advance s);
      reqs := !reqs @ [locate s (fun () -> contract_expr_ ~col s)]
    | Token.Ensures  ->
      let col = peek_col s in
      ignore (advance s);
      ens := !ens @ [locate s (fun () -> contract_expr_ ~col s)]
    | _ -> continue_ := false
  done;
  (* Statements, not one expression. A binding's body has sequenced them
     since the layout rule arrived -- `parse_body` loops -- and a
     definition's body did not, so the same two lines ran or did not
     depending on whether a binding happened to come first.

     Indented past the definition, not level with it: a definition anchors at
     its own column and the next one starts back there, so `>=` would make
     every definition swallow the file below it. A binding anchors at its
     `let` and its body sits level with that, which is why `parse_body` asks
     the other question. *)
  (* The body anchors at its own column, the way a binding's does. Measured
     against the definition's column instead, a line one indent in is past
     it and so continues the expression -- which is why two statements under
     a definition ran together as an application while the same two under a
     binding sequenced. The definition's own column is restored on the way
     out, so the item below it still starts something new. *)
  let outer = s.stmt_col in
  let outer_depth = s.stmt_depth in
  (* Only where the body begins a line of its own. Written on the `=` or
     `->` line it starts well right of the lines below it, and anchoring there
     would make its own continuation -- indented less than the first token,
     because the first token is out at the end of the head -- read as a new
     statement. `fn p -> Path.of_string` over an indented argument is the
     shape that says so. *)
  let anchored = has_newline_before_next s in
  if anchored then begin
    s.stmt_col <- peek_col s;
    s.stmt_depth <- s.paren_depth
  end;
  let body =
    Fun.protect
      ~finally:(fun () ->
        if anchored then (s.stmt_col <- outer; s.stmt_depth <- outer_depth))
      (fun () ->
         let e = ref (locate s (fun () -> expr_ 0 s)) in
         while anchored && is_expr_start (peek s) && peek_col s >= s.stmt_col do
           e := Ast.Seq (!e, locate s (fun () -> expr_ 0 s))
         done;
         !e)
  in
  if !reqs = [] && !ens = [] then body
  else Ast.Contract (!reqs, !ens, body)

and fn_ s =
  (* fn already consumed *)
  let params = ref [] in
  while is_pat_atom_start (peek s) do
    params := !params @ [pat_atom_ s]
  done;
  expect s Token.Arrow;
  Fn (!params, locate s (fun () -> parse_contract_body s))

(* ── Resource bracket ─────────────────────────────────────────────────────── *)

(* `with <resource> as <pat> -> <body>`. Unambiguous with `match ... with`,
   which only ever reaches its `with` after a scrutinee, never at the start
   of an expression. The resource expression stops at `as`, so it is parsed
   at precedence 0 and the keyword terminates it. *)
and parse_with_ s =
  (* with already consumed *)
  let resource = expr_ 0 s in
  expect s Token.As;
  let p = pat_ s in
  expect s Token.Arrow;
  let body = expr_ 0 s in
  Ast.With (resource, p, body)

(* ── Handle expression ────────────────────────────────────────────────────── *)

and parse_handle_ s =
  (* handle already consumed *)
  s.with_owners <- s.with_owners + 1;
  let body = expr_ 0 s in
  expect s Token.With;
  s.with_owners <- s.with_owners - 1;
  let cases = ref [] in
  let continue_ = ref true in
  while !continue_ do
    if peek s = Token.Pipe then begin
      ignore (advance s);
      let case = match peek s with
        | Token.Return ->
          ignore (advance s);
          let p = pat_atom_ s in
          expect s Token.Arrow;
          let b = locate s (fun () -> expr_ 0 s) in
          Ast.ReturnCase (p, b)
        (* `FS!read_file` reaches here as the Upper token "FS!" followed by an
           identifier, since `!` is a suffix character. Joining them gives the
           operation name, which is the public call with a `!` where its dot
           would be: you call FS.read_file, you intercept FS!read_file. *)
        | Token.Upper family
          when String.length family > 1
            && family.[String.length family - 1] = '!' ->
          ignore (advance s);
          let verb = expect_ident s in
          let op_name = family ^ verb in
          let arg_pat = pat_atom_ s in
          let cont_name = expect_cont_name s in
          expect s Token.Arrow;
          let b = locate s (fun () -> expr_ 0 s) in
          Ast.EffectCase (op_name, arg_pat, cont_name, b)
        | Token.Ident op_name ->
          ignore (advance s);
          let arg_pat = pat_atom_ s in
          let cont_name = expect_ident s in
          expect s Token.Arrow;
          let b = locate s (fun () -> expr_ 0 s) in
          Ast.EffectCase (op_name, arg_pat, cont_name, b)
        | t ->
          fail_at (peek_loc s) (Format.asprintf "unexpected token in handler case: %a"
            Token.pp t)
      in
      cases := !cases @ [case]
    end else
      continue_ := false
  done;
  Ast.Handle (body, !cases)

let build_multi_equation name arity eqs =
  let (p, b) = collapse_multi_equation arity eqs in
  Ast.TLLet (name, p, b)

(* Record a completed top-level function, rejecting a second definition of a
   name already defined. A function's equations are parsed as one contiguous
   group, so reaching this a second time means another `let` for the same
   name appeared after something else came between -- which used to either
   merge into the earlier definition or silently shadow it, depending on
   arity, with nothing at the definition site to say which. *)
let note_top_fn s loc name arity =
  match Hashtbl.find_opt s.top_fns name with
  | Some prev_arity ->
    fail_at loc (Printf.sprintf
      "'%s' is already defined above%s. Equations for a function must be \
       consecutive — move this one up beside the others, or rename it"
      name
      (if prev_arity = arity then ""
       else Printf.sprintf " (taking %d parameter%s)" prev_arity
              (if prev_arity = 1 then "" else "s")))
  | None -> Hashtbl.replace s.top_fns name arity

(* Parses `params (: T)? = body` plus any `let name ...` multi-equation
   continuations, for a NEW name introduced by `and` in a top-level
   mutually-recursive group. Requires at least one parameter — plain-value
   mutual recursion isn't supported in a strict language (see the local
   `parse_fn_binding` above for why). *)
let parse_top_fn_binding s name =
  let loc = peek_loc s in
  let params = ref [] in
  while is_pat_atom_start (peek s) do
    params := !params @ [pat_atom_ s]
  done;
  if !params = [] then
    fail_at loc (Printf.sprintf
      "'%s' in a mutually-recursive 'and' group must take at least one \
       parameter — plain recursive values aren't supported" name);
  let annot = if peek s = Token.Colon then
    (ignore (advance s); Some (parse_type_expr s))
  else None in
  expect s Token.Eq;
  let body_loc = peek_loc s in
  let body = locate s (fun () -> parse_contract_body s) in
  let body = match annot with
    | Some te -> Ast.Located (span_to_here s body_loc, Ast.Annot (te, body))
    | None -> body
  in
  let arity = List.length !params in
  let eqs = ref [(loc, !params, body)] in
  let more = ref true in
  while !more do
    let saved = mark s in
    (try
      (match peek s with Token.Let -> ignore (advance s) | _ -> raise Exit);
      (match peek s with
       | Token.Ident n when n = name -> ignore (advance s)
       | _ -> raise Exit);
      let eq_loc = peek_loc s in
      let ps = ref [] in
      while is_pat_atom_start (peek s) do ps := !ps @ [pat_atom_ s] done;
      if List.length !ps <> arity then raise Exit;
      (match peek s with Token.Eq -> ignore (advance s) | _ -> raise Exit);
      let b = locate s (fun () -> parse_contract_body s) in
      eqs := !eqs @ [(eq_loc, !ps, b)]
    with Exit -> rewind s saved; more := false)
  done;
  collapse_multi_equation arity !eqs

(* Documentation is a run of `--` lines directly above a definition, the way
   a reader already writes it. Each line stands alone -- a comment after code
   on the same line documents nothing -- the lines are consecutive, and the
   last one sits on the line above the definition. A blank line between the
   run and the definition separates them, which is how a file header stays a
   file header. *)
let doc_run_before s =
  let n = Array.length s.tokens in
  let j = ref s.pos in
  while !j < n && is_skippable (fst s.tokens.(!j)) do incr j done;
  if !j >= n then None
  else begin
    (* Standalone: nothing but the line's indentation before it, which the
       lexer reports as a `Newline` immediately behind it. *)
    let standalone i = i = 0 || (match fst s.tokens.(i - 1) with
      | Token.Newline -> true | _ -> false) in
    let line_of i = (snd s.tokens.(i)).Token.line in
    let want = ref (line_of !j) in
    let lines = ref [] in
    let i = ref (!j - 1) in
    let continue_ = ref true in
    while !continue_ && !i >= 0 do
      (match fst s.tokens.(!i) with
       | Token.Newline -> decr i
       | Token.LineComment text when line_of !i = !want - 1 && standalone !i ->
         lines := String.trim text :: !lines;
         want := line_of !i;
         decr i
       | _ -> continue_ := false)
    done;
    match !lines with [] -> None | ls -> Some (String.concat "\n" ls)
  end

(* ── Interfaces ───────────────────────────────────────────────────────────── *)

(* `Ord`, or `Foo.Ord` where the module declares more than one. A module
   declaring exactly one lends it its name, which is why the short spelling
   is the one anybody writes. *)
let parse_iface_name s =
  let loc = peek_loc s in
  match advance s with
  | Token.Upper n when peek s = Token.Dot ->
    ignore (advance s);
    (match advance s with
     | Token.Upper b -> n ^ "." ^ b
     | t -> fail_at loc (Format.asprintf
         "expected an interface name after '%s.', got %a" n Token.pp t))
  | Token.Ident m when peek s = Token.Dot
                    && (match peek2 s with Token.Upper _ -> true | _ -> false) ->
    (* A user module's namespace is its file name, which is lowercase. *)
    ignore (advance s);
    (match advance s with
     | Token.Upper b -> m ^ "." ^ b
     | t -> fail_at loc (Format.asprintf
         "expected an interface name after '%s.', got %a" m Token.pp t))
  | Token.Upper n -> n
  | t -> fail_at loc (Format.asprintf
      "expected an interface name, got %a" Token.pp t)

(* `interface Ord 'a(max: 'a -> 'a -> 'a, min: 'a -> 'a -> 'a)`. The shape a
   type declaration already has, and it means the same thing: a name, its
   parameters, and a list of named things with their types. *)
let parse_interface s =
  (* `interface` already consumed *)
  let loc = peek_loc s in
  let name = match advance s with
    | Token.Upper n -> n
    | t -> fail_at loc (Format.asprintf
        "expected an interface name, got %a" Token.pp t)
  in
  let params = ref [] in
  while (match peek s with Token.TypeVar _ -> true | _ -> false) do
    (match advance s with
     | Token.TypeVar n -> params := !params @ [n]
     | _ -> assert false)
  done;
  if peek s <> Token.LParen then
    fail_at (peek_loc s) (Format.asprintf
      "an interface lists its members in brackets, as in \
       'interface Ord 'a(max: 'a -> 'a -> 'a)'");
  ignore (advance s);
  let parse_member () =
    let mname = expect_field_name s in
    expect s Token.Colon;
    (* The same rule a named field follows: the comma or the closing bracket
       ends the member, so its type needs no brackets of its own. *)
    (mname, parse_type_expr s)
  in
  let members = ref [parse_member ()] in
  while peek s = Token.Comma do
    ignore (advance s);
    members := !members @ [parse_member ()]
  done;
  expect s Token.RParen;
  { Ast.if_name = name; if_params = !params; if_members = !members }

(* `implement Ord Int = let max a b = ...; let min a b = ...`.

   A `;` separates siblings here rather than nesting one binding inside the
   next: an implementation declares a run of the module's own bindings, and
   they are the module's, not each other's. *)
let parse_implement s =
  (* `implement` already consumed *)
  let iface = parse_iface_name s in
  let args = ref [] in
  while type_atom_ahead s && not (newline_breaks_expr s) do
    args := !args @ [parse_type_atom s]
  done;
  expect s Token.Eq;
  let parse_binding () =
    (* A run of comments above a member is its doc, exactly as it is above a
       top-level binding: an implementation declares the module's bindings,
       so they are documented where they are written. *)
    let doc = doc_run_before s in
    let loc = peek_loc s in
    (match peek s with
     | Token.Let -> ignore (advance s)
     | t -> fail_at loc (Format.asprintf
         "an implementation is a run of 'let' bindings, so '%a' cannot open \
          one" Token.pp t));
    let nloc = peek_loc s in
    let name = match advance s with
      | Token.Ident n -> n
      | t -> fail_at nloc (Format.asprintf
          "expected a name after 'let', got %a" Token.pp t)
    in
    let params = ref [] in
    while is_pat_atom_start (peek s) do params := !params @ [pat_atom_ s] done;
    expect s Token.Eq;
    let body = locate s (fun () -> parse_contract_body s) in
    ((name, !params, body, loc), Option.map (fun d -> (name, d)) doc)
  in
  let binds = ref [parse_binding ()] in
  while peek s = Token.Semicolon do
    ignore (advance s);
    binds := !binds @ [parse_binding ()]
  done;
  ({ Ast.im_iface = iface; im_args = !args;
     im_binds = List.map fst !binds },
   List.filter_map snd !binds)

(* ── Public API ───────────────────────────────────────────────────────────── *)

let parse_expr tokens =
  let s = make tokens in
  expr_ 0 s

let parse_type_def s =
  (* type already consumed *)
  let type_loc = peek_loc s in
  let type_name =
    let loc = peek_loc s in
    match advance s with
    | Token.Upper n -> n
    | t -> fail_at loc (Format.asprintf "expected type name, got %a" Token.pp t)
  in
  let params = ref [] in
  while (match peek s with Token.TypeVar _ -> true | _ -> false) do
    (match advance s with
     | Token.TypeVar n -> params := !params @ [n]
     | _ -> assert false)
  done;
  let parse_ctor_fields () =
    (* Peek: Upper -> space-separated positional type atoms, no parens.
       LParen + Ident -> named-field record shorthand: (ident : Type, ...).
       LParen + other -> single field needing grouping/tupling: (List Int),
         (Int, Int). *)
    match peek s with
    (* The newline check belongs on the first payload as much as the rest: a
       constructor without one would otherwise take the next line's type name
       as its payload, so `type Color = Red | Green` followed by a line
       starting with an uppercase name silently became `Green <that>`.

       It belongs on the bracketed payload for the same reason. Without it,
       `type S = C` followed by a line opening with `(` took that line as
       C's field list -- and `wand f`, which writes a leading unary minus
       bracketed, turned the two items `type S = C` and `-1` into source the
       parser then refused. Found by test/fuzz. *)
    | (Token.Upper _ | Token.TypeVar _ | Token.Ident _)
      when type_atom_ahead s && not (newline_breaks_expr s) ->
      let fields = ref [(None, parse_type_atom s)] in
      while type_atom_ahead s && not (newline_breaks_expr s) do
        fields := !fields @ [(None, parse_type_atom s)]
      done;
      (* A default belongs to a field a construction can leave out, and a
         positional payload has no name to leave out. *)
      if peek s = Token.Eq then
        fail_at (peek_loc s)
          "only a named field takes a default: give the field a name, as in \
           'A(n: Int = 3)'";
      (!fields, [])
    | Token.LParen when not (newline_breaks_expr s) ->
      let saved = mark s in
      ignore (advance s);
      (match peek s with
       | t when is_field_name t && peek2 s = Token.Colon ->
         let parse_named () =
           let fname = expect_field_name s in
           expect s Token.Colon;
           (* An applied type -- `List String`, `Option Node` -- and a
              function type -- `max: 'a -> 'a -> 'a` -- both read as one
              field type. The comma and the closing parenthesis are not type
              atoms, so the type stops where the field does. Positional
              fields stay atoms: `Pair Int Int` is two of them, not one
              applied to the other, and nothing ends one but the next. *)
           let ftype = parse_type_expr s in
           (* `= value` gives the field a default, which is what lets a
              construction leave it out. *)
           let dflt =
             if peek s = Token.Eq then begin
               ignore (advance s);
               Some (locate s (fun () -> expr_ 0 s))
             end else None
           in
           ((Some fname, ftype), Option.map (fun d -> (fname, d)) dflt)
         in
         let first = parse_named () in
         let rest = ref [] in
         while peek s = Token.Comma do
           ignore (advance s);
           rest := !rest @ [parse_named ()]
         done;
         expect s Token.RParen;
         let named = first :: !rest in
         (List.map fst named, List.filter_map snd named)
       | _ ->
         rewind s saved;
         ([(None, parse_type_atom s)], []))
    | _ -> ([], [])
  in
  (* Single-constructor shorthand: type Foo (fields...) desugars to type Foo = Foo (fields...) *)
  (* Same newline rule as the payload below: a `(` back at the declaration's
     own column opens the next item, not this one's field list. A field list
     indented past the `type` is still read as one. *)
  if peek s = Token.LParen && not (newline_breaks_expr s) then begin
    let (fields, defaults) = parse_ctor_fields () in
    Ast.Variants (type_name, !params,
      [{ Ast.name = type_name; loc = Some type_loc; fields; defaults }])
  end else begin
    expect s Token.Eq;
    (* After `=`, a shape that cannot be a constructor is a type expression,
       and the declaration is an alias: a tuple opens with `(`, a variable
       with `'a`, and a function type has an arrow in it. A leading
       constructor name is ambiguous -- `type Colour = Red` is a variant and
       `type Point = Pair` an alias -- and stays a `Variants` here, because
       whether `Red` names a type is not known until every declaration has
       been read. The typechecker settles those. *)
    let alias_ahead () =
      match peek s with
      | Token.LParen | Token.TypeVar _ -> true
      | Token.Upper _ ->
        (* `List Int -> Int` is an alias; `Circle Int` is a constructor. Only
           an arrow at this depth tells them apart, and the scan stops at the
           newline that ends the declaration. *)
        let i = ref s.pos and depth = ref 0 and found = ref false
        and go = ref true in
        while !go && !i < Array.length s.tokens do
          (match fst s.tokens.(!i) with
           | Token.LParen | Token.LBracket -> incr depth
           | Token.RParen | Token.RBracket -> decr depth
           | Token.Arrow when !depth = 0 -> found := true; go := false
           | Token.Newline | Token.EOF -> go := false
           | _ -> ());
          incr i
        done;
        !found
      | _ -> false
    in
    if alias_ahead () then
      Ast.Alias (type_name, !params, parse_type_expr s)
    else begin
    let parse_ctor () =
      let ctor_loc = peek_loc s in
      let name =
        let loc = ctor_loc in
        match advance s with
        | Token.Upper n -> n
        | t -> fail_at loc (Format.asprintf "expected constructor name, got %a" Token.pp t)
      in
      (* `of` is an ordinary word, so the OCaml spelling is caught where it
         is written rather than by reserving it everywhere. *)
      (if peek s = Token.Ident "of" then
         fail_at (peek_loc s)
           "a constructor takes its payload directly: 'Circle Int', not \
            'Circle of Int'");
      let (fields, defaults) = parse_ctor_fields () in
      { Ast.name; loc = Some ctor_loc; fields; defaults }
    in
    let ctors = ref [parse_ctor ()] in
    while peek s = Token.Pipe do
      ignore (advance s);
      ctors := !ctors @ [parse_ctor ()]
    done;
    Ast.Variants (type_name, !params, !ctors)
    end
  end

(* Shared by `parse_program` and `parse_program_with_locs`: every loop
   iteration appends at most one item to `items` (via `items := !items @
   [x]`), so a physical-inequality check on the list before/after an
   iteration cheaply detects "an item was added" without re-walking the
   list. `on_item` is invoked with (start loc, loc of the item's last
   consumed token) whenever that happens; `parse_program` passes a no-op,
   `parse_program_with_locs` records into a side list. *)
(* `uses {Shell, FS.Write}` -- the file's declared bound. It is the first
   item on purpose: a reviewer who has to search for it gains nothing over
   having no manifest at all, so its position is part of the syntax rather
   than a convention. Comments and a shebang may precede it. *)
let parse_manifest s =
  let loc = peek_loc s in
  ignore (advance s);              (* uses *)
  expect s Token.LBrace;
  let labels = ref [] in
  (* `Shell(git, curl)` -- the binaries the file may invoke -- and any other
     narrowed label's word list, which is read the same way. `Narrow` says
     which labels take one and what their words name; the reading of a word
     is the same either way. Entries are bare when they lex as one word, one
     path or one glob; anything else is quoted. *)
  let read_words label =
    ignore (advance s);            (* ( *)
    if peek s = Token.RParen then
      fail_at (peek_loc s) (Printf.sprintf
        "%s() admits nothing -- a file that reaches nothing drops the \
         label instead" label);
    let args = ref [] in
    let add word =
      (* A pattern that admits everything is the bare label written at
         greater length, and two spellings of one claim is one more than a
         reader should have to hold. *)
      if Narrow.admits_everything word then
        fail_at (peek_loc s) (Printf.sprintf
          "%s(*) admits everything, which is what bare %s already says -- \
           write %s" label label label);
      if List.mem word !args then
        fail_at (peek_loc s) (Printf.sprintf
          "'%s' is already in this %s(...) list" word label);
      args := !args @ [word]
    in
    (* One word, from however many tokens its spelling took. *)
    let add_joined w0 (floc : Token.loc) =
      let buf = Buffer.create 16 in
      Buffer.add_string buf w0;
      let end_ = ref (floc.Token.offset + String.length w0) in
      let rec join () =
        match Shell_scan.fragment (peek s) with
        | Some frag when (peek_loc s).Token.offset = !end_ ->
          ignore (advance s);
          Buffer.add_string buf frag;
          end_ := !end_ + String.length frag;
          join ()
        | _ -> ()
      in
      join ();
      add (Buffer.contents buf)
    in
    let read_arg () =
      let (first, floc) = advance_loc s in
      match first with
      | Token.String w -> add w
      (* `*` alone, and `*.example.com`, which the lexer reads as one glob
         token because a `*` with more after it is a glob everywhere else in
         the language. The word is its text; nothing here is a `Glob`
         value. *)
      | Token.Star -> add_joined "*" floc
      (* A host may be an address, which lexes as itself. *)
      | Token.IPv4 w0 | Token.Ident w0 | Token.Path w0 | Token.Glob w0 ->
        (* `docker-compose` arrives as `docker`, `-`, `compose`, and
           `demos/probe.sh` as an ident then a path -- inside $() a name
           is raw text, and the manifest should read the same spelling.
           Fragments are rejoined only when byte-adjacent, so a genuinely
           spaced `a - b` stays an error. *)
        add_joined w0 floc
      | t -> fail_at floc (Format.asprintf
          "expected a name in %s(...), got %a -- quote a name \
           wand cannot lex as one: %s(git, \"7zip\")"
          label Token.pp t label)
    in
    read_arg ();
    while peek s = Token.Comma do ignore (advance s); read_arg () done;
    expect s Token.RParen;
    !args
  in
  let read_label () =
    let rec parts acc =
      let part = match advance s with
        | Token.Upper u -> u
        | t -> fail_at (peek_loc s) (Format.asprintf
            "expected an effect name in the manifest, got %a"
            Token.pp t)
      in
      let acc = acc @ [part] in
      if peek s = Token.Dot then (ignore (advance s); parts acc) else acc
    in
    let name = String.concat "." (parts []) in
    let allow =
      if peek s <> Token.LParen then None
      else if Narrow.narrowable name then Some (read_words name)
      else
        fail_at (peek_loc s) (Printf.sprintf
          "%s takes no list in a manifest -- write bare %s" name name)
    in
    labels := !labels @ [(name, allow)]
  in
  if peek s <> Token.RBrace then begin
    read_label ();
    while peek s = Token.Comma do ignore (advance s); read_label () done
  end;
  expect s Token.RBrace;
  (* The manifest's loc spans `uses` through the closing brace -- the exact
     extent a ReplaceLine fix replaces. *)
  (!labels, span_to_here s loc)

let looks_like_manifest s =
  match peek s with
  | Token.Ident "uses" ->
    (* `uses` is an ordinary word elsewhere; only `uses {` starts a manifest,
       and `{` begins nothing else in the language. *)
    let saved = mark s in
    ignore (advance s);
    let is_manifest = peek s = Token.LBrace in
    rewind s saved;
    is_manifest
  | _ -> false



let parse_program_generic ~on_item tokens =
  let s = make tokens in
  let items = ref [] in
  let docs  = ref [] in
  let manifest = ref None in
  let pending_doc : string option ref = ref None in
  let attach_doc name =
    match !pending_doc with
    | None -> ()
    | Some d -> docs := (name, d) :: !docs; pending_doc := None
  in
  (* Before anything else: the manifest, if the file has one. *)
  if looks_like_manifest s then begin
    manifest := Some (parse_manifest s);
    let word_list label =
      match !manifest with
      | Some (labels, _) -> Option.join (List.assoc_opt label labels)
      | None -> None
    in
    s.shell_allow <- word_list "Shell";
    s.net_allow <- word_list "Net"
  end;
  (* Where the previous top-level item began, so an item that starts further
     in can be recognised for what it almost always is: a continuation the
     author expected to be joined to the line above. wand ends a definition
     at the end of a line, and juxtaposition cannot cross one -- an
     identifier is a perfectly good start to a new definition, so there is
     nothing to tell the two apart but the indentation. Left alone, this
     parses as separate items and surfaces much later as an unbound name
     that is plainly in scope, which is a bad way to learn a layout rule. *)
  let previous_item = ref None in
  (* Set when a `;` ended the item above. Definitions separated by `;` on one
     line are ordinary, so the flag alone means nothing -- it is the `;`
     together with a line indented under the definition it ended that says
     the author meant a sequence and got two statements. *)
  let after_semi = ref false in
  let continue_ = ref true in
  while !continue_ do
    (match doc_run_before s with
     | Some d -> pending_doc := Some d
     | None -> ());
    let start_loc = peek_loc s in
    (* This item's own column is what decides, for everything inside it,
       whether a line below continues it or starts something new. *)
    s.stmt_col <- start_loc.Token.col;
    s.stmt_depth <- 0;
    (* A line indented past the definition above it is read as the
       continuation it looks like -- see `newline_breaks_expr` -- so it
       normally never reaches this loop at all. A `;` is the one thing that
       ends the definition first, and then the indented lines below it do
       arrive here, as statements of their own. That parses, and it runs:

         let go () =
           IO.println "one";
           IO.println "two"

       prints `two` and then `one`, because the second line became a
       top-level statement and top-level statements run in file order. It
       used to do that silently, and the `!`-naming lint would then report
       that `go` cannot raise -- true of what was parsed, and the opposite
       of what was written. *)
    (match peek s, !previous_item with
     | (Token.Newline | Token.Semicolon | Token.EOF), _ -> ()
     | _, Some (prev_col, prev_line) when !after_semi ->
       after_semi := false;
       if start_loc.Token.col > prev_col && start_loc.Token.line > prev_line then
         fail_at start_loc
           "the ';' above ended the definition, so this line is a statement \
            of its own rather than part of it -- put the body in parentheses \
            to sequence it"
     | _ -> after_semi := false);
    let before_items = !items in
    (match peek s with
    | Token.EOF -> continue_ := false
    | _ when looks_like_manifest s ->
      let (_, loc) = parse_manifest s in
      fail_at loc (Printf.sprintf
        "the manifest must be the first thing in the file, before \
         everything but a shebang and comments%s"
        (if !manifest = None then "" else " (this file already has one)"))
    | Token.Newline ->
      ignore (advance s)
    | Token.Semicolon ->
      after_semi := true;
      ignore (advance s)
    | Token.Let ->
      let saved = mark s in
      ignore (advance s);
      (match peek s with
       (* `_` is a binder like any other here. Left out, it fell through to
          the expression parser, where `let _ = e` takes everything below it
          as its body -- so a file's remaining statements became the tail of
          one `let ... in` rather than items of their own. `wand f` then
          wrote them back as a single parenthesized block, and the rule that
          reports a `let _ =` binding Unit never saw one, because it looks at
          a file's items. A named binder was never affected: only `_` missed
          this branch. *)
       | Token.LBracket | Token.LParen | Token.LBrace | Token.Underscore ->
         (* Top-level pattern destructuring: let <pat> = <expr> *)
         let p = pat_ s in
         expect s Token.Eq;
         let body = locate s (fun () -> parse_contract_body s) in
         if peek s = Token.In then begin
           (* Actually a let-in expression — backtrack and parse as TLExpr *)
           rewind s saved;
           let e = locate s (fun () -> expr_ 0 s) in
           items := !items @ [Ast.TLExpr e]
         end else
           items := !items @ [Ast.TLLetPat (p, body)]
       | Token.Ident _ | Token.Upper _ ->
         let name_loc = peek_loc s in
         let name = match advance s with
           | Token.Ident n -> n
           | Token.Upper n -> n
           | _ -> assert false
         in
         if name = "rec" && is_pat_atom_start (peek s) then
           fail_at name_loc
             "a let is already recursive -- drop the 'rec' (mutual \
              recursion is 'let f ... and g ...')";
         let params_loc = peek_loc s in
         let params = ref [] in
         while is_pat_atom_start (peek s) do
           params := !params @ [pat_atom_ s]
         done;
         let annot = if peek s = Token.Colon then
           (ignore (advance s); Some (parse_type_expr s))
         else None in
         expect s Token.Eq;
         let loc = peek_loc s in
         let body = locate s (fun () -> parse_contract_body s) in
         let arity = List.length !params in
         if peek s = Token.In then begin
           (* let x = e in e2 — expression, not top-level binding *)
           rewind s saved;
           let e = locate s (fun () -> expr_ 0 s) in
           items := !items @ [Ast.TLExpr e]
         end else begin
         attach_doc name;
         if arity = 0 then begin
           let e = match annot with
             | Some te -> Ast.Annot (te, body)
             | None -> body
           in
           items := !items @ [Ast.TLLet (name, [], e)]
         end else begin
           let body = match annot with
             | Some te -> Ast.Located (span_to_here s loc, Ast.Annot (te, body))
             | None -> body
           in
           let eqs = ref [(params_loc, !params, body)] in
           let more = ref true in
           while !more do
             let saved2 = mark s in
             (try
               (match peek s with Token.Let -> ignore (advance s) | _ -> raise Exit);
               (match peek s with
                | Token.Ident n when n = name -> ignore (advance s)
                | _ -> raise Exit);
               (* Past this point the name is confirmed, so anything wrong is
                  a mistake in this equation rather than the start of an
                  unrelated binding: report it instead of backtracking into
                  a silent shadow. *)
               let eq_loc = peek_loc s in
               let ps = ref [] in
               while is_pat_atom_start (peek s) do ps := !ps @ [pat_atom_ s] done;
               if List.length !ps <> arity then
                 fail_at eq_loc (Printf.sprintf
                   "equation %d of '%s' takes %d parameter%s, but its first \
                    equation takes %d — every equation of a function must \
                    take the same number"
                   (List.length !eqs + 1) name
                   (List.length !ps) (if List.length !ps = 1 then "" else "s")
                   arity);
               (match peek s with Token.Eq -> ignore (advance s) | _ -> raise Exit);
               let b = locate s (fun () -> parse_contract_body s) in
               eqs := !eqs @ [(eq_loc, !ps, b)]
             with Exit -> rewind s saved2; more := false)
           done;
           if peek s = Token.And then begin
             let (first_params, first_body) = collapse_multi_equation arity !eqs in
             let bindings = ref [(name, first_params, first_body)] in
             while peek s = Token.And do
               ignore (advance s);
               let and_loc = peek_loc s in
               let name2 = match advance s with
                 | Token.Ident n -> n
                 | t -> fail_at and_loc (Format.asprintf
                     "expected identifier after 'and', got %a" Token.pp t)
               in
               let (params2, body2) = parse_top_fn_binding s name2 in
               bindings := !bindings @ [(name2, params2, body2)]
             done;
             if peek s = Token.In then begin
               (* let f = ... and g = ... in e2 — expression, not top-level *)
               rewind s saved;
               let e = locate s (fun () -> expr_ 0 s) in
               items := !items @ [Ast.TLExpr e]
             end else begin
               List.iter (fun (n, ps, _) ->
                 note_top_fn s name_loc n (List.length ps)) !bindings;
               items := !items @ [Ast.TLLetRec !bindings]
             end
           end else begin
             note_top_fn s name_loc name arity;
             items := !items @ [build_multi_equation name arity !eqs]
           end
         end
         end  (* close the outer begin from peek s = Token.In check *)
       | _ ->
         rewind s saved;
         let e = locate s (fun () -> expr_ 0 s) in
         items := !items @ [Ast.TLExpr e])
    | Token.Import ->
      ignore (advance s);
      let reach = match advance s with
        | Token.Upper name ->
          items := !items @ [Ast.TLImport (Ast.StdlibModule name)];
          Printf.sprintf "it binds '%s', so '%s.member' reaches into it" name name
        | Token.Path path  ->
          items := !items @ [Ast.TLImport (Ast.UserPath path)];
          Printf.sprintf
            "write 'let name = import %s' to bind it, then 'name.member'" path
        | t -> fail (Format.asprintf
            "expected module name or path after import, got %a" Token.pp t)
      in
      (* An import statement is the keyword and the name, and the line ends
         there. What followed on the same line used to become a second
         top-level item, so `import S(import S)` parsed as two imports where
         one application was written -- and the formatter, seeing two plain
         imports at the head of the file, sorted them. Asked of the raw next
         token, because the ordinary `peek` steps over the newline that ends
         the statement. Found by test/fuzz. *)
      let trailing =
        if s.pos < Array.length s.tokens then fst s.tokens.(s.pos) else Token.EOF
      in
      (match trailing with
       | Token.Newline | Token.LineComment _ | Token.Semicolon | Token.EOF -> ()
       | t ->
         fail_at (peek_loc s) (Format.asprintf
           "an import statement ends with the module name, so '%a' cannot \
            follow it here -- put this on a line of its own; %s"
           Token.pp t reach))
    | Token.Type ->
      ignore (advance s);
      (* The type's own name, which is what a declaration error is about. *)
      let tdef_loc = peek_loc s in
      let tdef = parse_type_def s in
      (match tdef with
       | Ast.Variants (name, _, _) | Ast.Alias (name, _, _) -> attach_doc name);
      items := !items @ [Ast.TLType (tdef, Some tdef_loc)]
    | Token.Interface ->
      ignore (advance s);
      let loc = peek_loc s in
      let idef = parse_interface s in
      attach_doc idef.Ast.if_name;
      items := !items @ [Ast.TLInterface (idef, Some loc)]
    | Token.Implement ->
      ignore (advance s);
      let loc = peek_loc s in
      let (idef, member_docs) = parse_implement s in
      List.iter (fun (n, d) -> docs := (n, d) :: !docs) member_docs;
      items := !items @ [Ast.TLImplement (idef, Some loc)]
    | _ ->
      let e = locate s (fun () -> expr_ 0 s) in
      items := !items @ [Ast.TLExpr e];
      if peek s = Token.Eq then begin
        let rec head_ident = function
          | Ast.Located (_, inner) -> head_ident inner
          | Ast.App (f, _) -> head_ident f
          | Ast.Var id -> Some id
          | _ -> None
        in
        let inner = match e with Ast.Located (_, i) -> i | i -> i in
        (match head_ident inner with
         | Some id ->
           let hint = keyword_hint (Token.Ident id) in
           if hint <> "" then
             fail_at (peek_loc s) (Format.asprintf "unexpected '='; did you mean 'let'?%s"
               hint)
         | None -> ())
      end);
    if !items != before_items then begin
      let last_loc = if s.pos > 0 then snd s.tokens.(s.pos - 1) else start_loc in
      previous_item := Some (start_loc.Token.col, last_loc.Token.line);
      on_item start_loc last_loc
    end
  done;
  { Ast.items = !items; docs = !docs; manifest = !manifest }

let parse_program tokens = parse_program_generic ~on_item:(fun _ _ -> ()) tokens

let parse_program_with_locs tokens =
  let locs = ref [] in
  let prog = parse_program_generic tokens
      ~on_item:(fun start_loc end_loc -> locs := !locs @ [(start_loc, end_loc)]) in
  (prog, !locs)
