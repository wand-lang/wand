(* How a `%{...}` or `%!{...}` in a command goes in -- which is decided by
   where it was written, so the lexer is the only thing that can see it.

   `Arg` is the bare `%{x}`: the value becomes exactly one argument,
   whatever it contains. `Inside q` is the same `%{x}` written between the
   author's own quotes, q being the quote it sits in: there the value is
   part of a word the author is already building, so it is escaped for that
   quote rather than wrapped in one of its own. `Source` is `%!{x}`, which
   is deliberately shell source and quoted for nothing.

   A string has no argument boundaries and so nothing to quote for; that is
   why `InterpStr` keeps plain pairs. *)
type hole =
  | Arg
  | Inside of char   (* the quote character it sits between *)
  | Source

(* Where an interpolation's body begins in the file it was written in. The
   body is lexed on its own, so without this its positions start again at
   1:1 and an error inside a `%{...}` points at the wrong place -- a line
   that may not exist, when the string sits far down a file. *)
type pos = { p_line : int; p_col : int; p_offset : int }

type t =
  (* Literals *)
  | Int of int
  | Float of float
  | String of string
  | Bool of bool
  (* Domain literals *)
  | Path of string
  | Glob of string
  | DateTime of string
  | Duration of string
  | URL of string
  | IPv4 of string
  | CIDR of string
  | Port of int
  | Version of string
  | Size of string
  (* Identifiers *)
  | Ident of string    (* lowercase — values, functions *)
  | Upper of string    (* uppercase — modules, types, constructors *)
  | TypeVar of string  (* 'a, 'b — type variables in generic declarations/annotations *)
  | Hole               (* ? *)
  (* Keywords *)
  | Let
  | In
  | Match
  | With
  | If
  | Unless
  | Then
  | Else
  | Type
  | Interface
  | Implement
  | Import
  | Requires
  | Ensures
  | Result
  | Fn
  | For
  | Do
  | End
  | When
  | As                 (* as — binds the value a `with` acquires *)
  | And                (* and — keyword, not && *)
  | Or                 (* or — keyword, not || *)
  | Handle             (* handle *)
  | Return             (* return — in effect handler cases *)
  | Try                (* try    *)
  (* Operators *)
  | Eq                 (* = *)
  | Arrow              (* -> *)
  | Pipe               (* | *)
  | PipeArrow          (* |> *)
  | Plus               (* + *)
  | Minus              (* - *)
  | Star               (* * *)
  | Slash              (* / *)
  | Percent            (* % *)
  | Dot                (* . *)
  | DotDot             (* .. *)
  | Colon              (* : *)
  | DoubleColon        (* :: *)
  | Comma              (* , *)
  | Underscore         (* _ *)
  | EqEq               (* == *)
  | BangEq             (* != *)
  | Lt                 (* < *)
  | Gt                 (* > *)
  | LtEq               (* <= *)
  | GtEq               (* >= *)
  | AmpAmp             (* && *)
  | PipePipe           (* || *)
  | Bang               (* ! *)
  | Dollar             (* $ *)
  | EnvVar of string   (* $HOME, $PATH, $MY_VAR — uppercase only *)
  | PlusPlus           (* ++ *)
  | InterpStr    of (string * string * pos) list * string  (* "lit %{src} ... tail" *)
  (* A backtick string. Kept apart from `String`/`InterpStr` all the way to
     the formatter, which has to give one back as one: rendered as `"..."`
     it would come back escaped, and a newline inside it would not read at
     all. *)
  | RawStr       of string
  | RawInterpStr of (string * string * pos) list * string
  | RunCmdRaw    of (string * string * hole * pos) list * string  (* $(cmd %{var} ...) *)
  | RunQueryRaw  of (string * string * hole * pos) list * string  (* $?(cmd %{var} ...) *)
  | CommandRaw   of (string * string * hole * pos) list * string  (* $*(cmd %{var} ...) *)
  | Regex        of string * string                  (* r/pattern/flags *)
  (* Delimiters *)
  | LParen             (* ( *)
  | RParen             (* ) *)
  | LBracket           (* [ *)
  | RBracket           (* ] *)
  | LBrace             (* { *)
  | RBrace             (* } *)
  | Semicolon          (* ; *)
  (* Structure *)
  | Newline
  | EOF
  | LineComment of string   (* -- to end of line *)

(* A source extent, not just a point: where it starts and where it stops.
   The `end_*` fields are exclusive -- the first position past the extent.
   The lexer stamps a token's true end; the parser widens a `Located`
   wrapper's loc to the whole expression it wraps. A loc built by `point`
   has zero width, which renderers read as "no range worth showing". *)
type loc = {
  (* Which file the text is in, or "" when nothing said. A position without
     one reads as "the file being run", which is what an error against a
     script means and what every position meant before this field existed.

     It is here rather than beside the position because a position outlives
     its file: a closure from one file is called from another, and by the
     time an error is raised there is nothing left to ask. *)
  file: string;
  line: int; col: int; offset: int;
  end_line: int; end_col: int; end_offset: int;
}

let point ?(file = "") line col offset =
  { file; line; col; offset;
    end_line = line; end_col = col; end_offset = offset }

(* `a` extended to stop where `b` stops. *)
let span_to (a : loc) (b : loc) =
  { a with end_line = b.end_line; end_col = b.end_col; end_offset = b.end_offset }

let pp ppf tok =
  let s = match tok with
    | Int n      -> Printf.sprintf "Int(%d)" n
    | Float f    -> Printf.sprintf "Float(%g)" f
    | String s   -> Printf.sprintf "String(%S)" s
    | Bool b     -> Printf.sprintf "Bool(%b)" b
    | Path s     -> Printf.sprintf "Path(%s)" s
    | Glob s     -> Printf.sprintf "Glob(%s)" s
    | DateTime s -> Printf.sprintf "DateTime(%s)" s
    | Duration s -> Printf.sprintf "Duration(%s)" s
    | URL s      -> Printf.sprintf "URL(%s)" s
    | IPv4 s     -> Printf.sprintf "IPv4(%s)" s
    | CIDR s     -> Printf.sprintf "CIDR(%s)" s
    | Port n     -> Printf.sprintf "Port(%d)" n
    | Version s  -> Printf.sprintf "Version(%s)" s
    | Size s     -> Printf.sprintf "Size(%s)" s
    | Ident s    -> Printf.sprintf "Ident(%s)" s
    | Upper s    -> Printf.sprintf "Upper(%s)" s
    | TypeVar s  -> Printf.sprintf "'%s" s
    | Hole       -> "Hole"
    | Let        -> "let"
    | In         -> "in"     | Match      -> "match"
    | With       -> "with"   | If         -> "if"
    | Unless     -> "unless"
    | Then       -> "then"   | Else       -> "else"
    | Type       -> "type"
    | Interface  -> "interface"
    | Implement  -> "implement"
    | Import     -> "import"
    | Requires   -> "requires" | Ensures  -> "ensures"
    | Result     -> "result" | Fn         -> "fn"
    | For        -> "for"
    | Do         -> "do"     | End        -> "end"
    | When       -> "when"
    | As         -> "as"
    | And        -> "and"   | Or         -> "or"
    | Handle     -> "handle" | Return    -> "return"  | Try    -> "try"
    | Eq         -> "="      | Arrow      -> "->"
    | Pipe       -> "|"      | PipeArrow  -> "|>"
    | Plus       -> "+"      | Minus      -> "-"
    | Star       -> "*"      | Slash      -> "/"    | Percent    -> "%"
    | Dot        -> "."      | DotDot     -> ".."
    | Colon      -> ":"      | DoubleColon -> "::"
    | Comma      -> ","
    | Underscore -> "_"      | EqEq       -> "=="
    | BangEq     -> "!="     | Lt         -> "<"
    | Gt         -> ">"      | LtEq       -> "<="
    | GtEq       -> ">="     | AmpAmp     -> "&&"
    | PipePipe   -> "||"     | Bang       -> "!"
    | Dollar     -> "$"
    | EnvVar s   -> Printf.sprintf "$%s" s
    | PlusPlus   -> "++"
    | InterpStr _  -> "InterpStr"
    | RawStr s     -> Printf.sprintf "RawStr(%S)" s
    | RawInterpStr _ -> "RawInterpStr"
    | RunCmdRaw _  -> "RunCmdRaw"
    | RunQueryRaw _ -> "RunQueryRaw"
    | CommandRaw _ -> "CommandRaw"
    | Regex (p, f)  -> Printf.sprintf "r/%s/%s" p f
    | LParen     -> "("      | RParen     -> ")"
    | LBracket   -> "["      | RBracket   -> "]"
    | LBrace     -> "{"      | RBrace     -> "}"
    | Semicolon  -> ";"      | Newline    -> "\\n"
    | EOF        -> "EOF"
    | LineComment s -> Printf.sprintf "LineComment(%S)" s
  in
  Format.pp_print_string ppf s

let equal a b = a = b

(* A word the language has taken. Used where a name is expected, so the
   message can say why a perfectly ordinary-looking word was refused. *)
let is_keyword = function
  | Let | In | Match | With | If | Unless | Then | Else | Type | Import
  | Interface | Implement
  | Requires | Ensures | Result | Fn | For | Do | End
  | When | As | And | Or | Handle | Return | Try -> true
  | _ -> false

(* The word a keyword was written as, for the positions that hold a field
   name rather than a name any scope can see. `let*` has no spelling a field
   could take, so it answers None with the rest. *)
let keyword_text = function
  | Let -> Some "let"           | In -> Some "in"
  | Match -> Some "match"       | With -> Some "with"
  | If -> Some "if"             | Then -> Some "then"
  | Unless -> Some "unless"
  | Else -> Some "else"         | Type -> Some "type"
  | Import -> Some "import"     | Requires -> Some "requires"
  | Interface -> Some "interface"
  | Implement -> Some "implement"
  | Ensures -> Some "ensures"   | Result -> Some "result"
  | Fn -> Some "fn"             | For -> Some "for"
  | Do -> Some "do"             | End -> Some "end"
  | When -> Some "when"
  | As -> Some "as"             | And -> Some "and"
  | Or -> Some "or"             | Handle -> Some "handle"
  | Return -> Some "return"     | Try -> Some "try"
  | _ -> None
