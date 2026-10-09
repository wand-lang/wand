(* Finds the command words of a shell command line: the first word, and the
   first word after each top-level `|`, `&&`, `||`, `;`, `&`. This is what
   a `Shell(git, curl)` manifest is checked against -- statically over the
   written text, and again at spawn over the resolved text.

   Deliberately not a shell parser. Quotes are tracked exactly as far as
   telling a real `|` from a quoted one; a subshell, a `$(...)` and a
   backtick span are scanned as command positions in their own right, since
   the shell runs what is in them; `$((...))` is arithmetic and runs
   nothing; redirections are skipped along with their targets. Wrappers
   (`env`, `xargs`, `sh`) are *not* peeled: the wrapper is the thing the
   manifest allows. *)

(* One piece of a command template as written: literal text, a quoted
   `%{...}` splice (always exactly one argument -- it cannot introduce
   operators), or a raw `%!{...}` splice (shell source: everything after
   it is data, structurally unknowable until the value arrives). *)
type seg =
  | Lit of string
  | QuotedHole
  | RawHole

type word_class =
  | Literal  of string   (* checkable, now or at spawn *)
  | Dynamic              (* an interpolation reaches into the word *)
  | Compound of string   (* a shell reserved word: control flow, not a binary *)

type scan = {
  words    : word_class list;  (* one per command position found *)
  raw_tail : bool;             (* a %!{...} appeared: the scan stops there *)
}

(* POSIX reserved words, plus `function`. Any of these in command position
   means the line's real commands live inside a compound body that neither
   the static check nor the spawn check can bound. `time` is deliberately
   absent: `time cmd` is a wrapper, and wrappers are the thing a manifest
   allows. *)
let reserved = [
  "if"; "then"; "elif"; "else"; "fi";
  "for"; "while"; "until"; "do"; "done";
  "case"; "esac"; "{"; "}"; "!"; "function";
]

let is_assignment w =
  (* NAME=value before the command word is an environment prefix; an
     assignment cannot execute anything. *)
  match String.index_opt w '=' with
  | None | Some 0 -> false
  | Some i ->
    let name_char j c =
      (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_'
      || (j > 0 && c >= '0' && c <= '9')
    in
    let rec ok j = j >= i || (name_char j w.[j] && ok (j + 1)) in
    ok 0

let scan (segs : seg list) : scan =
  let words = ref [] in
  let buf = Buffer.create 16 in
  let has_hole = ref false in
  let expecting = ref true in      (* the next word is a command word *)
  let skip_target = ref false in   (* the next word is a redirection target *)
  let raw_tail = ref false in
  (* The rest of a `NAME=$(...)` prefix after its substitution closes: still
     an assignment, so the next word is still the command word. *)
  let assign_tail = ref false in
  (* Everything before the word's first `=` is plain text: no quote, no
     backslash, no hole, no substitution. The shell takes a word as an
     assignment only when its name is plain, so `a'v'=b` is the command
     `av=b`; the buffer holds the text with the quoting gone, and read alone
     it said assignment, which hid that command from the list. *)
  let name_plain = ref true in
  let mark_name () =
    if not (String.contains (Buffer.contents buf) '=') then name_plain := false in
  let assignment w = is_assignment w && !name_plain in
  let finish_word () =
    let w = Buffer.contents buf in
    Buffer.clear buf;
    let dynamic = !has_hole in
    has_hole := false;
    let assign = assignment w in
    name_plain := true;
    if !assign_tail then assign_tail := false
    else if w = "" && not dynamic then ()
    else if !skip_target then skip_target := false
    else if not !expecting then ()
    (* Before the hole test: with the name plain, a hole is in the value,
       and the word is still a prefix. *)
    else if assign then ()  (* still expecting the command word *)
    else if dynamic then (words := Dynamic :: !words; expecting := false)
    else if List.mem w reserved then
      (words := Compound w :: !words; expecting := false)
    else (words := Literal w :: !words; expecting := false)
  in
  (* A subshell `(...)`, a command substitution `$(...)` and a backtick span
     all run commands, and what runs is the whole question here, so each is
     scanned as a command position of its own rather than skipped as opaque.
     Skipping them was a way past the manifest: `Shell(echo)` admitted
     `$(echo $(whoami))`, which runs whoami.

     Each nested context saves the quoting it interrupted -- inside `"..."`
     a substitution starts quoting afresh and the double quote resumes after
     the `)` -- and each contributes a word whose text arrives at run time,
     so a command word built from one reads as Dynamic and is checked at
     spawn. *)
  let scan_lit text =
    let n = String.length text in
    let i = ref 0 in
    let quote = ref ' ' in         (* ' ', '\'' or '"' *)
    (* The quoting each open context suspended, and whether it opened in
       the value of a `NAME=` prefix. *)
    let nested = ref [] in
    let in_backtick = ref false in
    let peek k = if !i + k < n then text.[!i + k] else '\000' in
    let enter () =
      (* What runs inside starts a command line of its own; what the context
         yields is text this word cannot be read from -- unless the word is
         a `NAME=` prefix, whose value runs nothing and leaves the command
         word still to come. Read as a command word, `x=$(echo) whoami`
         hid whoami behind a Dynamic word that nothing checked. *)
      let assign =
        !expecting && not !skip_target && assignment (Buffer.contents buf) in
      if assign then (Buffer.clear buf; has_hole := false; name_plain := true)
      else (has_hole := true; finish_word ());
      nested := (!quote, assign) :: !nested;
      quote := ' ';
      expecting := true
    in
    let leave () =
      finish_word ();
      let resume =
        match !nested with
        | (q, a) :: rest -> quote := q; nested := rest; a
        | [] -> quote := ' '; false
      in
      if resume then (assign_tail := true; expecting := true)
      else expecting := false
    in
    (* `$((...))` is arithmetic, not a command: sh evaluates it and runs
       nothing, so there is nothing here to check -- when it closes with
       `))`. When the parentheses balance before that, sh reads the same
       text as a command substitution around a subshell: `$((whoami) )`
       runs whoami. That form is scanned as `$(` followed by `(`, so the
       word inside is a command position like any other. *)
    let skip_arithmetic () =
      let start = !i in
      i := !i + 3;
      let depth = ref 2 in
      while !depth > 0 && !i < n do
        (match text.[!i] with
         | '(' -> incr depth
         | ')' -> decr depth
         | _ -> ());
        incr i
      done;
      let closed_as_arithmetic =
        !depth = 0 && !i >= 2 && text.[!i - 1] = ')' && text.[!i - 2] = ')'
      in
      if closed_as_arithmetic then (mark_name (); has_hole := true)
      else begin
        i := start + 2;
        enter ()
      end
    in
    while !i < n do
      let c = text.[!i] in
      if !quote = '\'' then begin
        (* Single quotes: every character is itself until the next one. *)
        if c = '\'' then quote := ' ' else Buffer.add_char buf c;
        incr i
      end else if !quote = '"' then begin
        (* Double quotes keep the word together, but the shell still reads
           `$(...)` and backticks inside them. *)
        if c = '\\' && !i + 1 < n then begin
          Buffer.add_char buf c; Buffer.add_char buf (peek 1); i := !i + 2
        end else if c = '$' && peek 1 = '(' && peek 2 = '(' then
          skip_arithmetic ()
        else if c = '$' && peek 1 = '(' then (enter (); i := !i + 2)
        else if c = '`' then (enter (); in_backtick := true; incr i)
        else begin
          if c = '"' then quote := ' ' else Buffer.add_char buf c;
          incr i
        end
      end else
        match c with
        | '\'' | '"' -> mark_name (); quote := c; incr i
        | '`' ->
          if !in_backtick then (leave (); in_backtick := false)
          else (enter (); in_backtick := true);
          incr i
        | '$' when peek 1 = '(' && peek 2 = '(' -> skip_arithmetic ()
        | '$' when peek 1 = '(' -> enter (); i := !i + 2
        | '\\' when !i + 1 < n ->
          mark_name (); Buffer.add_char buf (peek 1); i := !i + 2
        | ' ' | '\t' -> finish_word (); incr i
        (* A newline separates two commands, exactly as `;` does. Read as
           whitespace, the word after one was never a command position, so
           neither the allowlist check nor the check at spawn ever saw it:
           `$(echo %!{c})` with a newline in `c` ran whatever followed. *)
        | '\n' -> finish_word (); expecting := true; incr i
        | '(' -> finish_word (); nested := (!quote, false) :: !nested;
                 expecting := true; incr i
        | ')' -> leave (); incr i
        | '|' ->
          finish_word (); expecting := true;
          i := !i + (if peek 1 = '|' || peek 1 = '&' then 2 else 1)
        | '&' ->
          finish_word (); expecting := true;
          i := !i + (if peek 1 = '&' then 2 else 1)
        | ';' -> finish_word (); expecting := true; incr i
        | '<' | '>' ->
          (* A redirection: any digits gathered so far are its fd prefix,
             not a word. `>&2`-style forms carry their own target. *)
          if Buffer.contents buf <> ""
             && String.for_all (fun d -> d >= '0' && d <= '9')
                  (Buffer.contents buf)
          then Buffer.clear buf
          else finish_word ();
          let j = ref (!i + 1) in
          if !j < n && (text.[!j] = '>' || text.[!j] = '<') then incr j;
          if !j < n && text.[!j] = '&' then begin
            incr j;
            while !j < n && text.[!j] >= '0' && text.[!j] <= '9' do incr j done
          end else skip_target := true;
          i := !j
        | c -> Buffer.add_char buf c; incr i
    done
  in
  (try
     List.iter (fun seg ->
       match seg with
       | Lit text -> scan_lit text
       | QuotedHole -> mark_name (); has_hole := true
       | RawHole ->
         (* Shell source arrives here at runtime; nothing after it can be
            read structurally from the written text. The spawn-time rescan
            of the resolved line picks up where this leaves off. *)
         raw_tail := true;
         if !expecting then words := Dynamic :: !words;
         raise Exit)
       segs;
     finish_word ()
   with Exit -> ());
  { words = List.rev !words; raw_tail = !raw_tail }

(* The runtime side: the resolved command line, no holes left. *)
let scan_string text = scan [Lit text]

(* The narrowing check, which `Narrow` owns because `Net` is about to be its
   second user. Kept here as the name the shell side already calls. *)
let allowed ~allow word = Narrow.allowed ~rule:Narrow.binary ~allow word

(* The command template a $()/$?() payload was written as. Anything that is
   not literal text or a recognisable interpolation -- an arbitrary
   expression in command position -- reads as raw: its shape arrives at
   run time. *)
let rec segs_of_cmd (e : Ast.expr) : seg list =
  match e with
  | Ast.Located (_, inner) -> segs_of_cmd inner
  | Ast.String cmd | Ast.RawString cmd -> [Lit cmd]
  | Ast.CmdInterp (parts, tail) ->
    List.concat_map (fun (lit, _, h) ->
      [Lit lit;
       (match (h : Token.hole) with
        | Token.Source -> RawHole
        (* Quoted for whatever it lands in, so it is one word's worth of
           text either way: it cannot introduce an operator. *)
        | Token.Arg | Token.Inside _ -> QuotedHole)]) parts
    @ [Lit tail]
  | _ -> [RawHole]

(* A token that can be part of one binary name written bare in a manifest.
   `docker-compose` works as raw text inside $() but reaches the manifest
   parser as `docker`, `-`, `compose`; the same spelling should work in
   the manifest that bounds the command, so byte-adjacent fragments are
   joined back into one name. *)
let fragment = function
  | Token.Ident w -> Some w
  | Token.Int n when n >= 0 -> Some (string_of_int n)
  (* `demos/probe.sh` arrives as `demos` then the path `/probe.sh`. *)
  | Token.Path p -> Some p
  | Token.Minus -> Some "-"
  (* `docker-*` reaches here as `docker`, `-`, `*`, and `*.example.com` as
     one Glob token -- the lexer reads a `*` with more after it as a glob.
     A manifest word is not a `Glob` value; this is the token the spelling
     produced, and the word is the text. *)
  | Token.Star -> Some "*"
  | Token.IPv4 a -> Some a
  | Token.Glob g -> Some g
  | Token.Dot -> Some "."
  | Token.Plus -> Some "+"
  | Token.PlusPlus -> Some "++"
  | _ -> None

(* One manifest entry back as source text: bare exactly when the manifest
   parser would read it back as the same one name -- decided by lexing it,
   not by guessing the lexer's rules. Quotes stay for the unlexable:
   spaces, leading digits, keyword chunks. *)
let render_entry w =
  let reads_back () =
    match Lexer.tokenize w with
    | exception _ -> false
    | [(Token.Port n, _); (Token.EOF, _)] | [(Token.Port n, _)] ->
      ":" ^ string_of_int n = w
    | ((Token.Ident w0 | Token.Path w0 | Token.Glob w0), loc0) :: rest
      when loc0.Token.offset = 0 ->
      let rec go acc end_ = function
        | [] -> acc = w
        | (t, (loc : Token.loc)) :: tl ->
          (match fragment t with
           | Some frag when loc.Token.offset = end_ ->
             go (acc ^ frag) (end_ + String.length frag) tl
           | _ ->
             (match t with
              | Token.Newline | Token.EOF -> go acc end_ tl
              | _ -> false))
      in
      go w0 (String.length w0) rest
    | _ -> false
  in
  if reads_back () then w else "\"" ^ w ^ "\""

let render_label = function
  | (name, None) -> name
  | (name, Some args) ->
    name ^ "(" ^ String.concat ", " (List.map render_entry args) ^ ")"

(* ── The direct-exec fast path ────────────────────────────────────────────
   make's optimization: a command line in which a shell would find nothing
   to do means exactly what its words say, so the runner can exec it
   directly instead of paying a shell startup per spawn (~5ms on macOS,
   whose /bin/sh is bash). This classifier says when that is safe.

   The rules err toward the shell. Refused anywhere outside single quotes:
   every operator, expansion, quote and escape character a shell reads --
   the set is `sh_metachars` below -- and a newline. Refused in command
   position: a shell builtin or reserved word (whose meaning the shell
   supplies -- exec'ing /bin/echo is not running `echo`), and a word
   containing `=` (an assignment or environment prefix). A character on
   the list that is merely literal to a modern sh -- a caret, an argument
   `!` -- costs the fast path, never correctness.

   Single-quoted spans are handled rather than refused because `%{}`
   interpolation writes them: to sh every character inside is itself, and
   that is exactly how they are read here, so an interpolated argument
   still takes the fast path. Adjacent segments join into one word and
   `''` is a real, empty argument -- both as sh would. *)

let sh_metachars = "#;\"*?[]&|<>(){}$`\\~!^\n"

(* Names whose meaning in command position comes from the shell itself:
   POSIX special builtins, the utilities sh implements as builtins, and
   bash's own, since macOS /bin/sh is bash. The control-flow words are in
   `reserved` above. *)
let sh_builtins = [
  "."; ":"; "["; "alias"; "bg"; "bind"; "break"; "builtin"; "caller";
  "cd"; "command"; "compgen"; "complete"; "continue"; "declare"; "dirs";
  "disown"; "echo"; "enable"; "eval"; "exec"; "exit"; "export"; "false";
  "fc"; "fg"; "getopts"; "hash"; "help"; "history"; "in"; "jobs"; "kill";
  "let"; "local"; "logout"; "popd"; "printf"; "pushd"; "pwd"; "read";
  "readonly"; "return"; "select"; "set"; "shift"; "shopt"; "source";
  "suspend"; "test"; "time"; "times"; "trap"; "true"; "type"; "typeset";
  "ulimit"; "umask"; "unalias"; "unset"; "wait";
]

(* The command's words when a shell would only ever split and run them, or
   None when anything shell-special appears. `Some ws` is never empty. *)
let direct_words cmd : string list option =
  let n = String.length cmd in
  let words = ref [] in
  let buf = Buffer.create 16 in
  let in_word = ref false in
  let ok = ref true in
  let flush () =
    if !in_word then begin
      words := Buffer.contents buf :: !words;
      Buffer.clear buf;
      in_word := false
    end
  in
  let i = ref 0 in
  while !ok && !i < n do
    (match cmd.[!i] with
     | '\'' ->
       (* A single-quoted span: every character until the closing quote is
          itself. Unclosed is sh's error to report, so it is refused. *)
       in_word := true;
       incr i;
       let closed = ref false in
       while not !closed && !i < n do
         if cmd.[!i] = '\'' then closed := true
         else Buffer.add_char buf cmd.[!i];
         incr i
       done;
       if not !closed then ok := false
     | ' ' | '\t' -> flush (); incr i
     | c when String.contains sh_metachars c -> ok := false
     | c -> in_word := true; Buffer.add_char buf c; incr i)
  done;
  if not !ok then None
  else begin
    flush ();
    match List.rev !words with
    | [] -> None
    | (w0 :: _) as ws ->
      if String.contains w0 '='
         || List.mem w0 sh_builtins || List.mem w0 reserved
      then None
      else Some ws
  end
