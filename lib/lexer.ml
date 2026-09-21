open Token

(* Raised inside the scanner, where only the message is known. `tokenize`
   turns it into `LexError` with the position of the token being read --
   internal raises need not thread a location they cannot improve on. *)
exception Fail of string

exception LexError of Token.loc * string

type state = {
  src  : string;
  mutable pos  : int;
  (* Added to every offset this state reports. An interpolation's body is
     lexed as a string of its own, so its `pos` counts from that string;
     the base is where the body sits in the file. *)
  base : int;
  mutable line : int;
  mutable col  : int;
  (* where the token being scanned began -- the position a failure inside
     `next_token` is reported at, so an unterminated string points at its
     opening quote rather than at end of file. *)
  mutable tok_start : Token.loc;
  (* The file this text came from, stamped onto every position it produces.
     "" for source with no file behind it -- `wand t -e`, a session line. *)
  file : string;
}

let make ?(file = "") ?(line = 1) ?(col = 1) ?(base = 0) src =
  { src; pos = 0; base; line; col; file;
    tok_start = Token.point ~file line col base }

let len s = String.length s.src
let is_at_end s = s.pos >= len s
let peek s  = if s.pos     >= len s then '\000' else s.src.[s.pos]
let peek2 s = if s.pos + 1 >= len s then '\000' else s.src.[s.pos + 1]
let char_at s offset =
  let i = s.pos + offset in
  if i >= len s then '\000' else s.src.[i]
let advance s =
  let c = peek s in
  s.pos <- s.pos + 1;
  (if c = '\n' then (s.line <- s.line + 1; s.col <- 1)
   else s.col <- s.col + 1);
  c

let is_digit c = c >= '0' && c <= '9'
let is_lower c = c >= 'a' && c <= 'z'
let is_upper c = c >= 'A' && c <= 'Z'
let is_alpha c = is_lower c || is_upper c
let is_alnum_or_under c = is_alpha c || is_digit c || c = '_'
let is_path_body_char c = is_alnum_or_under c || c = '-' || c = '.' || c = '/'
let is_glob_char c = c = '*' || c = '?' || c = '['

(* ── A character wand has no use for ────────────────────────────────────── *)

(* How many bytes the character starting with this one takes. A byte that
   starts no character answers 1, and the caller says so rather than
   printing it. *)
let utf8_width lead =
  let b = Char.code lead in
  if b < 0x80 then 1
  else if b land 0xE0 = 0xC0 then 2
  else if b land 0xF0 = 0xE0 then 3
  else if b land 0xF8 = 0xF0 then 4
  else 1

(* What to write instead. These are what a document pasted into a script
   carries -- a word processor's dashes and quotes, and the space a browser
   leaves behind -- so they are the first thing a file from outside hits. *)
let write_instead = function
  | "\xe2\x80\x94" | "\xe2\x80\x93" -> Some "a hyphen, -"
  | "\xe2\x80\x98" | "\xe2\x80\x99" -> Some "an apostrophe, '"
  | "\xe2\x80\x9c" | "\xe2\x80\x9d" -> Some "a double quote, \""
  | "\xc2\xa0"                      -> Some "an ordinary space"
  | "\xe2\x80\xa6"                  -> Some "three dots, ..."
  | _ -> None

(* The character, not the byte it starts with. Naming the byte gave
   `unexpected character '\342'`, which says nothing a reader can act on --
   and the byte alone is not valid UTF-8, so a caller decoding the
   diagnostic strictly died rather than reporting. *)
let unexpected_character char_at_offset lead =
  let width = utf8_width lead in
  let buf = Buffer.create 4 in
  Buffer.add_char buf lead;
  for k = 0 to width - 2 do
    let b = char_at_offset k in
    if Char.code b land 0xC0 = 0x80 then Buffer.add_char buf b
  done;
  let text = Buffer.contents buf in
  if String.length text <> width then
    (* The continuation bytes are not there, so this starts no character. *)
    Printf.sprintf "unexpected byte 0x%02X, which begins no character"
      (Char.code lead)
  else if width = 1 && Char.code lead >= 0x80 then
    Printf.sprintf "unexpected byte 0x%02X, which begins no character"
      (Char.code lead)
  else
    match write_instead text with
    | Some what -> Printf.sprintf "unexpected character '%s' -- write %s" text what
    | None -> Printf.sprintf "unexpected character '%s'" text

(* ── Keywords & identifiers ─────────────────────────────────────────────── *)

let keyword_or_ident word = match word with
  | "let"      -> Let      | "in"       -> In
  | "match"    -> Match    | "with"     -> With
  | "if"       -> If       | "then"     -> Then
  | "else"     -> Else     | "type"     -> Type
  | "import"   -> Import
  | "interface" -> Interface
  | "implement" -> Implement
  | "requires" -> Requires
  | "ensures"  -> Ensures  | "result"   -> Result
  | "fn"       -> Fn       | "fun"      -> Fn
  | "for"      -> For      | "do"       -> Do
  | "end"      -> End
  | "when"     -> When     | "and"      -> And
  | "as"       -> As
  | "or"       -> Or       | "handle"   -> Handle
  | "return"   -> Return   | "try"      -> Try
  | "true"     -> Bool true
  | "false"    -> Bool false
  | "_"        -> Underscore
  | s when s.[0] >= 'A' && s.[0] <= 'Z' -> Upper s
  | s          -> Ident s

(* The body of a `%{...}`, up to the `}` that closes it.

   Brace depth decides where it ends, and a `$(...)` inside it is not wand
   source: it is shell, where a brace is an ordinary character. Counting
   those made the string in test/fuzz/regressions end at the `}` after the
   `)`, so the interpolation
   swallowed a brace the string meant to keep and the formatter wrote the
   literal back one `}` short -- source that no longer lexes. So the scan
   copies a command body through without reading braces in it, tracking only
   the parens that say where the command ends.

   A brace inside a nested string is text for the same reason: the
   interpolated expression may itself hold a string, and
   `"%{String.replace s "{" "["}"` ended a `{` short. A string opened here
   interpolates again, so what is open is a stack rather than a count. All
   three interpolation forms share this, which is why it is one function.
   Found by test/fuzz. *)
let read_interp_body s ~unterminated =
  let expr_buf = Buffer.create 16 in
  (* What is open, innermost last. A `Brace` waits for its `}` -- the body
     itself is the first -- and the two string forms wait for their closing
     mark, where a brace says nothing. *)
  let open_ = ref [`Brace] in
  let pop () = open_ := List.tl !open_ in
  let push m = open_ := m :: !open_ in
  (* Inside `$(`/`$?(`/`$*(`/`$!(`, copying verbatim until the parens
     balance. *)
  let cmd_parens = ref 0 in
  let add = Buffer.add_char expr_buf in
  while !open_ <> [] do
    if is_at_end s then raise (Fail unterminated);
    let c = advance s in
    match !open_ with
    | `Quoted :: _ ->
      add c;
      if c = '\\' then (if not (is_at_end s) then add (advance s))
      else if c = '"' then pop ()
      else if c = '%' && peek s = '{' then (add (advance s); push `Brace)
    | `Raw :: _ ->
      add c;
      if c = '`' then pop ()
      else if c = '%' && peek s = '{' then (add (advance s); push `Brace)
    | _ ->
      if !cmd_parens > 0 then begin
        if c = '(' then incr cmd_parens
        else if c = ')' then decr cmd_parens;
        add c
      end else if c = '$' && peek s = '(' then begin
        cmd_parens := 1;
        add c;
        add (advance s)
      end else if c = '$' && (peek s = '?' || peek s = '*' || peek s = '!')
                  && peek2 s = '(' then begin
        cmd_parens := 1;
        add c;
        add (advance s);
        add (advance s)
      end else if c = '"' then (add c; push `Quoted)
      else if c = '`' then (add c; push `Raw)
      else if c = '{' then (push `Brace; add c)
      else if c = '}' then begin
        pop ();
        if !open_ <> [] then add c
      end else
        add c
  done;
  Buffer.contents expr_buf

(* ── String literals ────────────────────────────────────────────────────── *)

let read_string s =
  let parts = ref [] in
  let buf = Buffer.create 16 in
  let rec loop () =
    if is_at_end s then raise (Fail "unterminated string literal");
    match advance s with
    | '"'  ->
      if !parts = [] then String (Buffer.contents buf)
      else InterpStr (!parts, Buffer.contents buf)
    | '\\' ->
      (* The file ending on the backslash is an unterminated string, not an
         escape of whatever `advance` answers past the end. *)
      if is_at_end s then raise (Fail "unterminated string literal");
      let c = match advance s with
        | 'n' -> '\n' | 't' -> '\t' | 'r' -> '\r'
        | '\\' -> '\\' | '"' -> '"' | '$' -> '$' | '%' -> '%' | '#' -> '#'
        | c -> raise (Fail (Printf.sprintf "unknown escape \\%c" c))
      in
      Buffer.add_char buf c; loop ()
    (* A string is text, not a command line: there are no argument
       boundaries to quote for, so raw interpolation would mean exactly what
       `%{...}` already means. Rejected rather than allowed as a synonym. *)
    | '%' when peek s = '!' && peek2 s = '{' ->
      raise (Fail "%!{...} is for shell commands, where it splices a \
                       value as shell source. A string has nothing to quote \
                       for, so write %{...} here.")
    (* `$` used to interpolate. It now means one thing -- reaching outside
       the program, as `$(cmd)` and `$NAME` do -- and text assembly is `%`.
       Refused for a release rather than read as literal text, because a
       `"${x}"` that quietly became the characters `${x}` is a wrong answer
       no one would look for. *)
    | '$' when peek s = '{' || (peek s = '!' && peek2 s = '{') ->
      raise (Fail "interpolation is %{...}, not ${...}. For the \
                       literal text, write \\${...}")
    (* The Ruby/Elixir spelling, refused for the same reason as `${...}`:
       text that quietly stayed text is a wrong answer no one would look
       for. *)
    | '#' when peek s = '{' ->
      raise (Fail "interpolation is %{...}, not #{...}. For the literal \
                       text, write \\#{...}")
    | '%' when peek s = '{' ->
      ignore (advance s);
      let lit = Buffer.contents buf in
      Buffer.clear buf;
      let at = { Token.p_line = s.line; p_col = s.col; p_offset = s.pos + s.base } in
      let body =
        read_interp_body s ~unterminated:"unterminated string interpolation" in
      parts := !parts @ [(lit, body, at)];
      loop ()
    (* `$NAME` is text here. Reading the environment is an expression like
       any other, so it goes through the one interpolation form -- write
       `%{$HOME}`. What this buys is that a string holding shell or Make
       source keeps it: `$HOME`, `$PATH`, `$(date)` all survive as written,
       and there is no longer a spelling that means one thing in text and
       another in code. *)
    | c -> Buffer.add_char buf c; loop ()
  in
  loop ()

(* ── Raw string literals: `...` ─────────────────────────────────────────── *)

(* Between backticks every character is itself. There are no escapes, so a
   backslash is a backslash and a double quote is just a quote -- which is
   the point: JSON,
   a regex, a Windows path and a here-doc can all be pasted in as they are.

   `%{...}` still interpolates, because a string form that could not take a
   value would only send people back to `"..."` and its escaping. That makes
   `%{` the one sequence a raw string cannot hold; an ordinary quoted string
   escapes it as backslash-percent, and needing that is rarer than
   everything this buys.

   A newline straight after the opening backtick is not part of the text, so
   a literal can start on its own line without the string starting blank.
   Nothing else is trimmed: trailing spaces, indentation and the final
   newline are all kept, since a raw string is for text whose shape matters. *)
let read_raw_string s =
  (* The opening backtick is consumed; drop a newline that follows it. *)
  if peek s = '\n' then ignore (advance s)
  else if peek s = '\r' && peek2 s = '\n' then (ignore (advance s); ignore (advance s));
  let parts = ref [] in
  let buf = Buffer.create 32 in
  let rec loop () =
    if is_at_end s then raise (Fail "unterminated `...` string");
    match advance s with
    | '`' ->
      if !parts = [] then RawStr (Buffer.contents buf)
      else RawInterpStr (!parts, Buffer.contents buf)
    | '%' when peek s = '!' && peek2 s = '{' ->
      raise (Fail "%!{...} is for shell commands, where it splices a \
                       value as shell source. A string has nothing to quote \
                       for, so write %{...} here.")
    | '%' when peek s = '{' ->
      ignore (advance s);
      let lit = Buffer.contents buf in
      Buffer.clear buf;
      let at = { Token.p_line = s.line; p_col = s.col; p_offset = s.pos + s.base } in
      let body =
        read_interp_body s
          ~unterminated:"unterminated %{...} interpolation in a `...` \
                         string. A `...` string cannot hold a literal %{ \
                         -- for that text, use an ordinary \"...\" string \
                         and write \\%{" in
      parts := !parts @ [(lit, body, at)];
      loop ()
    | c -> Buffer.add_char buf c; loop ()
  in
  loop ()

(* ── Run-command literals: $(cmd %{var}) ────────────────────────────────── *)

let read_run_cmd ?(form = "$()") s =
  let parts = ref [] in
  let buf = Buffer.create 16 in
  let depth = ref 1 in
  (* The shell's own quoting, tracked because the command ends at a `)` and
     a quoted one is not the end of anything: `$(echo "a)b")` is one
     command, and counting parens blind cut it in half -- taking the rest of
     the line with it, since what followed was read as wand source. Tracked
     only that far: which quote is open, and whether a backslash spent the
     next character. *)
  let quote = ref ' ' in
  let rec loop () =
    if is_at_end s then
      raise (Fail (if !quote = ' ' then
                     Printf.sprintf "unterminated %s command" form
                   else Printf.sprintf
                     "unterminated %s command: a %c quote is still open"
                     form !quote));
    match advance s with
    | '(' when !quote = ' ' ->
      incr depth; Buffer.add_char buf '('; loop ()
    | ')' when !quote = ' ' ->
      decr depth;
      if !depth > 0 then (Buffer.add_char buf ')'; loop ())
      else (* closing paren — done *)
        RunCmdRaw (!parts, Buffer.contents buf)
    (* `%{x}` quotes, `%!{x}` splices. Both read the same expression source;
       they differ only in what the evaluator does with the value.

       `$` keeps its own job inside a command, which is the shell's: `$HOME`
       and `$(date)` here are text the shell will expand, and wand no longer
       competes for them. *)
    | '$' when peek s = '{' || (peek s = '!' && peek2 s = '{') ->
      raise (Fail "interpolation is %{...}, not ${...}. For a \
                       variable the shell should expand, $ needs no escape \
                       -- write $NAME or ${NAME} once this release is past")
    | '%' when peek s = '{' || (peek s = '!' && peek2 s = '{') ->
      let raw = peek s = '!' in
      if raw then ignore (advance s);
      ignore (advance s);
      let lit = Buffer.contents buf in
      Buffer.clear buf;
      let at = { Token.p_line = s.line; p_col = s.col; p_offset = s.pos + s.base } in
      let expr_buf_contents =
        read_interp_body s ~unterminated:"unterminated command interpolation" in
      (* Backticks are not a quote the value has to be escaped for: what is
         inside them is source for a shell of its own, which reads an
         ordinary single-quoted argument exactly as the outer one would. *)
      let hole =
        if raw then Token.Source
        else match !quote with
          | '\'' | '"' as q -> Token.Inside q
          | _ -> Token.Arg
      in
      parts := !parts @ [(lit, expr_buf_contents, hole, at)];
      loop ()
    | '\\' when peek s = '\n' -> ignore (advance s); loop ()
    (* A backslash spends the next character wherever the shell would let it
       -- everywhere but inside single quotes, where it is itself. *)
    | '\\' when !quote <> '\'' && not (is_at_end s) ->
      Buffer.add_char buf '\\'; Buffer.add_char buf (advance s); loop ()
    | ('\'' | '"' | '`') as c when !quote = ' ' ->
      quote := c; Buffer.add_char buf c; loop ()
    | c when c = !quote ->
      quote := ' '; Buffer.add_char buf c; loop ()
    | c -> Buffer.add_char buf c; loop ()
  in
  loop ()

(* ── Comments ───────────────────────────────────────────────────────────── *)

(* `--` runs to the end of the line. The newline itself is left unconsumed so
   the following `Newline` token is still produced -- statement termination
   must not depend on whether a line ends in a comment. *)
let read_line_comment s =
  ignore (advance s);  (* consume second '-' *)
  let buf = Buffer.create 64 in
  while not (is_at_end s) && peek s <> '\n' do
    Buffer.add_char buf (advance s)
  done;
  Buffer.contents buf

(* ── Paths ──────────────────────────────────────────────────────────────── *)

(* Is there a `]` to reach, on this line and before the glob would have
   ended anyway? A character class runs to its `]`, and the scan for one used
   to stop only at the end of the file -- so an unmatched `[` turned the whole
   rest of the source into one glob. It swallowed the newline that ends the
   statement, and any bracket written after it: `wand f` wrapped a line
   holding one in brackets, and the closing bracket went into the glob, so
   the formatter wrote source that would not parse. Found by test/fuzz.

   An unmatched `[` is a literal `[` here, which is what fnmatch(3) makes of
   it too. Whitespace stops the scan because whitespace stops a glob: a class
   that seems to hold a space is a `[` that was never a class. *)
let closes_a_class s =
  let n = len s in
  let rec go i =
    if i >= n then false
    else match s.src.[i] with
      | ']' -> true
      | ' ' | '\t' | '\n' | '\r' -> false
      | _ -> go (i + 1)
  in
  go s.pos

let read_path_body s prefix =
  let buf = Buffer.create 16 in
  Buffer.add_string buf prefix;
  let has_glob = ref (String.exists is_glob_char prefix) in
  let rec loop () =
    if not (is_at_end s) then
      let c = peek s in
      if is_path_body_char c then begin
        Buffer.add_char buf (advance s); loop ()
      end else if is_glob_char c then begin
        has_glob := true;
        Buffer.add_char buf (advance s);
        (* consume rest of bracket expression *)
        if c = '[' && closes_a_class s then begin
          while not (is_at_end s) && peek s <> ']' do
            Buffer.add_char buf (advance s)
          done;
          if not (is_at_end s) then Buffer.add_char buf (advance s)
        end;
        loop ()
      end
  in
  loop ();
  let contents = Buffer.contents buf in
  if !has_glob then Glob contents else Path contents

(* ── Globs ──────────────────────────────────────────────────────────────── *)

(* What separates a glob from a path, held here so `String.to_glob` and the
   scanner answer the same question. `read_path_body` decides between the
   two by whether it saw a `*`, `?` or `[`, and this is that rule read from
   the other side: text with none of them is a path, and calling it a glob
   would give a pattern that matches one name -- a Path already says that,
   in a type whose functions are about naming a file rather than selecting
   several.

   The brackets are not checked here. Whether a class is well formed is a
   question for the matcher that compiles it, and it is the one that knows;
   `str_to_glob` asks it. *)
let glob_error text =
  if text = "" then Some "a glob cannot be empty"
  else if not (String.exists is_glob_char text) then
    Some "a glob has a `*`, a `?` or a `[`; text with none of them is a Path"
  else None

(* ── URLs ───────────────────────────────────────────────────────────────── *)

(* The characters a URL may hold, from RFC 3986: unreserved, the gen-delims
   and sub-delims that separate its parts, and `%` for an escape. Everything
   else -- a space, a control character, a quote, a backtick, and the
   brackets and slashes RFC 3986 lists as unwise -- has to be percent-encoded
   to appear in one, so a URL holding it raw is malformed and saying so is
   better than carrying it.

   Note `,` and `;` are on this list. They are legal in a URL and illegal in
   a URL *literal*, which is not a contradiction: `read_url` stops at them
   because they are the punctuation of the expression the literal sits in,
   and that is a rule about writing one down. `String.to_url` is the way to
   spell the ones the literal cannot, and it is the reason this predicate is
   separate from the scanner that ends the token. *)
let is_url_char c =
  is_alpha c || is_digit c
  || (match c with
      | '-' | '.' | '_' | '~'                                (* unreserved *)
      | ':' | '/' | '?' | '#' | '[' | ']' | '@'              (* gen-delims *)
      | '!' | '$' | '&' | '\'' | '(' | ')'
      | '*' | '+' | ',' | ';' | '='                          (* sub-delims *)
      | '%' -> true                                          (* escape *)
      | _ -> false)

(* Whether text is a URL, and why not when it is not. The one place that
   decides, so a literal, `String.to_url` and `Decode.url` cannot disagree
   about the same text -- the same reason `read_port` holds the port range.

   `String.to_url` used to decide by handing the string back to the lexer and
   asking whether it came out as a single URL token. That made the literal's
   punctuation rule into a rule about URLs themselves: `https://x/a?b=1,2` is
   a perfectly good URL, and there was no way to build one, because the
   literal stops at the `,` and the string went through the same scanner. It
   also answered a lexer's complaint rather than its own -- `ftp://x` came
   back "a comment is '-- ...' to the end of the line, not '//'". *)
let url_error text =
  let scheme_len =
    if String.length text >= 7 && String.sub text 0 7 = "http://" then 7
    else if String.length text >= 8 && String.sub text 0 8 = "https://" then 8
    else 0
  in
  if scheme_len = 0 then
    Some "a URL begins with http:// or https://"
  else
    let rec bad i =
      if i >= String.length text then None
      else if is_url_char text.[i] then bad (i + 1)
      else Some (Printf.sprintf
        "%C cannot appear in a URL: percent-encode it, or hold the text as a String"
        text.[i])
    in
    bad scheme_len

let read_url s scheme =
  (* s.pos is at ':' of "://" — consume all three *)
  ignore (advance s); ignore (advance s); ignore (advance s);
  let buf = Buffer.create 32 in
  Buffer.add_string buf scheme;
  Buffer.add_string buf "://";
  (* `;` ends a URL for the same reason a bracket does: it is punctuation
     that closes what the literal sits in, and a URL that ate it left the
     statement after it with nothing in front of it -- `(http://x; let y = 1
     in ())` came back "expected ), got let". A newline had always ended one,
     so the shape only appeared once `wand f` wrote the block on a single
     line. A URL that really holds a `;` is written as a string, as one
     holding a `,` already is. Found by test/fuzz. *)
  while not (is_at_end s)
     && not (List.mem (peek s) [' '; '\t'; '\n'; '\r'; ')'; ']'; '}'; ','; ';']) do
    Buffer.add_char buf (advance s)
  done;
  let text = Buffer.contents buf in
  (* Checked here as well as in `String.to_url`, so the literal is not the
     more permissive of the two. A literal cannot hold a space -- the loop
     above ends at one -- but it can reach `|` and `^`, and a value the
     checked constructor rejects should not be writable straight into the
     source. *)
  (match url_error text with Some why -> raise (Fail why) | None -> ());
  URL text

(* ── Versions ───────────────────────────────────────────────────────────── *)

(* The grammar `String.to_version` and `Decode.version` are checked against,
   held here for the reason `url_error` and the port range are: one rule, so
   three readers of the same text cannot disagree about it.

   It accepts two spellings the literal cannot, and the reasons are the same
   shape as the URL's `,`:

   Build metadata. `1.2.3+build.5` is semver, and `+` is the addition
   operator, so a literal reading it would have to decide what `1.2.3+1`
   means -- and it looks exactly like arithmetic. The literal leaves `+`
   alone and this is where a version carrying one is built.

   A leading `v`. A git tag is `v0.55.5` and a release note says `v1.2.3`,
   so text holding a version usually holds the `v` as well. `port_text`
   already does this in the other direction, adding the `:` a bare number
   from a config file lacks. The `v` is dropped rather than kept: it is a
   way of writing the version, not part of it, and keeping it would make
   `1.2.3` and `v1.2.3` two values that order the same and print
   differently. *)
let version_text s =
  let s = String.trim s in
  if String.length s > 1 && (s.[0] = 'v' || s.[0] = 'V') && is_digit s.[1] then
    String.sub s 1 (String.length s - 1)
  else s

(* Semantic Versioning 2.0.0, https://semver.org/spec/v2.0.0.html, as its
   grammar states it.

   A numeric identifier is `0` or a digit string with no leading zero, which
   rules 2 and 9 both require: `01.2.3` and `1.2.3-01` are not versions. An
   alphanumeric identifier is `[0-9A-Za-z-]` with at least one character that
   is not a digit -- no `_`, which the literal used to admit. No identifier
   may be empty, so `1.2.3-` and `1.2.3+` are not versions either.

   Build identifiers are the same set, except that a digit string may carry
   leading zeros: build metadata is never compared, so there is no number
   there for a zero to be in front of. *)
let is_version_id_char c = is_alpha c || is_digit c || c = '-'

let all_digits s = s <> "" && String.for_all is_digit s

let numeric_identifier s =
  all_digits s && (s = "0" || s.[0] <> '0') && int_of_string_opt s <> None

(* Rule 9: an identifier that is all digits is compared as a number and so
   must be one; anything else is compared as text and only has to be in the
   character set. *)
let prerelease_identifier s =
  s <> ""
  && String.for_all is_version_id_char s
  && (if all_digits s then numeric_identifier s else true)

let build_identifier s = s <> "" && String.for_all is_version_id_char s

let split_on_dot s = String.split_on_char '.' s

let version_error text =
  let n = String.length text in
  (* The core ends at the first `-` or `+`. A `-` after a `+` is inside the
     build metadata, which is why this looks for the first of either rather
     than for each in turn. *)
  let core_end =
    let rec find i =
      if i >= n then n
      else if text.[i] = '-' || text.[i] = '+' then i
      else find (i + 1)
    in
    find 0
  in
  let rest = String.sub text core_end (n - core_end) in
  let pre, build =
    match String.index_opt rest '+' with
    | Some i ->
      (String.sub rest 0 i, Some (String.sub rest (i + 1) (String.length rest - i - 1)))
    | None -> (rest, None)
  in
  let core = split_on_dot (String.sub text 0 core_end) in
  if List.length core <> 3 then
    Some "a version is three numbers: major.minor.patch"
  else if not (List.for_all all_digits core) then
    Some "a version's major, minor and patch are numbers"
  else if not (List.for_all numeric_identifier core) then
    Some "a version's numbers cannot have a leading zero"
  else if pre <> "" && pre.[0] <> '-' then
    Some (Printf.sprintf "%C cannot appear in a version" pre.[0])
  else
    let pre_ids =
      if pre = "" then []
      else split_on_dot (String.sub pre 1 (String.length pre - 1))
    in
    if not (List.for_all prerelease_identifier pre_ids) then
      Some "a prerelease is dot-separated identifiers of letters, digits and \
            hyphens, none empty, and a numeric one has no leading zero"
    else
      match build with
      | None -> None
      | Some b ->
        if List.for_all build_identifier (split_on_dot b) then None
        else
          Some "build metadata is dot-separated identifiers of letters, \
                digits and hyphens, none empty"

(* ── Size units ─────────────────────────────────────────────────────────── *)

let try_read_size_unit s =
  match peek s with
  | 'K' when peek2 s = 'B' -> ignore (advance s); ignore (advance s); Some "KB"
  | 'M' when peek2 s = 'B' -> ignore (advance s); ignore (advance s); Some "MB"
  | 'G' when peek2 s = 'B' -> ignore (advance s); ignore (advance s); Some "GB"
  | 'T' when peek2 s = 'B' -> ignore (advance s); ignore (advance s); Some "TB"
  | 'P' when peek2 s = 'B' -> ignore (advance s); ignore (advance s); Some "PB"
  | 'B'                     -> ignore (advance s);                     Some "B"
  | _ -> None

(* ── Duration units ─────────────────────────────────────────────────────── *)

let peek3 s = char_at s 2

let try_read_duration_unit s =
  match peek s with
  | 'm' when peek2 s = 's' ->
    ignore (advance s); ignore (advance s); Some "ms"
  | 'm' when peek2 s = 'i' && peek3 s = 'n' ->
    ignore (advance s); ignore (advance s); ignore (advance s); Some "min"
  | 'm' -> ignore (advance s); Some "m"
  | 's' -> ignore (advance s); Some "s"
  | 'h' -> ignore (advance s); Some "h"
  | 'd' -> ignore (advance s); Some "d"
  | 'w' -> ignore (advance s); Some "w"
  | _   -> None

let is_duration_start c = match c with
  | 'm' | 's' | 'h' | 'd' | 'w' -> true
  | _ -> false

let read_duration s first_digits =
  let buf = Buffer.create 16 in
  Buffer.add_string buf first_digits;
  (match try_read_duration_unit s with
   | Some u -> Buffer.add_string buf u
   | None   -> assert false);
  (* Greedily consume additional components like 30m in 1h30m *)
  let continue_ = ref true in
  while !continue_ && not (is_at_end s) && is_digit (peek s) do
    let saved = s.pos in
    let seg = Buffer.create 4 in
    while not (is_at_end s) && is_digit (peek s) do
      Buffer.add_char seg (advance s)
    done;
    (match try_read_duration_unit s with
     | Some u ->
       Buffer.add_string buf (Buffer.contents seg);
       Buffer.add_string buf u
     | None ->
       s.pos <- saved;
       continue_ := false)
  done;
  Duration (Buffer.contents buf)

(* The characters an instant is spelled with, after the date. This was
   `List.mem (peek s) ['T';':';'Z';'+';'-']`, which built the list again for
   every character it tested and compared with the polymorphic compare --
   about seventy allocations to read one timestamp. *)
let is_instant_char c =
  is_digit c || c = 'T' || c = ':' || c = 'Z' || c = '+' || c = '-'

(* ── Numbers (Int, Float, Date, DateTime, Time, IPv4, CIDR, Version, Size, Duration) *)

let read_numeric s first_char =
  let buf = Buffer.create 8 in
  Buffer.add_char buf first_char;
  while not (is_at_end s) && is_digit (peek s) do
    Buffer.add_char buf (advance s)
  done;
  let first = Buffer.contents buf in

  match peek s with

  (* Dot: Float / IPv4 / CIDR / Version / Size-with-decimal *)
  | '.' when peek2 s <> '.' ->
    ignore (advance s);
    let seg2 = Buffer.create 4 in
    while not (is_at_end s) && is_digit (peek s) do
      Buffer.add_char seg2 (advance s)
    done;
    let s2 = Buffer.contents seg2 in
    if s2 = "" then raise (Fail "expected digits after '.'");
    (match peek s with
     | '.' when peek2 s <> '.' ->
       (* Third segment → Version or IPv4 *)
       ignore (advance s);
       let seg3 = Buffer.create 4 in
       while not (is_at_end s) && is_digit (peek s) do
         Buffer.add_char seg3 (advance s)
       done;
       let s3 = Buffer.contents seg3 in
       if s3 = "" then raise (Fail "expected digits in segment");
       (match peek s with
        | '.' when peek2 s <> '.' ->
          (* Fourth segment → IPv4 or CIDR *)
          ignore (advance s);
          let seg4 = Buffer.create 4 in
          while not (is_at_end s) && is_digit (peek s) do
            Buffer.add_char seg4 (advance s)
          done;
          let s4 = Buffer.contents seg4 in
          if s4 = "" then raise (Fail "expected digits in segment");
          let octet_ok seg =
            match int_of_string_opt seg with
            | Some n -> n >= 0 && n <= 255
            | None   -> false
          in
          (* A leading zero is an octal octet to libc, and a decimal one
             here, so the two would disagree about which host the text
             names: `010.8.8.8` is private to wand and 8.8.8.8 to curl and
             to every resolver. A script that asks `IPv4.private?` and then
             hands the same text to a command would check one host and
             reach another. Refused, so there is one reading. Hex and bare
             integer forms are already refused. *)
          let no_leading_zero seg = String.length seg = 1 || seg.[0] <> '0' in
          if not (List.for_all no_leading_zero [first; s2; s3; s4]) then
            raise (Fail "invalid IPv4 address: an octet cannot have a \
                         leading zero -- write 10, not 010");
          if not (octet_ok first && octet_ok s2 && octet_ok s3 && octet_ok s4) then
            raise (Fail "invalid IPv4 address: each octet must be 0–255");
          let ipv4 = Printf.sprintf "%s.%s.%s.%s" first s2 s3 s4 in
          if peek s = '/' && is_digit (peek2 s) then begin
            ignore (advance s);
            let prefix = Buffer.create 3 in
            while not (is_at_end s) && is_digit (peek s) do
              Buffer.add_char prefix (advance s)
            done;
            let prefix_str = Buffer.contents prefix in
            (match int_of_string_opt prefix_str with
             | Some n when n >= 0 && n <= 32 ->
               CIDR (ipv4 ^ "/" ^ prefix_str)
             | _ ->
               raise (Fail "invalid CIDR prefix: must be 0–32"))
          end else
            IPv4 ipv4
        | _ ->
          (* Three segments → Version, optionally with pre-release *)
          let base = Printf.sprintf "%s.%s.%s" first s2 s3 in
          if peek s = '-' then begin
            ignore (advance s);
            let pre = Buffer.create 8 in
            Buffer.add_char pre '-';
            while not (is_at_end s)
               && (is_alnum_or_under (peek s) || peek s = '.' || peek s = '-') do
              Buffer.add_char pre (advance s)
            done;
            let text = base ^ Buffer.contents pre in
            (* Checked against the same grammar `String.to_version` uses, for
               the reason the URL literal is: the literal must not be able to
               write a value the checked constructor would refuse. It used to
               admit `_` in a prerelease and a leading zero in a number, and
               semver admits neither. *)
            (match version_error text with
             | Some why -> raise (Fail why)
             | None -> Version text)
          end else
            (match version_error base with
             | Some why -> raise (Fail why)
             | None -> Version base))
     | c when is_upper c ->
       (* e.g. 1.5GB *)
       (match try_read_size_unit s with
        | Some unit -> Size (first ^ "." ^ s2 ^ unit)
        | None -> Float (float_of_string (first ^ "." ^ s2)))
     | _ ->
       Float (float_of_string (first ^ "." ^ s2)))

  (* Dash: Date or DateTime (requires exactly 4-digit year) *)
  | '-' when String.length first = 4
          && is_digit (char_at s 1) && is_digit (char_at s 2)
          && char_at s 3 = '-'
          && is_digit (char_at s 4) && is_digit (char_at s 5) ->
    ignore (advance s);
    let mm1 = advance s and mm2 = advance s in
    ignore (advance s);
    let dd1 = advance s and dd2 = advance s in
    (* `first` is four digits -- the guard above says so -- so the whole
       date is ten characters written into one buffer. `Printf.sprintf` here
       cost more than the rest of reading the literal put together. *)
    let date =
      let b = Bytes.create 10 in
      Bytes.blit_string first 0 b 0 4;
      Bytes.set b 4 '-'; Bytes.set b 5 mm1; Bytes.set b 6 mm2;
      Bytes.set b 7 '-'; Bytes.set b 8 dd1; Bytes.set b 9 dd2;
      Bytes.unsafe_to_string b
    in
    if peek s = 'T' then begin
      let dt = Buffer.create 24 in
      Buffer.add_string dt date;
      while not (is_at_end s)
         && is_instant_char (peek s) do
        Buffer.add_char dt (advance s)
      done;
      DateTime (Buffer.contents dt)
    end else
      (* A bare date is a spelling of midnight UTC, not a type of its own.
         One instant type, one resolution: `2026-08-22 + 5h` moves five
         hours, where two types needed a rule for each to stand still at
         its own.

         The text is kept as it was written, the way an offset form is:
         `datetime_epoch` reads the meaning out of either, and a formatter
         that expanded this would delete a spelling the language offers. *)
      DateTime date

  (* Colon: a time of day, which is not a value. The shape is still read,
     so the refusal can name what to write instead of failing on the `:`
     somewhere further along. A time of day belongs to a day. *)
  | ':' when String.length first = 2
          && is_digit (char_at s 1) && is_digit (char_at s 2)
          && char_at s 3 = ':'
          && is_digit (char_at s 4) && is_digit (char_at s 5) ->
    ignore (advance s);
    let mm1 = advance s and mm2 = advance s in
    ignore (advance s);
    let ss1 = advance s and ss2 = advance s in
    let t = Printf.sprintf "%s:%c%c:%c%c" first mm1 mm2 ss1 ss2 in
    raise (Fail (Printf.sprintf
      "a time of day is not a value on its own -- write the instant, \
       2026-08-22T%s, or a Duration onto a day, DateTime.on! 2026 8 22 + %sh"
      t (String.sub t 0 2)))

  (* Duration suffix (lowercase unit letters) *)
  | c when is_duration_start c ->
    read_duration s first

  (* Size unit or plain Int *)
  | _ ->
    (match try_read_size_unit s with
     | Some unit -> Size (first ^ unit)
     | None ->
       (* A number too large for an Int is a lex error like any other. It
          used to reach `int_of_string`, whose failure escaped as OCaml's own
          -- the reader got "Error: int_of_string" and nothing about where. *)
       (match int_of_string_opt first with
        | Some n -> Int n
        | None ->
          raise (Fail (Printf.sprintf
            "%s is too large for an Int, which holds up to %d" first max_int))))

(* ── Regex literals: r/pattern/flags ────────────────────────────────────── *)

let read_regex s =
  ignore (advance s); (* consume opening '/' *)
  let buf = Buffer.create 16 in
  let rec loop () =
    if is_at_end s then raise (Fail "unterminated regex literal");
    match advance s with
    | '\\' ->
      Buffer.add_char buf '\\';
      if not (is_at_end s) then Buffer.add_char buf (advance s);
      loop ()
    | '/' -> () (* closing slash *)
    | c   -> Buffer.add_char buf c; loop ()
  in
  loop ();
  let pat = Buffer.contents buf in
  let flags = Buffer.create 4 in
  while not (is_at_end s) && List.mem (peek s) ['i'; 'm'; 's'] do
    Buffer.add_char flags (advance s)
  done;
  Regex (pat, Buffer.contents flags)

(* ── Identifiers ─────────────────────────────────────────────────────────── *)

let read_ident s first_char =
  let buf = Buffer.create 8 in
  Buffer.add_char buf first_char;
  while not (is_at_end s) && is_alnum_or_under (peek s) do
    Buffer.add_char buf (advance s)
  done;
  (* Consume one trailing ? or ! suffix (predicate / bang convention) *)
  if not (is_at_end s) && peek s = '?' then
    Buffer.add_char buf (advance s)
  else if not (is_at_end s) && peek s = '!' && peek2 s <> '=' then
    Buffer.add_char buf (advance s);
  let word = Buffer.contents buf in
  (* Regex literal: r/pattern/flags — only when r is followed immediately by / *)
  if word = "r" && peek s = '/' then read_regex s
  (* URL: http:// or https:// *)
  else if (word = "http" || word = "https")
       && peek s = ':' && peek2 s = '/' && char_at s 2 = '/' then
    read_url s word
  else
    keyword_or_ident word

(* ── Port ───────────────────────────────────────────────────────────────── *)

(* A port is a number from 0 to 65535. Outside that it is not a port, and a
   program that says so is wrong about something. Checked here rather than at
   each reader, so a literal, `String.to_port` and `Decode.port` cannot
   disagree about the same number -- they all come through this. *)
let read_port s =
  let buf = Buffer.create 5 in
  while not (is_at_end s) && is_digit (peek s) do
    Buffer.add_char buf (advance s)
  done;
  let digits = Buffer.contents buf in
  match int_of_string_opt digits with
  | Some n when n <= 65535 -> Port n
  | _ ->
    (* Also the arm for a number too large to be an Int at all, which used to
       escape as an OCaml failure rather than a lex error. *)
    raise (Fail (Printf.sprintf "invalid port :%s: must be 0-65535" digits))

(* ── Main tokeniser ─────────────────────────────────────────────────────── *)

let next_token s =
  let rec scan () =
    let l = s.line and c = s.col and o = s.pos + s.base in
    let loc = Token.point ~file:s.file l c o in
    s.tok_start <- loc;
    (* `ret` runs after its argument is scanned, so the state now sits just
       past the token -- exactly the exclusive end the loc records. *)
    let ret tok =
      (tok, { loc with Token.end_line = s.line; end_col = s.col;
                       end_offset = s.pos + s.base }) in
    if is_at_end s then ret EOF
    else match advance s with
    | ' ' | '\t' | '\r' -> scan ()
    | '\n' -> ret Newline
    | '"'  -> ret (read_string s)
    | '`'  -> ret (read_raw_string s)
    (* An open paren followed by a star is not wand syntax for anything, so
       the error below is a courtesy to someone arriving from OCaml. It
       yields where a real spelling needs that pair: a manifest word may be
       a pattern, and a narrowed label puts its paren directly before one.

       What follows the star tells them apart. A block comment opens with a
       star and a space, and a doc comment with a second star; a manifest
       pattern does neither, since a word beginning with two stars says no
       more than one. The cost is that an opener written with no space after
       the star loses the hint, which is not how anyone writes one. *)
    | '('  when peek s = '*'
                && (let c = peek2 s in
                    is_at_end s
                    || c = ' ' || c = '\t' || c = '\n' || c = '\r' || c = '*') ->
      raise (Fail "a comment is '-- ...' to the end of the line; write \
                       each line of this one with '--'")
    | '('  -> ret LParen
    | ')'  -> ret RParen
    | '['  -> ret LBracket
    | ']'  -> ret RBracket
    | '{'  -> ret LBrace
    | '}'  -> ret RBrace
    | ','  -> ret Comma
    | ';'  -> ret Semicolon
    | '?'  ->
      if not (is_at_end s) && (is_path_body_char (peek s) || is_glob_char (peek s)) then
        ret (read_path_body s "?")
      else
        ret Hole
    | '+' when peek s = '.' ->
      raise (Fail "operators are not spelled differently for Float -- \
                       there is no '+.'")
    | '+'  -> ret (if peek s = '+' then (ignore (advance s); PlusPlus) else Plus)
    | '*'  ->
      (* ** or *.foo etc → Glob; bare * → Star (multiplication) *)
      let prefix = if peek s = '*' then (ignore (advance s); "**") else "*" in
      if not (is_at_end s) && (is_path_body_char (peek s) || is_glob_char (peek s)) then
        (* `a *. b` reads as the glob "*." otherwise, and the type error it
           produces ("expected Glob, got ...") points nowhere near the
           mistake. A real glob always has more after the dot. *)
        (match read_path_body s prefix with
         | Glob "*." ->
           raise (Fail "operators are not spelled differently for Float \
                            -- there is no '*.'")
         | t -> ret t)
      else
        ret (if prefix = "**" then Glob "**" else Star)
    | '%'  -> ret Percent
    | '!'  -> ret (if peek s = '=' then (ignore (advance s); BangEq) else Bang)
    | '='  -> ret (if peek s = '=' then (ignore (advance s); EqEq)  else Eq)
    (* `<>` is inequality in several ML dialects and in SQL, so it is a
       reasonable thing to type. Saying which operator this language uses
       costs one line and saves the reader guessing from `unexpected '>'`. *)
    | '<' when peek s = '>' -> raise (Fail "the inequality operator is !=, not <>")
    | '<'  -> ret (if peek s = '=' then (ignore (advance s); LtEq)  else Lt)
    | '>'  -> ret (if peek s = '=' then (ignore (advance s); GtEq)  else Gt)
    | '&'  -> if peek s = '&' then ret (ignore (advance s); AmpAmp)
              else raise (Fail "unexpected '&'")
    | '^'  ->
      raise (Fail "string concatenation is '++', not '^'")
    | '|'  -> ret (match peek s with
               | '>' -> ignore (advance s); PipeArrow
               | '|' -> ignore (advance s); PipePipe
               | _   -> Pipe)
    | '-' when peek s = '.' ->
      raise (Fail "operators are not spelled differently for Float -- \
                       there is no '-.'")
    | '-'  ->
      ret (match peek s with
       | '>' -> ignore (advance s); Arrow
       | '-' -> LineComment (read_line_comment s)
       | _   -> Minus)
    (* `::` is cons. `:` gives a name a type, and reads a port when a digit
       follows it -- this branch runs first, so `::80` is a cons of 80 and
       never a port. *)
    | ':' when peek s = ':' -> ignore (advance s); ret DoubleColon
    | ':' when peek s = '=' ->
      raise (Fail "there is no mutation, so there is no ':=' -- \
                       let binds a new name instead")
    | ':'  -> ret (if is_digit (peek s) then read_port s else Colon)
    | '/' when peek s = '/' ->
      raise (Fail "a comment is '-- ...' to the end of the line, not '//'")
    | '/'  ->
      if not (is_at_end s) && (is_alpha (peek s) || is_digit (peek s)
                               || peek s = '_' || peek s = '.') then
        (* `a /. b` reads as the path "/." otherwise, and the type error it
           produces ("expected Path, got ...") points nowhere near the
           mistake. *)
        (match read_path_body s "/" with
         | Path "/." ->
           raise (Fail "operators are not spelled differently for Float \
                            -- there is no '/.'")
         | t -> ret t)
      else ret Slash
    | '.'  ->
      ret (match peek s with
       | '/' -> ignore (advance s); read_path_body s "./"
       | '.' when peek2 s = '/' ->
         ignore (advance s); ignore (advance s); read_path_body s "../"
       | '.' -> ignore (advance s); DotDot
       | _   -> Dot)
    | '$'  ->
      if peek s = '?' && peek2 s = '(' then (
        ignore (advance s); ignore (advance s);
        let raw = read_run_cmd ~form:"$?()" s in
        ret (match raw with
          | RunCmdRaw (parts, tail) -> RunQueryRaw (parts, tail)
          | t -> t))
      (* `$*(cmd)` is the command itself, unrun. It reads exactly as `$(cmd)`
         does -- same quoting, same interpolation -- and differs only in
         which token carries it. *)
      else if peek s = '*' && peek2 s = '(' then (
        ignore (advance s); ignore (advance s);
        let raw = read_run_cmd ~form:"$*()" s in
        ret (match raw with
          | RunCmdRaw (parts, tail) -> CommandRaw (parts, tail)
          | t -> t))
      else if peek s = '(' then (ignore (advance s); ret (read_run_cmd s))
      else if is_upper (peek s) then
        let buf = Buffer.create 8 in
        while not (is_at_end s) && (is_upper (peek s) || is_digit (peek s) || peek s = '_') do
          Buffer.add_char buf (advance s)
        done;
        ret (EnvVar (Buffer.contents buf))
      else ret Dollar
    | '~'  ->
      ret (if peek s = '/' then (ignore (advance s); read_path_body s "~/")
           else raise (Fail "unexpected '~'"))
    | '\'' ->
      if is_at_end s || not (is_lower (peek s)) then
        raise (Fail "expected a type variable like 'a after ''' -- and \
                         there are no character literals: a one-character \
                         string is \"x\"")
      else begin
        let buf = Buffer.create 8 in
        Buffer.add_char buf (advance s);
        while not (is_at_end s) && is_alnum_or_under (peek s) do
          Buffer.add_char buf (advance s)
        done;
        ret (TypeVar (Buffer.contents buf))
      end
    | c when is_digit c -> ret (read_numeric s c)
    | c when is_alpha c || c = '_' -> ret (read_ident s c)
    | '\\' when is_alpha (peek s) || peek s = '_' || peek s = '(' ->
      raise (Fail "a lambda is 'fn x -> ...', not '\\x -> ...'")
    (* The shebang on line one is handled in `tokenize`; any other `#` is a
       comment reflex from bash or Python. *)
    | '#' ->
      raise (Fail "a comment is '-- ...' to the end of the line, not '# ...'")
    | c -> raise (Fail (unexpected_character (char_at s) c))
  in
  scan ()

let tokenize ?(file = "") ?(line = 1) ?(col = 1) ?(base = 0) src =
  let s = make ~file ~line ~col ~base src in
  (* skip shebang line if present *)
  if String.length src >= 2 && src.[0] = '#' && src.[1] = '!' then
    while not (is_at_end s) && peek s <> '\n' do ignore (advance s) done;
  let toks = ref [] in
  let rec loop () =
    let (t, loc) =
      try next_token s
      with Fail msg ->
        (* The range runs from the failing token's start to wherever the
           scan stopped, so the whole partial token is marked. *)
        raise (LexError ({ s.tok_start with Token.end_line = s.line;
                           end_col = s.col; end_offset = s.pos + s.base }, msg))
    in
    toks := (t, loc) :: !toks;
    if t <> EOF then loop ()
  in
  loop ();
  List.rev !toks

let tokenize_plain src = List.map fst (tokenize src)
