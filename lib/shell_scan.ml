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

(* Where a `%{...}` splice lands, read from the shell context around it
   rather than from the quote the lexer happened to have open. The lexer
   saw one layer; the shell reads through several, and the value has to be
   quoted for the one it actually arrives in -- or, where no quoting is
   safe, the splice is refused. *)
type hole_ctx =
  | HArg               (* a word of its own: wrap in single quotes *)
  | HInside of char    (* inside the author's own '...' or "...": escape for it *)
  | HArith             (* an arithmetic operand: the value must be an Int *)
  | HErr of string     (* nothing here can carry a value safely; why *)

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

(* `[[` evaluates its `-eq`-style operands as arithmetic, reading a word's
   value as a number, so a value quoted as one argument is still re-read and
   the bracket cannot be bounded by a word list. It is control flow, refused
   under a narrowed manifest. (`((` is read as arithmetic in command position
   -- see the scan -- where a `%{}` operand is held to `Int`.) *)
let arith_commands = ["[["]

(* The quote a frame has open, which decides how a value spliced there is
   made safe. `QAnsi` is `$'...'`, where `\` escapes and the value cannot be
   quoted at all. *)
type quote = QNone | QSingle | QDouble | QAnsi

(* A command context the shell has entered: a `$(...)`, a backtick span, a
   `(...)` subshell, or an `$((...))`/`((...))` arithmetic. Each carries the
   quote open within it (fresh on entry, the shell having reset it) and,
   for arithmetic, the depth of its own inner parens. A hole reached inside
   a backtick frame cannot be quoted -- the backtick layer strips a
   backslash the value would need -- so `bt` is remembered. *)
type frame =
  { mutable q : quote; bt : bool; arith : bool; mutable pdepth : int;
    (* Opened in the value of a `NAME=` prefix: when it closes, the command
       word is still to come, so `X=$(date) git log` runs git. *)
    assign : bool }

(* One command template's atoms, holes kept apart from the text so look-ahead
   can step over one without reading into it. *)
type atom = Ch of char | Hole of int

let atoms_of (segs : seg list) : atom array * bool array =
  let atoms = ref [] and raws = ref [] and k = ref 0 in
  List.iter (fun seg ->
    match seg with
    | Lit s -> String.iter (fun c -> atoms := Ch c :: !atoms) s
    | QuotedHole -> atoms := Hole !k :: !atoms; raws := false :: !raws; incr k
    | RawHole    -> atoms := Hole !k :: !atoms; raws := true  :: !raws; incr k)
    segs;
  (Array.of_list (List.rev !atoms), Array.of_list (List.rev !raws))

(* The scan, over the atoms of one command template. `stop_at_raw` is set for
   the word list: after a `%!{...}` the rest of the line is shell source
   nothing can read, so the scan stops there. For hole contexts it is not
   set -- a raw splice is the author's own, and a `%{}` after it is still
   read for the quoting it needs, on the best-effort assumption the raw text
   left the quoting as it found it. *)
let run (atoms : atom array) (raws : bool array) ~stop_at_raw =
  let n = Array.length atoms in
  let words = ref [] in
  let buf = Buffer.create 16 in
  let has_hole = ref false in
  let expecting = ref true in      (* the next word is a command word *)
  let skip_target = ref false in   (* the next word is a redirection target *)
  let raw_tail = ref false in
  let at_word_start = ref true in  (* a `#` here opens a comment *)
  let comment = ref false in       (* in a `#` comment, to the line's end *)
  (* A heredoc the current line opened (`<<WORD`), waiting for its body to
     start at the next newline, and whether `<<-` stripped leading tabs. *)
  let hd_pending = ref None in
  (* The delimiter of the heredoc body being read now, with the line built
     so far to match it against. A `%{}` in a heredoc body cannot be made
     one argument -- `$(...)` in the body still expands whatever quoting the
     value carried -- so it is refused; the body text is otherwise data. *)
  let hd_active = ref None in
  let hd_strip = ref false in
  let hd_line = Buffer.create 16 in
  let ctxs = Array.make (Array.length raws) HArg in
  (* The rest of a `NAME=$(...)` prefix after its substitution closes: still
     an assignment, so the next word is still the command word. *)
  let assign_tail = ref false in
  (* Everything before the word's first `=` is plain text: no quote, no
     backslash, no hole, no substitution. The shell takes a word as an
     assignment only when its name is plain, so `a'v'=b` is the command
     `av=b`; the buffer holds the text with the quoting gone, and read alone
     it said assignment, which hid that command from the list. *)
  let name_plain = ref true in
  (* The command contexts open now, innermost first; the base frame is the
     line itself. *)
  let frames = ref [ { q = QNone; bt = false; arith = false; pdepth = 0; assign = false } ] in
  let cur () = List.hd !frames in
  let in_arith () = (cur ()).arith in
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
    (* Arithmetic operands are numbers, not commands. *)
    else if in_arith () then ()
    else if w = "" && not dynamic then ()
    else if !skip_target then skip_target := false
    else if not !expecting then ()
    (* Before the hole test: with the name plain, a hole is in the value,
       and the word is still a prefix. *)
    else if assign then ()  (* still expecting the command word *)
    else if dynamic then (words := Dynamic :: !words; expecting := false)
    else if List.mem w reserved || List.mem w arith_commands then
      (words := Compound w :: !words; expecting := false)
    else (words := Literal w :: !words; expecting := false)
  in
  (* A subshell `(...)`, a command substitution `$(...)` and a backtick span
     all run commands, and what runs is the whole question here, so each is
     scanned as a command position of its own rather than skipped as opaque.
     Skipping them was a way past the manifest: `Shell(echo)` admitted
     `$(echo $(whoami))`, which runs whoami. Each nested context pushes a
     frame of its own, with the quoting reset as the shell resets it, and
     contributes a word whose text arrives at run time, so a command word
     built from one reads as Dynamic.

     A `NAME=` prefix is the exception: its value runs nothing and leaves the
     command word still to come, so `x=$(echo) whoami` does not hide whoami
     behind a Dynamic word that nothing checked. *)
  (* `value` is set for a context whose output lands in the current word --
     a `$(...)` or a backtick span -- so that word reads as Dynamic, its text
     unknowable until the value arrives. A `(...)` subshell, a process
     substitution and an `$((...))` run or compute but do not fill a word, so
     they leave the word as it was. *)
  let push_frame ?(bt = false) ?(arith = false) ?(value = false) () =
    let assign =
      value && !expecting && not !skip_target && not (in_arith ())
      && assignment (Buffer.contents buf) in
    if assign then (Buffer.clear buf; has_hole := false; name_plain := true)
    else (if value then has_hole := true; finish_word ());
    frames := { q = QNone; bt; arith; pdepth = 0; assign } :: !frames;
    expecting := not arith;
    at_word_start := true
  in
  let leave () =
    finish_word ();
    let resumed =
      match !frames with
      | top :: (_ :: _ as rest) -> frames := rest; top.assign
      | _ -> false
    in
    (* A prefix's own `$(...)` leaves the command word still to come; any
       other `)` ends a command position. *)
    if resumed then (assign_tail := true; expecting := true)
    else expecting := false;
    at_word_start := true
  in
  let at j = if j >= 0 && j < n then (match atoms.(j) with Ch c -> c | Hole _ -> '\000') else '\000' in
  (* `$((...))` vs `$( (...) )`: the shell reads `$((` as arithmetic only
     when it closes with `))`. Look ahead over the atoms, counting parens
     from the two just opened and ignoring holes, to tell them apart before
     deciding what to do with the inside. `((whoami) )` closes early and is
     a subshell; the inside is then a command position. *)
  let closes_as_arithmetic start =
    let depth = ref 2 and j = ref start and prev = ref ' ' and prev2 = ref ' '
    and arith = ref false and stop = ref false in
    while not !stop && !j < n do
      (match atoms.(!j) with
       | Ch '(' -> incr depth; prev2 := !prev; prev := '('
       | Ch ')' -> decr depth; prev2 := !prev; prev := ')';
         if !depth = 0 then
           (arith := (!prev = ')' && !prev2 = ')'); stop := true)
       | Ch c -> prev2 := !prev; prev := c
       | Hole _ -> prev2 := !prev; prev := 'x');
      incr j
    done;
    !arith
  in
  let hole_ctx () =
    if !comment then HErr "a `%{}` value cannot go inside a `#` comment"
    else
      let f = cur () in
      if f.q = QAnsi then
        HErr "a `%{}` value cannot be quoted inside a $'...' string; splice \
              it outside the quotes"
      else if f.bt then
        HErr "a `%{}` value cannot be quoted inside backticks; use $(...) \
              instead, where it is one argument"
      else if f.arith then HArith
      else match f.q with
        | QNone   -> HArg
        | QSingle -> HInside '\''
        | QDouble -> HInside '"'
        | QAnsi   -> assert false
  in
  let i = ref 0 in
  let peek_ch k = at (!i + k) in
  (try
    while !i < n do
      (match atoms.(!i) with
       | Hole k ->
         if raws.(k) then begin
           raw_tail := true;
           if stop_at_raw then begin
             if !expecting && not (in_arith ()) then words := Dynamic :: !words;
             raise Exit
           end
         end else if !hd_active <> None then
           ctxs.(k) <- HErr "a `%{}` value cannot go inside a heredoc body; \
                             pipe it in with |> instead"
         else begin
           ctxs.(k) <- hole_ctx ();
           mark_name (); has_hole := true; at_word_start := false
         end;
         incr i
       | Ch c ->
         (match !hd_active with
          | Some delim ->
            (* In a heredoc body: match the delimiter line, and keep the
               body otherwise as data. *)
            if c = '\n' then begin
              let line = Buffer.contents hd_line in
              let line = if !hd_strip then
                  (let j = ref 0 in
                   while !j < String.length line && line.[!j] = '\t' do incr j done;
                   String.sub line !j (String.length line - !j))
                else line in
              Buffer.clear hd_line;
              if line = delim then hd_active := None
            end
            else Buffer.add_char hd_line c;
            incr i
          | None ->
         let f = cur () in
         if !comment then begin
           if c = '\n' then (comment := false; finish_word ();
                             expecting := true; at_word_start := true);
           incr i
         end
         else if f.q = QSingle then begin
           if c = '\'' then f.q <- QNone else Buffer.add_char buf c;
           incr i
         end
         else if f.q = QAnsi then begin
           (* `$'...'`: `\` spends the next character, and only an unescaped
              `'` ends it. Read as a plain single quote, a `\'` inside closed
              it early and desynced the scan. *)
           if c = '\\' && !i + 1 < n then (Buffer.add_char buf (peek_ch 1); i := !i + 2)
           else (if c = '\'' then f.q <- QNone else Buffer.add_char buf c; incr i)
         end
         else if f.q = QDouble then begin
           if c = '\\' && !i + 1 < n then
             (Buffer.add_char buf c; Buffer.add_char buf (peek_ch 1); i := !i + 2)
           else if c = '$' && peek_ch 1 = '(' && peek_ch 2 = '(' then
             (if closes_as_arithmetic (!i + 3)
              then (i := !i + 3; push_frame ~arith:true ())
              else (i := !i + 2; push_frame ~value:true ()))
           else if c = '$' && peek_ch 1 = '(' then (i := !i + 2; push_frame ~value:true ())
           else if c = '`' then
             (incr i; if f.bt then leave () else push_frame ~bt:true ~value:true ())
           else (if c = '"' then f.q <- QNone else Buffer.add_char buf c; incr i)
         end
         else begin
           (* Unquoted. *)
           match c with
           | '#' when !at_word_start -> comment := true; incr i
           | '\'' -> mark_name (); f.q <- QSingle; at_word_start := false; incr i
           | '"'  -> mark_name (); f.q <- QDouble; at_word_start := false; incr i
           | '$' when peek_ch 1 = '\'' ->
             mark_name (); f.q <- QAnsi; at_word_start := false; i := !i + 2
           | '`' -> incr i; if f.bt then leave () else push_frame ~bt:true ~value:true ()
           | '$' when peek_ch 1 = '(' && peek_ch 2 = '(' ->
             if closes_as_arithmetic (!i + 3)
             then (i := !i + 3; push_frame ~arith:true ())
             else (i := !i + 2; push_frame ~value:true ())
           | '$' when peek_ch 1 = '(' -> i := !i + 2; push_frame ~value:true ()
           (* Inside arithmetic, parens are its own; outside, `((` opens a
              nested arithmetic and a lone `(` a subshell. *)
           | '(' when f.arith -> f.pdepth <- f.pdepth + 1; incr i
           | '(' when peek_ch 1 = '(' && closes_as_arithmetic (!i + 2) ->
             i := !i + 2; push_frame ~arith:true ()
           | '(' -> i := !i + 1; push_frame ()
           | ')' when f.arith && f.pdepth > 0 -> f.pdepth <- f.pdepth - 1; incr i
           | ')' when f.arith ->
             (* The first `)` of the closing `))`, which the look-ahead
                promised. *)
             i := !i + 2; leave ()
           | ')' -> leave (); incr i
           | '\\' when !i + 1 < n ->
             mark_name (); Buffer.add_char buf (peek_ch 1); at_word_start := false;
             i := !i + 2
           | ' ' | '\t' -> finish_word (); at_word_start := true; incr i
           (* A newline separates two commands, exactly as `;` does -- and
              starts a heredoc body the line before it opened. *)
           | '\n' ->
             finish_word (); expecting := true; at_word_start := true;
             (match !hd_pending with
              | Some (d, strip) ->
                hd_active := Some d; hd_strip := strip;
                hd_pending := None; Buffer.clear hd_line
              | None -> ());
             incr i
           | '|' -> finish_word (); expecting := true; at_word_start := true;
                    i := !i + (if peek_ch 1 = '|' || peek_ch 1 = '&' then 2 else 1)
           | '&' -> finish_word (); expecting := true; at_word_start := true;
                    i := !i + (if peek_ch 1 = '&' then 2 else 1)
           | ';' -> finish_word (); expecting := true; at_word_start := true; incr i
           | ('<' | '>') when peek_ch 1 = '(' ->
             (* Process substitution `<(cmd)` / `>(cmd)`: a command of its
                own, not a redirection to a file. *)
             finish_word (); i := !i + 2; push_frame ()
           | '<' when peek_ch 1 = '<' && peek_ch 2 <> '<' ->
             (* A heredoc, `<<WORD` or `<<-WORD`, the delimiter maybe quoted.
                The command before it is already read; its body, after the
                next newline, is data a `%{}` cannot safely enter. *)
             finish_word ();
             let j = ref (!i + 2) in
             let strip = at !j = '-' in
             if strip then incr j;
             while at !j = ' ' || at !j = '\t' do incr j done;
             let delim = Buffer.create 8 in
             (if at !j = '\'' || at !j = '"' then begin
                let q = at !j in incr j;
                while !j < n && at !j <> q do Buffer.add_char delim (at !j); incr j done;
                if !j < n then incr j
              end else
                while !j < n &&
                      (let d = at !j in
                       d <> ' ' && d <> '\t' && d <> '\n' && d <> ';'
                       && d <> '|' && d <> '&' && d <> ')' && d <> '<' && d <> '>')
                do Buffer.add_char delim (at !j); incr j done);
             hd_pending := Some (Buffer.contents delim, strip);
             skip_target := false; at_word_start := true;
             i := !j
           | '<' | '>' ->
             (* A redirection: any digits gathered so far are its fd prefix,
                not a word. `>&2`-style forms carry their own target. *)
             if Buffer.contents buf <> ""
                && String.for_all (fun d -> d >= '0' && d <= '9')
                     (Buffer.contents buf)
             then Buffer.clear buf
             else finish_word ();
             let j = ref (!i + 1) in
             if at !j = '>' || at !j = '<' then incr j;
             if at !j = '&' then begin
               incr j;
               while at !j >= '0' && at !j <= '9' do incr j done
             end else skip_target := true;
             at_word_start := true;
             i := !j
           | _ -> Buffer.add_char buf c; at_word_start := false; incr i
         end))
    done;
    finish_word ()
  with Exit -> ());
  ({ words = List.rev !words; raw_tail = !raw_tail }, ctxs)

let scan (segs : seg list) : scan =
  let (atoms, raws) = atoms_of segs in
  fst (run atoms raws ~stop_at_raw:true)

(* The context each `%{...}` lands in, one per hole in order, read from the
   whole command template rather than from the quote the lexer had open. *)
let hole_contexts (segs : seg list) : hole_ctx array =
  let (atoms, raws) = atoms_of segs in
  snd (run atoms raws ~stop_at_raw:false)

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
           text either way: it cannot introduce an operator. An arithmetic
           operand is a number, which is one word too. *)
        | Token.Arg | Token.Inside _ | Token.Arith -> QuotedHole)]) parts
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
