(* One diagnostic, as every consumer sees it: the CLI's text, `--json`, and
   the language server all render from this shape. Positions travel here as
   data from wherever they are known -- never recovered by parsing a message
   string, which is what this module exists to end. *)

type severity = Error | Warning

(* A machine-applicable correction, carried alongside the human text so a
   tool can fix without re-parsing prose. `InsertLine`/`ReplaceLine` come
   from lint findings; `Replace` is the drift errors' plain substitution. *)
type fix =
  | InsertLine  of string   (* a line the file lacks (the manifest) *)
  | ReplaceLine of string   (* the corrected form of the flagged line *)
  | DeleteLine              (* the flagged line should not exist (a dead import) *)
  | Replace     of { from_ : string; to_ : string }
  (* Put this at the end of the flagged line. The correction is known
     without reading the line, which is what separates it from
     `ReplaceLine`: the rule that asks for it does not have the source. *)
  | AppendToLine of string

type t = {
  severity : severity;
  code     : string;             (* "E-TYPE", "V-DROP1", ... *)
  loc      : Token.loc option;   (* None renders as 1:1 in JSON *)
  message  : string;             (* bare text: no label, no position *)
  fix      : fix option;
  (* Further diagnostics found in the same pass, each standing on its own.
     A check answers with one diagnostic everywhere, so the rest travel with
     the first rather than changing what every consumer takes. `all` is what
     a consumer that can show more than one reads. *)
  others   : t list;
}

(* ── Construction ────────────────────────────────────────────────────────── *)

let contains msg needle =
  let n = String.length needle and m = String.length msg in
  let rec go i = i + n <= m && (String.sub msg i n = needle || go (i + 1)) in
  go 0

(* The drift errors name their correction in prose; where the correction is
   a plain textual substitution, carry it as data too. Keyed on fragments
   test_drift.ml locks, so rewording a message that breaks a key breaks a
   test alongside it. *)
let drift_fixes = [
  "not '//'",                     ("//", "--");
  "not '# ...'",                  ("#", "--");
  "not ${...}",                   ("${", "%{");
  "not #{...}",                   ("#{", "%{");
  "drop the 'rec'",               ("let rec", "let");
  "not '^'",                      ("^", "++");
  "boolean operator is '&&'",     ("and", "&&");
  "boolean operator is '||'",     ("or", "||");
  "boolean not is '!'",           ("not", "!");
]

(* An explicit `fix` (a structured correction the raise site computed)
   wins; otherwise the drift table is consulted. *)
let error ~code ?loc ?fix message =
  let fix =
    match fix with
    | Some _ -> fix
    | None ->
      (match List.find_opt (fun (frag, _) -> contains message frag) drift_fixes with
       | Some (_, (from_, to_)) -> Some (Replace { from_; to_ })
       | None -> None)
  in
  { severity = Error; code; loc; message; fix; others = [] }

(* The diagnostic and the ones that travel with it, in the order they were
   found. A consumer that shows one takes the head and is unchanged. *)
let all d = d :: d.others

(* ── Rendering ───────────────────────────────────────────────────────────── *)

(* The error strings the CLI has always printed: "type error: 3:5: text",
   the label dropped for a plain failure. Everything that still hands the
   caller a string renders it from here, so the text output cannot drift
   from the structured form. *)
(* A diagnostic quotes the source it is about, and a terminal reads control
   bytes in what it prints: an ESC in a command word cleared the screen and
   hid the text after it. The bytes wand's own messages use -- newline and
   tab -- are kept; every other C0 control and DEL is shown as `\xNN`, so
   the message reads as text and runs nothing. *)
let sanitize s =
  let buf = Buffer.create (String.length s) in
  String.iter (fun c ->
    let n = Char.code c in
    if (n < 0x20 && c <> '\n' && c <> '\t') || n = 0x7f
    then Buffer.add_string buf (Printf.sprintf "\\x%02x" n)
    else Buffer.add_char buf c) s;
  Buffer.contents buf

let legacy d =
  let label = match d.code with
    | "E-LEX"   -> "lex error: "
    | "E-PARSE" -> "parse error: "
    | "E-TYPE"  -> "type error: "
    | _         -> ""
  in
  let pos = match d.loc with
    | Some l when d.code = "E-PACKAGE" && l.Token.file <> "" ->
      Printf.sprintf "%s:%d:%d: " l.Token.file l.Token.line l.Token.col
    | Some l -> Printf.sprintf "%d:%d: " l.Token.line l.Token.col
    | None   -> ""
  in
  sanitize (label ^ pos ^ d.message)

let escape_json s =
  let buf = Buffer.create (String.length s + 8) in
  String.iter (fun c ->
    match c with
    | '"'  -> Buffer.add_string buf "\\\""
    | '\\' -> Buffer.add_string buf "\\\\"
    | '\n' -> Buffer.add_string buf "\\n"
    | '\t' -> Buffer.add_string buf "\\t"
    | c when Char.code c < 0x20 ->
      Buffer.add_string buf (Printf.sprintf "\\u%04x" (Char.code c))
    | c -> Buffer.add_char buf c) s;
  Buffer.contents buf

let fix_json = function
  | InsertLine l  -> Printf.sprintf "{\"insert_line\":\"%s\"}" (escape_json l)
  | ReplaceLine l -> Printf.sprintf "{\"replace_line\":\"%s\"}" (escape_json l)
  | DeleteLine    -> "{\"delete_line\":true}"
  | AppendToLine l ->
    Printf.sprintf "{\"append_to_line\":\"%s\"}" (escape_json l)
  | Replace { from_; to_ } ->
    Printf.sprintf "{\"replace\":{\"from\":\"%s\",\"to\":\"%s\"}}"
      (escape_json from_) (escape_json to_)

(* The object shape is part of the CLI's contract (reference.md, "REPL and
   CLI"): `severity`, `code`, `line`, `col`, `message`; `file` when a file
   was named; `fix` when a correction exists. *)
let to_json ?file d =
  let severity = match d.severity with Error -> "error" | Warning -> "warning" in
  let file_field = match file with
    | None -> ""
    | Some f -> Printf.sprintf "\"file\":\"%s\"," (escape_json f)
  in
  let (line, col) = match d.loc with
    | Some l -> (l.Token.line, l.Token.col)
    | None   -> (1, 1)
  in
  (* A loc with width carries its end too; a point loc (or none) keeps the
     original object shape, so consumers that never asked for ranges see
     exactly what they always saw. *)
  let end_fields = match d.loc with
    | Some l when l.Token.end_offset > l.Token.offset ->
      Printf.sprintf "\"end_line\":%d,\"end_col\":%d,"
        l.Token.end_line l.Token.end_col
    | _ -> ""
  in
  Printf.sprintf
    "{\"severity\":\"%s\",\"code\":\"%s\",%s\"line\":%d,\"col\":%d,%s\"message\":\"%s\"%s}"
    severity d.code file_field line col end_fields (escape_json d.message)
    (match d.fix with None -> "" | Some fx -> ",\"fix\":" ^ fix_json fx)

let to_json_array ?file ds =
  "[" ^ String.concat "," (List.map (to_json ?file) ds) ^ "]"
