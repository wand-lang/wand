(* The wand language server: LSP over stdio, which is Content-Length
   framing plus JSON-RPC. Hand-rolled over yojson on purpose -- the v1
   surface is a handful of methods, and the opam `lsp` package would be
   the largest dependency in the tree, moving with ocaml-lsp's needs
   rather than ours.

   One loop, single-threaded: read a message, handle it, write what it
   produced. Every answer costs milliseconds (the whole buffer is
   re-checked per change; wand files are small), so there is no request
   queue and no cancellation machinery.

   `handle` is the whole protocol as a function -- state and one incoming
   message to new state and outgoing messages -- so the tests feed it
   parsed JSON directly; `serve` is only the stdio instantiation. *)

module J = Yojson.Safe

(* ── JSON access, total ──────────────────────────────────────────────────── *)

let mem k = function
  | `Assoc l -> (match List.assoc_opt k l with Some v -> v | None -> `Null)
  | _ -> `Null

let str = function `String s -> Some s | _ -> None
let int_ = function `Int n -> Some n | _ -> None

(* ── Framing ─────────────────────────────────────────────────────────────── *)

(* Header lines arrive "\r\n"-terminated; `input_line` leaves the '\r'. *)
let trim_cr s =
  let n = String.length s in
  if n > 0 && s.[n - 1] = '\r' then String.sub s 0 (n - 1) else s

(* The largest body this will read. A `didChange` carries a whole document,
   so the bound is generous; what it is for is a header that names a length
   no client meant to send. `Content-Length: 999999999999999` asked the
   runtime for a terabyte of bytes and ended the server with "Out of
   memory". *)
let max_content_length = 64 * 1024 * 1024

(* Digits only, and a length that could be a body. `int_of_string` also reads
   `0x10` and `-1`, and a negative length reached `Bytes.create`, which ends
   the server with `Invalid_argument`. *)
let content_length_of line =
  let lower = String.lowercase_ascii line in
  let prefix = "content-length:" in
  if String.length lower >= String.length prefix
     && String.sub lower 0 (String.length prefix) = prefix
  then
    let digits =
      String.trim
        (String.sub line (String.length prefix)
           (String.length line - String.length prefix))
    in
    if digits <> "" && String.for_all (fun c -> c >= '0' && c <= '9') digits
    then
      match int_of_string_opt digits with
      | Some n when n >= 0 && n <= max_content_length -> Some n
      | _ -> None
    else None
  else None

(* What one read of the stream produced.

   A body that is not JSON leaves the framing intact, so the session goes on
   and the client hears about the one message. Anything else -- the stream
   ended, or a header named a length this will not read -- has lost the
   framing, and a transport that has lost framing cannot be resynchronized.

   Three cases, not two: read as one, an unparseable body is the client
   having gone away, and the server exits -- every later request goes
   unanswered and the editor shows no diagnostics for the rest of the
   session, with nothing to say why. *)
type frame =
  | Frame of J.t
  | Bad_body
  | Ended

let read_message ic : frame =
  let rec headers len =
    match input_line ic with
    | exception End_of_file -> None
    | line ->
      let line = trim_cr line in
      if line = "" then Some len
      else headers (match content_length_of line with Some n -> Some n | None -> len)
  in
  match headers None with
  | None | Some None -> Ended
  | Some (Some n) ->
    (match really_input_string ic n with
     | exception End_of_file -> Ended
     | body ->
       (match J.from_string body with
        | json -> Frame json
        | exception _ -> Bad_body))

let write_message oc (json : J.t) =
  let s = J.to_string json in
  output_string oc (Printf.sprintf "Content-Length: %d\r\n\r\n" (String.length s));
  output_string oc s;
  flush oc

(* ── Messages out ────────────────────────────────────────────────────────── *)

let response id result : J.t =
  `Assoc [("jsonrpc", `String "2.0"); ("id", id); ("result", result)]

let error_response id code message : J.t =
  `Assoc [("jsonrpc", `String "2.0"); ("id", id);
          ("error", `Assoc [("code", `Int code); ("message", `String message)])]

let notification method_ params : J.t =
  `Assoc [("jsonrpc", `String "2.0"); ("method", `String method_);
          ("params", params)]

(* ── Diagnostics ─────────────────────────────────────────────────────────── *)

(* wand positions are 1-based extents; LSP ranges are 0-based. A diagnostic
   without a position lands at the top of the file. *)
let range_of_loc (loc : Token.loc option) : J.t =
  let pos line col =
    `Assoc [("line", `Int (max 0 (line - 1)));
            ("character", `Int (max 0 (col - 1)))]
  in
  match loc with
  | None -> `Assoc [("start", pos 1 1); ("end", pos 1 1)]
  | Some l ->
    let end_ =
      if l.Token.end_offset > l.Token.offset
      then pos l.Token.end_line l.Token.end_col
      else pos l.Token.line l.Token.col
    in
    `Assoc [("start", pos l.Token.line l.Token.col); ("end", end_)]

let json_of_diag (d : Diag.t) : J.t =
  `Assoc [
    ("range", range_of_loc d.Diag.loc);
    ("severity", `Int (match d.Diag.severity with Diag.Error -> 1 | Diag.Warning -> 2));
    ("code", `String d.Diag.code);
    ("source", `String "wand");
    ("message", `String d.Diag.message);
  ]

(* file:// URIs carry a percent-encoded filesystem path; anything else
   (untitled: buffers) keeps its tail as a relative name, which
   `typecheck_source` resolves against the working directory. *)
let percent_decode s =
  let buf = Buffer.create (String.length s) in
  let n = String.length s in
  let hex c =
    match c with
    | '0'..'9' -> Some (Char.code c - Char.code '0')
    | 'a'..'f' -> Some (Char.code c - Char.code 'a' + 10)
    | 'A'..'F' -> Some (Char.code c - Char.code 'A' + 10)
    | _ -> None
  in
  let i = ref 0 in
  while !i < n do
    (match s.[!i] with
     | '%' when !i + 2 < n ->
       (match hex s.[!i + 1], hex s.[!i + 2] with
        | Some h, Some l -> Buffer.add_char buf (Char.chr ((h lsl 4) lor l)); i := !i + 2
        | _ -> Buffer.add_char buf '%')
     | c -> Buffer.add_char buf c);
    incr i
  done;
  Buffer.contents buf

let path_of_uri uri =
  let file_prefix = "file://" in
  if String.length uri > String.length file_prefix
     && String.sub uri 0 (String.length file_prefix) = file_prefix
  then
    percent_decode
      (String.sub uri (String.length file_prefix)
         (String.length uri - String.length file_prefix))
  else
    match String.index_opt uri ':' with
    | Some i -> String.sub uri (i + 1) (String.length uri - i - 1)
    | None -> uri

(* What the Problems pane shows for one buffer: the check's errors, or its
   findings. Holes wait for locations; a diagnostic that cannot point
   anywhere is noise, not information.

   A check answers with one diagnostic and the ones that travel with it --
   every unbound name in the buffer, where there are several -- so the pane
   lists them all rather than one at a time. *)
let analyze uri text : Runner.source_check option * Diag.t list =
  match Runner.typecheck_source ~path:(path_of_uri uri) text with
  | Error d -> (None, Diag.all d)
  | Ok sc -> (Some sc, List.map (Lint.to_diag ~strict:false) sc.Runner.sc_findings)

let publish uri diags =
  notification "textDocument/publishDiagnostics"
    (`Assoc [("uri", `String uri);
             ("diagnostics", `List (List.map json_of_diag diags))])

(* ── The protocol ────────────────────────────────────────────────────────── *)

(* One open buffer. `check` is the last *successful* check -- kept through
   failed re-checks, because hover and completion are asked mid-keystroke,
   exactly when the buffer is most likely to be momentarily broken; names
   move rarely enough that the previous scope answers honestly. *)
type doc = {
  d_text  : string;
  d_check : Runner.source_check option;
  d_diags : Diag.t list;            (* as last published *)
}

type state = {
  docs : (string * doc) list;       (* uri -> buffer *)
  shutdown_seen : bool;
  quit : int option;                (* Some code once `exit` arrives *)
  next_req : int;                   (* ids for server->client requests *)
  (* Whether a change that completes a resolvable name may earn its import
     and its manifest labels without being asked. On, because that is what
     the tier is for and what it has always done; a switch because it is the
     one thing here that edits a buffer nobody asked it to edit. The client
     sends it in `initializationOptions` and again on
     `workspace/didChangeConfiguration`. *)
  auto_edit : bool;
}

let initial =
  { docs = []; shutdown_seen = false; quit = None; next_req = 1;
    auto_edit = true }

(* `wand.autoEdit` wherever the client puts it: under `initializationOptions`
   it arrives bare, and under `settings` it arrives beneath the section name.
   Absent leaves the setting alone, so a client that sends neither keeps the
   default. *)
let auto_edit_of (v : J.t) : bool option =
  let rec look v =
    match v with
    | `Assoc kvs ->
      (match List.assoc_opt "autoEdit" kvs with
       | Some (`Bool b) -> Some b
       | _ ->
         (match List.assoc_opt "wand" kvs with
          | Some inner -> look inner
          | None -> None))
    | _ -> None
  in
  look v

let capabilities : J.t =
  `Assoc [
    ("capabilities", `Assoc [
       (* Full-text sync: the whole buffer per change, which is what the
          checker consumes anyway. Nothing here is incremental. *)
       ("textDocumentSync", `Assoc [("openClose", `Bool true);
                                    ("change", `Int 1)]);
       ("hoverProvider", `Bool true);
       ("completionProvider", `Assoc [("triggerCharacters", `List [`String "."])]);
       ("codeActionProvider", `Bool true);
       ("codeLensProvider", `Assoc [("resolveProvider", `Bool false)]);
       ("definitionProvider", `Bool true);
       ("documentFormattingProvider", `Bool true);
     ]);
    ("serverInfo", `Assoc [("name", `String "wand");
                           ("version", `String Version.value)]);
  ]

(* ── Text geometry ───────────────────────────────────────────────────────── *)

(* Columns here are byte indexes, like everywhere else in the pipeline; a
   non-ASCII line can be off by the UTF-16 difference, which is the same
   approximation the published diagnostics have always made. *)
let lines_of = String.split_on_char '\n'

(* `List.nth_opt` answers None for a line past the end and raises for a
   negative one. A position is whatever the client sent, so both are just a
   line that is not there. *)
let line_at lines l = if l < 0 then None else List.nth_opt lines l

let pos0 line character : J.t =
  `Assoc [("line", `Int line); ("character", `Int character)]

let range0 l1 c1 l2 c2 : J.t =
  `Assoc [("start", pos0 l1 c1); ("end", pos0 l2 c2)]

let text_edit range newText : J.t =
  `Assoc [("range", range); ("newText", `String newText)]

(* A line-level edit as the protocol wants it. Inserting past the last line
   only happens when the file lacks a final newline; the text then rides in
   after a leading newline at the end of that line. *)
let json_of_line_edit lines (e : Autoedit.edit) : J.t =
  let n_lines = List.length lines in
  match e with
  | Autoedit.Insert_line (n, text) when n > n_lines ->
    let last = match line_at lines (n_lines - 1) with Some l -> l | None -> "" in
    let c = String.length last in
    text_edit (range0 (n_lines - 1) c (n_lines - 1) c) ("\n" ^ text)
  | Autoedit.Insert_line (n, text) ->
    text_edit (range0 (n - 1) 0 (n - 1) 0) (text ^ "\n")
  | Autoedit.Replace_line (n, text) ->
    let old = match line_at lines (n - 1) with Some l -> l | None -> "" in
    text_edit (range0 (n - 1) 0 (n - 1) (String.length old)) text

let workspace_edit uri edits : J.t =
  `Assoc [("changes", `Assoc [(uri, `List edits)])]

(* ── Names under the cursor ──────────────────────────────────────────────── *)

let is_word_char c = Autoedit.is_ident_continuation c || c = '.'

(* The (possibly qualified) name the position touches, with its extent on
   the line: `List.map!` hovered anywhere inside answers the whole name. *)
let word_at line_text character : (string * int * int) option =
  let n = String.length line_text in
  let i = min (max character 0) n in
  let l = ref i and r = ref i in
  while !l > 0 && is_word_char line_text.[!l - 1] do decr l done;
  while !r < n && is_word_char line_text.[!r] do incr r done;
  if !r <= !l then None
  else Some (String.sub line_text !l (!r - !l), !l, !r)

let scope_of (d : doc) =
  match d.d_check with Some sc -> sc.Runner.sc_scope | None -> []

let docs_of (d : doc) =
  match d.d_check with Some sc -> sc.Runner.sc_docs | None -> []

(* What a name means where the buffer stands: its scheme and doc string.
   Scope first; a qualified name whose namespace is not in scope is
   answered from the standard library's signature -- the same modules the
   auto-import tier can bring in. *)
(* An operation a handler catches. Nothing about it is in the scope -- it is
   not a name a script can bind -- so its answer is assembled from the
   operations table: the shape a case binds and resumes with, the effect
   catching it accounts for, and what a script writes to perform it. That
   last is the part a reader cannot work out from the name, and the reason
   this is worth answering at all. *)
let describe_operation name : (string * string option) option =
  match Typechecker.find_operation name with
  | None -> None
  | Some op ->
    let effect_name = Effect_set.name_of op.Typechecker.op_effect in
    let detail =
      match op.Typechecker.op_types () with
      | Some (payload, resume) ->
        Typechecker.string_of_typ payload ^ " -> " ^ Typechecker.string_of_typ resume
      | None -> "payload left open"
    in
    let performers =
      match List.map (fun p -> "`" ^ p ^ "`") op.Typechecker.op_performers with
      | [] -> ""
      | [one] -> one
      | many ->
        let rec commas = function
          | [a; b] -> a ^ " and " ^ b
          | x :: rest -> x ^ ", " ^ commas rest
          | [] -> ""
        in commas many
    in
    let binds =
      match op.Typechecker.op_types () with
      | Some (payload, resume) ->
        Printf.sprintf "A case binds `%s` and resumes with `%s`."
          (Typechecker.string_of_typ payload) (Typechecker.string_of_typ resume)
      | None ->
        "This operation carries more than one shape, so a case is not \
         checked against a payload type."
    in
    let doc =
      if performers = "" then
        Printf.sprintf
          "Handles the `%s` effect. Nothing a script can write performs this \
           operation, so a case for it will not fire.\n\n%s" effect_name binds
      else
        Printf.sprintf "Handles the `%s` effect of %s.\n\n%s"
          effect_name performers binds
    in
    Some (detail, Some doc)

(* An operation first -- the table is the only place they are described, and
   nothing else can answer for one. A name that is not in it falls through,
   so a script's own `deploy!` is still looked up in scope. *)
let describe (d : doc) word : (string * string option) option =
  match describe_operation word with
  | Some answer -> Some answer
  | None ->
  let scope = scope_of d in
  match String.split_on_char '.' word with
  | [ns; m] when m <> "" ->
    (match List.assoc_opt ns scope with
     | Some (Typechecker.Namespace (members, _)) ->
       (match List.assoc_opt m members with
        | Some s -> Some (Typechecker.string_of_scheme s,
                          List.assoc_opt word (docs_of d))
        | None -> None)
     | _ ->
       (match Runner.stdlib_module_sig ns with
        | Some (env, sdocs) ->
          (match List.assoc_opt m env with
           | Some s -> Some (Typechecker.string_of_scheme s,
                             List.assoc_opt m sdocs)
           | None -> None)
        | None -> None))
  | [plain] when plain <> "" ->
    (match List.assoc_opt plain scope with
     | Some (Typechecker.Namespace _) -> Some ("module", None)
     | Some s -> Some (Typechecker.string_of_scheme s,
                       List.assoc_opt plain (docs_of d))
     | None ->
       if List.mem_assoc plain Stdlib_embed.table
       then Some ("module", None)
       else None)
  | _ -> None

(* A label in the manifest names an effect, and answering for it from the
   scope was wrong in both directions: `Env` and `IO` came back as the
   modules they also are, when what the line declares is the effect their
   calls perform, and `FS.Read`, `Raise` and `Proc` name no module at all,
   so they came back as nothing. Inside the manifest's extent the label is
   the only thing the word can mean, so it is answered before the scope is
   consulted. *)
let describe_manifest_label (d : doc) word line0 : (string * string option) option =
  match d.d_check with
  | Some { Runner.sc_manifest = Some loc; _ }
    when loc.Token.line <= line0 + 1 && line0 + 1 <= loc.Token.end_line ->
    Effect_set.of_name word
    |> Option.map (fun e ->
         ("effect",
          Some (Effect_set.description e
                ^ "\n\nThe manifest is the bound on what this file may do. "
                ^ "Doing more than it says is a type error, and declaring "
                ^ "more than the file does is an `A-USES1` warning; "
                ^ "`wand t --fix` writes the line the file has earned.")))
  | _ -> None

(* A name the scope does not know: a local -- parameter, `let ... in`,
   pattern variable. Locals carry no positions, so the item enclosing the
   cursor stands in for lexical scope: right whenever the item does not
   rebind the name, and the innermost binding wins when it does. *)
let describe_local (d : doc) word line0 : (string * string option) option =
  match d.d_check with
  | None -> None
  | Some sc ->
    let line = line0 + 1 in
    List.find_opt (fun ((loc : Token.loc), _) ->
      loc.Token.line <= line && line <= loc.Token.end_line)
      sc.Runner.sc_locals
    |> Option.map snd
    |> fun locals ->
       Option.bind locals (List.assoc_opt word)
       |> Option.map (fun t -> (t, None))

(* A doc string is markdown to the client, and its examples are not: `>> `
   opens two blockquotes, and what the example produces -- the line under
   the prompt -- is a lazy continuation of the same paragraph, so the
   expression and its answer arrive on one line with the prompt eaten.
   Each example goes in a fence instead, where the prompt is a prompt and
   the answer keeps its own line, the way `wand d` prints it. The fence is
   left unlabelled: a transcript is not wand source, and a highlighter told
   otherwise colours the prompt as an operator. *)
let doc_markdown text =
  let out = Buffer.create (String.length text + 32) in
  let line l = Buffer.add_string out l; Buffer.add_char out '\n' in
  List.iter (function
    | Runner.Prose l -> line l
    | Runner.Example (expr, expected) ->
      line "```";
      List.iteri (fun i l -> line ((if i = 0 then ">> " else ".. ") ^ l))
        (String.split_on_char '\n' expr);
      List.iter line expected;
      line "```")
    (Runner.doc_blocks text);
  String.trim (Buffer.contents out)

(* The name on its own line, the type under it: long signatures (a wide
   effect set especially) stay readable instead of wrapping mid-type. A
   module and an effect have no signature to put there, so the kind takes
   the place of one. *)
let hover_markdown word (typ, doc) =
  let head =
    if typ = "module" || typ = "effect"
    then Printf.sprintf "```wand\n%s %s\n```" typ word
    else Printf.sprintf "```wand\n%s\n: %s\n```" word typ
  in
  match doc with
  | Some text -> head ^ "\n\n---\n\n" ^ doc_markdown text
  | None -> head

(* ── Definition ──────────────────────────────────────────────────────────── *)

(* Standard library sources are not files on the user's machine -- they are
   carried in the binary -- so a jump into one lands in a virtual document
   the client asks the server to fill (`wand/stdlibSource`). The URI names
   the module; the content comes from the same bytes the definition sites
   were computed from. *)
let stdlib_uri name = "wand-stdlib:/" ^ name ^ ".wand"

let location target_uri (loc : Token.loc) : J.t =
  let l = max 0 (loc.Token.line - 1) and c = max 0 (loc.Token.col - 1) in
  `Assoc [("uri", `String target_uri); ("range", range0 l c l c)]

(* Where the name under the cursor is defined: the buffer's own definition
   sites first; a qualified name whose namespace is a standard library
   module jumps into its virtual document (to the member, or to the top
   when the member is not a definition -- a label, say); a bare module name
   jumps to the module. A namespace bound by a user import falls back to
   the binding line, which is where the path is written. *)
let definition_of (d : doc) uri word : J.t option =
  let defs = match d.d_check with Some sc -> sc.Runner.sc_defs | None -> [] in
  match String.split_on_char '.' word with
  | [ns; m] when m <> "" ->
    (match Runner.stdlib_module_source_and_defs ns with
     | Some (_, mdefs) ->
       (match List.assoc_opt m mdefs with
        | Some loc -> Some (location (stdlib_uri ns) loc)
        | None -> Some (location (stdlib_uri ns) (Token.point 1 1 0)))
     | None -> Option.map (location uri) (List.assoc_opt ns defs))
  | [plain] when plain <> "" ->
    (match List.assoc_opt plain defs with
     | Some loc -> Some (location uri loc)
     | None ->
       if List.mem_assoc plain Stdlib_embed.table
       then Some (location (stdlib_uri plain) (Token.point 1 1 0))
       else None)
  | _ -> None

(* ── Code lenses ─────────────────────────────────────────────────────────── *)

(* The type of each definition, on the line above it. Hover answers this one
   name at a time, and a reader going down a file wants it for all of them
   at once -- the signature a wand file does not write out is still the
   first thing to know about a definition.

   Values only. A `type` line already says what it declares, and the lexer
   is what tells the two apart: a lowercase name is a value, an uppercase
   one a type or a constructor. The file's own environment answers, not the
   scope, so an import cannot put a lens on a line the file did not write.

   Where a line binds several names -- `let (a, b) = ...` -- each lens
   carries its name, because two bare types side by side say which is
   which only by accident of order. A line binding one name shows the type
   alone. *)
let code_lenses (d : doc) : J.t list =
  match d.d_check with
  | None -> []
  | Some sc ->
    let is_value name =
      name <> "" && Autoedit.is_ident_continuation name.[0]
      && Char.lowercase_ascii name.[0] = name.[0]
    in
    let defs =
      List.filter_map (fun (name, (loc : Token.loc)) ->
        if not (is_value name) then None
        else Option.map (fun sch ->
               (loc.Token.line, name, Typechecker.string_of_scheme sch))
               (List.assoc_opt name sc.Runner.sc_env))
        sc.Runner.sc_defs
    in
    let alone line =
      List.length (List.filter (fun (l, _, _) -> l = line) defs) = 1
    in
    List.map (fun (line, name, typ) ->
      let title = if alone line then typ else name ^ " : " ^ typ in
      let l = max 0 (line - 1) in
      `Assoc [("range", range0 l 0 l 0);
              (* No command: the title is the whole of what a lens says
                 here, and a lens that runs nothing should not look
                 clickable. It is sent all the same, because a lens the
                 client has to resolve needs a resolve provider. *)
              ("command", `Assoc [("title", `String title);
                                  ("command", `String "")])])
      (List.sort compare defs)

(* ── Completion ──────────────────────────────────────────────────────────── *)

(* CompletionItemKind, the few this server distinguishes. *)
let kind_module = 9
let kind_function = 3
let kind_value = 12
let kind_constructor = 20  (* EnumMember: what editors show a constructor as *)

(* Completion items at a cursor. The environment is the buffer's scope; a
   qualified prefix whose namespace is unimported completes from the
   standard library's signature, and accepting such an item carries the
   auto-import (and manifest) edits the lexical tier would have made --
   `additionalTextEdits`, so the edit happens on accept. *)
let completion_items (d : doc) line_idx line_text character : J.t list =
  let prefix_line = String.sub line_text 0 (min character (String.length line_text)) in
  let scope = scope_of d in
  let typed = match word_at prefix_line (String.length prefix_line) with
    | Some (w, _, _) -> w
    | None -> ""
  in
  let (ns_needs_import, env) =
    match String.split_on_char '.' typed with
    | [ns; _] when not (List.mem_assoc ns scope) ->
      (match Runner.stdlib_module_sig ns with
       | Some (sig_env, _) -> (Some ns, scope @ [(ns, Typechecker.Namespace (sig_env, []))])
       | None -> (None, scope))
    | _ -> (None, scope)
  in
  let ctors = match d.d_check with Some sc -> sc.Runner.sc_ctors | None -> [] in
  let { Complete.start; candidates } = Complete.ident_at ~ctors env prefix_line in
  (* Bare prefixes also offer the stdlib modules themselves; typing the dot
     then completes their members. *)
  let module_candidates =
    if String.contains typed '.' then []
    else
      List.filter_map (fun (name, _) ->
        if Complete.has_prefix ~prefix:typed name
           && not (List.mem_assoc name scope)
        then Some name else None)
        Stdlib_embed.table
  in
  let lines = lines_of d.d_text in
  let item cand =
    let is_module = List.mem cand module_candidates in
    let is_ctor = List.mem cand ctors in
    let described = if is_module then None else describe d cand in
    let kind =
      if is_module then kind_module
      else if is_ctor then kind_constructor
      else match described with
        | Some (t, _) when t = "module" -> kind_module
        | Some (t, _) when Diag.contains t "->" -> kind_function
        | Some _ -> kind_value
        | None -> kind_value
    in
    let extra =
      match String.split_on_char '.' cand, ns_needs_import with
      | [ns; m], Some needed when ns = needed ->
        List.map (json_of_line_edit lines)
          (Autoedit.edits_for_member ~sig_of:Runner.stdlib_module_sig
             ~text:d.d_text ns m)
      | _ -> []
    in
    let fields =
      [("label", `String cand);
       ("kind", `Int kind);
       ("textEdit",
        text_edit (range0 line_idx start line_idx character) cand)]
      @ (match described with
         | Some (t, _) when t <> "module" -> [("detail", `String t)]
         | _ -> [])
      (* The suggest widget truncates `detail` inline; the expandable side
         panel gets the same name-over-type block hover shows, so the full
         signature and doc string are one chevron away. *)
      @ (match described with
         | Some (t, doc) when t <> "module" ->
           [("documentation",
             `Assoc [("kind", `String "markdown");
                     ("value", `String (hover_markdown cand (t, doc)))])]
         | _ -> [])
      @ (if extra = [] then [] else [("additionalTextEdits", `List extra)])
    in
    `Assoc fields
  in
  List.map item (candidates @ module_candidates)

(* ── Code actions ────────────────────────────────────────────────────────── *)

(* A diagnostic's fix as a workspace edit, by the same rules `wand t --fix`
   applies it with (fix.ml) -- one representation, two consumers, and a fix
   that behaved differently in the two paths would be a bug. Answers the
   action's title and edits, or nothing when the fix does not apply to the
   text as it stands. *)
let action_edit lines text (d : Diag.t) : (string * J.t list) option =
  let loc_line = match d.Diag.loc with Some l -> l.Token.line | None -> 1 in
  match d.Diag.fix with
  | Some (Diag.InsertLine t) ->
    let n = match lines with
      | first :: _ when String.length first >= 2
                        && first.[0] = '#' && first.[1] = '!' -> 2
      | _ -> 1
    in
    let title =
      if Fix.is_manifest_text t then "Add manifest: " ^ t
      else "Insert `" ^ t ^ "`"
    in
    Some (title, [json_of_line_edit lines (Autoedit.Insert_line (n, t))])
  | Some (Diag.ReplaceLine t) ->
    let n =
      if Fix.is_manifest_text t then
        match Fix.manifest_line lines with Some n -> n | None -> loc_line
      else loc_line
    in
    (match line_at lines (n - 1) with
     | Some old when old <> t ->
       let title =
         if Fix.is_manifest_text t then "Update manifest: " ^ t
         else "Replace with `" ^ String.trim t ^ "`"
       in
       Some (title, [json_of_line_edit lines (Autoedit.Replace_line (n, t))])
     | _ -> None)
  | Some (Diag.AppendToLine t) ->
    (match line_at lines (loc_line - 1) with
     | Some old ->
       Some ("Continue the line with `" ^ String.trim t ^ "`",
             [json_of_line_edit lines (Autoedit.Replace_line (loc_line, old ^ t))])
     | None -> None)
  | Some Diag.DeleteLine ->
    (match line_at lines (loc_line - 1) with
     | Some old ->
       let title = "Delete `" ^ String.trim old ^ "`" in
       (* The whole line, newline included. *)
       Some (title, [text_edit (range0 (loc_line - 1) 0 loc_line 0) ""])
     | None -> None)
  | Some (Diag.Replace { from_; to_ }) ->
    (* Only when the flagged extent is exactly the text to replace --
       anything less would be a guess about where in the line it meant. *)
    (match d.Diag.loc with
     | Some l when l.Token.end_offset > l.Token.offset
                && l.Token.end_offset <= String.length text
                && String.sub text l.Token.offset
                     (l.Token.end_offset - l.Token.offset) = from_ ->
       Some (Printf.sprintf "Replace `%s` with `%s`" from_ to_,
             [text_edit
                (range0 (l.Token.line - 1) (l.Token.col - 1)
                   (l.Token.end_line - 1) (l.Token.end_col - 1))
                to_])
     | _ -> None)
  | None -> None

(* The diagnostics whose flagged lines touch the requested range. *)
let overlaps_range (d : Diag.t) start_line end_line =
  match d.Diag.loc with
  | None -> start_line = 0
  | Some l ->
    let dl_start = l.Token.line - 1 in
    let dl_end =
      (if l.Token.end_offset > l.Token.offset then l.Token.end_line else l.Token.line) - 1
    in
    dl_start <= end_line && dl_end >= start_line

let code_actions (d : doc) uri start_line end_line : J.t list =
  let lines = lines_of d.d_text in
  List.filter_map (fun (diag : Diag.t) ->
    if not (overlaps_range diag start_line end_line) then None
    else
      match action_edit lines d.d_text diag with
      | None -> None
      | Some (title, edits) ->
        Some (`Assoc [
          ("title", `String title);
          ("kind", `String "quickfix");
          ("diagnostics", `List [json_of_diag diag]);
          ("edit", workspace_edit uri edits);
        ]))
    d.d_diags

let handle (st : state) (msg : J.t) : state * J.t list =
  let id = mem "id" msg in
  let params = mem "params" msg in
  let uri_of () = str (mem "uri" (mem "textDocument" params)) in
  let doc_of uri = List.assoc_opt uri st.docs in
  let position () =
    let p = mem "position" params in
    match int_ (mem "line" p), int_ (mem "character" p) with
    | Some l, Some c -> Some (l, c)
    | _ -> None
  in
  (* A request about a position needs the buffer and its line in hand;
     anything missing answers null rather than guessing. *)
  let at_position k =
    match uri_of (), position () with
    | Some uri, Some (l, c) ->
      (match doc_of uri with
       | Some d ->
         (match line_at (lines_of d.d_text) l with
          | Some line_text -> k uri d l c line_text
          | None -> (st, [response id `Null]))
       | None -> (st, [response id `Null]))
    | _ -> (st, [response id `Null])
  in
  let store st uri d = { st with docs = (uri, d) :: List.remove_assoc uri st.docs } in
  let check_and_publish st uri ?prev text =
    let (check, diags) = analyze uri text in
    let check = match check, prev with
      | None, Some (p : doc) -> p.d_check   (* keep the last good scope *)
      | c, _ -> c
    in
    (store st uri { d_text = text; d_check = check; d_diags = diags },
     [publish uri diags])
  in
  match str (mem "method" msg) with
  (* After shutdown, only exit does anything; a request in between is the
     client's error and is told so. *)
  | Some m when st.shutdown_seen && m <> "exit" ->
    if id = `Null then (st, [])
    else (st, [error_response id (-32600) "shutdown has been requested"])
  | Some "initialize" ->
    let st =
      match auto_edit_of (mem "initializationOptions" params) with
      | Some b -> { st with auto_edit = b }
      | None -> st
    in
    (st, [response id capabilities])
  | Some "workspace/didChangeConfiguration" ->
    (match auto_edit_of (mem "settings" params) with
     | Some b -> ({ st with auto_edit = b }, [])
     | None -> (st, []))
  | Some "initialized" -> (st, [])
  | Some "shutdown" -> ({ st with shutdown_seen = true }, [response id `Null])
  | Some "exit" -> ({ st with quit = Some (if st.shutdown_seen then 0 else 1) }, [])
  | Some "textDocument/didOpen" ->
    let doc = mem "textDocument" params in
    (match str (mem "uri" doc), str (mem "text" doc) with
     | Some uri, Some text -> check_and_publish st uri text
     | _ -> (st, []))
  | Some "textDocument/didChange" ->
    let uri = uri_of () in
    (* Full sync: each change carries the entire text; the last one wins. *)
    let text =
      match mem "contentChanges" params with
      | `List (_ :: _ as changes) -> str (mem "text" (List.nth changes (List.length changes - 1)))
      | _ -> None
    in
    (match uri, text with
     | Some uri, Some text ->
       let prev = doc_of uri in
       let (st, out) = check_and_publish st uri ?prev text in
       (* The lexical tier: a change that completed a
          resolvable qualified name earns its import -- and its manifest
          labels -- as an applyEdit, no gesture. Compared against the text
          as it stood, so nothing fires twice and an undo is respected. *)
       let auto =
         match prev with
         | Some p when st.auto_edit && p.d_text <> text ->
           Autoedit.changes ~sig_of:Runner.stdlib_module_sig
             ~old_text:p.d_text text
         | _ -> []
       in
       if auto = [] then (st, out)
       else
         let lines = lines_of text in
         let req =
           `Assoc [("jsonrpc", `String "2.0");
                   ("id", `Int st.next_req);
                   ("method", `String "workspace/applyEdit");
                   ("params", `Assoc [
                      ("label", `String "wand: import");
                      ("edit", workspace_edit uri
                                 (List.map (json_of_line_edit lines) auto))])]
         in
         ({ st with next_req = st.next_req + 1 }, out @ [req])
     | _ -> (st, []))
  | Some "textDocument/didClose" ->
    (match uri_of () with
     | Some uri ->
       ({ st with docs = List.remove_assoc uri st.docs }, [publish uri []])
     | None -> (st, []))
  | Some "textDocument/hover" ->
    at_position (fun _uri d l c line_text ->
      match word_at line_text c with
      | None -> (st, [response id `Null])
      | Some (word, start, stop) ->
        (match (match describe_manifest_label d word l with
                | Some info -> Some info
                | None ->
                  (match describe d word with
                   | Some info -> Some info
                   | None -> describe_local d word l)) with
         | None -> (st, [response id `Null])
         | Some info ->
           (st, [response id (`Assoc [
              ("contents", `Assoc [("kind", `String "markdown");
                                   ("value", `String (hover_markdown word info))]);
              ("range", range0 l start l stop)])])))
  | Some "textDocument/codeLens" ->
    (match uri_of () with
     | Some uri ->
       (match doc_of uri with
        | Some d -> (st, [response id (`List (code_lenses d))])
        | None -> (st, [response id (`List [])]))
     | None -> (st, [response id (`List [])]))
  | Some "textDocument/completion" ->
    at_position (fun _uri d l c line_text ->
      (st, [response id (`List (completion_items d l line_text c))]))
  | Some "textDocument/definition" ->
    at_position (fun uri d _l c line_text ->
      match word_at line_text c with
      | None -> (st, [response id `Null])
      | Some (word, _, _) ->
        (match definition_of d uri word with
         | Some loc -> (st, [response id loc])
         | None -> (st, [response id `Null])))
  | Some "wand/stdlibSource" ->
    (* The client's virtual-document provider asking for a module's text. *)
    (match str (mem "uri" params) with
     | Some u ->
       let name = Filename.remove_extension (Filename.basename (path_of_uri u)) in
       (match Runner.stdlib_module_source_and_defs name with
        | Some (src, _) -> (st, [response id (`String src)])
        | None -> (st, [response id `Null]))
     | None -> (st, [response id `Null]))
  | Some "textDocument/codeAction" ->
    (match uri_of () with
     | Some uri ->
       (match doc_of uri with
        | Some d ->
          let r = mem "range" params in
          let sl = match int_ (mem "line" (mem "start" r)) with Some n -> n | None -> 0 in
          let el = match int_ (mem "line" (mem "end" r)) with Some n -> n | None -> sl in
          (st, [response id (`List (code_actions d uri sl el))])
        | None -> (st, [response id (`List [])]))
     | None -> (st, [response id (`List [])]))
  | Some "textDocument/formatting" ->
    (match uri_of () with
     | Some uri ->
       (match doc_of uri with
        | Some d ->
          (match Formatter.format_source d.d_text with
           | exception _ -> (st, [response id `Null])  (* not parseable: no edit *)
           | out when out = d.d_text -> (st, [response id (`List [])])
           | out ->
             let lines = lines_of d.d_text in
             let last = List.length lines - 1 in
             let last_len =
               match line_at lines last with Some l -> String.length l | None -> 0
             in
             (st, [response id (`List [
                text_edit (range0 0 0 last last_len) out])]))
        | None -> (st, [response id `Null]))
     | None -> (st, [response id `Null]))
  | Some _ ->
    (* Unknown notifications are ignored by contract; unknown requests are
       answered, or the client waits forever. *)
    if id = `Null then (st, [])
    else (st, [error_response id (-32601) "method not found"])
  | None -> (st, [])

(* ── The stdio instantiation ─────────────────────────────────────────────── *)

let serve ic oc : int =
  set_binary_mode_in ic true;
  set_binary_mode_out oc true;
  let rec loop st =
    match st.quit with
    | Some code -> code
    | None ->
      match read_message ic with
      | Ended ->
        (* The client went away without `exit`. *)
        if st.shutdown_seen then 0 else 1
      | Bad_body ->
        (* The framing held, so the session does. JSON-RPC answers a body it
           could not parse with -32700 and a null id, because there is no id
           to answer under. *)
        write_message oc (error_response `Null (-32700) "parse error");
        loop st
      | Frame msg ->
        (* An editor is a long-lived client, and a request it phrases oddly
           is not a reason to take its diagnostics away for the rest of the
           session: the request fails and the loop goes on. *)
        let (st, outgoing) =
          try handle st msg with
          | Out_of_memory as e -> raise e
          | e ->
            (* A notification carries no id and so has nothing to answer
               under; the failure is swallowed rather than sent nowhere. *)
            (match mem "id" msg with
             | `Null -> (st, [])
             | id -> (st, [error_response id (-32603) (Printexc.to_string e)]))
        in
        List.iter (write_message oc) outgoing;
        loop st
  in
  loop initial
