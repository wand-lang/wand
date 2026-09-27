open Wand

(* The server in process: feed request JSON to `handle`, assert on the JSON
   it answers -- no pty, no subprocess, no channels. Stdio is only the
   production instantiation of the same loop, and its framing is covered
   separately at the bottom. *)

let notif method_ params : Yojson.Safe.t =
  `Assoc [("jsonrpc", `String "2.0"); ("method", `String method_);
          ("params", params)]

let request id method_ params : Yojson.Safe.t =
  `Assoc [("jsonrpc", `String "2.0"); ("id", `Int id);
          ("method", `String method_); ("params", params)]

let did_open uri text =
  notif "textDocument/didOpen"
    (`Assoc [("textDocument",
              `Assoc [("uri", `String uri); ("languageId", `String "wand");
                      ("version", `Int 1); ("text", `String text)])])

let did_change uri text =
  notif "textDocument/didChange"
    (`Assoc [("textDocument", `Assoc [("uri", `String uri); ("version", `Int 2)]);
             ("contentChanges", `List [`Assoc [("text", `String text)]])])

let did_close uri =
  notif "textDocument/didClose"
    (`Assoc [("textDocument", `Assoc [("uri", `String uri)])])

(* Run a message sequence from the initial state, collecting every outgoing
   message in order. *)
let session msgs =
  let (st, outs) =
    List.fold_left (fun (st, acc) msg ->
      let (st, out) = Lsp.handle st msg in
      (st, acc @ out)
    ) (Lsp.initial, []) msgs
  in
  (st, outs)

let m = Lsp.mem
let s j = match Lsp.str j with Some s -> s | None -> Alcotest.fail "expected a string"
let int_of j = match j with `Int n -> n | _ -> Alcotest.fail "expected an int"

let diagnostics_of out =
  (* The one publishDiagnostics notification in `out` for each check. *)
  List.filter_map (fun o ->
    if Lsp.str (m "method" o) = Some "textDocument/publishDiagnostics"
    then Some (m "params" o) else None) out

let uri = "file:///tmp/wand_lsp_test.wand"

(* ── Lifecycle ───────────────────────────────────────────────────────────── *)

let test_initialize () =
  let (_, out) = session [request 1 "initialize" (`Assoc [])] in
  match out with
  | [resp] ->
    Alcotest.(check int) "answers the same id" 1 (int_of (m "id" resp));
    let caps = m "capabilities" (m "result" resp) in
    Alcotest.(check int) "full-text sync" 1
      (int_of (m "change" (m "textDocumentSync" caps)));
    Alcotest.(check string) "names itself" "wand"
      (s (m "name" (m "serverInfo" (m "result" resp))))
  | _ -> Alcotest.failf "expected exactly one response, got %d" (List.length out)

let test_shutdown_then_exit () =
  let (st, out) = session [request 2 "shutdown" `Null] in
  (match out with
   | [resp] -> Alcotest.(check bool) "result is null" true (m "result" resp = `Null)
   | _ -> Alcotest.fail "expected one response");
  (* A request after shutdown is the client's error. *)
  let (st, out) = Lsp.handle st (request 3 "initialize" `Null) in
  (match out with
   | [resp] ->
     Alcotest.(check int) "invalid request" (-32600)
       (int_of (m "code" (m "error" resp)))
   | _ -> Alcotest.fail "expected an error response");
  let (st, _) = Lsp.handle st (notif "exit" `Null) in
  Alcotest.(check (option int)) "clean exit" (Some 0) st.Lsp.quit

let test_exit_without_shutdown () =
  let (st, _) = session [notif "exit" `Null] in
  Alcotest.(check (option int)) "protocol error exit" (Some 1) st.Lsp.quit

let test_unknown_method () =
  let (_, out) = session [request 7 "workspace/symbol" `Null] in
  (match out with
   | [resp] ->
     Alcotest.(check int) "method not found" (-32601)
       (int_of (m "code" (m "error" resp)))
   | _ -> Alcotest.fail "expected an error response");
  let (_, out) = session [notif "$/cancelRequest" `Null] in
  Alcotest.(check int) "unknown notification ignored" 0 (List.length out)

(* ── Diagnostics ─────────────────────────────────────────────────────────── *)

let test_open_publishes_the_error () =
  let (_, out) = session [did_open uri "let x = 1\nlet y = x ++ \"s\"\n"] in
  match diagnostics_of out with
  | [params] ->
    Alcotest.(check string) "for the opened uri" uri (s (m "uri" params));
    (match m "diagnostics" params with
     | `List [d] ->
       Alcotest.(check string) "code" "E-TYPE" (s (m "code" d));
       Alcotest.(check int) "severity error" 1 (int_of (m "severity" d));
       Alcotest.(check string) "source" "wand" (s (m "source" d));
       (* 1-based 2:9-2:17 becomes 0-based 1:8-1:16 -- the whole
          expression at fault, not its first token. *)
       let range = m "range" d in
       Alcotest.(check (pair int int)) "start" (1, 8)
         (int_of (m "line" (m "start" range)),
          int_of (m "character" (m "start" range)));
       Alcotest.(check (pair int int)) "end" (1, 16)
         (int_of (m "line" (m "end" range)),
          int_of (m "character" (m "end" range)))
     | _ -> Alcotest.fail "expected exactly one diagnostic")
  | _ -> Alcotest.fail "expected one publishDiagnostics"

let test_findings_are_warnings () =
  let (_, out) = session [did_open uri "let is_ready? x = x > 1\nis_ready? 2\n"] in
  match diagnostics_of out with
  | [params] ->
    (match m "diagnostics" params with
     | `List [d] ->
       Alcotest.(check string) "code" "V-PRED2" (s (m "code" d));
       Alcotest.(check int) "severity warning" 2 (int_of (m "severity" d))
     | _ -> Alcotest.fail "expected exactly one diagnostic")
  | _ -> Alcotest.fail "expected one publishDiagnostics"

let test_change_republishes () =
  let (_, out) =
    session [
      did_open uri "let x = 1\nlet y = x ++ \"s\"\n";
      did_change uri "let x = \"1\"\nlet y = x ++ \"s\"\ny\n";
    ]
  in
  match diagnostics_of out with
  | [_; fixed] ->
    Alcotest.(check bool) "the fix clears the pane" true
      (m "diagnostics" fixed = `List [])
  | l -> Alcotest.failf "expected two publishes, got %d" (List.length l)

let test_close_clears () =
  let (st, out) =
    session [did_open uri "let x = 1 ++ 2\nx\n"; did_close uri]
  in
  Alcotest.(check bool) "the doc is forgotten" true
    (List.assoc_opt uri st.Lsp.docs = None);
  match List.rev (diagnostics_of out) with
  | last :: _ ->
    Alcotest.(check bool) "cleared on close" true
      (m "diagnostics" last = `List [])
  | [] -> Alcotest.fail "expected a clearing publish"

(* ── Editor features ─────────────────────────────────────────────────────── *)

let contains msg needle =
  let n = String.length needle and l = String.length msg in
  let rec go i = i + n <= l && (String.sub msg i n = needle || go (i + 1)) in
  go 0

let at_position id method_ uri line char =
  request id method_
    (`Assoc [("textDocument", `Assoc [("uri", `String uri)]);
             ("position", `Assoc [("line", `Int line); ("character", `Int char)])])

(* The response carrying `id`, or fail. *)
let response_for id outs =
  match List.find_opt (fun o -> m "id" o = `Int id) outs with
  | Some o -> m "result" o
  | None -> Alcotest.failf "no response with id %d" id

let test_hover_stdlib_member () =
  let text = "uses {IO}\nimport List\nlet double xs = List.map (fn x -> x * 2) xs\nprintln \"done\"\n" in
  let (_, outs) =
    session [did_open uri text; at_position 11 "textDocument/hover" uri 2 20]
  in
  match response_for 11 outs with
  | `Null -> Alcotest.fail "expected a hover"
  | result ->
    let value = s (m "value" (m "contents" result)) in
    Alcotest.(check bool) "names the member, type on the next line" true
      (contains value "List.map\n: ");
    Alcotest.(check bool) "shows a signature" true (contains value "->");
    (* The whole qualified name, 0-based, on its line. *)
    let range = m "range" result in
    Alcotest.(check int) "range start" 16
      (int_of (m "character" (m "start" range)));
    Alcotest.(check int) "range end" 24
      (int_of (m "character" (m "end" range)))

(* The flagship: the signature that can't lie, with its effect set, for a
   member the buffer has not even imported yet. *)
let test_hover_shows_effects () =
  let (_, outs) =
    session [did_open uri "FS.write_file! /tmp/x \"hi\"\n";
             at_position 12 "textDocument/hover" uri 0 4]
  in
  match response_for 12 outs with
  | `Null -> Alcotest.fail "expected a hover"
  | result ->
    let value = s (m "value" (m "contents" result)) in
    Alcotest.(check bool) "the effect set is in the signature" true
      (contains value "FS.Write")

let test_hover_survives_a_broken_recheck () =
  let good = "uses {IO}\nimport List\nlet d xs = List.map (fn x -> x) xs\nprintln \"x\"\n" in
  let (_, outs) =
    session [did_open uri good;
             did_change uri (good ^ "(");   (* no longer parses *)
             at_position 13 "textDocument/hover" uri 2 12]
  in
  (match response_for 13 outs with
   | `Null -> Alcotest.fail "the last good scope should still answer"
   | result ->
     Alcotest.(check bool) "still describes List.map" true
       (contains (s (m "value" (m "contents" result))) "List.map"))

let test_hover_on_nothing () =
  let (_, outs) =
    session [did_open uri "let x = 1\nx\n";
             at_position 14 "textDocument/hover" uri 0 6]  (* the '=' *)
  in
  Alcotest.(check bool) "null on nothing" true (response_for 14 outs = `Null)

(* Locals are not in the top-level scope; the enclosing item's recorded
   binders answer for them. *)
let test_hover_on_a_parameter () =
  let text = "let pick flag = if flag then 1 else 2\npick true\n" in
  let (_, outs) =
    session [did_open uri text;
             at_position 15 "textDocument/hover" uri 0 20]  (* `flag` use *)
  in
  match response_for 15 outs with
  | `Null -> Alcotest.fail "expected a hover on the parameter"
  | result ->
    Alcotest.(check bool) "the parameter's inferred type" true
      (contains (s (m "value" (m "contents" result))) "flag\n: Bool")

(* A doc's example is a transcript, and markdown would run the expression
   and its answer together on one line. The fence is what keeps them
   apart. *)
let test_hover_fences_a_doc_example () =
  let text = "uses {IO}\nimport List\nlet f xs = List.head xs\n" in
  let (_, outs) =
    session [did_open uri text; at_position 16 "textDocument/hover" uri 2 16]
  in
  match response_for 16 outs with
  | `Null -> Alcotest.fail "expected a hover"
  | result ->
    let value = s (m "value" (m "contents" result)) in
    Alcotest.(check bool) "the prompt survives, the answer keeps its line" true
      (contains value "```\n>> List.head [1, 2, 3]\nSome(1) : Option Int\n```")

(* In the manifest a label is an effect. `Env` is also a module, and saying
   so there describes the wrong thing. *)
let test_hover_on_a_manifest_label () =
  let (_, outs) =
    session [did_open uri "uses {Env, FS.Read}\nimport Env\nEnv.get \"HOME\"\n";
             at_position 17 "textDocument/hover" uri 0 7]
  in
  match response_for 17 outs with
  | `Null -> Alcotest.fail "expected a hover on the label"
  | result ->
    let value = s (m "value" (m "contents" result)) in
    Alcotest.(check bool) "an effect, not a module" true
      (contains value "effect Env" && not (contains value "module"));
    Alcotest.(check bool) "says what the label admits" true
      (contains value "environment variables")

(* A label that names no module at all was answered with nothing before. *)
let test_hover_on_a_dotted_manifest_label () =
  let (_, outs) =
    session [did_open uri "uses {Env, FS.Read}\nimport Env\nEnv.get \"HOME\"\n";
             at_position 18 "textDocument/hover" uri 0 13]
  in
  match response_for 18 outs with
  | `Null -> Alcotest.fail "expected a hover on FS.Read"
  | result ->
    Alcotest.(check bool) "the read half of the filesystem" true
      (contains (s (m "value" (m "contents" result))) "effect FS.Read")

(* Outside the manifest the same word is the module it has always been. *)
let test_hover_outside_the_manifest_is_the_module () =
  let (_, outs) =
    session [did_open uri "uses {Env}\nimport Env\nEnv.get \"HOME\"\n";
             at_position 19 "textDocument/hover" uri 1 8]
  in
  match response_for 19 outs with
  | `Null -> Alcotest.fail "expected a hover on the import"
  | result ->
    Alcotest.(check bool) "still the module" true
      (contains (s (m "value" (m "contents" result))) "module Env")

let lens_titles result =
  match result with
  | `List items ->
    List.map (fun i -> (int_of (m "line" (m "start" (m "range" i))),
                        s (m "title" (m "command" i)))) items
  | _ -> Alcotest.fail "expected a code lens list"

let test_code_lens_types_the_definitions () =
  let text = "let double n = n * 2\nlet name = \"x\"\ntype Shape = Circle Int\ndouble 2\n" in
  let (_, outs) =
    session [did_open uri text; at_position 20 "textDocument/codeLens" uri 0 0]
  in
  let titles = lens_titles (response_for 20 outs) in
  Alcotest.(check (list (pair int string))) "one per value definition, its type"
    [(0, "Int -> Int"); (1, "String")] titles

(* Several names off one line each say which they are; a type declaration
   gets no lens -- its line already says what it declares. *)
let test_code_lens_names_a_shared_line () =
  let text = "let (a, b) = (1, \"x\")\na\n" in
  let (_, outs) =
    session [did_open uri text; at_position 21 "textDocument/codeLens" uri 0 0]
  in
  Alcotest.(check (list (pair int string))) "named, because order is not a label"
    [(0, "a : Int"); (0, "b : String")]
    (lens_titles (response_for 21 outs))

(* A member of an implementation is a definition in its own right, so its
   lens sits above its own `let`. Given the block's position, all four ran
   together on one line above `implement`. *)
let test_code_lens_types_each_member () =
  let text =
    "interface Pair 'a(first: 'a -> 'a -> 'a, second: 'a -> 'a -> 'a)\n\n\
     implement Pair Int =\n\
     \  let first a b = a;\n\
     \  let second a b = b\n\n\
     first 1 2\n"
  in
  let (_, outs) =
    session [did_open uri text; at_position 22 "textDocument/codeLens" uri 0 0]
  in
  Alcotest.(check (list (pair int string))) "one per member, at its own let"
    [(3, "Int -> Int -> Int"); (4, "Int -> Int -> Int")]
    (lens_titles (response_for 22 outs))

let items_of result = match result with
  | `List items -> items
  | _ -> Alcotest.fail "expected a completion list"

let find_item label items =
  List.find_opt (fun i -> Lsp.str (m "label" i) = Some label) items

let test_completion_in_scope () =
  let text = "import List\nlet doubled = List.ma (fn x -> x + 1) [1]\ndoubled\n" in
  let (_, outs) =
    session [did_open uri text;
             (* the cursor just past "List.ma" *)
             at_position 15 "textDocument/completion" uri 1 21]
  in
  let items = items_of (response_for 15 outs) in
  match find_item "List.map" items with
  | None -> Alcotest.fail "List.map not offered"
  | Some item ->
    Alcotest.(check int) "a function" 3 (int_of (m "kind" item));
    Alcotest.(check bool) "typed detail" true (contains (s (m "detail" item)) "->");
    let edit = m "textEdit" item in
    Alcotest.(check int) "replaces from the name's start" 14
      (int_of (m "character" (m "start" (m "range" edit))));
    Alcotest.(check bool) "no auto-import for an imported module" true
      (m "additionalTextEdits" item = `Null)

let test_completion_auto_imports () =
  let (_, outs) =
    session [did_open uri "uses {IO}\n\nFS.wri\n";
             at_position 16 "textDocument/completion" uri 2 6]
  in
  let items = items_of (response_for 16 outs) in
  match find_item "FS.write_file!" items with
  | None -> Alcotest.fail "FS.write_file! not offered"
  | Some item ->
    (match m "additionalTextEdits" item with
     | `List [imp; man] ->
       Alcotest.(check string) "the import rides along" "import FS\n"
         (s (m "newText" imp));
       Alcotest.(check string) "and the manifest label" "uses {FS.Write, IO}"
         (s (m "newText" man))
     | _ -> Alcotest.fail "expected the import and manifest edits")

let test_completion_offers_modules () =
  let (_, outs) =
    session [did_open uri "let x = 1\nRege\n";
             at_position 17 "textDocument/completion" uri 1 4]
  in
  let items = items_of (response_for 17 outs) in
  match find_item "Regex" items with
  | None -> Alcotest.fail "Regex not offered"
  | Some item -> Alcotest.(check int) "as a module" 9 (int_of (m "kind" item))

let code_action_at id uri sl el =
  request id "textDocument/codeAction"
    (`Assoc [("textDocument", `Assoc [("uri", `String uri)]);
             ("range", `Assoc [
                ("start", `Assoc [("line", `Int sl); ("character", `Int 0)]);
                ("end",   `Assoc [("line", `Int el); ("character", `Int 0)])]);
             ("context", `Assoc [("diagnostics", `List [])])])

let test_code_action_updates_manifest () =
  let text = "uses {IO}\nimport FS\nimport IO\nlet r = FS.write_file! /tmp/wand_lsp_ca \"hi\"\nIO.println \"done\"\n" in
  let (_, outs) = session [did_open uri text; code_action_at 18 uri 0 4] in
  match items_of (response_for 18 outs) with
  | [action] ->
    Alcotest.(check bool) "titled as a manifest update" true
      (contains (s (m "title" action)) "Update manifest:");
    (match m uri (m "changes" (m "edit" action)) with
     | `List [edit] ->
       Alcotest.(check string) "the corrected line" "uses {FS.Write, IO}"
         (s (m "newText" edit));
       Alcotest.(check int) "on the manifest line" 0
         (int_of (m "line" (m "start" (m "range" edit))))
     | _ -> Alcotest.fail "expected one text edit")
  | l -> Alcotest.failf "expected one action, got %d" (List.length l)

let test_code_action_removes_dead_import () =
  let text = "import List\nlet {head!} = import List\nhead! [1]\n" in
  let (_, outs) = session [did_open uri text; code_action_at 19 uri 0 0] in
  match items_of (response_for 19 outs) with
  | [action] ->
    Alcotest.(check string) "titled" "Delete `import List`" (s (m "title" action));
    (match m uri (m "changes" (m "edit" action)) with
     | `List [edit] ->
       Alcotest.(check string) "deletes the line" "" (s (m "newText" edit));
       Alcotest.(check int) "through the newline" 1
         (int_of (m "line" (m "end" (m "range" edit))))
     | _ -> Alcotest.fail "expected one text edit")
  | l -> Alcotest.failf "expected one action, got %d" (List.length l)

let formatting_req id uri =
  request id "textDocument/formatting"
    (`Assoc [("textDocument", `Assoc [("uri", `String uri)]);
             ("options", `Assoc [("tabSize", `Int 2); ("insertSpaces", `Bool true)])])

let test_formatting () =
  let (_, outs) =
    session [did_open uri "let x   =   1\nx\n"; formatting_req 20 uri]
  in
  (match items_of (response_for 20 outs) with
   | [edit] ->
     Alcotest.(check bool) "canonical spacing" true
       (contains (s (m "newText" edit)) "let x = 1")
   | l -> Alcotest.failf "expected one whole-document edit, got %d" (List.length l));
  (* Already formatted: no edits, rather than a no-op rewrite. *)
  let (_, outs) =
    session [did_open uri "let x = 1\nx\n"; formatting_req 21 uri]
  in
  Alcotest.(check bool) "fixed point means no edits" true
    (response_for 21 outs = `List [])

(* ── Definition ──────────────────────────────────────────────────────────── *)

let test_definition_same_file () =
  let text = "let helper x = x + 1\nlet run = helper 2\nrun\n" in
  let (_, outs) =
    session [did_open uri text; at_position 22 "textDocument/definition" uri 1 12]
  in
  match response_for 22 outs with
  | `Null -> Alcotest.fail "expected a definition"
  | result ->
    Alcotest.(check string) "in the same buffer" uri (s (m "uri" result));
    Alcotest.(check int) "on the defining line" 0
      (int_of (m "line" (m "start" (m "range" result))))

let test_definition_stdlib_member () =
  let text = "import List\nlet y = List.map (fn x -> x) [1]\ny\n" in
  let (_, outs) =
    session [did_open uri text; at_position 23 "textDocument/definition" uri 1 13]
  in
  match response_for 23 outs with
  | `Null -> Alcotest.fail "expected a definition"
  | result ->
    Alcotest.(check string) "a stdlib virtual document" "wand-stdlib:/List.wand"
      (s (m "uri" result));
    (* `let map` sits below the module's own imports and doc comment. *)
    Alcotest.(check bool) "at the member, not the top" true
      (int_of (m "line" (m "start" (m "range" result))) > 0)

let test_definition_bare_module () =
  let (_, outs) =
    session [did_open uri "import List\nList.length [1]\n";
             at_position 24 "textDocument/definition" uri 0 9]
  in
  match response_for 24 outs with
  | `Null -> Alcotest.fail "expected a definition"
  | result ->
    Alcotest.(check string) "the module's document" "wand-stdlib:/List.wand"
      (s (m "uri" result));
    Alcotest.(check int) "at the top" 0
      (int_of (m "line" (m "start" (m "range" result))))

let test_stdlib_source_request () =
  let (_, outs) =
    session [request 25 "wand/stdlibSource"
               (`Assoc [("uri", `String "wand-stdlib:/List.wand")])]
  in
  (match response_for 25 outs with
   | `String src ->
     Alcotest.(check bool) "serves the module's source" true
       (contains src "let map")
   | _ -> Alcotest.fail "expected the source text");
  let (_, outs) =
    session [request 26 "wand/stdlibSource"
               (`Assoc [("uri", `String "wand-stdlib:/Nope.wand")])]
  in
  Alcotest.(check bool) "unknown module is null" true
    (response_for 26 outs = `Null)

(* ── Auto-edits on didChange ─────────────────────────────────────────────── *)

let apply_edits_of outs =
  List.filter_map (fun o ->
    if Lsp.str (m "method" o) = Some "workspace/applyEdit"
    then Some (m "params" o) else None) outs

(* The one thing here that edits a buffer nobody asked it to edit, so it can
   be turned off -- in `initializationOptions` at startup, or later through
   `workspace/didChangeConfiguration`. On by default: that is what the tier
   is for and what it has always done. *)
let test_auto_import_can_be_turned_off () =
  let (_, outs) =
    session [
      request 1 "initialize" (`Assoc [("initializationOptions",
                                       `Assoc [("autoEdit", `Bool false)])]);
      did_open uri "uses {IO}\n\nprintln \"hi\"\n";
      did_change uri "uses {IO}\n\nprintln \"hi\"\nFS.write_file! \n";
    ]
  in
  Alcotest.(check int) "no edit was pushed" 0 (List.length (apply_edits_of outs))

let test_auto_import_off_by_configuration () =
  let (_, outs) =
    session [
      notif "workspace/didChangeConfiguration"
        (`Assoc [("settings", `Assoc [("wand", `Assoc [("autoEdit", `Bool false)])])]);
      did_open uri "uses {IO}\n\nprintln \"hi\"\n";
      did_change uri "uses {IO}\n\nprintln \"hi\"\nFS.write_file! \n";
    ]
  in
  Alcotest.(check int) "no edit was pushed" 0 (List.length (apply_edits_of outs))

let test_auto_import_fires_on_completion () =
  let (_, outs) =
    session [
      did_open uri "uses {IO}\n\nprintln \"hi\"\n";
      did_change uri "uses {IO}\n\nprintln \"hi\"\nFS.write_file! \n";
    ]
  in
  match apply_edits_of outs with
  | [params] ->
    (match m uri (m "changes" (m "edit" params)) with
     | `List [imp; man] ->
       Alcotest.(check string) "the import" "import FS\n" (s (m "newText" imp));
       Alcotest.(check int) "into the block position" 2
         (int_of (m "line" (m "start" (m "range" imp))));
       Alcotest.(check string) "the manifest" "uses {FS.Write, IO}"
         (s (m "newText" man))
     | _ -> Alcotest.fail "expected the import and manifest edits")
  | l -> Alcotest.failf "expected one applyEdit, got %d" (List.length l)

let test_auto_import_fires_once () =
  let (_, outs) =
    session [
      did_open uri "uses {IO}\n\nprintln \"hi\"\n";
      did_change uri "uses {IO}\n\nprintln \"hi\"\nFS.write_file! \n";
      (* the same reference again, argument growing: nothing new completed *)
      did_change uri "uses {IO}\n\nprintln \"hi\"\nFS.write_file! /tmp/x \n";
    ]
  in
  Alcotest.(check int) "one applyEdit across the session" 1
    (List.length (apply_edits_of outs))

(* ── Framing ─────────────────────────────────────────────────────────────── *)

let test_framing_round_trip () =
  let path = Filename.temp_file "wand_lsp" ".bin" in
  Fun.protect ~finally:(fun () -> try Sys.remove path with Sys_error _ -> ())
    (fun () ->
      let msg = request 9 "initialize" (`Assoc [("a", `String "b")]) in
      Out_channel.with_open_bin path (fun oc ->
        Lsp.write_message oc msg;
        Lsp.write_message oc (notif "exit" `Null));
      In_channel.with_open_bin path (fun ic ->
        (match Lsp.read_message ic with
         | Lsp.Frame j -> Alcotest.(check string) "first message survives"
                       (Yojson.Safe.to_string msg) (Yojson.Safe.to_string j)
         | _ -> Alcotest.fail "no first message");
        (match Lsp.read_message ic with
         | Lsp.Frame j -> Alcotest.(check (option string)) "second too"
                       (Some "exit") (Lsp.str (m "method" j))
         | _ -> Alcotest.fail "no second message");
        Alcotest.(check bool) "then the stream ends" true
          (Lsp.read_message ic = Lsp.Ended)))

(* A frame the server will not read. Each of these ended the process before:
   a negative length reached `Bytes.create` and raised `Invalid_argument`, a
   huge one asked the runtime for the memory and got "Out of memory", and a
   body that was not JSON was read as the client having gone away -- so the
   server exited and every later request in that session went unanswered. *)

let framed headers body =
  let path = Filename.temp_file "wand_lsp" ".bin" in
  Out_channel.with_open_bin path (fun oc ->
    List.iter (fun h -> Out_channel.output_string oc (h ^ "\r\n")) headers;
    Out_channel.output_string oc "\r\n";
    Out_channel.output_string oc body);
  path

let read_one path =
  Fun.protect ~finally:(fun () -> try Sys.remove path with Sys_error _ -> ())
    (fun () -> In_channel.with_open_bin path Lsp.read_message)

let test_a_length_the_server_will_not_read () =
  List.iter (fun header ->
    match read_one (framed [header] "") with
    | Lsp.Ended -> ()
    | Lsp.Bad_body -> Alcotest.failf "%s: read as a bad body" header
    | Lsp.Frame _ -> Alcotest.failf "%s: read as a message" header)
    ["Content-Length: -1";
     "Content-Length: 999999999999999";
     "Content-Length: 0x10";
     "Content-Length: nonsense"]

let test_a_body_that_is_not_json_keeps_the_session () =
  match read_one (framed ["Content-Length: 5"] "{not}") with
  | Lsp.Bad_body -> ()
  | Lsp.Ended -> Alcotest.fail "read as the client going away"
  | Lsp.Frame _ -> Alcotest.fail "read as a message"

(* And the session goes on: the server answers the one message with a parse
   error and reads the next. *)
let test_the_server_answers_a_bad_body_and_reads_on () =
  let path =
    let p = Filename.temp_file "wand_lsp" ".bin" in
    Out_channel.with_open_bin p (fun oc ->
      Out_channel.output_string oc "Content-Length: 5\r\n\r\n{not}";
      Lsp.write_message oc (request 1 "shutdown" `Null);
      Lsp.write_message oc (notif "exit" `Null));
    p
  in
  let out = Filename.temp_file "wand_lsp_out" ".bin" in
  Fun.protect
    ~finally:(fun () ->
      List.iter (fun f -> try Sys.remove f with Sys_error _ -> ()) [path; out])
    (fun () ->
      let code =
        In_channel.with_open_bin path (fun ic ->
          Out_channel.with_open_bin out (fun oc -> Lsp.serve ic oc))
      in
      (* 0 is `shutdown` and then `exit`, both read past the bad body. The
         bad body used to answer "the client went away", which is the 1 this
         would otherwise be. *)
      Alcotest.(check int) "the session ran to the client's exit" 0 code;
      let written = In_channel.with_open_bin out In_channel.input_all in
      Alcotest.(check bool) "the bad body was answered with -32700" true
        (Lint.contains written "-32700"))

let () =
  Alcotest.run "lsp" [
    "lifecycle", [
      Alcotest.test_case "initialize"            `Quick test_initialize;
      Alcotest.test_case "shutdown then exit"    `Quick test_shutdown_then_exit;
      Alcotest.test_case "exit without shutdown" `Quick test_exit_without_shutdown;
      Alcotest.test_case "unknown method"        `Quick test_unknown_method;
    ];
    "diagnostics", [
      Alcotest.test_case "open publishes"     `Quick test_open_publishes_the_error;
      Alcotest.test_case "findings warn"      `Quick test_findings_are_warnings;
      Alcotest.test_case "change republishes" `Quick test_change_republishes;
      Alcotest.test_case "close clears"       `Quick test_close_clears;
    ];
    "hover", [
      Alcotest.test_case "stdlib member"   `Quick test_hover_stdlib_member;
      Alcotest.test_case "effect set"      `Quick test_hover_shows_effects;
      Alcotest.test_case "broken recheck"  `Quick test_hover_survives_a_broken_recheck;
      Alcotest.test_case "nothing"         `Quick test_hover_on_nothing;
      Alcotest.test_case "parameter"       `Quick test_hover_on_a_parameter;
      Alcotest.test_case "doc example"     `Quick test_hover_fences_a_doc_example;
      Alcotest.test_case "manifest label"  `Quick test_hover_on_a_manifest_label;
      Alcotest.test_case "dotted label"    `Quick test_hover_on_a_dotted_manifest_label;
      Alcotest.test_case "module outside"  `Quick test_hover_outside_the_manifest_is_the_module;
    ];
    "code lens", [
      Alcotest.test_case "types definitions" `Quick test_code_lens_types_the_definitions;
      Alcotest.test_case "types each member" `Quick test_code_lens_types_each_member;
      Alcotest.test_case "shared line"       `Quick test_code_lens_names_a_shared_line;
    ];
    "completion", [
      Alcotest.test_case "in scope"        `Quick test_completion_in_scope;
      Alcotest.test_case "auto-imports"    `Quick test_completion_auto_imports;
      Alcotest.test_case "offers modules"  `Quick test_completion_offers_modules;
    ];
    "code actions", [
      Alcotest.test_case "manifest update" `Quick test_code_action_updates_manifest;
      Alcotest.test_case "dead import"     `Quick test_code_action_removes_dead_import;
    ];
    "formatting", [
      Alcotest.test_case "whole document"  `Quick test_formatting;
    ];
    "definition", [
      Alcotest.test_case "same file"      `Quick test_definition_same_file;
      Alcotest.test_case "stdlib member"  `Quick test_definition_stdlib_member;
      Alcotest.test_case "bare module"    `Quick test_definition_bare_module;
      Alcotest.test_case "stdlib source"  `Quick test_stdlib_source_request;
    ];
    "auto-edits", [
      Alcotest.test_case "fires on completion" `Quick test_auto_import_fires_on_completion;
      Alcotest.test_case "fires once"          `Quick test_auto_import_fires_once;
      Alcotest.test_case "turned off"          `Quick test_auto_import_can_be_turned_off;
      Alcotest.test_case "turned off later"    `Quick test_auto_import_off_by_configuration;
    ];
    "framing", [
      Alcotest.test_case "round trip"        `Quick test_framing_round_trip;
      Alcotest.test_case "a length refused"  `Quick
        test_a_length_the_server_will_not_read;
      Alcotest.test_case "a body that is not JSON" `Quick
        test_a_body_that_is_not_json_keeps_the_session;
      Alcotest.test_case "the session goes on" `Quick
        test_the_server_answers_a_bad_body_and_reads_on;
    ];
  ]
