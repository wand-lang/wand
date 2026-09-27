(* Completion as a pure question: given a line of input and the names in
   scope, what could the identifier being typed become? The REPL feeds the
   answers to linenoise; the language server will feed them to
   textDocument/completion. Neither the terminal nor the protocol appears
   here -- which is also what lets the logic be tested directly instead of
   through a pty. *)

let is_ident_char = function
  | 'a'..'z' | 'A'..'Z' | '0'..'9' | '_' | '!' | '?' | '.' -> true
  | _ -> false

let has_prefix = String.starts_with

(* Names that exist without an import. *)
let builtin_names = ["Ok"; "Error"]

(* What the identifier ending at the end of the line could become. `start`
   is where the completed text begins: every candidate replaces the line
   from `start` to its end -- the REPL rebuilds the whole line, the
   language server turns the same pair into a text edit. *)
type completion = { start : int; candidates : string list }

(* `limit` is the offset the identifier cannot begin before -- the end of a
   `:t `-style command prefix, or 0 for a bare line. *)
(* `ctors` are the constructors the scope cannot show: a module's, which the
   scope holds none of, and every spelling with its type -- `PullPolicy.`,
   `apps.PullPolicy.`. They are offered once a dot has been typed. *)
let ident_at ?(limit = 0) ?(ctors = []) (env : Typechecker.env) (line : string)
    : completion =
  let n = String.length line in
  let i = ref (n - 1) in
  while !i >= limit && is_ident_char line.[!i] do decr i done;
  let start = !i + 1 in
  let prefix = String.sub line start (n - start) in
  let candidates =
    (* `FS!` is a handler case naming an operation, not a member of the FS
       module: operations live in their own table rather than the scope, so
       they are matched before the `.` forms below. Writing the namespace is
       what asks for them -- a bare prefix never offers one, because the only
       place an operation can be written is a handler case. *)
    match String.index_opt prefix '!' with
    | Some i when i > 0 && not (String.contains prefix '.') ->
      List.filter (has_prefix ~prefix) (Typechecker.operation_names ())
    | _ ->
    match String.split_on_char '.' prefix with
    | [ns; member_prefix] ->
      (match List.assoc_opt ns env with
       | Some (Typechecker.Namespace (members, _)) ->
         List.filter_map (fun (name, _) ->
           if has_prefix ~prefix:member_prefix name
           then Some (ns ^ "." ^ name) else None)
           members
       | _ -> [])
    | [ident_prefix] ->
      List.filter_map (fun (name, _) ->
        if has_prefix ~prefix:ident_prefix name
           && not (String.contains name '.') then Some name else None) env
      @ List.filter (has_prefix ~prefix:ident_prefix) builtin_names
    | _ -> []
  in
  let candidates =
    if String.contains prefix '.' then
      candidates
      @ List.filter (fun c ->
          has_prefix ~prefix c && not (List.mem c candidates)) ctors
    else candidates
  in
  { start; candidates }

(* The REPL's `:` vocabulary, kept beside the logic that completes it so
   the whole dispatch is testable. *)
(* `:env`/`:v` is not here. It still answers, with a line saying where its
   two jobs went, but a retired command is not one to suggest. *)
let special_commands =
  [":type"; ":t"; ":doc"; ":d"; ":edit"; ":e";
   ":load"; ":l"; ":reload"; ":r"; ":clear"; ":c";
   ":reset"; ":s"; ":exit"; ":x"; ":help"; ":h"]

let ident_arg_commands = [":type "; ":t "; ":doc "; ":d "]

(* Whole-line replacements for the line being edited -- the shape linenoise
   consumes. A line starting with `:` completes the command name, or, past
   a command that takes an identifier, the identifier. *)
let line_completions (env : Typechecker.env) (line : string) : string list =
  let ident_lines ~limit =
    let { start; candidates } = ident_at ~limit env line in
    let before = String.sub line 0 start in
    List.map (fun c -> before ^ c) candidates
  in
  if String.length line > 0 && line.[0] = ':' then
    match
      List.find_opt (fun cmd -> has_prefix ~prefix:cmd line) ident_arg_commands
    with
    | Some cmd -> ident_lines ~limit:(String.length cmd)
    | None -> List.filter (has_prefix ~prefix:line) special_commands
  else ident_lines ~limit:0
