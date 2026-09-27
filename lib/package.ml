(* A package: the directory tree under a `wand.mod`, and what that file says. *)

type require = {
  path    : string;
  version : string;
  name    : string option;
  local   : string option;
}

type t = {
  root    : string;
  file    : string;
  modul   : string;
  wand    : string;
  wand_at : Token.loc option;
  require : require list;
}

exception Error of Token.loc option * string

let file_name = "wand.mod"

let fail loc msg = raise (Error (loc, msg))

let rec find_root dir =
  if Sys.file_exists (Filename.concat dir file_name) then Some dir
  else
    let up = Filename.dirname dir in
    if up = dir then None else find_root up

let located e = match e with
  | Ast.Located (loc, _) -> Some loc
  | _ -> None

let show_kind e = match Ast.strip_located e with
  | Ast.URL _ -> "a URL"
  | Ast.Version _ -> "a version"
  | Ast.Path _ -> "a path"
  | Ast.String _ | Ast.Interp _ | Ast.RawString _ -> "a string"
  | Ast.Int _ -> "a number"
  | Ast.List _ -> "a list"
  | Ast.MapLit _ -> "a record"
  | Ast.Var _ -> "a name"
  | _ -> "an expression"

let read_fields ?(key_at = fun _ -> None) ~what ~allowed ~at e =
  match Ast.strip_located e with
  | Ast.MapLit kvs ->
    List.iter (fun (k, v) ->
      if not (List.mem k allowed) then
        fail (match located v, key_at k with
            | Some l, _ | None, Some l -> Some l
            | None, None -> at)
          (Printf.sprintf "%s has no field `%s`; its fields are %s"
             what k (String.concat ", " (List.map (Printf.sprintf "`%s`") allowed))))
      kvs;
    kvs
  | _ ->
    fail (match located e with Some l -> Some l | None -> at)
      (Printf.sprintf "%s is a record, `{ field = value, ... }`, not %s"
         what (show_kind e))

let field ~what ~at kvs k =
  match List.assoc_opt k kvs with
  | Some v -> v
  | None -> fail at (Printf.sprintf "%s needs a `%s` field" what k)

let as_url ~what ~at k v = match Ast.strip_located v with
  | Ast.URL (u, _) -> u
  | _ ->
    fail (match located v with Some l -> Some l | None -> at)
      (Printf.sprintf "`%s` in %s is a URL, such as https://github.com/you/%s, not %s"
         k what "pkg" (show_kind v))

let as_version ~what ~at k v = match Ast.strip_located v with
  | Ast.Version s -> s
  | _ ->
    fail (match located v with Some l -> Some l | None -> at)
      (Printf.sprintf "`%s` in %s is a version, such as 1.2.0, not %s"
         k what (show_kind v))

let read_require ~at e =
  let what = "a `require` entry" in
  let at = match located e with Some l -> Some l | None -> at in
  let kvs = read_fields ~what ~allowed:["path"; "version"; "name"; "local"] ~at e in
  let path = as_url ~what ~at "path" (field ~what ~at kvs "path") in
  let version = as_version ~what ~at "version" (field ~what ~at kvs "version") in
  let name = Option.map (fun v -> match Ast.strip_located v with
    | Ast.Var n when n <> "" && n.[0] >= 'a' && n.[0] <= 'z' -> n
    | _ ->
      fail (match located v with Some l -> Some l | None -> at)
        "`name` in a `require` entry is a lowercase name, such as json2")
    (List.assoc_opt "name" kvs) in
  let local = Option.map (fun v -> match Ast.strip_located v with
    | Ast.Path p -> p
    | _ ->
      fail (match located v with Some l -> Some l | None -> at)
        (Printf.sprintf "`local` in a `require` entry is a path, such as ../json, not %s"
           (show_kind v)))
    (List.assoc_opt "local" kvs) in
  { path; version; name; local }

let key_loc tokens key =
  let n = Array.length tokens in
  let rec go i =
    if i + 1 >= n then None
    else match tokens.(i), tokens.(i + 1) with
      | (Token.Ident k, loc), (Token.Eq, _) when k = key -> Some loc
      | _ -> go (i + 1)
  in
  go 0

let parse ~file src =
  let tokens = Lexer.tokenize ~file src in
  let at = Some (Token.point ~file 1 1 0) in
  let at_key k = match key_loc (Array.of_list tokens) k with
    | Some l -> Some l
    | None -> at
  in
  let prog =
    try Parser.parse_program tokens
    with Parser.ParseError (loc, msg) -> fail loc msg
  in
  let e = match prog.Ast.items with
    | [Ast.TLExpr e] -> e
    | _ -> fail at "wand.mod holds one record, `{ module = ..., wand = ..., require = [...] }`, and nothing else"
  in
  let what = "wand.mod" in
  let kvs =
    read_fields ~key_at:(fun k -> key_loc (Array.of_list tokens) k)
      ~what ~allowed:["module"; "wand"; "require"] ~at e in
  let modul = as_url ~what ~at:(at_key "module") "module" (field ~what ~at kvs "module") in
  let wand_at = at_key "wand" in
  let wand = as_version ~what ~at:wand_at "wand" (field ~what ~at kvs "wand") in
  let require = match List.assoc_opt "require" kvs with
    | None -> []
    | Some v ->
      (match Ast.strip_located v with
       | Ast.List es -> List.map (read_require ~at) es
       | _ ->
         fail (match located v with Some l -> Some l | None -> at_key "require")
           (Printf.sprintf "`require` in wand.mod is a list of entries, not %s"
              (show_kind v)))
  in
  (modul, wand, wand_at, require)

let read root =
  let file = Filename.concat root file_name in
  let src =
    try In_channel.with_open_text file In_channel.input_all
    with Sys_error msg -> fail None ("cannot read " ^ file ^ ": " ^ msg)
  in
  let (modul, wand, wand_at, require) = parse ~file src in
  { root; file; modul; wand; wand_at; require }

(* The versions a `wand` field accepts: from itself up to the next major,
   where before 1.0 each minor counts as a major. *)
let upper_bound v =
  let major = Evaluator.version_number v 0 and minor = Evaluator.version_number v 1 in
  if major = 0 then Printf.sprintf "0.%d.0" (minor + 1)
  else Printf.sprintf "%d.0.0" (major + 1)

let accepts range running =
  Evaluator.compare_versions running range >= 0
  && Evaluator.compare_versions running (upper_bound range) < 0

let check_wand ?(running = Version.value) pkg =
  if not (accepts pkg.wand running) then
    fail pkg.wand_at
      (Printf.sprintf
         "this package needs wand %s or later, before %s, and this is wand %s. \
          Run it with a wand in that range, or set `wand = %s` here once the \
          package works with it"
         pkg.wand (upper_bound pkg.wand) running running)

(* The package a file belongs to, read and checked, or None for a file
   outside every package. *)
let of_file path =
  let dir = Filename.dirname path in
  let dir = if Filename.is_relative dir then Filename.concat (Sys.getcwd ()) dir else dir in
  match find_root dir with
  | None -> None
  | Some root ->
    let pkg = read root in
    check_wand pkg;
    Some pkg
