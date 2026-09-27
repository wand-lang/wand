(* `wand p`: the commands that write wand.mod and wand.sum. *)

exception Failed of string

let fail msg = raise (Failed msg)

(* ── Writing wand.mod ──────────────────────────────────────────────────── *)

let render_entry (r : Package.require) =
  let fields =
    (match r.name with Some n -> ["name = " ^ n] | None -> [])
    @ ["path = " ^ r.path; "version = " ^ r.version]
    @ (match r.local with Some l -> ["local = " ^ l] | None -> [])
  in
  "{ " ^ String.concat ", " fields ^ " }"

let render ~modul ~wand (require : Package.require list) =
  let require =
    List.sort (fun (a : Package.require) (b : Package.require) ->
      compare (a.path, a.version) (b.path, b.version)) require
  in
  let head = Printf.sprintf "{ module  = %s\n, wand    = %s\n" modul wand in
  match require with
  | [] -> head ^ "}\n"
  | first :: rest ->
    head ^ ", require =\n    [ " ^ render_entry first ^ "\n"
    ^ String.concat "" (List.map (fun r -> "    , " ^ render_entry r ^ "\n") rest)
    ^ "    ]\n}\n"

let write_file path text = Out_channel.with_open_text path (fun oc -> output_string oc text)

(* ── init ──────────────────────────────────────────────────────────────── *)

let running_range () =
  Printf.sprintf "%d.%d.0"
    (Semver.version_number Version.value 0) (Semver.version_number Version.value 1)

let init ~dir url =
  let file = Filename.concat dir Package.file_name in
  if Sys.file_exists file then fail (file ^ " already exists");
  let is_url =
    List.exists (fun p ->
      String.length url > String.length p && String.sub url 0 (String.length p) = p)
      ["https://"; "http://"]
  in
  if not is_url then
    fail (Printf.sprintf
      "a module is named by its URL, such as https://github.com/you/%s, not %s"
      (Filename.basename dir) url);
  write_file file (render ~modul:url ~wand:(running_range ()) [])

(* ── Reading the imports ───────────────────────────────────────────────── *)

type import = Url of string | Alias of string

let rec wand_files root dir =
  let here = Filename.concat root dir in
  let names = Sys.readdir here in
  Array.sort compare names;
  List.concat_map (fun n ->
    let rel = if dir = "" then n else Filename.concat dir n in
    let full = Filename.concat root rel in
    if n <> "" && n.[0] = '.' then []
    else if Sys.is_directory full then
      if Sys.file_exists (Filename.concat full Package.file_name) then []
      else wand_files root rel
    else if Filename.check_suffix n ".wand" then [full]
    else []) (Array.to_list names)

let imports_of_file file =
  let src = In_channel.with_open_text file In_channel.input_all in
  let prog =
    try Parser.parse_program (Lexer.tokenize ~file src) with
    | Parser.ParseError (loc, msg) ->
      fail (Printf.sprintf "%s%s: %s" file
              (match loc with Some l -> Printf.sprintf ":%d:%d" l.Token.line l.Token.col | None -> "")
              msg)
    | Lexer.LexError (l, msg) ->
      fail (Printf.sprintf "%s:%d:%d: %s" file l.Token.line l.Token.col msg)
  in
  let of_kind = function
    | Ast.ModuleURL u -> Some (Url u)
    | Ast.ModuleAlias n -> Some (Alias n)
    | Ast.StdlibModule _ | Ast.UserPath _ -> None
  in
  List.filter_map (function
    | Ast.TLImport k -> of_kind k
    | Ast.TLLet (_, [], body) | Ast.TLLetPat (_, body) ->
      (match Ast.strip_located body with
       | Ast.ImportExpr k -> of_kind k
       | _ -> None)
    | _ -> None) prog.Ast.items

(* ── Versions on the remote ────────────────────────────────────────────── *)

let is_version s =
  let core = match String.index_opt s '-' with Some i -> String.sub s 0 i | None -> s in
  match String.split_on_char '.' core with
  | [a; b; c] ->
    List.for_all (fun p -> p <> "" && String.for_all (fun c -> c >= '0' && c <= '9') p) [a; b; c]
  | _ -> false

(* The versions a module has tagged, or None when the URL names no
   repository git can read. *)
let tagged_versions url =
  Unix.putenv "GIT_TERMINAL_PROMPT" "0";
  let (code, out) = Package.run_git ["ls-remote"; "--tags"; url] in
  if code <> 0 then None
  else
    Some (List.sort_uniq Semver.compare_versions
      (List.filter_map (fun line ->
        match String.split_on_char '\t' line with
        | [_; reference] ->
          let prefix = "refs/tags/v" in
          let n = String.length prefix in
          if String.length reference > n && String.sub reference 0 n = prefix then
            let tag = String.sub reference n (String.length reference - n) in
            let tag =
              if Filename.check_suffix tag "^{}" then Filename.chop_suffix tag "^{}" else tag
            in
            if is_version tag then Some tag else None
          else None
        | _ -> None) (String.split_on_char '\n' out)))

let latest versions =
  let releases = List.filter (fun v -> not (String.contains v '-')) versions in
  match List.rev (if releases = [] then versions else releases) with
  | v :: _ -> Some v
  | [] -> None

(* The module a URL belongs to: the longest prefix that is a repository
   with a tagged version. *)
let find_module url =
  let segs = Package.segments url in
  let scheme = match String.index_opt url ':' with
    | Some i -> String.sub url 0 i
    | None -> "https"
  in
  let rec try_prefix n =
    if n < 2 then
      fail (Printf.sprintf "no repository with a tagged version was found for %s" url)
    else
      let candidate =
        scheme ^ "://" ^ String.concat "/" (List.filteri (fun i _ -> i < n) segs) in
      match tagged_versions candidate with
      | Some vs ->
        (match latest vs with
         | Some v -> (candidate, v)
         | None ->
           fail (Printf.sprintf
             "%s has no version tags. A release is a tag such as v0.1.0" candidate))
      | None -> try_prefix (n - 1)
  in
  try_prefix (List.length segs)

(* ── tidy and upgrade ──────────────────────────────────────────────────── *)

let package_here dir =
  match Package.find_root (Package.absolute dir) with
  | Some root -> Package.read root
  | None -> fail "there is no wand.mod here or above. Run `wand p init <url>` first"

(* Fetch every version the build reaches, and write wand.sum to hold
   exactly those. *)
let settle (pkg : Package.t) =
  let seen = ref [] in
  Package.recording := Some seen;
  Hashtbl.reset Package.builds;
  Package.main := Some pkg;
  Fun.protect ~finally:(fun () -> Package.recording := None)
    (fun () -> ignore (Package.build_list pkg));
  Package.write_sums pkg !seen

let report say = List.iter print_endline (List.rev say)

let rewrite (pkg : Package.t) require =
  if require <> pkg.require then
    write_file pkg.file (render ~modul:pkg.modul ~wand:pkg.wand require);
  let pkg = { pkg with require } in
  settle pkg

let tidy ~dir =
  let pkg = package_here dir in
  let imports = List.concat_map imports_of_file (wand_files pkg.root "") in
  let say = ref [] in
  let require = ref pkg.require in
  List.iter (function
    | Url u ->
      if Package.entry_for { pkg with require = !require } u = None then begin
        let (path, version) = find_module u in
        say := Printf.sprintf "added %s %s" path version :: !say;
        require := !require @ [{ Package.path; version; name = None; local = None }]
      end
    | Alias n ->
      if not (List.exists (fun (r : Package.require) -> r.name = Some n) !require) then
        fail (Printf.sprintf
          "`import %s` names no `require` entry. Add one with `name = %s` to %s"
          n n pkg.file)) imports;
  let used (r : Package.require) =
    List.exists (function
      | Url u ->
        (match Package.entry_for { pkg with require = !require } u with
         | Some e -> e == r
         | None -> false)
      | Alias n -> r.name = Some n) imports
  in
  let kept = List.filter used !require in
  List.iter (fun (r : Package.require) ->
    if not (List.memq r kept) then
      say := Printf.sprintf "removed %s %s" r.path r.version :: !say) !require;
  rewrite pkg kept;
  report !say

let upgrade ~dir target =
  let pkg = package_here dir in
  let (only, pinned) = match target with
    | None -> (None, None)
    | Some t ->
      (match String.index_opt t '@' with
       | Some i -> (Some (String.sub t 0 i), Some (String.sub t (i + 1) (String.length t - i - 1)))
       | None -> (Some t, None))
  in
  (match only with
   | Some u when not (List.exists (fun (r : Package.require) -> r.path = u) pkg.require) ->
     fail (Printf.sprintf "%s is not in the `require` list of %s" u pkg.file)
   | _ -> ());
  let say = ref [] in
  let require = List.map (fun (r : Package.require) ->
    if (match only with Some u -> u <> r.path | None -> false) then r
    else
      let target_version = match pinned with
        | Some v ->
          if not (is_version v) then
            fail (Printf.sprintf "%s is not a version, such as 1.2.0" v);
          if Package.major v <> Package.major r.version then
            fail (Printf.sprintf
              "%s %s is a different major from %s, and so a different module. \
               Require it as its own entry, with a `name`, and change the \
               imports that should use it"
              r.path v r.version);
          Some v
        | None ->
          (match tagged_versions r.path with
           | None -> fail (Printf.sprintf "cannot list the versions of %s" r.path)
           | Some vs ->
             latest (List.filter (fun v -> Package.major v = Package.major r.version) vs))
      in
      match target_version with
      | Some v when v <> r.version ->
        say := Printf.sprintf "%s %s -> %s" r.path r.version v :: !say;
        { r with version = v }
      | _ -> r) pkg.require in
  rewrite pkg require;
  report !say
