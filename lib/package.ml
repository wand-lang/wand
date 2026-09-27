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

let last_segment_of u =
  match List.rev (List.filter (( <> ) "") (String.split_on_char '/' u)) with
  | s :: _ -> s
  | [] -> u

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
  let major v =
    let m = Semver.version_number v 0 in
    if m = 0 then Printf.sprintf "0.%d" (Semver.version_number v 1) else string_of_int m
  in
  let req_at = at_key "require" in
  let rec dups = function
    | [] -> ()
    | r :: rest ->
      List.iter (fun o ->
        if o.path = r.path && major o.version = major r.version then
          fail req_at (Printf.sprintf
            "%s is required twice at major %s; keep one entry" r.path (major r.version))
        else if o.path = r.path && o.name = None && r.name = None then
          fail req_at (Printf.sprintf
            "%s is required at %s and at %s; give one of them a name, such as \
             `name = %s%s`, and import it by that name"
            r.path r.version o.version (last_segment_of r.path)
            (String.map (fun c -> if c = '.' then '_' else c) (major o.version)))
        else match o.name, r.name with
          | Some a, Some b when a = b ->
            fail req_at (Printf.sprintf "two `require` entries have `name = %s`" a)
          | _ -> ()) rest;
      dups rest
  in
  dups require;
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
  let major = Semver.version_number v 0 and minor = Semver.version_number v 1 in
  if major = 0 then Printf.sprintf "0.%d.0" (minor + 1)
  else Printf.sprintf "%d.0.0" (major + 1)

let accepts range running =
  Semver.compare_versions running range >= 0
  && Semver.compare_versions running (upper_bound range) < 0

let check_wand ?(running = Version.value) pkg =
  if not (accepts pkg.wand running) then
    fail pkg.wand_at
      (Printf.sprintf
         "this package needs wand %s or later, before %s, and this is wand %s. \
          Run it with a wand in that range, or set `wand = %s` here once the \
          package works with it"
         pkg.wand (upper_bound pkg.wand) running running)

(* Read once for as long as the file is unchanged, which matters to the
   language server: it outlives any one edit of wand.mod. *)
let known : (string, float * t) Hashtbl.t = Hashtbl.create 4

let of_dir dir =
  let dir = if Filename.is_relative dir then Filename.concat (Sys.getcwd ()) dir else dir in
  match find_root dir with
  | None -> None
  | Some root ->
    let stamp =
      try (Unix.stat (Filename.concat root file_name)).Unix.st_mtime with Unix.Unix_error _ -> 0.
    in
    (match Hashtbl.find_opt known root with
     | Some (at, pkg) when at = stamp -> Some pkg
     | _ ->
       let pkg = read root in
       check_wand pkg;
       Hashtbl.replace known root (stamp, pkg);
       Some pkg)

(* The package a file belongs to, read and checked, or None for a file
   outside every package. *)
let of_file path = of_dir (Filename.dirname path)

let url_path u =
  match String.index_opt u ':' with
  | Some i when i + 2 < String.length u && String.sub u i 3 = "://" ->
    String.sub u (i + 3) (String.length u - i - 3)
  | _ -> u

let segments u =
  List.filter (fun s -> s <> "") (String.split_on_char '/' (url_path u))

let last_segment u =
  match List.rev (segments u) with
  | s :: _ -> s
  | [] -> u

let cache_root () =
  match Sys.getenv_opt "XDG_CACHE_HOME" with
  | Some d when d <> "" -> Filename.concat (Filename.concat d "wand") "mod"
  | _ ->
    let home = Option.value (Sys.getenv_opt "HOME") ~default:"." in
    List.fold_left Filename.concat home [".cache"; "wand"; "mod"]

let cache_dir r =
  Filename.concat (cache_root ()) (url_path r.path ^ "@" ^ r.version)

(* The entry whose path is the longest prefix of the URL, by whole segments. *)
let entry_for pkg url =
  let under r =
    let p = segments r.path and u = segments url in
    let rec prefix a b = match a, b with
      | [], _ -> true
      | x :: a, y :: b -> x = y && prefix a b
      | _ :: _, [] -> false
    in
    prefix p u
  in
  List.fold_left (fun best r ->
    if not (under r) then best
    else match best with
      | Some b when List.length (segments b.path) >= List.length (segments r.path) -> best
      | _ -> Some r) None (List.filter (fun r -> r.name = None) pkg.require)

exception Unresolved of string

let absolute dir =
  let full = if Filename.is_relative dir then Filename.concat (Sys.getcwd ()) dir else dir in
  let parts = List.fold_left (fun acc seg -> match seg with
    | "" | "." -> acc
    | ".." -> (match acc with _ :: rest -> rest | [] -> [])
    | s -> s :: acc) [] (String.split_on_char '/' full) in
  "/" ^ String.concat "/" (List.rev parts)

let check_private_path ~base_dir file =
  match find_root (absolute (Filename.dirname file)) with
  | None -> ()
  | Some root when Some root = find_root (absolute base_dir) -> ()
  | Some root ->
    let full = absolute file in
    let n = String.length root in
    let below =
      if String.length full > n && String.sub full 0 n = root
      then String.sub full n (String.length full - n)
      else ""
    in
    match List.find_opt (fun s -> String.length s > 0 && s.[0] = '_')
            (String.split_on_char '/' below) with
    | Some s ->
      raise (Unresolved (Printf.sprintf
        "`%s` is private to the package at %s, so no other package can import it"
        (Filename.remove_extension s) root))
    | None -> ()

(* ── Fetching ─────────────────────────────────────────────────────────── *)

let run_git args =
  let log = Filename.temp_file "wand-git" ".log" in
  let args = "-C" :: Filename.get_temp_dir_name () :: args in
  let code = Sys.command (Filename.quote_command "git" args ~stdout:log ~stderr:log) in
  let out = try In_channel.with_open_text log In_channel.input_all with Sys_error _ -> "" in
  (try Sys.remove log with Sys_error _ -> ());
  (code, String.trim out)

let rec files_under dir rel =
  let names = Sys.readdir (Filename.concat dir rel) in
  Array.sort compare names;
  List.concat_map (fun n ->
    let r = if rel = "" then n else rel ^ "/" ^ n in
    if Sys.is_directory (Filename.concat dir r) then files_under dir r else [r])
    (Array.to_list names)

(* One hash for a tree: each file's path and the hash of its bytes, in path
   order. *)
let tree_hash dir =
  let lines = List.map (fun r ->
    let bytes = In_channel.with_open_bin (Filename.concat dir r) In_channel.input_all in
    Printf.sprintf "%s %s\n" r Digestif.SHA256.(to_hex (digest_string bytes)))
    (files_under dir "") in
  "sha256:" ^ Digestif.SHA256.(to_hex (digest_string (String.concat "" lines)))

let rec remove_tree path =
  if Sys.is_directory path then begin
    Array.iter (fun n -> remove_tree (Filename.concat path n)) (Sys.readdir path);
    Sys.rmdir path
  end else Sys.remove path

let rec set_read_only path =
  if Sys.is_directory path then
    Array.iter (fun n -> set_read_only (Filename.concat path n)) (Sys.readdir path)
  else Unix.chmod path 0o444

let rec mkdir_p dir =
  if not (Sys.file_exists dir) then begin
    mkdir_p (Filename.dirname dir);
    try Unix.mkdir dir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ()
  end

let fetch r =
  let dir = cache_dir r in
  let parent = Filename.dirname dir in
  mkdir_p parent;
  let tmp = Filename.concat parent
      (Printf.sprintf ".fetch-%d-%s" (Unix.getpid ()) (Filename.basename dir)) in
  if Sys.file_exists tmp then remove_tree tmp;
  Printf.eprintf "wand: fetching %s %s\n%!" r.path r.version;
  let (code, out) =
    run_git ["-c"; "advice.detachedHead=false"; "clone"; "--quiet"; "--depth"; "1";
             "--branch"; "v" ^ r.version; r.path; tmp] in
  if code <> 0 then begin
    (try remove_tree tmp with Sys_error _ -> ());
    raise (Unresolved (Printf.sprintf
      "cannot fetch %s %s: git clone of the tag v%s failed%s"
      r.path r.version r.version (if out = "" then "" else ":\n" ^ out)))
  end;
  remove_tree (Filename.concat tmp ".git");
  let h = tree_hash tmp in
  set_read_only tmp;
  (try Unix.rename tmp dir
   with Unix.Unix_error _ -> remove_tree tmp);
  h

let hashes : (string, string) Hashtbl.t = Hashtbl.create 4

(* The hash of a version in the cache, fetching it first when it is not
   there. Hashed once a run. *)
let cached_hash r =
  let dir = cache_dir r in
  match Hashtbl.find_opt hashes dir with
  | Some h -> h
  | None ->
    let h = if Sys.file_exists dir then tree_hash dir else fetch r in
    Hashtbl.replace hashes dir h;
    h

(* ── wand.sum ──────────────────────────────────────────────────────────── *)

let sum_file pkg = Filename.concat pkg.root "wand.sum"

let read_sums pkg =
  match In_channel.with_open_text (sum_file pkg) In_channel.input_all with
  | exception Sys_error _ -> []
  | text ->
    List.filter_map (fun line ->
      match String.split_on_char ' ' (String.trim line) with
      | [url; version; hash] -> Some ((url, version), hash)
      | _ -> None) (String.split_on_char '\n' text)

let write_sums pkg sums =
  let sums = List.sort_uniq compare sums in
  Out_channel.with_open_text (sum_file pkg) (fun oc ->
    List.iter (fun ((url, version), hash) ->
      Printf.fprintf oc "%s %s %s\n" url version hash) sums)

(* Set while `wand p tidy` or `upgrade` runs: every version checked is
   collected here, and a version wand.sum has no line for is recorded rather
   than refused. A mismatch is refused either way. *)
let recording : ((string * string) * string) list ref option ref = ref None

let verify pkg r =
  let h = cached_hash r in
  (match !recording with
   | Some seen -> seen := ((r.path, r.version), h) :: !seen
   | None -> ());
  match List.assoc_opt (r.path, r.version) (read_sums pkg) with
  | Some want when want = h -> ()
  | None when !recording <> None -> ()
  | Some want ->
    raise (Unresolved (Printf.sprintf
      "%s %s does not match wand.sum: wand.sum has %s, and the copy in %s \
       hashes to %s. The module changed after it was recorded, or the cache \
       was changed. Remove %s to fetch it again, and change wand.sum only if \
       you trust the new code"
      r.path r.version want (cache_dir r) h (cache_dir r)))
  | None ->
    raise (Unresolved (Printf.sprintf
      "wand.sum has no line for %s %s. Run `wand p tidy`" r.path r.version))

(* ── The build ────────────────────────────────────────────────────────── *)

(* Before 1.0 each minor is a major. *)
let major v =
  let m = Semver.version_number v 0 in
  if m = 0 then Printf.sprintf "0.%d" (Semver.version_number v 1) else string_of_int m

let key r = (r.path, major r.version)

(* The package of the file the run or the check started from. Its `local`
   fields and its wand.sum hold for every package in the build. *)
let main : t option ref = ref None

let set_main_file path = main := of_file path

let local_dir main (path, mj) =
  List.find_map (fun r ->
    match r.local with
    | Some l when key r = (path, mj) ->
      Some (if Filename.is_relative l then Filename.concat main.root l else l)
    | _ -> None) main.require

(* Where one version of a module is read from, checked. *)
let module_dir main r =
  match local_dir main (key r) with
  | Some dir ->
    if not (Sys.file_exists dir) then
      raise (Unresolved (Printf.sprintf
        "%s has `local = %s` in %s, and there is no such directory"
        r.path dir main.file));
    dir
  | None -> verify main r; cache_dir r

let builds : (string, float * ((string * string) * require) list) Hashtbl.t = Hashtbl.create 2

(* Minimal Version Selection: every version any package in the graph
   requires, and for each module and major the highest of them. *)
let build_list main =
  let stamp = match Hashtbl.find_opt known main.root with Some (at, _) -> at | None -> 0. in
  match Hashtbl.find_opt builds main.root with
  | Some (at, list) when at = stamp -> list
  | _ ->
    let chosen = Hashtbl.create 8 and seen = Hashtbl.create 8 in
    let rec visit r =
      if not (Hashtbl.mem seen (r.path, r.version)) then begin
        Hashtbl.replace seen (r.path, r.version) ();
        (match Hashtbl.find_opt chosen (key r) with
         | Some c when Semver.compare_versions c.version r.version >= 0 -> ()
         | _ -> Hashtbl.replace chosen (key r) r);
        let dir = module_dir main r in
        match of_dir dir with
        | Some dep when dep.root = absolute dir || dep.root = dir -> List.iter visit dep.require
        | _ -> ()
      end
    in
    List.iter visit main.require;
    let list = Hashtbl.fold (fun k r acc -> (k, r) :: acc) chosen [] in
    Hashtbl.replace builds main.root (stamp, list);
    list

let selected main r =
  match List.assoc_opt (key r) (build_list main) with
  | Some chosen -> chosen
  | None -> r

let in_package ~base_dir what =
  match of_dir base_dir with
  | Some p -> p
  | None ->
    raise (Unresolved (Printf.sprintf
      "`import %s` needs a wand.mod that requires it, and this file is in \
       no package. Run `wand p init <url>` in the package's directory" what))

let file_in main r rest =
  (match List.find_opt (fun s -> String.length s > 0 && s.[0] = '_') rest with
   | Some s ->
     raise (Unresolved (Printf.sprintf
       "`%s` is private to %s, so no other package can import it" s r.path))
   | None -> ());
  let chosen = selected main r in
  let dir = module_dir main chosen in
  let file = match rest with
    | [] -> last_segment r.path
    | _ -> String.concat Filename.dir_sep rest
  in
  Filename.concat dir (file ^ ".wand")

let resolve_url ~base_dir url =
  let pkg = in_package ~base_dir url in
  let r = match entry_for pkg url with
    | Some r -> r
    | None ->
      raise (Unresolved (Printf.sprintf
        "%s is not in the `require` list of %s. Run `wand p tidy`" url pkg.file))
  in
  let rest = List.filteri (fun i _ -> i >= List.length (segments r.path)) (segments url) in
  file_in (Option.value !main ~default:pkg) r rest

let resolve_alias ~base_dir name =
  let pkg = in_package ~base_dir name in
  match List.find_opt (fun r -> r.name = Some name) pkg.require with
  | Some r -> file_in (Option.value !main ~default:pkg) r []
  | None ->
    raise (Unresolved (Printf.sprintf
      "`import %s`: no `require` entry in %s has `name = %s`. A standard \
       library module's name is capitalised, as in `import List`"
      name pkg.file name))

(* The module URL and version a file of the build was read from, for a
   message that has to tell two majors of one module apart. *)
let describe_file path =
  let real p = try Unix.realpath p with Unix.Unix_error _ -> absolute p in
  let path = real path in
  let under dir =
    let dir = real dir in
    let n = String.length dir in
    if String.length path > n && String.sub path 0 n = dir && path.[n] = '/'
    then Some (String.sub path (n + 1) (String.length path - n - 1))
    else None
  in
  let named r rest =
    let rest = Filename.remove_extension rest in
    let url = if rest = last_segment r.path then r.path else r.path ^ "/" ^ rest in
    Some (Printf.sprintf "%s %s" url r.version)
  in
  let from_local =
    match !main with
    | None -> None
    | Some m ->
      List.find_map (fun (k, r) ->
        match local_dir m k with
        | Some dir -> Option.bind (under dir) (named r)
        | None -> None) (try build_list m with Unresolved _ -> [])
  in
  match from_local with
  | Some d -> Some d
  | None ->
    match under (cache_root ()) with
    | None -> None
    | Some rest ->
      (match String.index_opt rest '@' with
       | None -> None
       | Some at ->
         let modpath = String.sub rest 0 at in
         let after = String.sub rest (at + 1) (String.length rest - at - 1) in
         (match String.index_opt after '/' with
          | None -> None
          | Some slash ->
            let version = String.sub after 0 slash in
            let file = String.sub after (slash + 1) (String.length after - slash - 1) in
            named { path = "https://" ^ modpath; version; name = None; local = None } file))
