(* A package: the directory tree under a `wand.pkg`, and what that file says. *)

type require = {
  path    : string;
  version : string;
  name    : string option;
  local   : string option;
}

type t = {
  root    : string;
  file    : string;
  url   : string;
  wand    : string;
  wand_at : Token.loc option;
  require : require list;
}

exception Error of Token.loc option * string

let file_name = "wand.pkg"

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

(* A URL that leaves out its scheme means https. *)
let normalize_url u =
  match String.index_opt u ':' with
  | Some i when i + 2 < String.length u && String.sub u i 3 = "://" -> u
  | _ -> "https://" ^ u

let as_url ~what ~at k v = match Ast.strip_located v with
  | Ast.URL (u, _) -> normalize_url u
  | _ ->
    fail (match located v with Some l -> Some l | None -> at)
      (Printf.sprintf "`%s` in %s is a URL, such as github.com/you/%s, not %s"
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

(* A name for one major of a package: its last segment as a name, then the
   major, as `json2` or `pkg_fixture0_2`. *)
let alias_for path version =
  let m = Semver.version_number version 0 in
  let major =
    if m = 0 then Printf.sprintf "0_%d" (Semver.version_number version 1) else string_of_int m in
  Parser.suggested_name (last_segment_of path) ^ major

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
  let tokens = Lexer.tokenize ~file ~bare_urls:true src in
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
    | _ -> fail at "wand.pkg holds one record, `{ package = ..., wand = ..., require = [...] }`, and nothing else"
  in
  let what = "wand.pkg" in
  let kvs =
    read_fields ~key_at:(fun k -> key_loc (Array.of_list tokens) k)
      ~what ~allowed:["package"; "wand"; "require"] ~at e in
  let url = as_url ~what ~at:(at_key "package") "package" (field ~what ~at kvs "package") in
  let wand_at = at_key "wand" in
  let wand = as_version ~what ~at:wand_at "wand" (field ~what ~at kvs "wand") in
  let require = match List.assoc_opt "require" kvs with
    | None -> []
    | Some v ->
      (match Ast.strip_located v with
       | Ast.List es -> List.map (read_require ~at) es
       | _ ->
         fail (match located v with Some l -> Some l | None -> at_key "require")
           (Printf.sprintf "`require` in wand.pkg is a list of entries, not %s"
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
             `name = %s`, and import it by that name"
            r.path r.version o.version (alias_for r.path o.version))
        else match o.name, r.name with
          | Some a, Some b when a = b ->
            fail req_at (Printf.sprintf "two `require` entries have `name = %s`" a)
          | _ -> ()) rest;
      dups rest
  in
  dups require;
  (url, wand, wand_at, require)

(* ── Sections ────────────────────────────────────────────────────────── *)

(* wand.pkg is the record a person edits, then the sections wand p writes,
   each opened by a marker line. The record ends at the first marker. *)
type sections = {
  record : string;
  iface  : string list option;
  sum    : string list option;
}

let marker = "-- DO NOT EDIT"

let marker_line name = Printf.sprintf "%s: %s, written by `wand p`" marker name

let starts_with p l = String.length l >= String.length p && String.sub l 0 (String.length p) = p

(* The line the record closes on, counted from 0: where the `{` it opens
   with is closed again. Strings and `--` comments are stepped over, so a
   brace in either does not count. None when it never closes, which the
   record's own parser then reports. *)
let record_end lines =
  let depth = ref 0 and opened = ref false and found = ref None in
  List.iteri (fun i line ->
    if !found = None then begin
      let n = String.length line in
      let j = ref 0 and in_string = ref false in
      while !j < n && !found = None do
        let c = line.[!j] in
        if !in_string then begin
          if c = '\\' then incr j
          else if c = '"' then in_string := false
        end
        else if c = '"' then in_string := true
        else if c = '-' && !j + 1 < n && line.[!j + 1] = '-' then j := n
        else if c = '{' then (incr depth; opened := true)
        else if c = '}' then begin
          decr depth;
          if !opened && !depth = 0 then found := Some i
        end;
        incr j
      done
    end) lines;
  !found

(* A line of the sum section: a URL, a version and a hash. *)
let is_sum_line line =
  match String.split_on_char ' ' (String.trim line) with
  | [_; _; h] -> String.length h > 7 && String.sub h 0 7 = "sha256:"
  | _ -> false

let is_blank_or_comment line =
  let t = String.trim line in
  t = "" || starts_with "--" t

let split_sections ~file text =
  let lines = String.split_on_char '\n' text in
  let closes = record_end lines in
  let record = ref [] and iface = ref None and sum = ref None in
  let current = ref `Record in
  List.iteri (fun i line ->
    let at = Some (Token.point ~file (i + 1) 1 0) in
    if starts_with marker line then begin
      let name =
        if line = marker_line "interface" then `Interface
        else if line = marker_line "sum" then `Sum
        else fail at (Printf.sprintf
          "a section of wand.pkg opens with `%s` or `%s`, and wand p writes \
           both; run `wand p tidy` to write them again"
          (marker_line "interface") (marker_line "sum"))
      in
      (match name, !current with
       | `Interface, `Record -> iface := Some []
       | `Sum, (`Record | `Interface) when !sum = None -> sum := Some []
       | _ -> fail at "wand.pkg holds the record, then the interface section, then the \
                       sum section, each once; run `wand p tidy` to write them again");
      current := name
    end else
      match !current with
      | `Record ->
        (* Past the record's `}` and before any section, a line is in
           neither. Read as more of the record, an interface line whose
           marker had gone was reported as a mistake in the record's own
           syntax -- `cons is '::'` -- which named nothing that was wrong. *)
        (match closes with
         | Some e when i > e && not (is_blank_or_comment line) ->
           fail at "wand.pkg has text after the record that is not in a section. \
                    A section opens with a `-- DO NOT EDIT: ...` line that wand p \
                    writes; run `wand p tidy` to write the sections again"
         | _ -> ());
        record := line :: !record
      | `Interface -> iface := Option.map (fun l -> line :: l) !iface
      | `Sum -> sum := Option.map (fun l -> line :: l) !sum) lines;
  let trim l =
    let rec drop = function "" :: rest -> drop rest | l -> l in
    List.rev (drop (List.rev (drop l)))
  in
  { record = String.concat "\n" (List.rev !record);
    iface = Option.map (fun l -> trim (List.rev l)) !iface;
    sum = Option.map (fun l -> trim (List.rev l)) !sum }

let join_sections { record; iface; sum } =
  let section name = function
    | Some lines -> "\n" ^ marker_line name ^ "\n" ^ String.concat "\n" lines ^ "\n"
    | None -> ""
  in
  String.trim record ^ "\n" ^ section "interface" iface ^ section "sum" sum

(* Set while `wand p tidy` or `wand p interface` runs. Both write the
   sections, so neither needs the ones on disk to be well formed: a section
   that cannot be read is written again. Before this, a marker line typed
   wrong stopped every command, `wand p tidy` among them, and the error
   said to run `wand p tidy`. *)
let repairing = ref false

(* What `repairing` read and could not keep, for the command to say so. *)
let dropped : string list ref = ref []

(* The sections, read the way `wand p tidy` needs them: the record up to its
   `}`, every well-formed sum line after it wherever it stands -- so a hash
   already recorded is still checked, not recorded afresh -- and the
   interface lines under a correct interface marker. Anything else after
   the record is dropped, and named in `dropped`. *)
let salvage_sections lines =
  match record_end lines with
  | None -> None
  | Some e ->
    let record = List.filteri (fun i _ -> i <= e) lines in
    let rest = List.filteri (fun i _ -> i > e) lines in
    let iface = ref None and sum = ref [] in
    let in_iface = ref false in
    List.iter (fun line ->
      if starts_with marker line then begin
        in_iface := line = marker_line "interface" && !iface = None;
        if !in_iface then iface := Some []
        else if line <> marker_line "sum" then dropped := line :: !dropped
      end
      else if is_sum_line line then sum := line :: !sum
      else if !in_iface then iface := Option.map (fun l -> line :: l) !iface
      else if not (is_blank_or_comment line) then dropped := line :: !dropped)
      rest;
    let trim l =
      let rec drop = function "" :: rest -> drop rest | l -> l in
      List.rev (drop (List.rev (drop l)))
    in
    dropped := List.rev !dropped;
    Some { record = String.concat "\n" record;
           iface = Option.map (fun l -> trim (List.rev l)) !iface;
           sum = (match List.rev !sum with [] -> None | l -> Some l) }

let read_sections root =
  let file = Filename.concat root file_name in
  match In_channel.with_open_text file In_channel.input_all with
  | text ->
    (match split_sections ~file text with
     | s -> s
     | exception (Error _ as e) when !repairing ->
       dropped := [];
       (match salvage_sections (String.split_on_char '\n' text) with
        | Some s -> s
        | None -> raise e))
  | exception Sys_error msg -> fail None ("cannot read " ^ file ^ ": " ^ msg)

let write_sections root sections =
  Out_channel.with_open_text (Filename.concat root file_name)
    (fun oc -> output_string oc (join_sections sections))

let read root =
  let file = Filename.concat root file_name in
  let sections = read_sections root in
  let (url, wand, wand_at, require) = parse ~file sections.record in
  { root; file; url; wand; wand_at; require }

(* The versions a `wand` field accepts: from itself up to the next major.
   Before 1.0 that is 1.0.0, so a new minor of wand refuses no package. *)
let upper_bound v =
  let major = Semver.version_number v 0 in
  if major = 0 then "1.0.0"
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
   language server: it outlives any one edit of wand.pkg. *)
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

exception Unresolved of string

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
  | Some d when d <> "" -> Filename.concat (Filename.concat d "wand") "pkg"
  | _ ->
    let home = Option.value (Sys.getenv_opt "HOME") ~default:"." in
    List.fold_left Filename.concat home [".cache"; "wand"; "pkg"]

(* A path that names a place in the cache has only real segments: a `.`
   or `..` would put the clone somewhere else, and a requirement can come
   from any package's wand.pkg, so where it lands is not the package's to
   choose. *)
let check_segments what segs =
  match List.find_opt (fun s -> s = "." || s = "..") segs with
  | Some s ->
    raise (Unresolved (Printf.sprintf
      "%s has a `%s` segment. A package path names one place, so it is \
       written without `.` and `..`" what s))
  | None -> ()

let cache_dir r =
  check_segments r.path (String.split_on_char '/' (url_path r.path));
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

(* What follows the package's own URL in `url`, when `url` is that URL or a
   file under it and no `require` entry names it more closely. Code in a
   package imports its own files by the URL other packages use, as a Go
   module does, so one import line works in both places. *)
let own_rest pkg url =
  let own = segments pkg.url and u = segments url in
  let rec strip a b = match a, b with
    | [], rest -> Some rest
    | x :: a, y :: b when x = y -> strip a b
    | _ -> None
  in
  match strip own u with
  | None -> None
  | Some rest ->
    match entry_for pkg url with
    | Some r when List.length (segments r.path) > List.length own -> None
    | _ -> Some rest

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

(* Every walk of a package's tree reads what is there with `lstat`, so a link
   is a link and never the place it names. A package is files and
   directories: a symbolic link in one is refused, because the tree's hash
   would cover the link and not what it reaches. Followed, a link in a
   fetched tag had the hash read files outside the cache, the chmod below
   make them read-only, and the cleanup after a lost race delete them. *)
let rec files_under ~name dir rel =
  let names = Sys.readdir (Filename.concat dir rel) in
  Array.sort compare names;
  List.concat_map (fun n ->
    let r = if rel = "" then n else rel ^ "/" ^ n in
    match (Unix.lstat (Filename.concat dir r)).Unix.st_kind with
    | Unix.S_DIR -> files_under ~name dir r
    | Unix.S_REG -> [r]
    | Unix.S_LNK ->
      raise (Unresolved (Printf.sprintf
        "%s holds a symbolic link, `%s`. A package is files and directories \
         only, so that its hash covers everything in it" name r))
    | _ ->
      raise (Unresolved (Printf.sprintf
        "%s holds `%s`, which is not a file or a directory" name r)))
    (Array.to_list names)

(* One hash for a tree: each file's path and the hash of its bytes, in path
   order. *)
let tree_hash ~name dir =
  let lines = List.map (fun r ->
    let bytes = In_channel.with_open_bin (Filename.concat dir r) In_channel.input_all in
    Printf.sprintf "%s %s\n" r Digestif.SHA256.(to_hex (digest_string bytes)))
    (files_under ~name dir "") in
  "sha256:" ^ Digestif.SHA256.(to_hex (digest_string (String.concat "" lines)))

let rec remove_tree path =
  match (Unix.lstat path).Unix.st_kind with
  | Unix.S_DIR ->
    Array.iter (fun n -> remove_tree (Filename.concat path n)) (Sys.readdir path);
    Sys.rmdir path
  | _ -> Sys.remove path

let rec set_read_only path =
  match (Unix.lstat path).Unix.st_kind with
  | Unix.S_DIR ->
    Array.iter (fun n -> set_read_only (Filename.concat path n)) (Sys.readdir path)
  | Unix.S_REG -> Unix.chmod path 0o444
  | _ -> ()

let rec mkdir_p dir =
  if not (Sys.file_exists dir) then begin
    mkdir_p (Filename.dirname dir);
    try Unix.mkdir dir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ()
  end

let fetch r =
  let dir = cache_dir r in
  let parent = Filename.dirname dir in
  (try mkdir_p parent
   with Unix.Unix_error (e, _, _) ->
     raise (Unresolved (Printf.sprintf "cannot make the cache directory %s: %s"
                          parent (Unix.error_message e))));
  let tmp = Filename.concat parent
      (Printf.sprintf ".fetch-%d-%s" (Unix.getpid ()) (Filename.basename dir)) in
  if Sys.file_exists tmp then remove_tree tmp;
  Printf.eprintf "wand: fetching %s %s\n%!" r.path r.version;
  let (code, out) =
    run_git ["-c"; "advice.detachedHead=false"; "clone"; "--quiet"; "--depth"; "1";
             "--branch"; "v" ^ r.version; r.path; tmp] in
  if code <> 0 then begin
    (try remove_tree tmp with Sys_error _ | Unix.Unix_error _ -> ());
    raise (Unresolved (Printf.sprintf
      "cannot fetch %s %s: git clone of the tag v%s failed%s"
      r.path r.version r.version (if out = "" then "" else ":\n" ^ out)))
  end;
  remove_tree (Filename.concat tmp ".git");
  let name = Printf.sprintf "%s %s" r.path r.version in
  let h =
    try tree_hash ~name tmp
    with e -> (try remove_tree tmp with Sys_error _ | Unix.Unix_error _ -> ()); raise e in
  set_read_only tmp;
  (try Unix.rename tmp dir
   with Unix.Unix_error _ -> remove_tree tmp);
  h

let hashes : (string, string) Hashtbl.t = Hashtbl.create 4

(* Whether a version missing from the cache may be fetched. The language
   server turns this off: opening a file in an editor is not a request to
   reach the hosts its wand.pkg names, and a repository opened to read it
   would otherwise clone whatever it requires. A command typed at a
   terminal still fetches. *)
let fetch_allowed = ref true

(* The hash of a version in the cache, fetching it first when it is not
   there. Hashed once a run. *)
let cached_hash r =
  let dir = cache_dir r in
  match Hashtbl.find_opt hashes dir with
  | Some h -> h
  | None ->
    let h =
      if Sys.file_exists dir
      then tree_hash ~name:(Printf.sprintf "%s %s" r.path r.version) dir
      else if not !fetch_allowed then
        raise (Unresolved (Printf.sprintf
          "%s %s is not in the package cache, and the editor fetches nothing. \
           Run `wand p tidy` in a terminal to fetch it" r.path r.version))
      else fetch r in
    Hashtbl.replace hashes dir h;
    h

(* ── The sum section ───────────────────────────────────────────────────── *)

let read_sums pkg =
  List.filter_map (fun line ->
    match String.split_on_char ' ' (String.trim line) with
    | [url; version; hash] -> Some ((url, version), hash)
    | _ -> None) (Option.value (read_sections pkg.root).sum ~default:[])

let write_sums pkg sums =
  let sums = List.sort_uniq compare sums in
  let lines = List.map (fun ((url, version), hash) ->
    Printf.sprintf "%s %s %s" url version hash) sums in
  let sections = read_sections pkg.root in
  write_sections pkg.root
    { sections with sum = (if lines = [] then None else Some lines) }

(* Set while `wand p tidy` or `upgrade` runs: every version checked is
   collected here, and a version the sum section has no line for is recorded rather
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
      "%s %s does not match the sum section of wand.pkg, which has %s, and the copy in %s \
       hashes to %s. The module changed after it was recorded, or the cache \
       was changed. Remove %s to fetch it again, and change the sum section only if \
       you trust the new code. If the sum section was changed by hand, restore it \
       from version control, or remove its line for %s %s and run `wand p tidy`"
      r.path r.version want (cache_dir r) h (cache_dir r) r.path r.version))
  | None ->
    raise (Unresolved (Printf.sprintf
      "the sum section of wand.pkg has no line for %s %s. Run `wand p tidy`" r.path r.version))

(* ── The build ────────────────────────────────────────────────────────── *)

(* Before 1.0 each minor is a major. *)
let major v =
  let m = Semver.version_number v 0 in
  if m = 0 then Printf.sprintf "0.%d" (Semver.version_number v 1) else string_of_int m

let key r = (r.path, major r.version)

(* The package of the file the run or the check started from. Its `local`
   fields and its sum section hold for every package in the build. *)
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
      "`import %s` needs a wand.pkg that requires it, and this file is in \
       no package. Run `wand p init <url>` in the package's directory" what))

let file_in main r rest =
  check_segments (String.concat "/" (r.path :: rest)) rest;
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
  let url = normalize_url url in
  let pkg = in_package ~base_dir url in
  match own_rest pkg url with
  | Some rest ->
    let file = match rest with
      | [] -> last_segment pkg.url
      | _ -> String.concat Filename.dir_sep rest
    in
    Filename.concat pkg.root (file ^ ".wand")
  | None ->
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

(* Whether a command-line target that is not a file on disk names a module
   by URL. The shell hands wand strings, so what an import says with its
   literal -- `./x` a path, `github.com/you/x` a URL -- is said here by
   shape: a scheme, or a host (a first segment with a dot) and a path under
   it. Anything written like a path stays one, so a mistyped
   `deploy.wand` or `scripts/deploy` is still "no such file". *)
let names_a_url target =
  let has_scheme =
    match String.index_opt target ':' with
    | Some i -> i + 2 < String.length target && String.sub target i 3 = "://"
    | None -> false
  in
  has_scheme ||
  (target <> ""
   && not (List.mem target.[0] ['.'; '/'; '~'])
   && not (Filename.check_suffix target ".wand")
   && (match segments target with
       | host :: _ :: _ -> String.contains host '.'
       | _ -> false))

(* The file `wand <url>` runs. The URL resolves the way an import of it
   would from `dir`: the package there must require it, its version comes
   from that package's wand.pkg, and the copy is fetched and checked against
   the sum section. That package is the main one for the whole run, so the
   script's own imports resolve against the build the caller pinned rather
   than against whatever the fetched package's wand.pkg says. *)
let resolve_entry ~dir target =
  let bare = url_path target in
  let url = normalize_url target in
  let pkg = match of_dir dir with
    | Some p -> p
    | None ->
      raise (Unresolved (Printf.sprintf
        "`wand %s` reads the version from wand.pkg, and this directory \
         is in no package. Run `wand p init`, then `wand p add %s`" bare bare))
  in
  (match entry_for pkg url with
   | Some _ -> ()
   | None ->
     raise (Unresolved (Printf.sprintf
       "%s is not in the `require` list of %s. Run `wand p add %s` to require it"
       bare pkg.file bare)));
  main := Some pkg;
  let file = resolve_url ~base_dir:dir url in
  if not (Sys.file_exists file) then begin
    let r = selected pkg (Option.get (entry_for pkg url)) in
    let rest = List.filteri (fun i _ -> i >= List.length (segments r.path))
                 (segments url) in
    let name = match rest with [] -> [last_segment r.path] | _ -> rest in
    raise (Unresolved (Printf.sprintf "%s %s has no file %s.wand"
      (url_path r.path) r.version (String.concat "/" name)))
  end;
  file

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
