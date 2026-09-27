(* `wand p`: the commands that write wand.pkg. *)

exception Failed of string

let fail msg = raise (Failed msg)

(* ── Writing wand.pkg ──────────────────────────────────────────────────── *)

let short u =
  let p = "https://" in
  let n = String.length p in
  if String.length u > n && String.sub u 0 n = p then String.sub u n (String.length u - n) else u

let render_entry (r : Package.require) =
  let fields =
    (match r.name with Some n -> ["name = " ^ n] | None -> [])
    @ ["path = " ^ short r.path; "version = " ^ r.version]
    @ (match r.local with Some l -> ["local = " ^ l] | None -> [])
  in
  "{ " ^ String.concat ", " fields ^ " }"

let render ~url ~wand (require : Package.require list) =
  let require =
    List.sort (fun (a : Package.require) (b : Package.require) ->
      compare (a.path, a.version) (b.path, b.version)) require
  in
  let head = Printf.sprintf "{ package = %s\n, wand    = %s\n" (short url) wand in
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
    match Lexer.tokenize ~bare_urls:true url with
    | [(Token.URL _, _); (Token.EOF, _)] -> true
    | _ | exception Lexer.LexError _ -> false
  in
  if not is_url then
    fail (Printf.sprintf
      "a package is named by its URL, such as github.com/you/%s, not %s"
      (Filename.basename dir) url);
  write_file file (render ~url:(Package.normalize_url url) ~wand:(running_range ()) [])

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
    | Ast.ModuleURL u -> Some (Url (Package.normalize_url u))
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
  | None -> fail "there is no wand.pkg here or above. Run `wand p init <url>` first"

(* Fetch every version the build reaches, and write the sum section to hold
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
  if require <> pkg.require then begin
    let sections = Package.read_sections pkg.root in
    Package.write_sections pkg.root
      { sections with record = render ~url:pkg.url ~wand:pkg.wand require }
  end;
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

(* ── The api section ──────────────────────────────────────────────────────── *)

(* The files that make up the interface: every module not under a `_`
   segment, and not a test. *)
let public_modules (pkg : Package.t) =
  List.filter_map (fun full ->
    let n = String.length pkg.root in
    let rel = String.sub full (n + 1) (String.length full - n - 1) in
    let segs = String.split_on_char '/' rel in
    let base = Filename.basename rel in
    if List.exists (fun s -> s <> "" && s.[0] = '_') segs
       || (String.length base > 5 && String.sub base 0 5 = "test_")
    then None
    else Some (Filename.chop_suffix rel ".wand", full))
    (wand_files pkg.root "")

let current_api (pkg : Package.t) =
  let modules = public_modules pkg in
  match Runner.api_lines ~root:pkg.root modules with
  | Ok lines -> lines
  | Error d ->
    let first = List.find_map (fun (_, path) ->
      match Runner.typecheck_file path with
      | Error d -> Some (path ^ ": " ^ Diag.legacy d)
      | Ok _ -> None) modules in
    fail (Option.value first ~default:("the package does not typecheck: " ^ Diag.legacy d))

(* The version line and the interface lines of an api section. *)
let parse_api lines =
  let is_version_line l = String.length l > 8 && String.sub l 0 8 = "version " in
  let version = List.find_map (fun l ->
    if is_version_line l then Some (String.trim (String.sub l 8 (String.length l - 8)))
    else None) lines in
  (version, List.filter (fun l -> l <> "" && not (is_version_line l)) lines)

let render_api version lines =
  (match version with Some v -> ["version " ^ v; ""] | None -> []) @ lines

let released_versions (pkg : Package.t) =
  let (code, out) = Package.run_git ["-C"; pkg.root; "tag"; "--list"; "v*"] in
  if code <> 0 then []
  else
    List.sort Semver.compare_versions
      (List.filter_map (fun t ->
        let t = String.trim t in
        if String.length t > 1 && t.[0] = 'v' && is_version (String.sub t 1 (String.length t - 1))
        then Some (String.sub t 1 (String.length t - 1)) else None)
        (String.split_on_char '\n' out))

let last_release pkg = match List.rev (released_versions pkg) with v :: _ -> Some v | [] -> None

(* ── The bump ──────────────────────────────────────────────────────────── *)

type bump = Patch | Minor | Major

let bump_name = function Patch -> "patch" | Minor -> "minor" | Major -> "major"

let split_once text sep =
  let n = String.length sep and m = String.length text in
  let rec go i =
    if i + n > m then None
    else if String.sub text i n = sep
    then Some (String.sub text 0 i, String.sub text (i + n) (m - i - n))
    else go (i + 1)
  in
  go 0

(* An interface line as (name, what it says), padding taken out. *)
let api_entry line =
  let p = "type " in
  let n = String.length p in
  if String.length line > n && String.sub line 0 n = p then
    let rest = String.sub line n (String.length line - n) in
    let stop = ref 0 in
    while !stop < String.length rest && not (List.mem rest.[!stop] [' '; '(']) do incr stop done;
    ("type " ^ String.sub rest 0 !stop, line)
  else
    match split_once line " : " with
    | Some (name, sig_) -> (String.trim name, String.trim name ^ " : " ^ String.trim sig_)
    | None -> (line, line)

type change = Removed of string | Changed of string * string | Added of string

let changes ~before ~after =
  let b = List.map api_entry before and a = List.map api_entry after in
  List.filter_map (fun (name, old) ->
    match List.assoc_opt name a with
    | None -> Some (Removed old)
    | Some now when now <> old -> Some (Changed (old, now))
    | Some _ -> None) b
  @ List.filter_map (fun (name, now) ->
    if List.mem_assoc name b then None else Some (Added now)) a

let needed changes =
  if List.exists (function Removed _ | Changed _ -> true | Added _ -> false) changes then Major
  else if changes <> [] then Minor
  else Patch

let show_change = function
  | Removed old -> "- " ^ old
  | Added now -> "+ " ^ now
  | Changed (old, now) -> "- " ^ old ^ "\n+ " ^ now

(* Before 1.0 a breaking change moves the minor, and anything else the
   patch. *)
let next_version last bump =
  let n i = Semver.version_number last i in
  match bump, n 0 with
  | Major, 0 -> Printf.sprintf "0.%d.0" (n 1 + 1)
  | (Minor | Patch), 0 -> Printf.sprintf "0.%d.%d" (n 1) (n 2 + 1)
  | Major, m -> Printf.sprintf "%d.0.0" (m + 1)
  | Minor, m -> Printf.sprintf "%d.%d.0" m (n 1 + 1)
  | Patch, m -> Printf.sprintf "%d.%d.%d" m (n 1) (n 2 + 1)

(* ── release ───────────────────────────────────────────────────────────── *)

let git_in (pkg : Package.t) args = Package.run_git ("-C" :: pkg.root :: args)

let release ~dir asked =
  let pkg = package_here dir in
  let asked = match asked with
    | None -> None
    | Some "major" -> Some Major
    | Some "minor" -> Some Minor
    | Some "patch" -> Some Patch
    | Some other ->
      fail (Printf.sprintf "the bump is major, minor or patch, not %s" other)
  in
  let (code, status) = git_in pkg ["status"; "--porcelain"; "--"; "."] in
  if code <> 0 then fail ("the package is not in a git repository: " ^ status);
  let dirty = List.filter (fun l ->
    l <> "" && not (Filename.check_suffix l Package.file_name))
      (String.split_on_char '\n' status) in
  if dirty <> [] then
    fail (String.concat "\n"
      ("commit or stash these first, so the tag holds the code the api section describes:"
       :: List.map (fun l -> "  " ^ String.trim l) dirty));
  let lines = current_api pkg in
  let now = List.filter (( <> ) "") lines in
  let version = match last_release pkg with
    | None -> (match asked with Some Major -> "1.0.0" | _ -> "0.1.0")
    | Some last ->
      let before = match git_in pkg ["show"; "v" ^ last ^ ":./" ^ Package.file_name] with
        | (0, text) ->
          (match (Package.split_sections ~file:Package.file_name text).api with
           | Some lines -> snd (parse_api lines)
           | None -> fail (Printf.sprintf "v%s has no api section in wand.pkg to compare with" last))
        | _ -> fail (Printf.sprintf "v%s holds no wand.pkg to compare with" last)
      in
      let found = changes ~before ~after:now in
      let least = match needed found, Semver.version_number last 0 with
        | Minor, 0 -> Patch
        | b, _ -> b
      in
      let rank = function Patch -> 0 | Minor -> 1 | Major -> 2 in
      let bump = match asked with
        | None -> least
        | Some b when rank b >= rank least -> b
        | Some b ->
          fail (String.concat "\n"
            (Printf.sprintf "these changes need a %s release, not a %s one:"
               (bump_name least) (bump_name b)
             :: List.map show_change
                  (List.filter (fun c -> rank (needed [c]) >= rank least) found)))
      in
      next_version last bump
  in
  let sections = Package.read_sections pkg.root in
  Package.write_sections pkg.root
    { sections with api = Some (render_api (Some version) lines) };
  let step args =
    match git_in pkg args with
    | (0, _) -> ()
    | (_, out) -> fail (Printf.sprintf "git %s failed: %s" (String.concat " " args) out)
  in
  step ["add"; Package.file_name];
  (match git_in pkg ["diff"; "--cached"; "--quiet"; "--"; Package.file_name] with
   | (0, _) -> ()
   | _ -> step ["commit"; "--quiet"; "-m"; "Release v" ^ version; "--"; Package.file_name]);
  step ["tag"; "v" ^ version];
  Printf.printf "tagged v%s. Push it: git push origin HEAD v%s\n" version version

let api ~dir ~check =
  let pkg = package_here dir in
  let lines = current_api pkg in
  let sections = Package.read_sections pkg.root in
  let (version, recorded) = match sections.api with
    | Some l -> parse_api l
    | None -> (None, [])
  in
  if check then begin
    if sections.api = None then
      fail "wand.pkg has no api section. Run `wand p api` and commit it";
    let found = changes ~before:recorded ~after:(List.filter (( <> ) "") lines) in
    if found <> [] || sections.api <> Some (render_api version lines) then
      fail (String.concat "\n"
        ("the api section of wand.pkg does not match the code. Run `wand p api` \
          and commit the change:"
         :: List.map show_change found));
    let tag = last_release pkg in
    if version <> tag then
      fail (Printf.sprintf "the api section says version %s, and the latest release tag is %s"
              (Option.value version ~default:"(none)")
              (match tag with Some v -> "v" ^ v | None -> "(none)"))
  end else
    Package.write_sections pkg.root { sections with api = Some (render_api version lines) }
