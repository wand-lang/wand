open Wand

let contains msg needle =
  let n = String.length needle and m = String.length msg in
  let rec go i = i + n <= m && (String.sub msg i n = needle || go (i + 1)) in
  go 0

let fresh_dir () =
  let d = Filename.concat (Filename.get_temp_dir_name ())
      (Printf.sprintf "wand-pkg-%d-%d" (Unix.getpid ()) (Random.bits ())) in
  Unix.mkdir d 0o755; d

let write path text = Out_channel.with_open_text path (fun oc -> output_string oc text)

let parse src = Package.parse ~file:"wand.pkg" src

(* Replace the sum section of dir's wand.pkg with these lines. *)
let write_sum dir text =
  let sections = Package.read_sections dir in
  let lines = List.filter (( <> ) "") (String.split_on_char '\n' text) in
  Package.write_sections dir { sections with sum = Some lines }

let sum_section dir =
  String.concat "\n" (Option.value (Package.read_sections dir).sum ~default:[])

let interface_section dir =
  String.concat "\n" (Option.value (Package.read_sections dir).iface ~default:[])


let parse_error label needle src =
  match parse src with
  | exception Package.Error (_, msg) ->
    if not (contains msg needle) then
      Alcotest.failf "%s: expected %S in: %s" label needle msg
  | _ -> Alcotest.failf "%s: expected an error" label

(* The sections are what `wand p` writes, so a person who breaks one can
   always have them written again. *)
let record = "{ package = https://x.dev/a\n, wand    = 0.93.0\n}\n"
let iface_marker = "-- DO NOT EDIT: interface, written by `wand p`"
let sum_marker = "-- DO NOT EDIT: sum, written by `wand p`"
let a_sum = "https://x.dev/b 1.0.0 sha256:7fad9f3e89a24df16d0956882e356db96304aae3bf686e75e8dbd5bf63700afe"

let sections_of text =
  let dir = fresh_dir () in
  write (Filename.concat dir "wand.pkg") text;
  Package.read_sections dir

let repaired text =
  Package.repairing := true;
  Fun.protect ~finally:(fun () -> Package.repairing := false)
    (fun () -> sections_of text)

(* Text between the record and the first section is in neither. Read as
   more of the record, an interface line was reported as `cons is '::'`. *)
let test_text_after_the_record () =
  (match sections_of (record ^ "\nlib.double : Int -> Int\n\n" ^ sum_marker ^ "\n" ^ a_sum ^ "\n") with
   | exception Package.Error (_, msg) ->
     if not (contains msg "text after the record that is not in a section") then
       Alcotest.failf "the error does not say what is wrong: %s" msg;
     if not (contains msg "wand p tidy") then
       Alcotest.failf "the error does not say what to run: %s" msg
   | _ -> Alcotest.fail "text in no section was read");
  (* A comment there is still the record's. *)
  ignore (sections_of (record ^ "-- a note\n\n" ^ sum_marker ^ "\n" ^ a_sum ^ "\n"))

(* `wand p tidy` and `wand p interface` read a broken file as far as it
   goes: the record, every sum line wherever it stands, and the interface
   under its marker. What else stands after the record is dropped. Before,
   they stopped on the same error as everything else -- which told the
   reader to run `wand p tidy`. *)
let test_a_broken_file_is_read_to_repair () =
  let s = repaired (record ^ "\n" ^ iface_marker ^ "\nlib.double : Int -> Int\n\n\
                    -- DO NOT EDIT: sums, written by `wand p`\n" ^ a_sum ^ "\n") in
  Alcotest.(check (option (list string))) "a mistyped marker keeps its sum line"
    (Some [a_sum]) s.Package.sum;
  Alcotest.(check (option (list string))) "and the interface"
    (Some ["lib.double : Int -> Int"]) s.Package.iface;
  Alcotest.(check (list string)) "and drops the marker"
    ["-- DO NOT EDIT: sums, written by `wand p`"] !Package.dropped;
  let s = repaired (record ^ "\n" ^ sum_marker ^ "\n" ^ a_sum ^ "\n\n" ^ iface_marker ^ "\nversion 0.2.0\n") in
  Alcotest.(check (option (list string))) "sections in the wrong order keep both"
    (Some ["version 0.2.0"]) s.Package.iface;
  Alcotest.(check (option (list string))) "the sum too" (Some [a_sum]) s.Package.sum;
  let s = repaired (record ^ "\nlib.double : Int -> Int\n" ^ a_sum ^ "\n") in
  Alcotest.(check (option (list string))) "with no markers the sum line is kept"
    (Some [a_sum]) s.Package.sum;
  Alcotest.(check (option (list string))) "and no interface is made up" None s.Package.iface;
  Alcotest.(check (list string)) "the interface line is dropped"
    ["lib.double : Int -> Int"] !Package.dropped;
  (* Only these two commands read this way. *)
  (match sections_of (record ^ "\nlib.double : Int -> Int\n") with
   | exception Package.Error _ -> ()
   | _ -> Alcotest.fail "a build read a broken wand.pkg")

let test_reads_the_file () =
  let (url, wand, _, require) = parse {|{ package = https://github.com/mjstahl/json
, wand    = 0.4.0
, require =
    [ { path = https://github.com/mjstahl/text, version = 1.2.0 }
    , { name = json2, path = https://github.com/mjstahl/json, version = 2.1.0, local = ../json }
    ]
}|} in
  Alcotest.(check string) "package" "https://github.com/mjstahl/json" url;
  Alcotest.(check string) "wand" "0.4.0" wand;
  match require with
  | [a; b] ->
    Alcotest.(check string) "first path" "https://github.com/mjstahl/text" a.Package.path;
    Alcotest.(check string) "first version" "1.2.0" a.version;
    Alcotest.(check (option string)) "no name" None a.name;
    Alcotest.(check (option string)) "name" (Some "json2") b.name;
    Alcotest.(check (option string)) "local" (Some "../json") b.local
  | _ -> Alcotest.fail "expected two entries"

let test_require_is_optional () =
  let (_, _, _, require) = parse "{ package = https://x.dev/a, wand = 0.85.0 }" in
  Alcotest.(check int) "no entries" 0 (List.length require)

let test_refuses_what_is_not_data () =
  parse_error "a list" "wand.pkg is a record" "[1]";
  parse_error "two items" "holds one record" "let x = 1\n{ package = https://x.dev/a }";
  parse_error "unknown field" "has no field `extra`"
    "{ package = https://x.dev/a, wand = 0.85.0, extra = 1 }";
  parse_error "missing wand" "needs a `wand` field" "{ package = https://x.dev/a }";
  parse_error "a string version" "is a version, such as 1.2.0, not a string"
    {|{ package = https://x.dev/a, wand = "0.85.0" }|};
  parse_error "a package that is not a URL" "is a URL"
    "{ package = ./a, wand = 0.85.0 }";
  parse_error "an entry field" "has no field `tag`"
    "{ package = https://x.dev/a, wand = 0.85.0, require = [ { path = https://x.dev/b, version = 1.0.0, tag = 1 } ] }";
  parse_error "an uppercase alias" "lowercase name"
    "{ package = https://x.dev/a, wand = 0.85.0, require = [ { path = https://x.dev/b, version = 1.0.0, name = Json } ] }";
  parse_error "a local that is not a path" "is a path"
    {|{ package = https://x.dev/a, wand = 0.85.0, require = [ { path = https://x.dev/b, version = 1.0.0, local = "x" } ] }|}

let test_the_wand_range () =
  let yes range running =
    Alcotest.(check bool) (range ^ " accepts " ^ running) true (Package.accepts range running) in
  let no range running =
    Alcotest.(check bool) (range ^ " refuses " ^ running) false (Package.accepts range running) in
  yes "0.4.0" "0.4.0"; yes "0.4.0" "0.4.9"; yes "0.4.0" "0.86.0"; no "0.4.0" "1.0.0";
  no "0.4.2" "0.4.1";
  yes "1.2.0" "1.9.3"; no "1.2.0" "2.0.0"; no "1.2.0" "1.1.0"

let test_found_above_the_file () =
  let root = fresh_dir () in
  let sub = Filename.concat root "lib" in
  Unix.mkdir sub 0o755;
  write (Filename.concat root "wand.pkg") "{ package = https://x.dev/a, wand = 99.0.0 }";
  let file = Filename.concat sub "main.wand" in
  write file "1 + 1";
  (match Runner.run_file file with
   | Error e ->
     Alcotest.(check bool) "names the range" true
       (contains e "needs wand 99.0.0 or later, before 100.0.0");
     Alcotest.(check bool) "names the file" true (contains e "wand.pkg:1:")
   | Ok _ -> Alcotest.fail "expected the range to refuse this wand");
  write (Filename.concat root "wand.pkg")
    (Printf.sprintf "{ package = https://x.dev/a, wand = %s }" Version.value);
  Alcotest.(check (result string string)) "runs in range" (Ok "2") (Runner.run_file file)

(* An app requiring json through a local copy, as a directory tree. *)
let with_two_packages f =
  let root = fresh_dir () in
  let app = Filename.concat root "app" and json = Filename.concat root "json" in
  List.iter (fun d -> Unix.mkdir d 0o755)
    [app; json; Filename.concat json "_internal"];
  write (Filename.concat json "wand.pkg")
    (Printf.sprintf "{ package = https://x.dev/me/json, wand = %s }" Version.value);
  write (Filename.concat json "json.wand") {|let parse s = "parsed %{s}"|};
  write (Filename.concat json "decode.wand") "let decode s = s";
  write (Filename.concat (Filename.concat json "_internal") "p.wand") "let x = 1";
  write (Filename.concat json "inside.wand") "import ./_internal/p\np.x";
  write (Filename.concat app "wand.pkg")
    (Printf.sprintf
       "{ package = https://x.dev/me/app, wand = %s, require = [ { path = https://x.dev/me/json, version = 1.4.0, local = ../json } ] }"
       Version.value);
  f ~app ~json

let run_in dir name src =
  let file = Filename.concat dir name in
  write file src;
  Runner.run_file file

let error_says label needle = function
  | Error e ->
    if not (contains e needle) then Alcotest.failf "%s: expected %S in: %s" label needle e
  | Ok v -> Alcotest.failf "%s: expected an error, got %s" label v

let test_url_imports () =
  with_two_packages (fun ~app ~json:_ ->
    Alcotest.(check (result string string)) "root and a file below it"
      (Ok "parsed x")
      (run_in app "main.wand"
         "import https://x.dev/me/json\nimport https://x.dev/me/json/decode\njson.parse (decode.decode \"x\")");
    error_says "not required" "is not in the `require` list"
      (run_in app "other.wand" "import https://x.dev/other/thing\n1");
    error_says "a private file" "`_internal` is private to https://x.dev/me/json"
      (run_in app "priv.wand" "import https://x.dev/me/json/_internal/p\np.x"))

(* A package imports its own files by its own URL, as other packages do,
   and a file reached by the URL and by a path is one module. *)
let test_own_url_imports () =
  with_two_packages (fun ~app:_ ~json ->
    write (Filename.concat json "kind.wand")
      "type Kind = Kind Int\nlet unwrap (k: Kind) = match k with | Kind n -> n";
    Alcotest.(check (result string string)) "the root and a file below it"
      (Ok "parsed x")
      (run_in json "own.wand"
         "import https://x.dev/me/json\nimport https://x.dev/me/json/decode\njson.parse (decode.decode \"x\")");
    Alcotest.(check (result string string)) "one module by URL and by path"
      (Ok "3")
      (run_in json "both.wand"
         "let A = import ./kind\nlet B = import https://x.dev/me/json/kind\nA.unwrap (B.Kind 3)");
    Alcotest.(check (result string string)) "its own private files" (Ok "1")
      (run_in json "priv.wand" "import https://x.dev/me/json/_internal/p\np.x"))

let test_tidy_adds_no_entry_for_the_package () =
  let root = fresh_dir () in
  write (Filename.concat root "wand.pkg")
    (Printf.sprintf "{ package = x.dev/me/a, wand = %s }" Version.value);
  write (Filename.concat root "b.wand") "let x = 1";
  write (Filename.concat root "a.wand") "import x.dev/me/a/b\nlet f = b.x";
  Package_cmd.tidy ~dir:root;
  let sections = Package.read_sections root in
  Alcotest.(check bool) "no require entry" false (contains sections.record "require")

let test_url_import_outside_a_package () =
  let dir = fresh_dir () in
  error_says "no wand.pkg" "this file is in no package"
    (run_in dir "loose.wand" "import https://x.dev/me/json\n1")

let test_private_by_path () =
  with_two_packages (fun ~app ~json ->
    error_says "from another package" "`_internal` is private to the package at"
      (run_in app "bypath.wand" "let p = import ../json/_internal/p\np.x");
    Alcotest.(check (result string string)) "from its own package" (Ok "1")
      (Runner.run_file (Filename.concat json "inside.wand")))

(* ── wand <url> ──────────────────────────────────────────────────────── *)

(* A target on the command line is a string, so its shape decides: a scheme,
   or a host with a path under it. What is written like a path stays one. *)
let test_names_a_url () =
  List.iter (fun (target, want) ->
    Alcotest.(check bool) target want (Package.names_a_url target))
    [ "github.com/wand-lang/plimsoll/cli", true;
      "https://x.dev/me/json", true;
      "x.dev/me", true;
      "notes.txt", false;           (* one segment: a file that is not there *)
      "deploy.wand", false;
      "a.b/deploy.wand", false;
      "scripts/deploy", false;      (* no host *)
      "./x.dev/me", false;
      "/x.dev/me", false;
      "~/x.dev/me", false;
      "", false ]

let test_resolve_entry () =
  with_two_packages (fun ~app ~json ->
    let resolved target = Package.resolve_entry ~dir:app target in
    let says label needle target =
      match resolved target with
      | exception Package.Unresolved e ->
        if not (contains e needle) then
          Alcotest.failf "%s: expected %S in: %s" label needle e
      | f -> Alcotest.failf "%s: expected an error, got %s" label f
    in
    let same a b = Unix.realpath a = Unix.realpath b in
    Alcotest.(check bool) "a file below the package" true
      (same (resolved "x.dev/me/json/decode") (Filename.concat json "decode.wand"));
    Alcotest.(check bool) "the package's own file, with a scheme" true
      (same (resolved "https://x.dev/me/json") (Filename.concat json "json.wand"));
    Alcotest.(check bool) "the package here is the main one" true
      (match !Package.main with
       | Some m -> same m.Package.root app
       | None -> false);
    says "not required names wand p add"
      "Run `wand p add x.dev/other/thing` to require it" "x.dev/other/thing";
    says "a file the package does not have"
      "x.dev/me/json 1.4.0 has no file nope.wand" "x.dev/me/json/nope";
    says "a private file" "`_internal` is private" "x.dev/me/json/_internal/p");
  let dir = fresh_dir () in
  match Package.resolve_entry ~dir "x.dev/me/json" with
  | exception Package.Unresolved e ->
    Alcotest.(check bool) "no wand.pkg says how to make one" true
      (contains e "is in no package. Run `wand p init`, then `wand p add x.dev/me/json`")
  | f -> Alcotest.failf "expected an error outside a package, got %s" f

(* The script runs from the package that required it, and so do its own
   imports: `tool` requires `lib` with no local copy, and only the app says
   where `lib` is. Read with the script's own wand.pkg as the main one, the
   run would go to the network for it. *)
let test_run_entry_keeps_the_caller_s_build () =
  let root = fresh_dir () in
  let at p = Filename.concat root p in
  List.iter (fun d -> Unix.mkdir (at d) 0o755) ["app"; "tool"; "lib"];
  let v = Version.value in
  write (at "lib/wand.pkg") (Printf.sprintf "{ package = https://x.dev/me/lib, wand = %s }" v);
  write (at "lib/lib.wand") {|let shout s = "%{s}!"|};
  write (at "tool/wand.pkg") (Printf.sprintf
    "{ package = https://x.dev/me/tool, wand = %s, require = [ { path = https://x.dev/me/lib, version = 1.0.0 } ] }" v);
  write (at "tool/cli.wand") "import https://x.dev/me/lib\nlib.shout \"gen\"";
  write (at "app/wand.pkg") (Printf.sprintf
    "{ package = https://x.dev/me/app, wand = %s, require = [ \
     { path = https://x.dev/me/tool, version = 1.0.0, local = ../tool }, \
     { path = https://x.dev/me/lib, version = 1.0.0, local = ../lib } ] }" v);
  let file = Package.resolve_entry ~dir:(at "app") "x.dev/me/tool/cli" in
  Alcotest.(check (result string string)) "the script and its import run"
    (Ok "gen!") (Runner.run_file ~keep_main:true file)

(* A git repository standing in for https://x.dev/me/json, which git is told
   to read from disk, and a cache of this test's own. *)
let entry_of path version = { Package.path; version; name = None; local = None }

let with_remote f =
  let root = fresh_dir () in
  let repo = Filename.concat root "repos/me/json" in
  ignore (Sys.command (Filename.quote_command "mkdir" ["-p"; repo]));
  write (Filename.concat repo "json.wand") {|let parse s = "parsed %{s}"|};
  write (Filename.concat repo "wand.pkg")
    (Printf.sprintf "{ package = https://x.dev/me/json, wand = %s }" Version.value);
  let git args = ignore (Sys.command (Filename.quote_command "git" ("-C" :: repo :: args)
      ~stdout:"/dev/null" ~stderr:"/dev/null")) in
  git ["init"; "-q"];
  git ["add"; "."];
  git ["-c"; "user.email=t@t"; "-c"; "user.name=t"; "commit"; "-qm"; "one"];
  git ["tag"; "v1.4.0"];
  Unix.putenv "XDG_CACHE_HOME" (Filename.concat root "cache");
  Unix.putenv "GIT_CONFIG_COUNT" "1";
  Unix.putenv "GIT_CONFIG_KEY_0" ("url.file://" ^ root ^ "/repos/.insteadOf");
  Unix.putenv "GIT_CONFIG_VALUE_0" "https://x.dev/";
  let app = Filename.concat root "app" in
  Unix.mkdir app 0o755;
  f ~app ~repo

let app_mod version =
  Printf.sprintf
    "{ package = https://x.dev/me/app, wand = %s, require = [ { path = https://x.dev/me/json, version = %s } ] }"
    Version.value version

let test_fetch_and_sum () =
  with_remote (fun ~app ~repo:_ ->
    write (Filename.concat app "wand.pkg") (app_mod "1.4.0");
    let main = "import https://x.dev/me/json\njson.parse \"x\"" in
    error_says "fetched, and no line in the sum section" "sum section of wand.pkg has no line for https://x.dev/me/json 1.4.0"
      (run_in app "main.wand" main);
    let cached = Package.cache_dir
        { Package.path = "https://x.dev/me/json"; version = "1.4.0"; name = None; local = None } in
    Alcotest.(check bool) "in the cache" true
      (Sys.file_exists (Filename.concat cached "json.wand"));
    Alcotest.(check bool) "without .git" false
      (Sys.file_exists (Filename.concat cached ".git"));
    let h = Package.tree_hash ~name:"json" cached in
    write_sum app (Printf.sprintf "https://x.dev/me/json 1.4.0 %s\n" h);
    Alcotest.(check (result string string)) "runs once recorded" (Ok "parsed x")
      (run_in app "main.wand" main);
    Hashtbl.reset Package.hashes;
    write_sum app "https://x.dev/me/json 1.4.0 sha256:00\n";
    error_says "a mismatch" "does not match the sum section" (run_in app "main.wand" main);
    (* The other cause: the sum section, changed by hand. *)
    error_says "and the other cause of one" "restore it from version control"
      (run_in app "main.wand" main);
    write (Filename.concat app "wand.pkg") (app_mod "1.5.0");
    error_says "no such tag" "git clone of the tag v1.5.0 failed" (run_in app "main.wand" main))

(* A tag holding a symbolic link is refused, and the fetch leaves what the
   link names alone. Followed, the hash read files outside the cache and the
   chmod made them read-only. *)
let test_a_link_in_a_package_is_refused () =
  with_remote (fun ~app ~repo ->
    let root = Filename.dirname app in
    let victim = Filename.concat root "victim" in
    Unix.mkdir victim 0o755;
    write (Filename.concat victim "kept.txt") "mine";
    Unix.chmod (Filename.concat victim "kept.txt") 0o644;
    Unix.symlink victim (Filename.concat repo "outside");
    let git args = ignore (Sys.command (Filename.quote_command "git" ("-C" :: repo :: args)
        ~stdout:"/dev/null" ~stderr:"/dev/null")) in
    git ["add"; "."];
    git ["-c"; "user.email=t@t"; "-c"; "user.name=t"; "commit"; "-qm"; "two"];
    git ["tag"; "v2.0.0"];
    write (Filename.concat app "wand.pkg") (app_mod "2.0.0");
    error_says "the fetch is refused" "holds a symbolic link, `outside`"
      (run_in app "main.wand" "import https://x.dev/me/json\njson.parse \"x\"");
    Alcotest.(check int) "what the link names keeps its mode" 0o644
      (Unix.stat (Filename.concat victim "kept.txt")).Unix.st_perm;
    Alcotest.(check bool) "and its contents" true
      (Sys.file_exists (Filename.concat victim "kept.txt"));
    let cached = Package.cache_dir (entry_of "https://x.dev/me/json" "2.0.0") in
    Alcotest.(check bool) "nothing is cached" false (Sys.file_exists cached);
    Alcotest.(check (list string)) "and no clone is left behind" []
      (List.filter (fun n -> String.starts_with ~prefix:".fetch-" n)
         (Array.to_list (Sys.readdir (Filename.dirname cached)))))

(* With fetching off, as the language server has it, a version missing
   from the cache is reported and nothing is cloned. *)
let test_no_fetch_when_it_is_off () =
  with_remote (fun ~app ~repo:_ ->
    write (Filename.concat app "wand.pkg") (app_mod "1.4.0");
    Package.fetch_allowed := false;
    Fun.protect ~finally:(fun () -> Package.fetch_allowed := true) (fun () ->
      error_says "it says to fetch it" "Run `wand p tidy`"
        (run_in app "main.wand" "import https://x.dev/me/json\njson.parse \"x\""));
    Alcotest.(check bool) "and nothing was cloned" false
      (Sys.file_exists (Package.cache_dir (entry_of "https://x.dev/me/json" "1.4.0"))))

(* A requirement can come from any package's wand.pkg, so where its clone
   lands is not up to the path: one with `..` is refused. *)
let test_a_dot_segment_is_refused () =
  match Package.cache_dir (entry_of "https://x.dev/me/../../../elsewhere" "1.0.0") with
  | _ -> Alcotest.fail "a path with `..` named a cache directory"
  | exception Package.Unresolved m ->
    Alcotest.(check bool) "it says why" true
      (String.length m > 0 && Option.is_some (String.index_opt m '`'))

(* Repositories under root/repos, each a list of tags and the files at
   that tag, and git told to read https://x.dev/ from there. *)
let with_repos repos f =
  let root = fresh_dir () in
  List.iter (fun (name, tags) ->
    let repo = Filename.concat root ("repos/me/" ^ name) in
    ignore (Sys.command (Filename.quote_command "mkdir" ["-p"; repo]));
    let git args = ignore (Sys.command (Filename.quote_command "git" ("-C" :: repo :: args)
        ~stdout:"/dev/null" ~stderr:"/dev/null")) in
    git ["init"; "-q"];
    List.iter (fun (tag, files) ->
      List.iter (fun (file, text) -> write (Filename.concat repo file) text) files;
      git ["add"; "."];
      git ["-c"; "user.email=t@t"; "-c"; "user.name=t"; "commit"; "-qm"; tag];
      git ["tag"; "v" ^ tag]) tags) repos;
  Unix.putenv "XDG_CACHE_HOME" (Filename.concat root "cache");
  Unix.putenv "GIT_CONFIG_COUNT" "1";
  Unix.putenv "GIT_CONFIG_KEY_0" ("url.file://" ^ root ^ "/repos/.insteadOf");
  Unix.putenv "GIT_CONFIG_VALUE_0" "https://x.dev/";
  let app = Filename.concat root "app" in
  Unix.mkdir app 0o755;
  f ~app

let entry path version = { Package.path; version; name = None; local = None }

let record_sums app entries =
  write_sum app
    (String.concat "" (List.map (fun (path, version) ->
       let r = entry path version in
       let h = if Sys.file_exists (Package.cache_dir r) then Package.tree_hash ~name:"r" (Package.cache_dir r)
         else Package.fetch r in
       Printf.sprintf "%s %s %s\n" path version h) entries))

let json_at v = [
  ("wand.pkg", Printf.sprintf "{ package = https://x.dev/me/json, wand = %s }" Version.value);
  ("json.wand", Printf.sprintf "let version = \"%s\"" v) ]

let test_minimal_version_selection () =
  with_repos [
    ("json", [("1.4.0", json_at "1.4"); ("1.5.0", json_at "1.5"); ("2.0.0", json_at "2.0")]);
    ("text", [("1.0.0", [
       ("wand.pkg", Printf.sprintf
          "{ package = https://x.dev/me/text, wand = %s, require = [ { path = https://x.dev/me/json, version = 1.5.0 } ] }"
          Version.value);
       ("text.wand", "import https://x.dev/me/json\nlet v = json.version") ])]) ]
    (fun ~app ->
      write (Filename.concat app "wand.pkg") (Printf.sprintf
        "{ package = https://x.dev/me/app, wand = %s, require =\n\
        \  [ { path = https://x.dev/me/json, version = 1.4.0 }\n\
        \  , { path = https://x.dev/me/text, version = 1.0.0 }\n\
        \  , { name = json2, path = https://x.dev/me/json, version = 2.0.0 }\n\
        \  ] }" Version.value);
      record_sums app [
        ("https://x.dev/me/json", "1.4.0"); ("https://x.dev/me/json", "1.5.0");
        ("https://x.dev/me/json", "2.0.0"); ("https://x.dev/me/text", "1.0.0") ];
      Alcotest.(check (result string string))
        "the highest 1.x anyone requires, and 2.x beside it"
        (Ok "1.5 1.5 2.0")
        (run_in app "main.wand"
           "import https://x.dev/me/json\nimport https://x.dev/me/text\nimport json2\n\
            \"%{json.version} %{text.v} %{json2.version}\""))

let test_two_majors_need_a_name () =
  let src = "{ package = https://x.dev/a, wand = 0.85.0, require = [ { path = https://x.dev/b, version = 1.0.0 }, { path = https://x.dev/b, version = 2.0.0 } ] }" in
  parse_error "two majors" "give one of them a name, such as `name = b2`" src;
  parse_error "one major twice" "is required twice at major 1"
    "{ package = https://x.dev/a, wand = 0.85.0, require = [ { path = https://x.dev/b, version = 1.0.0 }, { path = https://x.dev/b, version = 1.2.0 } ] }";
  parse_error "before 1.0 a minor is a major" "such as `name = b0_1`"
    "{ package = https://x.dev/a, wand = 0.85.0, require = [ { path = https://x.dev/b, version = 0.2.0 }, { path = https://x.dev/b, version = 0.1.0 } ] }"

let test_unknown_alias () =
  with_two_packages (fun ~app ~json:_ ->
    error_says "no such name" "no `require` entry in"
      (run_in app "alias.wand" "import jsn\n1"))

let test_two_majors_in_a_type_error () =
  let root = fresh_dir () in
  let dir n = let d = Filename.concat root n in Unix.mkdir d 0o755; d in
  let app = dir "app" and j1 = dir "j1" and j2 = dir "j2" in
  List.iter (fun d ->
    write (Filename.concat d "json.wand")
      "type Value = V Int\nlet make n = V n\nlet get v = match v with\n  | V n -> n\n";
    write (Filename.concat d "wand.pkg")
      (Printf.sprintf "{ package = https://x.dev/me/json, wand = %s }" Version.value)) [j1; j2];
  write (Filename.concat app "wand.pkg") (Printf.sprintf
    "{ package = https://x.dev/me/app, wand = %s, require = [ { path = https://x.dev/me/json, version = 1.4.0, local = ../j1 }, { name = json2, path = https://x.dev/me/json, version = 2.1.0, local = ../j2 } ] }"
    Version.value);
  error_says "both named with their versions"
    "expected a `Value` from https://x.dev/me/json 2.1.0, got a `Value` from https://x.dev/me/json 1.4.0"
    (run_in app "main.wand"
       "import https://x.dev/me/json\nimport json2\nlet v = json.make 1\njson2.get v")

let read_file path = In_channel.with_open_text path In_channel.input_all

let test_init_tidy_upgrade () =
  with_repos [
    ("json", [("1.4.0", json_at "1.4"); ("1.5.0", json_at "1.5")]);
    ("text", [("1.0.0", [
       ("wand.pkg", Printf.sprintf
          "{ package = https://x.dev/me/text, wand = %s, require = [ { path = https://x.dev/me/json, version = 1.4.0 } ] }"
          Version.value);
       ("text.wand", "import https://x.dev/me/json\nlet v = json.version") ])]) ]
    (fun ~app ->
      Package_cmd.init ~dir:app (Some "https://x.dev/me/app");
      Alcotest.(check bool) "init refuses a second wand.pkg" true
        (match Package_cmd.init ~dir:app (Some "https://x.dev/me/app") with
         | exception Package_cmd.Failed _ -> true
         | () -> false);
      let main = "import https://x.dev/me/json\nimport https://x.dev/me/text\n\"%{json.version} %{text.v}\"" in
      write (Filename.concat app "main.wand") main;
      Package_cmd.tidy ~dir:app;
      let wmod = read_file (Filename.concat app "wand.pkg") in
      Alcotest.(check bool) "json at its latest" true
        (contains wmod "{ path = x.dev/me/json, version = 1.5.0 }");
      Alcotest.(check bool) "text added" true
        (contains wmod "{ path = x.dev/me/text, version = 1.0.0 }");
      let sums = sum_section app in
      Alcotest.(check int) "a line for every version the build reads" 3
        (List.length (List.filter (( <> ) "") (String.split_on_char '\n' sums)));
      Alcotest.(check (result string string)) "runs" (Ok "1.5 1.5")
        (Runner.run_file (Filename.concat app "main.wand"));
      Package_cmd.upgrade ~dir:app (Some "https://x.dev/me/json@1.4.0");
      Alcotest.(check (result string string)) "pinned" (Ok "1.4 1.4")
        (Runner.run_file (Filename.concat app "main.wand"));
      Package_cmd.upgrade ~dir:app None;
      Alcotest.(check (result string string)) "upgraded" (Ok "1.5 1.5")
        (Runner.run_file (Filename.concat app "main.wand"));
      Alcotest.(check bool) "a new major is refused" true
        (match Package_cmd.upgrade ~dir:app (Some "https://x.dev/me/json@2.0.0") with
         | exception Package_cmd.Failed msg -> contains msg "different major"
         | () -> false);
      write (Filename.concat app "main.wand") "import https://x.dev/me/json\njson.version";
      Package_cmd.tidy ~dir:app;
      Alcotest.(check bool) "text removed" false
        (contains (read_file (Filename.concat app "wand.pkg")) "text");
      Alcotest.(check bool) "and its lines" false
        (contains (sum_section app) "text"))

(* A version whose wand.pkg needs a newer wand than this one is passed over
   by add, tidy and upgrade, and asking for it by name is refused. *)
let test_upgrade_skips_what_this_wand_cannot_use () =
  let n = Semver.version_number Version.value in
  let newer =
    if n 0 = 0 then Printf.sprintf "0.%d.0" (n 1 + 1) else Printf.sprintf "%d.%d.0" (n 0) (n 1 + 1) in
  let json_needing wand v = [
    ("wand.pkg", Printf.sprintf "{ package = https://x.dev/me/json, wand = %s }" wand);
    ("json.wand", Printf.sprintf "let version = \"%s\"" v) ] in
  with_repos [
    ("json", [("1.4.0", json_at "1.4"); ("1.4.1", json_at "1.4.1");
              ("1.5.0", json_needing newer "1.5")]) ]
    (fun ~app ->
      write (Filename.concat app "wand.pkg") (Printf.sprintf
        "{ package = https://x.dev/me/app, wand = %s, require = [ { path = https://x.dev/me/json, version = 1.4.0 } ] }"
        Version.value);
      write (Filename.concat app "main.wand") "import https://x.dev/me/json\njson.version";
      Package_cmd.upgrade ~dir:app None;
      Alcotest.(check (result string string)) "the newest this wand can use" (Ok "1.4.1")
        (Runner.run_file (Filename.concat app "main.wand"));
      Package_cmd.upgrade ~dir:app None;
      Alcotest.(check bool) "and it stays there" true
        (contains (read_file (Filename.concat app "wand.pkg")) "version = 1.4.1");
      Alcotest.(check bool) "asking for the version it passed over is refused" true
        (match Package_cmd.upgrade ~dir:app (Some "https://x.dev/me/json@1.5.0") with
         | exception Package_cmd.Failed msg ->
           contains msg (Printf.sprintf "json 1.5.0 needs wand %s or later" newer)
         | () -> false);
      let fresh name =
        let d = Filename.concat (Filename.dirname app) name in
        Unix.mkdir d 0o755;
        Package_cmd.init ~dir:d (Some ("https://x.dev/me/" ^ name));
        write (Filename.concat d "main.wand") "import https://x.dev/me/json\njson.version";
        d
      in
      let added = fresh "added" in
      Package_cmd.add ~dir:added "https://x.dev/me/json" ~name:None;
      Alcotest.(check bool) "add takes the newest this wand can use" true
        (contains (read_file (Filename.concat added "wand.pkg")) "version = 1.4.1");
      Alcotest.(check bool) "add refuses the version by name" true
        (match Package_cmd.add ~dir:(fresh "named") "https://x.dev/me/json@1.5.0" ~name:None with
         | exception Package_cmd.Failed msg -> contains msg "needs wand"
         | () -> false);
      let tidied = fresh "tidied" in
      Package_cmd.tidy ~dir:tidied;
      Alcotest.(check bool) "tidy takes the newest this wand can use" true
        (contains (read_file (Filename.concat tidied "wand.pkg")) "version = 1.4.1"))

let test_schemeless_urls () =
  let (url, _, _, require) =
    parse "{ package = x.dev/me/app, wand = 0.85.0, require = [ { path = x.dev/me/json, version = 1.4.0 } ] }" in
  Alcotest.(check string) "the package gains https" "https://x.dev/me/app" url;
  (match require with
   | [r] -> Alcotest.(check string) "an entry gains https" "https://x.dev/me/json" r.Package.path
   | _ -> Alcotest.fail "expected one entry");
  with_two_packages (fun ~app ~json:_ ->
    Alcotest.(check (result string string)) "an import without its scheme"
      (Ok "parsed x")
      (run_in app "bare.wand" "import x.dev/me/json\njson.parse \"x\""));
  Alcotest.(check bool) "outside an import, a dotted word before / is no URL" true
    (match Lexer.tokenize_plain "r.a/b" with
     | Token.Ident "r" :: _ -> true
     | _ -> false);
  let dir = fresh_dir () in
  Package_cmd.init ~dir (Some "x.dev/me/tool");
  Alcotest.(check bool) "init writes the short form" true
    (contains (In_channel.with_open_text (Filename.concat dir "wand.pkg") In_channel.input_all)
       "{ package = x.dev/me/tool\n")

let test_bump_rules () =
  let before = ["type m.Algorithm = Sha256 | Sha512"; "m.of : String -> Int"] in
  let bump after = Package_cmd.needed (Package_cmd.changes ~before ~after) in
  let check label want got =
    Alcotest.(check string) label (Package_cmd.bump_name want) (Package_cmd.bump_name got) in
  check "nothing changed" Fix (bump before);
  check "padding is not a change" Fix
    (bump ["type m.Algorithm = Sha256 | Sha512"; "m.of   : String -> Int"]);
  check "an export added" Feature (bump (before @ ["m.to : Int -> String"]));
  check "an export removed" Breaking (bump ["type m.Algorithm = Sha256 | Sha512"]);
  check "a type made more general" Breaking
    (bump ["type m.Algorithm = Sha256 | Sha512"; "m.of : 'a -> Int"]);
  check "an effect added" Breaking
    (bump ["type m.Algorithm = Sha256 | Sha512"; "m.of : String -> Int ! {IO}"]);
  check "a variant added" Breaking
    (bump ["type m.Algorithm = Sha256 | Sha512 | Md5"; "m.of : String -> Int"]);
  (* A record whose fields do not fit on one line: the code gives it as one
     string, and wand.pkg read back as a line for each field. *)
  let long = "type m.Pod(\n  name: String,\n  image: String\n)" in
  let read_back = ["type m.Pod("; "  name: String,"; "  image: String"; ")"] in
  let bump_long before after = Package_cmd.needed (Package_cmd.changes ~before ~after) in
  check "a record on several lines, read back" Fix (bump_long read_back [long]);
  check "and a field of it changed" Breaking
    (bump_long read_back ["type m.Pod(\n  name: String,\n  image: Int\n)"]);
  let next last b = Package_cmd.next_version last b in
  Alcotest.(check (list string)) "versions"
    ["0.4.0"; "0.3.2"; "0.3.2"; "2.0.0"; "1.3.0"; "1.2.4"]
    [next "0.3.1" Breaking; next "0.3.1" Feature; next "0.3.1" Fix;
     next "1.2.3" Breaking; next "1.2.3" Feature; next "1.2.3" Fix];
  let move last v = Package_cmd.bump_name (Package_cmd.kind_of_move last v) in
  Alcotest.(check (list string)) "the kind of a move to a version"
    ["breaking"; "breaking"; "fix"; "breaking"; "feature"; "fix"]
    [move "0.4.0" "1.0.0"; move "0.3.1" "0.4.0"; move "0.3.1" "0.3.5";
     move "1.2.3" "2.0.0"; move "1.2.3" "1.4.0"; move "1.2.3" "1.2.9"]

(* A record whose fields go on several lines passes the check that the
   section written for it is held to. *)
let test_interface_with_a_long_record () =
  let root = fresh_dir () in
  Package_cmd.init ~dir:root (Some "x.dev/me/pods");
  write (Filename.concat root "pods.wand")
    "type Pod(name: String, image: String, replicas: Int = 1, namespace: String = \"default\", labels: Map String = {})\n";
  Package_cmd.interface ~dir:root ~check:false;
  Alcotest.(check bool) "the fields went on lines of their own" true
    (contains (interface_section root) "\n  image: String,\n");
  Package_cmd.interface ~dir:root ~check:true

let test_interface_and_release () =
  List.iter (fun (k, v) -> Unix.putenv k v)
    [("GIT_AUTHOR_NAME", "t"); ("GIT_AUTHOR_EMAIL", "t@t");
     ("GIT_COMMITTER_NAME", "t"); ("GIT_COMMITTER_EMAIL", "t@t")];
  let root = fresh_dir () in
  let git args = Package.run_git ("-C" :: root :: "-c" :: "user.email=t@t" :: "-c" :: "user.name=t" :: args) |> fst in
  ignore (git ["init"; "-q"]);
  Package_cmd.init ~dir:root (Some "x.dev/me/digest");
  write (Filename.concat root "digest.wand")
    "type Algorithm = Sha256 | Sha512\nlet name a = match a with\n  | Sha256 -> \"sha256\"\n  | Sha512 -> \"sha512\"\nlet _helper x = x\n";
  Unix.mkdir (Filename.concat root "_internal") 0o755;
  write (Filename.concat root "_internal/p.wand") "let hidden = 1";
  write (Filename.concat root "test_digest.wand") "let {test} = import Test\ntest \"a\" (fn t -> t.eq 1 1)";
  Package_cmd.interface ~dir:root ~check:false;
  Alcotest.(check string) "the whole file"
    "{ package = x.dev/me/digest\n, wand    = VERSION\n}\n\n\
     -- DO NOT EDIT: interface, written by `wand p`\n\
     type digest.Algorithm = Sha256 | Sha512\n\ndigest.name : Algorithm -> String\n"
    ((let text = read_file (Filename.concat root "wand.pkg") in
      let range = Package_cmd.running_range () in
      let i = Option.get (List.find_opt (fun i -> String.sub text i (String.length range) = range) (List.init (String.length text - String.length range) Fun.id)) in
      String.sub text 0 i ^ "VERSION" ^ String.sub text (i + String.length range) (String.length text - i - String.length range)));
  ignore (git ["add"; "."]);
  ignore (git ["commit"; "-qm"; "one"]);
  Package_cmd.release ~dir:root None;
  Alcotest.(check bool) "the first release" true
    (contains (interface_section root) "version 0.1.0");
  Package_cmd.interface ~dir:root ~check:true;
  write (Filename.concat root "digest.wand")
    "type Algorithm = Sha256 | Sha512\nlet name a = 1\n";
  ignore (git ["commit"; "-qam"; "two"]);
  Alcotest.(check bool) "check sees the change" true
    (match Package_cmd.interface ~dir:root ~check:true with
     | exception Package_cmd.Failed msg -> contains msg "+ digest.name : 'a -> Int"
     | () -> false);
  let refused label needle asked =
    Alcotest.(check bool) label true
      (match Package_cmd.release ~dir:root (Some asked) with
       | exception Package_cmd.Failed msg -> contains msg needle
       | () -> false)
  in
  refused "a fix is refused" "need a breaking release, not a fix release" "fix";
  refused "the old word still works, and is refused the same way"
    "need a breaking release, not a fix release" "patch";
  refused "a version too small for the change"
    "need a breaking release, and 0.1.1 after 0.1.0 is a fix release" "0.1.1";
  refused "a version that is not after the last" "is not after the latest release, 0.1.0" "0.1.0";
  refused "a word that is neither" "breaking, feature or fix, or a version" "big";
  Package_cmd.release ~dir:root None;
  Alcotest.(check bool) "before 1.0 a break moves the minor" true
    (contains (interface_section root) "version 0.2.0");
  write (Filename.concat root "digest.wand")
    "type Algorithm = Sha256 | Sha512\nlet name a = 1\nlet more = 2\n";
  ignore (git ["commit"; "-qam"; "three"]);
  Package_cmd.release ~dir:root (Some "1.0.0");
  Alcotest.(check bool) "a version asked for by name leaves 0.x" true
    (contains (interface_section root) "version 1.0.0");
  write (Filename.concat root "digest.wand")
    "type Algorithm = Sha256 | Sha512\nlet name a = 1\nlet more = 2\nlet most = 3\n";
  ignore (git ["commit"; "-qam"; "four"]);
  Package_cmd.release ~dir:root (Some "feature");
  Alcotest.(check bool) "from 1.0 a feature moves the minor" true
    (contains (interface_section root) "version 1.1.0");
  write (Filename.concat root "junk.txt") "x";
  Alcotest.(check bool) "a dirty tree is refused" true
    (match Package_cmd.release ~dir:root None with
     | exception Package_cmd.Failed msg -> contains msg "junk.txt"
     | () -> false)

(* A type from another module is written by that module's path, so a
   rename of an import changes nothing. A section an earlier wand wrote has
   the old local names in it, and is read with the imports of its tag. *)
let test_renamed_import () =
  List.iter (fun (k, v) -> Unix.putenv k v)
    [("GIT_AUTHOR_NAME", "t"); ("GIT_AUTHOR_EMAIL", "t@t");
     ("GIT_COMMITTER_NAME", "t"); ("GIT_COMMITTER_EMAIL", "t@t")];
  let root = fresh_dir () in
  let git args = Package.run_git ("-C" :: root :: "-c" :: "user.email=t@t" :: "-c" :: "user.name=t" :: args) |> fst in
  ignore (git ["init"; "-q"]);
  Package_cmd.init ~dir:root (Some "x.dev/me/k8s");
  Unix.mkdir (Filename.concat root "meta") 0o755;
  write (Filename.concat root "meta/v1.wand") "type Sel(k: String = \"a\")\n";
  write (Filename.concat root "app.wand")
    "let meta_v1 = import ./meta/v1\ntype Pod(sel: meta_v1.Sel = meta_v1.Sel(k = \"b\"))\n";
  let sections = Package.read_sections root in
  Package.write_sections root
    { sections with iface = Some
        [ "version 0.1.0"; "";
          "type app.Pod(sel: meta_v1.Sel = meta_v1.Sel(k = \"b\"))";
          "type meta/v1.Sel(k: String = \"a\")" ] };
  ignore (git ["add"; "."]);
  ignore (git ["commit"; "-qm"; "one"]);
  ignore (git ["tag"; "v0.1.0"]);
  write (Filename.concat root "app.wand")
    "let MetaV1 = import ./meta/v1\ntype Pod(sel: MetaV1.Sel = MetaV1.Sel(k = \"b\"))\n";
  ignore (git ["commit"; "-qam"; "two"]);
  Package_cmd.release ~dir:root (Some "fix");
  let section = interface_section root in
  Alcotest.(check bool) "a fix release" true (contains section "version 0.1.1");
  Alcotest.(check bool) "the type by its module's path" true
    (contains section "type app.Pod(sel: meta/v1.Sel = meta/v1.Sel(k = \"b\"))");
  write (Filename.concat root "app.wand")
    "let {Sel} = import ./meta/v1\ntype Pod(sel: Sel = Sel(k = \"b\"))\n";
  ignore (git ["commit"; "-qam"; "three"]);
  Package_cmd.interface ~dir:root ~check:true;
  write (Filename.concat root "app.wand")
    "let {Sel} = import ./meta/v1\ntype Pod(sel: Sel = Sel(k = \"c\"))\n";
  ignore (git ["commit"; "-qam"; "four"]);
  Alcotest.(check bool) "a changed default still needs a breaking release" true
    (match Package_cmd.release ~dir:root (Some "fix") with
     | exception Package_cmd.Failed msg -> contains msg "need a breaking release"
     | () -> false)

let test_sections () =
  let text =
    "{ package = x.dev/a\n, wand    = 0.85.0\n}\n\n\
     -- DO NOT EDIT: interface, written by `wand p`\nversion 0.1.0\n\na.f : Int\n\n\
     -- DO NOT EDIT: sum, written by `wand p`\nhttps://x.dev/b 1.0.0 sha256:00\n" in
  let sections = Package.split_sections ~file:"wand.pkg" text in
  Alcotest.(check (option (list string))) "interface" (Some ["version 0.1.0"; ""; "a.f : Int"]) sections.iface;
  Alcotest.(check (option (list string))) "sum" (Some ["https://x.dev/b 1.0.0 sha256:00"]) sections.sum;
  Alcotest.(check string) "a round trip" text (Package.join_sections sections);
  let refused label needle text =
    match Package.split_sections ~file:"wand.pkg" text with
    | exception Package.Error (_, msg) ->
      if not (contains msg needle) then Alcotest.failf "%s: expected %S in %s" label needle msg
    | _ -> Alcotest.failf "%s: expected an error" label
  in
  refused "a changed marker" "opens with"
    "{ package = x.dev/a, wand = 0.85.0 }\n-- DO NOT EDIT: api\n";
  refused "sum before interface" "then the interface section, then the sum section"
    "{ package = x.dev/a, wand = 0.85.0 }\n-- DO NOT EDIT: sum, written by `wand p`\n\
     -- DO NOT EDIT: interface, written by `wand p`\n";
  refused "a section twice" "each once"
    "{ package = x.dev/a, wand = 0.85.0 }\n-- DO NOT EDIT: interface, written by `wand p`\n\
     -- DO NOT EDIT: interface, written by `wand p`\n"

let test_tidy_keeps_the_interface_section () =
  let root = fresh_dir () in
  write (Filename.concat root "wand.pkg") (Printf.sprintf
    "{ package = x.dev/me/a, wand = %s, require = [ { path = x.dev/me/unused, version = 1.0.0, local = ../x } ] }\n\n\
     -- DO NOT EDIT: interface, written by `wand p`\na.f : Int\n" Version.value);
  write (Filename.concat root "a.wand") "let f = 1";
  Package_cmd.tidy ~dir:root;
  let sections = Package.read_sections root in
  Alcotest.(check bool) "the entry is gone" false (contains sections.record "unused");
  Alcotest.(check (option (list string))) "the interface section is kept" (Some ["a.f : Int"]) sections.iface

let test_add () =
  with_repos [
    ("json", [("1.4.0", json_at "1.4"); ("1.5.0", json_at "1.5"); ("2.0.0", json_at "2.0")]) ]
    (fun ~app ->
      Package_cmd.init ~dir:app (Some "x.dev/me/app");
      let refused label needle f =
        match f () with
        | exception Package_cmd.Failed msg ->
          if not (contains msg needle) then Alcotest.failf "%s: expected %S in %s" label needle msg
        | () -> Alcotest.failf "%s: expected a refusal" label
      in
      Package_cmd.add ~dir:app "x.dev/me/json@1.4.0" ~name:None;
      Alcotest.(check bool) "the entry" true
        (contains (Package.read_sections app).record "{ path = x.dev/me/json, version = 1.4.0 }");
      Alcotest.(check bool) "its sum line" true
        (contains (sum_section app) "https://x.dev/me/json 1.4.0 sha256:");
      refused "the same major again" "run `wand p upgrade x.dev/me/json@1.5.0`"
        (fun () -> Package_cmd.add ~dir:app "x.dev/me/json@1.5.0" ~name:None);
      refused "a required package, no version" "is already required at 1.4.0, and every file in it"
        (fun () -> Package_cmd.add ~dir:app "x.dev/me/json/decode" ~name:None);
      refused "a second major with no name" "`wand p add x.dev/me/json@2.0.0 --name json2`"
        (fun () -> Package_cmd.add ~dir:app "x.dev/me/json@2.0.0" ~name:None);
      refused "no such release" "has no release 9.9.9"
        (fun () -> Package_cmd.add ~dir:app "x.dev/me/json@9.9.9" ~name:(Some "json9"));
      Package_cmd.add ~dir:app "x.dev/me/json@2.0.0" ~name:(Some "json2");
      Alcotest.(check (result string string)) "both majors import" (Ok "1.4 2.0")
        (run_in app "main.wand"
           "import x.dev/me/json\nimport json2\n\"%{json.version} %{json2.version}\""))

(* `upgrade` takes the URL as wand.pkg writes it, and moves a 0.x entry to
   another minor only when it is asked for that version. *)
let test_upgrade_a_zero_minor () =
  with_repos [
    ("json", [("0.1.0", json_at "0.1.0"); ("0.1.1", json_at "0.1.1"); ("0.2.0", json_at "0.2.0")]) ]
    (fun ~app ->
      Package_cmd.init ~dir:app (Some "x.dev/me/app");
      write (Filename.concat app "main.wand") "import x.dev/me/json\njson.version";
      Package_cmd.add ~dir:app "x.dev/me/json@0.1.0" ~name:None;
      let refused label needle f =
        match f () with
        | exception Package_cmd.Failed msg ->
          if not (contains msg needle) then Alcotest.failf "%s: expected %S in %s" label needle msg
        | () -> Alcotest.failf "%s: expected a refusal" label
      in
      Package_cmd.upgrade ~dir:app (Some "x.dev/me/json");
      Alcotest.(check (result string string)) "a bare upgrade stays in the minor" (Ok "0.1.1")
        (Runner.run_file (Filename.concat app "main.wand"));
      refused "add names the upgrade too" "run `wand p upgrade x.dev/me/json@0.2.0`"
        (fun () -> Package_cmd.add ~dir:app "x.dev/me/json@0.2.0" ~name:None);
      Package_cmd.upgrade ~dir:app (Some "x.dev/me/json@0.2.0");
      Alcotest.(check (result string string)) "asked for, it moves in place" (Ok "0.2.0")
        (Runner.run_file (Filename.concat app "main.wand"));
      Alcotest.(check bool) "one entry" true
        (contains (Package.read_sections app).record
           "[ { path = x.dev/me/json, version = 0.2.0 }\n    ]");
      refused "a package that is not required" "x.dev/me/text is not in the `require` list"
        (fun () -> Package_cmd.upgrade ~dir:app (Some "x.dev/me/text")))

let test_init_from_origin () =
  List.iter (fun (remote, want) ->
    Alcotest.(check (option string)) remote want (Package_cmd.url_of_remote remote))
    [ ("git@github.com:wand-lang/pkg-fixture.git", Some "github.com/wand-lang/pkg-fixture");
      ("https://github.com/wand-lang/pkg-fixture.git", Some "github.com/wand-lang/pkg-fixture");
      ("ssh://git@github.com/wand-lang/pkg-fixture.git", Some "github.com/wand-lang/pkg-fixture");
      ("https://gitlab.com/a/b/c", Some "gitlab.com/a/b/c");
      ("/srv/git/tool.git", None) ];
  let dir = fresh_dir () in
  let git args = ignore (Package.run_git ("-C" :: dir :: args)) in
  let refused label needle =
    match Package_cmd.init ~dir None with
    | exception Package_cmd.Failed msg ->
      if not (contains msg needle) then Alcotest.failf "%s: expected %S in %s" label needle msg
    | () -> Alcotest.failf "%s: expected a refusal" label
  in
  refused "not a repository" "not a git repository";
  git ["init"; "-q"];
  refused "no origin" "no remote named origin";
  git ["remote"; "add"; "origin"; "git@github.com:wand-lang/pkg-fixture.git"];
  Package_cmd.init ~dir None;
  Alcotest.(check bool) "named by origin" true
    (contains (Package.read_sections dir).record "{ package = github.com/wand-lang/pkg-fixture\n")

let git_present = Sys.command "git --version >/dev/null 2>&1" = 0

let () =
  Random.self_init ();
  Alcotest.run "Package" [
    "wand.pkg", [
      Alcotest.test_case "reads the file"      `Quick test_reads_the_file;
      Alcotest.test_case "require is optional" `Quick test_require_is_optional;
      Alcotest.test_case "refuses what is not data" `Quick test_refuses_what_is_not_data;
      Alcotest.test_case "the wand range"      `Quick test_the_wand_range;
      Alcotest.test_case "found above the file" `Quick test_found_above_the_file;
      Alcotest.test_case "text after the record" `Quick test_text_after_the_record;
      Alcotest.test_case "a broken file is read to repair" `Quick
        test_a_broken_file_is_read_to_repair;
    ];
    "imports", [
      Alcotest.test_case "by URL"              `Quick test_url_imports;
      Alcotest.test_case "by URL, no package"  `Quick test_url_import_outside_a_package;
      Alcotest.test_case "by its own URL"      `Quick test_own_url_imports;
      Alcotest.test_case "tidy and its own URL" `Quick test_tidy_adds_no_entry_for_the_package;
      Alcotest.test_case "private by path"     `Quick test_private_by_path;
      Alcotest.test_case "without a scheme"    `Quick test_schemeless_urls;
    ];
    "wand <url>", [
      Alcotest.test_case "what names a URL"    `Quick test_names_a_url;
      Alcotest.test_case "resolving the file"  `Quick test_resolve_entry;
      Alcotest.test_case "the caller's build"  `Quick test_run_entry_keeps_the_caller_s_build;
    ];
    "fetching", [
      Alcotest.test_case "fetch and the sum section" `Quick
        (fun () -> if git_present then test_fetch_and_sum () else Alcotest.skip ());
      Alcotest.test_case "a link in a package is refused" `Quick
        (fun () -> if git_present then test_a_link_in_a_package_is_refused () else Alcotest.skip ());
      Alcotest.test_case "a dot segment is refused" `Quick test_a_dot_segment_is_refused;
      Alcotest.test_case "no fetch when it is off" `Quick
        (fun () -> if git_present then test_no_fetch_when_it_is_off () else Alcotest.skip ());
    ];
    "the build", [
      Alcotest.test_case "minimal version selection" `Quick
        (fun () -> if git_present then test_minimal_version_selection () else Alcotest.skip ());
      Alcotest.test_case "two majors need a name" `Quick test_two_majors_need_a_name;
      Alcotest.test_case "an unknown alias"   `Quick test_unknown_alias;
      Alcotest.test_case "two majors in a type error" `Quick test_two_majors_in_a_type_error;
      Alcotest.test_case "init from origin" `Quick
        (fun () -> if git_present then test_init_from_origin () else Alcotest.skip ());
      Alcotest.test_case "add" `Quick
        (fun () -> if git_present then test_add () else Alcotest.skip ());
      Alcotest.test_case "init, tidy, upgrade" `Quick
        (fun () -> if git_present then test_init_tidy_upgrade () else Alcotest.skip ());
      Alcotest.test_case "upgrade a 0.x minor" `Quick
        (fun () -> if git_present then test_upgrade_a_zero_minor () else Alcotest.skip ());
      Alcotest.test_case "add, tidy and upgrade pass over what this wand cannot use" `Quick
        (fun () -> if git_present then test_upgrade_skips_what_this_wand_cannot_use ()
          else Alcotest.skip ());
    ];
    "releasing", [
      Alcotest.test_case "the bump rules"      `Quick test_bump_rules;
      Alcotest.test_case "a long record passes the check" `Quick test_interface_with_a_long_record;
      Alcotest.test_case "sections"            `Quick test_sections;
      Alcotest.test_case "tidy keeps the interface section" `Quick test_tidy_keeps_the_interface_section;
      Alcotest.test_case "interface and release"     `Quick
        (fun () -> if git_present then test_interface_and_release () else Alcotest.skip ());
      Alcotest.test_case "a renamed import is not a change" `Quick
        (fun () -> if git_present then test_renamed_import () else Alcotest.skip ());
    ];
  ]
