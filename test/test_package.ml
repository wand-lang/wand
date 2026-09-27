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

let parse src = Package.parse ~file:"wand.mod" src

let parse_error label needle src =
  match parse src with
  | exception Package.Error (_, msg) ->
    if not (contains msg needle) then
      Alcotest.failf "%s: expected %S in: %s" label needle msg
  | _ -> Alcotest.failf "%s: expected an error" label

let test_reads_the_file () =
  let (modul, wand, _, require) = parse {|{ module  = https://github.com/mjstahl/json
, wand    = 0.4.0
, require =
    [ { path = https://github.com/mjstahl/text, version = 1.2.0 }
    , { name = json2, path = https://github.com/mjstahl/json, version = 2.1.0, local = ../json }
    ]
}|} in
  Alcotest.(check string) "module" "https://github.com/mjstahl/json" modul;
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
  let (_, _, _, require) = parse "{ module = https://x.dev/a, wand = 0.85.0 }" in
  Alcotest.(check int) "no entries" 0 (List.length require)

let test_refuses_what_is_not_data () =
  parse_error "a list" "wand.mod is a record" "[1]";
  parse_error "two items" "holds one record" "let x = 1\n{ module = https://x.dev/a }";
  parse_error "unknown field" "has no field `extra`"
    "{ module = https://x.dev/a, wand = 0.85.0, extra = 1 }";
  parse_error "missing wand" "needs a `wand` field" "{ module = https://x.dev/a }";
  parse_error "a string version" "is a version, such as 1.2.0, not a string"
    {|{ module = https://x.dev/a, wand = "0.85.0" }|};
  parse_error "a module that is not a URL" "is a URL"
    "{ module = ./a, wand = 0.85.0 }";
  parse_error "an entry field" "has no field `tag`"
    "{ module = https://x.dev/a, wand = 0.85.0, require = [ { path = https://x.dev/b, version = 1.0.0, tag = 1 } ] }";
  parse_error "an uppercase alias" "lowercase name"
    "{ module = https://x.dev/a, wand = 0.85.0, require = [ { path = https://x.dev/b, version = 1.0.0, name = Json } ] }";
  parse_error "a local that is not a path" "is a path"
    {|{ module = https://x.dev/a, wand = 0.85.0, require = [ { path = https://x.dev/b, version = 1.0.0, local = "x" } ] }|}

let test_the_wand_range () =
  let yes range running =
    Alcotest.(check bool) (range ^ " accepts " ^ running) true (Package.accepts range running) in
  let no range running =
    Alcotest.(check bool) (range ^ " refuses " ^ running) false (Package.accepts range running) in
  yes "0.4.0" "0.4.0"; yes "0.4.0" "0.4.9"; no "0.4.0" "0.5.0"; no "0.4.2" "0.4.1";
  yes "1.2.0" "1.9.3"; no "1.2.0" "2.0.0"; no "1.2.0" "1.1.0"

let test_found_above_the_file () =
  let root = fresh_dir () in
  let sub = Filename.concat root "lib" in
  Unix.mkdir sub 0o755;
  write (Filename.concat root "wand.mod") "{ module = https://x.dev/a, wand = 0.4.0 }";
  let file = Filename.concat sub "main.wand" in
  write file "1 + 1";
  (match Runner.run_file file with
   | Error e ->
     Alcotest.(check bool) "names the range" true
       (contains e "needs wand 0.4.0 or later, before 0.5.0");
     Alcotest.(check bool) "names the file" true (contains e "wand.mod:1:")
   | Ok _ -> Alcotest.fail "expected the range to refuse this wand");
  write (Filename.concat root "wand.mod")
    (Printf.sprintf "{ module = https://x.dev/a, wand = %s }" Version.value);
  Alcotest.(check (result string string)) "runs in range" (Ok "2") (Runner.run_file file)

(* An app requiring json through a local copy, as a directory tree. *)
let with_two_packages f =
  let root = fresh_dir () in
  let app = Filename.concat root "app" and json = Filename.concat root "json" in
  List.iter (fun d -> Unix.mkdir d 0o755)
    [app; json; Filename.concat json "_internal"];
  write (Filename.concat json "wand.mod")
    (Printf.sprintf "{ module = https://x.dev/me/json, wand = %s }" Version.value);
  write (Filename.concat json "json.wand") {|let parse s = "parsed %{s}"|};
  write (Filename.concat json "decode.wand") "let decode s = s";
  write (Filename.concat (Filename.concat json "_internal") "p.wand") "let x = 1";
  write (Filename.concat json "inside.wand") "import ./_internal/p\np.x";
  write (Filename.concat app "wand.mod")
    (Printf.sprintf
       "{ module = https://x.dev/me/app, wand = %s, require = [ { path = https://x.dev/me/json, version = 1.4.0, local = ../json } ] }"
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

let test_url_import_outside_a_package () =
  let dir = fresh_dir () in
  error_says "no wand.mod" "this file is in no package"
    (run_in dir "loose.wand" "import https://x.dev/me/json\n1")

let test_private_by_path () =
  with_two_packages (fun ~app ~json ->
    error_says "from another package" "`_internal` is private to the package at"
      (run_in app "bypath.wand" "let p = import ../json/_internal/p\np.x");
    Alcotest.(check (result string string)) "from its own package" (Ok "1")
      (Runner.run_file (Filename.concat json "inside.wand")))

(* A git repository standing in for https://x.dev/me/json, which git is told
   to read from disk, and a cache of this test's own. *)
let with_remote f =
  let root = fresh_dir () in
  let repo = Filename.concat root "repos/me/json" in
  ignore (Sys.command (Filename.quote_command "mkdir" ["-p"; repo]));
  write (Filename.concat repo "json.wand") {|let parse s = "parsed %{s}"|};
  write (Filename.concat repo "wand.mod")
    (Printf.sprintf "{ module = https://x.dev/me/json, wand = %s }" Version.value);
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
    "{ module = https://x.dev/me/app, wand = %s, require = [ { path = https://x.dev/me/json, version = %s } ] }"
    Version.value version

let test_fetch_and_sum () =
  with_remote (fun ~app ~repo:_ ->
    write (Filename.concat app "wand.mod") (app_mod "1.4.0");
    let main = "import https://x.dev/me/json\njson.parse \"x\"" in
    error_says "fetched, and no line in wand.sum" "wand.sum has no line for https://x.dev/me/json 1.4.0"
      (run_in app "main.wand" main);
    let cached = Package.cache_dir
        { Package.path = "https://x.dev/me/json"; version = "1.4.0"; name = None; local = None } in
    Alcotest.(check bool) "in the cache" true
      (Sys.file_exists (Filename.concat cached "json.wand"));
    Alcotest.(check bool) "without .git" false
      (Sys.file_exists (Filename.concat cached ".git"));
    let h = Package.tree_hash cached in
    write (Filename.concat app "wand.sum") (Printf.sprintf "https://x.dev/me/json 1.4.0 %s\n" h);
    Alcotest.(check (result string string)) "runs once recorded" (Ok "parsed x")
      (run_in app "main.wand" main);
    Hashtbl.reset Package.hashes;
    write (Filename.concat app "wand.sum") "https://x.dev/me/json 1.4.0 sha256:00\n";
    error_says "a mismatch" "does not match wand.sum" (run_in app "main.wand" main);
    write (Filename.concat app "wand.mod") (app_mod "1.5.0");
    error_says "no such tag" "git clone of the tag v1.5.0 failed" (run_in app "main.wand" main))

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
  write (Filename.concat app "wand.sum")
    (String.concat "" (List.map (fun (path, version) ->
       let r = entry path version in
       let h = if Sys.file_exists (Package.cache_dir r) then Package.tree_hash (Package.cache_dir r)
         else Package.fetch r in
       Printf.sprintf "%s %s %s\n" path version h) entries))

let json_at v = [
  ("wand.mod", Printf.sprintf "{ module = https://x.dev/me/json, wand = %s }" Version.value);
  ("json.wand", Printf.sprintf "let version = \"%s\"" v) ]

let test_minimal_version_selection () =
  with_repos [
    ("json", [("1.4.0", json_at "1.4"); ("1.5.0", json_at "1.5"); ("2.0.0", json_at "2.0")]);
    ("text", [("1.0.0", [
       ("wand.mod", Printf.sprintf
          "{ module = https://x.dev/me/text, wand = %s, require = [ { path = https://x.dev/me/json, version = 1.5.0 } ] }"
          Version.value);
       ("text.wand", "import https://x.dev/me/json\nlet v = json.version") ])]) ]
    (fun ~app ->
      write (Filename.concat app "wand.mod") (Printf.sprintf
        "{ module = https://x.dev/me/app, wand = %s, require =\n\
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
  let src = "{ module = https://x.dev/a, wand = 0.85.0, require = [ { path = https://x.dev/b, version = 1.0.0 }, { path = https://x.dev/b, version = 2.0.0 } ] }" in
  parse_error "two majors" "give one of them a name, such as `name = b2`" src;
  parse_error "one major twice" "is required twice at major 1"
    "{ module = https://x.dev/a, wand = 0.85.0, require = [ { path = https://x.dev/b, version = 1.0.0 }, { path = https://x.dev/b, version = 1.2.0 } ] }";
  parse_error "before 1.0 a minor is a major" "such as `name = b0_1`"
    "{ module = https://x.dev/a, wand = 0.85.0, require = [ { path = https://x.dev/b, version = 0.2.0 }, { path = https://x.dev/b, version = 0.1.0 } ] }"

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
    write (Filename.concat d "wand.mod")
      (Printf.sprintf "{ module = https://x.dev/me/json, wand = %s }" Version.value)) [j1; j2];
  write (Filename.concat app "wand.mod") (Printf.sprintf
    "{ module = https://x.dev/me/app, wand = %s, require = [ { path = https://x.dev/me/json, version = 1.4.0, local = ../j1 }, { name = json2, path = https://x.dev/me/json, version = 2.1.0, local = ../j2 } ] }"
    Version.value);
  error_says "both named with their versions"
    "expected a `Value` from https://x.dev/me/json 2.1.0, got a `Value` from https://x.dev/me/json 1.4.0"
    (run_in app "main.wand"
       "import https://x.dev/me/json\nimport json2\nlet v = json.make 1\njson2.get v")

let git_present = Sys.command "git --version >/dev/null 2>&1" = 0

let () =
  Random.self_init ();
  Alcotest.run "Package" [
    "wand.mod", [
      Alcotest.test_case "reads the file"      `Quick test_reads_the_file;
      Alcotest.test_case "require is optional" `Quick test_require_is_optional;
      Alcotest.test_case "refuses what is not data" `Quick test_refuses_what_is_not_data;
      Alcotest.test_case "the wand range"      `Quick test_the_wand_range;
      Alcotest.test_case "found above the file" `Quick test_found_above_the_file;
    ];
    "imports", [
      Alcotest.test_case "by URL"              `Quick test_url_imports;
      Alcotest.test_case "by URL, no package"  `Quick test_url_import_outside_a_package;
      Alcotest.test_case "private by path"     `Quick test_private_by_path;
    ];
    "fetching", [
      Alcotest.test_case "fetch and wand.sum"  `Quick
        (fun () -> if git_present then test_fetch_and_sum () else Alcotest.skip ());
    ];
    "the build", [
      Alcotest.test_case "minimal version selection" `Quick
        (fun () -> if git_present then test_minimal_version_selection () else Alcotest.skip ());
      Alcotest.test_case "two majors need a name" `Quick test_two_majors_need_a_name;
      Alcotest.test_case "an unknown alias"   `Quick test_unknown_alias;
      Alcotest.test_case "two majors in a type error" `Quick test_two_majors_in_a_type_error;
    ];
  ]
