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
  ]
