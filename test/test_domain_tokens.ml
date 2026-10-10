open Wand

let tokens s =
  Lexer.tokenize s
  |> List.map fst
  |> List.filter (fun t -> t <> Token.EOF && t <> Token.Newline)

(* An interpolation carries where its body sits in the file, which these
   tests do not fix and do not care about: they are about what was lexed,
   not where. Flattened to one value on both sides. *)
let z = { Token.p_line = 0; p_col = 0; p_offset = 0 }

let nopos (t : Token.t) : Token.t =
  let two ps = List.map (fun (a, b, _) -> (a, b, z)) ps in
  let three ps = List.map (fun (a, b, h, _) -> (a, b, h, z)) ps in
  match t with
  | Token.InterpStr (ps, tail)    -> Token.InterpStr (two ps, tail)
  | Token.RawInterpStr (ps, tail) -> Token.RawInterpStr (two ps, tail)
  | Token.RunCmdRaw (ps, tail)    -> Token.RunCmdRaw (three ps, tail)
  | Token.RunQueryRaw (ps, tail)  -> Token.RunQueryRaw (three ps, tail)
  | Token.CommandRaw (ps, tail)   -> Token.CommandRaw (three ps, tail)
  | other -> other

let check label input expected =
  let got = List.map nopos (tokens input) in
  Alcotest.(check (list (testable Token.pp Token.equal))) label expected got

(* ── Paths ──────────────────────────────────────────────────────────────── *)

let test_paths () =
  check "absolute"        "/etc/passwd"          [Path "/etc/passwd"];
  check "absolute nested" "/var/log/app.log"     [Path "/var/log/app.log"];
  check "relative"        "./script.wand"        [Path "./script.wand"];
  check "parent"          "../sibling"           [Path "../sibling"];
  check "home"            "~/projects"           [Path "~/projects"];
  (* slash in expression is division — space separates *)
  check "slash operator"  "n / 2"                [Ident "n"; Slash; Int 2];
  (* paths in expression position *)
  check "two paths"       "copy /src /dst"
    [Ident "copy"; Path "/src"; Path "/dst"]

(* ── Dates ──────────────────────────────────────────────────────────────── *)

(* A bare date is a spelling of midnight UTC: one instant type, and the
   text kept as it was written, the way an offset form is. It becomes the
   instant it names wherever the meaning is read. *)
let test_dates () =
  check "basic date"   "2024-01-15"  [DateTime "2024-01-15"];
  check "end of year"  "2024-12-31"  [DateTime "2024-12-31"];
  check "epoch"        "1970-01-01"  [DateTime "1970-01-01"]

let test_date_disambiguation () =
  (* space before minus → int + minus, not date *)
  check "int minus int" "2024 - 1"   [Int 2024; Minus; Int 1];
  check "plain int"     "2024"       [Int 2024]

(* ── DateTime ───────────────────────────────────────────────────────────── *)

let test_datetimes () =
  check "utc"           "2024-01-15T14:32:01Z"         [DateTime "2024-01-15T14:32:01Z"];
  check "no tz"         "2024-01-15T00:00:00"          [DateTime "2024-01-15T00:00:00"];
  check "positive offset" "2024-01-15T09:00:00+05:30"  [DateTime "2024-01-15T09:00:00+05:30"];
  check "negative offset" "2024-01-15T14:30:00-05:00"  [DateTime "2024-01-15T14:30:00-05:00"];
  check "zero offset"   "2024-01-15T14:30:00+00:00"    [DateTime "2024-01-15T14:30:00+00:00"]

(* ── Times ──────────────────────────────────────────────────────────────── *)

(* A time of day is not a value. The shape is still read, so the refusal
   can name the instant form rather than failing on the `:` further on. *)
let refuses label input needle =
  match (try Ok (tokens input) with Lexer.LexError (_, m) -> Error m) with
  | Ok _ -> Alcotest.failf "%s: expected a lex error" label
  | Error m ->
    if not (Lint.contains m needle) then
      Alcotest.failf "%s: expected %S in: %s" label needle m

let test_times () =
  refuses "afternoon" "14:32:01" "a time of day is not a value";
  refuses "midnight"  "00:00:00" "2026-08-22T";
  refuses "noon"      "12:00:00" "DateTime.on!"

let test_time_disambiguation () =
  (* colon with space → colon + int, not time *)
  check "colon space int" ": 80"    [Colon; Int 80]

(* ── Durations ──────────────────────────────────────────────────────────── *)

let test_durations () =
  check "minutes"      "5min"    [Duration "5min"];
  check "hours"        "1h"      [Duration "1h"];
  check "compound"     "1h30m"   [Duration "1h30m"];
  check "days"         "2d"      [Duration "2d"];
  check "milliseconds" "500ms"   [Duration "500ms"];
  check "seconds"      "30s"     [Duration "30s"];
  check "weeks"        "1w"      [Duration "1w"];
  check "complex"      "2d12h30m" [Duration "2d12h30m"]

(* ── URLs ───────────────────────────────────────────────────────────────── *)

let test_urls () =
  check "https bare"   "https://example.com"           [URL "https://example.com"];
  check "http"         "http://localhost:8080"          [URL "http://localhost:8080"];
  check "with path"    "https://example.com/api/v1"    [URL "https://example.com/api/v1"];
  check "with query"   "https://example.com/s?q=foo"   [URL "https://example.com/s?q=foo"];
  (* A URL ends where the punctuation around it begins. `;` was missing from
     that set, so a URL as any but the last statement of a `( a; b )` block
     ate the separator and the statement after it arrived with nothing in
     front of it: `(http://x; let y = 1 in ())` came back "expected ), got
     let". A newline had always ended one, so the shape only appeared once
     `wand f` wrote the block on a single line. Found by test/fuzz. *)
  check "a semicolon ends it" "http://x; y"
    [URL "http://x"; Semicolon; Ident "y"];
  check "as a bracket does"   "(http://x)"
    [LParen; URL "http://x"; RParen];
  (* The literal is checked against the same grammar `URL.of_string` uses, so
     it cannot be the more permissive of the two. `|` is not a URL character
     and has to be percent-encoded; it used to lex straight through, which
     left a value the checked constructor would have refused. *)
  refuses "an illegal character" "http://a|b" "cannot appear in a URL"

(* ── IPv4 ───────────────────────────────────────────────────────────────── *)

(* Semantic Versioning 2.0.0's grammar, which the literal is checked against
   as well as `Version.of_string`, so the literal cannot write a value the
   checked constructor would refuse. It used to admit `_` in a prerelease and
   a leading zero in a number, and the spec admits neither. *)
let test_versions () =
  check "three numbers"  "1.2.3"          [Version "1.2.3"];
  check "a prerelease"   "1.2.3-alpha.1"  [Version "1.2.3-alpha.1"];
  (* Only a *numeric* identifier is barred from a leading zero; one that is
     not all digits is compared as text and may start with one. *)
  check "a digit-led identifier" "1.2.3-0a" [Version "1.2.3-0a"];
  refuses "a leading zero"       "01.2.3"        "leading zero";
  refuses "an underscore"        "1.2.3-alpha_1" "letters, digits and hyphens";
  refuses "an empty identifier"  "1.2.3-"        "none empty";
  refuses "a numeric identifier with a leading zero" "1.2.3-01" "leading zero";
  (* `+` is the addition operator, so build metadata is not written as a
     literal. `Version.of_string` is where a version carrying one comes
     from. *)
  check "a + is not part of one" "1.2.3+1"
    [Version "1.2.3"; Plus; Int 1]

let test_ipv4 () =
  check "loopback"   "127.0.0.1"       [IPv4 "127.0.0.1"];
  check "private"    "192.168.1.1"     [IPv4 "192.168.1.1"];
  check "zeros"      "0.0.0.0"         [IPv4 "0.0.0.0"];
  check "broadcast"  "255.255.255.255" [IPv4 "255.255.255.255"]

let test_ipv4_vs_float () =
  (* two segments → float, not IPv4 *)
  check "float not ipv4" "192.168"  [Float 192.168]

let test_ipv4_invalid () =
  let lex_err label src =
    match Lexer.tokenize_plain src with
    | _ -> Alcotest.failf "%s: expected LexError but got tokens" label
    | exception Lexer.LexError _ -> ()
  in
  lex_err "octet 999"    "999.0.0.1";
  lex_err "octet 256"    "256.0.0.1";
  lex_err "octet neg"    "192.168.1.-1"

(* ── CIDR ───────────────────────────────────────────────────────────────── *)

let test_cidr () =
  check "class c"    "192.168.0.0/24"  [CIDR "192.168.0.0/24"];
  check "class a"    "10.0.0.0/8"      [CIDR "10.0.0.0/8"];
  check "default"    "0.0.0.0/0"       [CIDR "0.0.0.0/0"]

let test_cidr_invalid () =
  let lex_err label src =
    match Lexer.tokenize_plain src with
    | _ -> Alcotest.failf "%s: expected LexError but got tokens" label
    | exception Lexer.LexError _ -> ()
  in
  lex_err "prefix 33"    "10.0.0.0/33";
  lex_err "prefix 128"   "10.0.0.0/128"

(* A number too large to be an `Int` is a lex error naming the limit. It
   used to reach `int_of_string`, whose failure escaped as OCaml's own, so
   the reader got "Error: int_of_string" and nothing about where. *)
let test_int_too_large () =
  check "the largest there is" "4611686018427387903" [Int 4611686018427387903];
  (match Lexer.tokenize_plain "30000000000000000000" with
   | _ -> Alcotest.fail "expected a LexError for a number past max_int"
   | exception Lexer.LexError (_, msg) ->
     if not (String.length msg > 0 && String.length msg > 20) then
       Alcotest.failf "expected a message naming the limit, got: %s" msg)

(* ── Ports ──────────────────────────────────────────────────────────────── *)

let test_ports () =
  check "http"   ":80"    [Port 80];
  check "https"  ":443"   [Port 443];
  check "dev"    ":8080"  [Port 8080];
  check "zero"   ":0"     [Port 0];
  check "last"   ":65535" [Port 65535]

(* A port is 0 to 65535, checked where CIDR's prefix is checked: in the
   lexer, so a literal, `Port.of_string` and `Decode.port` cannot disagree
   about the same number. The overflow case used to escape as an OCaml
   `int_of_string` failure rather than a lex error. *)
let test_port_out_of_range () =
  let lex_err label src =
    match Lexer.tokenize_plain src with
    | _ -> Alcotest.failf "%s: expected LexError but got tokens" label
    | exception Lexer.LexError _ -> ()
  in
  lex_err "one past the end" ":65536";
  lex_err "five digits"      ":99999";
  lex_err "wider than an Int" ":99999999999999999999"

let test_port_disambiguation () =
  (* colon with space before digits → colon token, not port *)
  check "type annotation" ": Int"  [Colon; Upper "Int"]

(* ── Versions ───────────────────────────────────────────────────────────── *)

let test_versions () =
  check "semver"       "1.2.3"          [Version "1.2.3"];
  check "zeros"        "0.1.0"          [Version "0.1.0"];
  check "pre-release"  "1.2.3-alpha.1"  [Version "1.2.3-alpha.1"];
  check "rc"           "2.0.0-rc.1"     [Version "2.0.0-rc.1"]

let test_version_vs_float () =
  (* two segments → float *)
  check "float not version" "1.2"  [Float 1.2]

(* ── Sizes ──────────────────────────────────────────────────────────────── *)

let test_sizes () =
  check "bytes"      "100B"     [Size "100B"];
  check "kilobytes"  "512KB"    [Size "512KB"];
  check "megabytes"  "10MB"     [Size "10MB"];
  check "gigabytes"  "1GB"      [Size "1GB"];
  check "terabytes"  "2TB"      [Size "2TB"];
  check "petabytes"  "1PB"      [Size "1PB"];
  check "decimal"    "1.5GB"    [Size "1.5GB"];
  check "decimal kb" "2.5KB"    [Size "2.5KB"]

(* ── Suite ──────────────────────────────────────────────────────────────── *)

(* ── Environment variables ───────────────────────────────────────────────── *)

let test_envvars () =
  check "simple"           "$HOME"      [EnvVar "HOME"];
  check "with underscore"  "$MY_VAR"    [EnvVar "MY_VAR"];
  check "with digits"      "$VAR2"      [EnvVar "VAR2"];
  (* $( lexes as raw command literal *)
  check "cmd sub raw" "$(x)"     [RunCmdRaw ([], "x")];
  (* The command ends at its closing paren, and a paren the author quoted is
     not it: counting them blind ended the command early and read the rest
     of the line as wand source. *)
  check "a quoted paren is text" {|$(echo "a)b")|}
    [RunCmdRaw ([], {|echo "a)b"|})];
  check "a paren in single quotes is text" "$(echo 'a)b')"
    [RunCmdRaw ([], "echo 'a)b'")];
  check "an escaped paren is text" {|$(echo \)x)|}
    [RunCmdRaw ([], {|echo \)x|})];
  check "a real subshell still nests" "$((cd /tmp) && ls)"
    [RunCmdRaw ([], "(cd /tmp) && ls")];
  (* Where a hole sits decides what its value is quoted for. *)
  check "a bare hole is an argument" "$(echo %{x})"
    [RunCmdRaw ([("echo ", "x", Token.Arg, z)], "")];
  check "a hole in double quotes is escaped for them" {|$(echo "hi %{x}")|}
    [RunCmdRaw ([({|echo "hi |}, "x", Token.Inside '"', z)], "\"")];
  check "a hole in single quotes is escaped for them" "$(echo 'hi %{x}')"
    [RunCmdRaw ([("echo 'hi ", "x", Token.Inside '\'', z)], "'")];
  check "a raw hole is shell source" "$(echo %!{x})"
    [RunCmdRaw ([("echo ", "x", Token.Source, z)], "")];
  (* `$*(` is the command itself. It reads exactly as `$(` does -- same
     quoting, same holes -- and differs only in the token it comes back as. *)
  check "cmd value raw" "$*(x)" [CommandRaw ([], "x")];
  check "a quoted paren is text in one too" {|$*(echo "a)b")|}
    [CommandRaw ([], {|echo "a)b"|})];
  check "and a hole is quoted the same way" "$*(echo %{x})"
    [CommandRaw ([("echo ", "x", Token.Arg, z)], "")];
  (* $ followed by lowercase is not an env var *)
  check "lowercase not env" "$home"     [Dollar; Ident "home"]

let () =
  Alcotest.run "Domain tokens" [
    "paths", [
      Alcotest.test_case "paths"              `Quick test_paths;
    ];
    "dates", [
      Alcotest.test_case "dates"              `Quick test_dates;
      Alcotest.test_case "date disambiguation" `Quick test_date_disambiguation;
      Alcotest.test_case "datetimes"          `Quick test_datetimes;
      Alcotest.test_case "times"              `Quick test_times;
      Alcotest.test_case "time disambiguation" `Quick test_time_disambiguation;
    ];
    "durations", [
      Alcotest.test_case "durations"          `Quick test_durations;
    ];
    "network", [
      Alcotest.test_case "urls"               `Quick test_urls;
      Alcotest.test_case "versions"           `Quick test_versions;
      Alcotest.test_case "ipv4"               `Quick test_ipv4;
      Alcotest.test_case "ipv4 vs float"      `Quick test_ipv4_vs_float;
      Alcotest.test_case "ipv4 invalid"       `Quick test_ipv4_invalid;
      Alcotest.test_case "cidr"               `Quick test_cidr;
      Alcotest.test_case "cidr invalid"       `Quick test_cidr_invalid;
      Alcotest.test_case "ports"              `Quick test_ports;
      Alcotest.test_case "port out of range"  `Quick test_port_out_of_range;
      Alcotest.test_case "int too large"      `Quick test_int_too_large;
      Alcotest.test_case "port disambiguation" `Quick test_port_disambiguation;
    ];
    "versions", [
      Alcotest.test_case "versions"           `Quick test_versions;
      Alcotest.test_case "version vs float"   `Quick test_version_vs_float;
    ];
    "sizes", [
      Alcotest.test_case "sizes"              `Quick test_sizes;
    ];
    "env vars", [
      Alcotest.test_case "env vars"           `Quick test_envvars;
    ];
  ]
