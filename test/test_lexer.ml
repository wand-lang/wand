open Wand

let tokenize s = List.map fst (Lexer.tokenize s)

(* Strip EOF for cleaner test assertions *)
let tokens s =
  tokenize s |> List.filter (fun t -> t <> Token.EOF && t <> Token.Newline)

let check_tokens label input expected =
  let got = tokens input in
  Alcotest.(check (list (testable Token.pp Token.equal))) label expected got

(* ── Literals ──────────────────────────────────────────────────────────── *)

let test_integer () =
  check_tokens "single int"   "42"     [Int 42];
  check_tokens "zero"         "0"      [Int 0];
  check_tokens "negative int" "-3"     [Minus; Int 3]

let test_float () =
  check_tokens "simple float" "3.14"   [Float 3.14];
  check_tokens "leading zero" "0.5"    [Float 0.5]

let test_string () =
  check_tokens "simple string"   {|"hello"|}        [String "hello"];
  check_tokens "empty string"    {|""|}             [String ""];
  check_tokens "escape newline"  {|"a\nb"|}         [String "a\nb"];
  check_tokens "escape tab"      {|"a\tb"|}         [String "a\tb"];
  check_tokens "escape quote"    {|"say \"hi\""|} [String {|say "hi"|}]

(* A brace inside a string in the interpolated expression is text, not a
   bracket. Counted with the rest, `"%{f "{" }"` ran off the end of the file
   and `"%{f "}"}"` ended one character into the argument. Found by
   test/fuzz. *)
let test_a_brace_in_a_nested_string () =
  let bodies src =
    List.filter_map
      (function
        | Token.InterpStr (parts, _) | Token.RawInterpStr (parts, _) ->
          Some (List.map (fun (_, body, _) -> body) parts)
        | _ -> None)
      (tokens src)
    |> List.concat
  in
  Alcotest.(check (list string)) "an open brace"
    [{|f "{" |}] (bodies {|"%{f "{" }"|});
  Alcotest.(check (list string)) "a closing brace"
    [{|f "}"|}] (bodies {|"%{f "}"}"|});
  Alcotest.(check (list string)) "a string that interpolates again"
    [{|f "a%{g "{"}b"|}] (bodies {|"%{f "a%{g "{"}b"}"|});
  Alcotest.(check (list string)) "an escaped quote"
    [{|f "\"{"|}] (bodies {|"%{f "\"{"}"|});
  Alcotest.(check (list string)) "a raw string"
    [{|f `{`|}] (bodies {|"%{f `{`}"|})

let test_bool () =
  check_tokens "true"  "true"  [Bool true];
  check_tokens "false" "false" [Bool false]

(* ── Identifiers & keywords ─────────────────────────────────────────────── *)

let test_ident () =
  check_tokens "lowercase"       "foo"       [Ident "foo"];
  check_tokens "with underscore" "foo_bar"   [Ident "foo_bar"];
  check_tokens "with digits"     "x1"        [Ident "x1"];
  check_tokens "single char"     "x"         [Ident "x"]

let test_upper () =
  check_tokens "uppercase"  "Foo"     [Upper "Foo"];
  check_tokens "module path" "List"   [Upper "List"]

let test_keywords () =
  let cases = [
    "let",      Token.Let;
    "in",       In;
    "match",    Match;
    "with",     With;
    "if",       If;
    "then",     Then;
    "else",     Else;
    "type",     Type;
    "import",   Import;
    "requires", Requires;
    "ensures",  Ensures;
    "result",   Result;
    "fn",       Fn;
    "for",      For;
    "do",       Do;
    "end",      End;
    "when",     When;
    "and",      And;
    "or",       Or;
  ] in
  List.iter (fun (src, tok) -> check_tokens src src [tok]) cases

(* `let*` is OCaml's binding operator and not wand's. `let` is a keyword and
   `*` is the operator, which is what these two tokens say. *)
let test_let_star () =
  check_tokens "let*" "let*" [Let; Star]

let test_underscore () =
  check_tokens "wildcard _" "_" [Underscore]

let test_hole () =
  check_tokens "hole ?" "?" [Hole]

(* ── Operators ──────────────────────────────────────────────────────────── *)

let test_operators () =
  let cases = [
    "=",  Token.Eq;
    "->", Arrow;
    "|>", PipeArrow;
    "|",  Pipe;
    "+",  Plus;
    "-",  Minus;
    "*",  Star;
    "/",  Slash;
    "..", DotDot;
    ".",  Dot;
    ":",  Colon;
    ",",  Comma;
    "==", EqEq;
    "!=", BangEq;
    "<=", LtEq;
    ">=", GtEq;
    "<",  Lt;
    ">",  Gt;
    "&&", AmpAmp;
    "||", PipePipe;
    "!",  Bang;
  ] in
  List.iter (fun (src, tok) -> check_tokens src src [tok]) cases

(* ── Delimiters ─────────────────────────────────────────────────────────── *)

let test_delimiters () =
  check_tokens "parens"    "()"    [LParen; RParen];
  check_tokens "brackets"  "[]"    [LBracket; RBracket];
  check_tokens "braces"    "{}"    [LBrace; RBrace];
  check_tokens "semicolon" ";"     [Semicolon]

(* ── Comments ───────────────────────────────────────────────────────────── *)

(* A comment is `--` to the end of the line, and nothing else is one. The
   block form counted its closers, so pasted shell ended it early at the
   default arm of a `case`. Each refusal names the form to write. *)
let refuses label input needle =
  match Lexer.tokenize input with
  | exception Lexer.LexError (_, msg) ->
    Alcotest.(check bool) (label ^ ": the message names the fix") true
      (let n = String.length needle and m = String.length msg in
       let rec at i = i + n <= m && (String.sub msg i n = needle || at (i + 1)) in
       at 0)
  | _ -> Alcotest.failf "%s: lexed without an error" label

let test_comments () =
  refuses "block comment" "(* ignored *) 1" "'-- ...'";
  refuses "doc comment" "(** actual doc *) true" "'-- ...'";
  refuses "slash comment" "// nope\n1" "'-- ...'";
  refuses "hash comment" "# nope\n1" "'-- ...'"

(* A character wand has no use for is reported as the character, and the
   message is text a caller can decode.

   Naming the byte it starts with gave `unexpected character '\342'`: not
   valid UTF-8, so a harness reading diagnostics as text died rather than
   reporting, and a byte is nothing a reader can act on. The three that
   matter are what a pasted document carries. *)
let test_a_character_outside_the_language () =
  let message src =
    match Lexer.tokenize src with
    | _ -> Alcotest.failf "%S lexed without an error" src
    | exception Lexer.LexError (_, msg) -> msg
  in
  let holds msg needle =
    let n = String.length needle and m = String.length msg in
    let rec at i = i + n <= m && (String.sub msg i n = needle || at (i + 1)) in
    at 0
  in
  let names label src character fix =
    let msg = message src in
    Alcotest.(check bool) (label ^ ": names the character") true
      (holds msg character);
    Alcotest.(check bool) (label ^ ": names the fix") true (holds msg fix)
  in
  names "em dash"      "1 \xe2\x80\x94 2" "\xe2\x80\x94" "a hyphen";
  names "en dash"      "1 \xe2\x80\x93 2" "\xe2\x80\x93" "a hyphen";
  names "curly quote"  "\xe2\x80\x98hi\xe2\x80\x99" "\xe2\x80\x98" "an apostrophe";
  names "curly double" "\xe2\x80\x9chi\xe2\x80\x9d" "\xe2\x80\x9c" "a double quote";
  names "no-break space" "let x =\xc2\xa01" "\xc2\xa0" "an ordinary space";
  names "ellipsis"     "x \xe2\x80\xa6" "\xe2\x80\xa6" "three dots";
  (* One with no ASCII to suggest still arrives whole. *)
  Alcotest.(check bool) "a wide character is printed whole" true
    (holds (message "let x = \xe4\xb8\xad") "\xe4\xb8\xad");
  (* A byte that starts no character has nothing to print, so it is named. *)
  Alcotest.(check bool) "a lone byte is named, not printed" true
    (holds (message "let x = \xff") "0xFF")

(* The hint above is a courtesy for OCaml drift, and a manifest pattern
   needs the same two characters: a narrowed label puts its paren directly
   before the star. What follows the star is what tells them apart, so these
   have to lex rather than read as an unclosed comment.

   `wand f` sorts a manifest's words and a star sorts first, so a pattern
   anywhere in the list ends up first in the formatted output. A rule that
   only worked away from the paren would have the formatter writing a file
   it could not read back. *)
let test_a_pattern_after_a_paren_lexes () =
  List.iter (fun src ->
    match Lexer.tokenize src with
    | _ -> ()
    | exception Lexer.LexError (_, msg) ->
      Alcotest.failf "%s did not lex: %s" src msg)
    ["uses {Shell(*.sh)}"; "uses {Shell(*)}"; "uses {Shell(*-compose)}";
     "uses {Net(*.example.com)}"; "(*.sh"]

let test_line_comments () =
  check_tokens "line comment to end of line" "-- ignored\n1"
    [LineComment " ignored"; Int 1];
  check_tokens "trailing line comment" "1 -- note\n2"
    [Int 1; LineComment " note"; Int 2];
  check_tokens "empty line comment" "--\n1" [LineComment ""; Int 1];
  (* `--` must not swallow the constructs it sits next to. *)
  check_tokens "subtraction unaffected"  "5 - 3"   [Int 5; Minus; Int 3];
  check_tokens "spaced double minus"     "5 - -3"  [Int 5; Minus; Minus; Int 3];
  check_tokens "arrow unaffected"        "x -> y"  [Ident "x"; Arrow; Ident "y"];
  check_tokens "dashed path unaffected"  "./my-file"  [Path "./my-file"];
  check_tokens "date literal unaffected" "2024-01-15" [DateTime "2024-01-15"];
  (* The instant scanner builds the date into one buffer and tests the
     characters after it with a predicate. It used to use `Printf.sprintf`
     and rebuild a five-element list for every character, which cost more
     than the rest of reading the literal. Every spelling it accepts is
     pinned here, since the two are easy to narrow by accident -- the `+`
     and `-` of an offset especially. *)
  check_tokens "bare day"     "2026-08-22" [DateTime "2026-08-22"];
  check_tokens "instant, Z"   "2026-01-15T03:22:11Z" [DateTime "2026-01-15T03:22:11Z"];
  check_tokens "plus offset"  "2026-01-15T03:22:11+05:00" [DateTime "2026-01-15T03:22:11+05:00"];
  check_tokens "minus offset" "2026-01-15T03:22:11-05:00" [DateTime "2026-01-15T03:22:11-05:00"];
  (* And the date still ends where it ends: what follows is its own token. *)
  check_tokens "a day then a minus" "2026-08-22 - 5h"
    [DateTime "2026-08-22"; Minus; Duration "5h"]

(* ── Whitespace ─────────────────────────────────────────────────────────── *)

let test_whitespace () =
  check_tokens "spaces ignored"  "1 + 2"      [Int 1; Plus; Int 2];
  check_tokens "tabs ignored"    "1\t+\t2"    [Int 1; Plus; Int 2];
  check_tokens "multi space"     "  foo  "    [Ident "foo"]

(* ── Sequences ──────────────────────────────────────────────────────────── *)

let test_let_binding () =
  check_tokens "let binding" "let x = 42"
    [Token.Let; Ident "x"; Eq; Int 42]

let test_function_arrow () =
  check_tokens "match case" "| 0 -> true"
    [Pipe; Int 0; Arrow; Bool true]

let test_pipeline () =
  check_tokens "pipeline" "xs |> map f"
    [Ident "xs"; PipeArrow; Ident "map"; Ident "f"]

let test_suffix_idents () =
  check_tokens "? suffix"       "empty?"        [Ident "empty?"];
  check_tokens "! suffix"       "get!"              [Ident "get!"];
  check_tokens "standalone ?"   "?"                 [Hole];
  check_tokens "standalone !"   "!true"             [Bang; Bool true];
  check_tokens "!= unaffected"  "foo != bar"        [Ident "foo"; BangEq; Ident "bar"];
  check_tokens "!= no space"    "foo!=bar"          [Ident "foo"; BangEq; Ident "bar"];
  check_tokens "? in expr"      "empty? xs"      [Ident "empty?"; Ident "xs"]

(* ── Suite ──────────────────────────────────────────────────────────────── *)

let () =
  Alcotest.run "Lexer" [
    "literals", [
      Alcotest.test_case "integers"  `Quick test_integer;
      Alcotest.test_case "floats"    `Quick test_float;
      Alcotest.test_case "strings"   `Quick test_string;
      Alcotest.test_case "a brace in a nested string" `Quick
        test_a_brace_in_a_nested_string;
      Alcotest.test_case "booleans"  `Quick test_bool;
    ];
    "identifiers", [
      Alcotest.test_case "lowercase"    `Quick test_ident;
      Alcotest.test_case "uppercase"    `Quick test_upper;
      Alcotest.test_case "keywords"     `Quick test_keywords;
      Alcotest.test_case "let*"         `Quick test_let_star;
      Alcotest.test_case "a character outside the language" `Quick
        test_a_character_outside_the_language;
      Alcotest.test_case "underscore"   `Quick test_underscore;
      Alcotest.test_case "hole"         `Quick test_hole;
      Alcotest.test_case "? and ! suffix" `Quick test_suffix_idents;
    ];
    "operators", [
      Alcotest.test_case "operators"  `Quick test_operators;
      Alcotest.test_case "delimiters" `Quick test_delimiters;
    ];
    "comments", [
      Alcotest.test_case "comments"      `Quick test_comments;
      Alcotest.test_case "a pattern after a paren lexes" `Quick
        test_a_pattern_after_a_paren_lexes;
      Alcotest.test_case "line comments" `Quick test_line_comments;
      Alcotest.test_case "whitespace" `Quick test_whitespace;
    ];
    "sequences", [
      Alcotest.test_case "let binding"    `Quick test_let_binding;
      Alcotest.test_case "function arrow" `Quick test_function_arrow;
      Alcotest.test_case "pipeline"       `Quick test_pipeline;
    ];
  ]
