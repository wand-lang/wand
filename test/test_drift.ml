open Wand

(* wand has no training-data presence, so a model writing it drifts toward
   the languages it knows: OCaml, Haskell, Python, Ruby, bash. Each
   predictable intrusion must produce an error that names the wand
   correction -- a generic "syntax error at line N" leaves the
   edit-typecheck loop circling. These tests lock in the message text. *)

let contains msg needle =
  let n = String.length needle and m = String.length msg in
  let rec go i = i + n <= m && (String.sub msg i n = needle || go (i + 1)) in
  go 0

let expect_error label src needle =
  match Runner.run_string src with
  | Ok v ->
    Alcotest.failf "%s: expected an error mentioning %S, got value %s"
      label needle v
  | Error msg ->
    if not (contains msg needle) then
      Alcotest.failf "%s: error does not name the correction %S:\n%s"
        label needle msg

let ok label src =
  match Runner.run_string src with
  | Ok _ -> ()
  | Error msg -> Alcotest.failf "%s: expected success, got: %s" label msg

(* ── OCaml ────────────────────────────────────────────────────────────────── *)

(* `::` is cons. It used to be corrected to `:`, which is the drift rule
   that argued for this spelling in the first place. *)
let test_ocaml_cons () =
  ok "cons in an expression" "1 :: [2, 3]";
  ok "cons in a pattern" "let f l = match l with | [h :: t] -> h | [] -> 0\nf [1]"

let test_ocaml_assignment () =
  expect_error "x := 3" "x := 3"
    "there is no mutation, so there is no ':='"

let test_ocaml_let_rec () =
  expect_error "top-level let rec"
    "let rec f n = if n == 0 then 1 else n * f (n - 1)\nf 3"
    "a let is already recursive -- drop the 'rec'";
  expect_error "let rec inside an expression"
    "let g () = let rec f n = n in f 1\ng ()"
    "a let is already recursive -- drop the 'rec'"

let test_ocaml_fun_is_accepted () =
  (* `fun` is close enough to be read as `fn` rather than refused. *)
  ok "fun aliases fn" "let f = fun x -> x + 1\nf 1"

let test_ocaml_variant_of () =
  expect_error "Circle of Int"
    "type Shape = Circle of Int"
    "a constructor takes its payload directly: 'Circle Int'"

(* The strongest prior of them all: `{r with f = v}` is how OCaml, Elm, F#
   and Rust-adjacent code all update a record. Braces are a map in wand and
   the update names its type, so the error has to carry the whole form. *)
let test_ocaml_record_update () =
  (* The message carries the reader's own names: their record, and every
     field they named. The type is the one thing the braces do not hold. *)
  expect_error "braces and with"
    "type T (a : Int, b : Int)\nlet r = T(a = 1, b = 2)\nlet u = {r with b = 3}\nu"
    "a record update names its type: `T(r, b = ...)`";
  expect_error "and every field they named"
    "type T (a : Int, b : Int)\nlet r = T(a = 1, b = 2)\nlet u = {r with a = 8, b = 3}\nu"
    "`T(r, a = ..., b = ...)`";
  ok "and the wand form works"
    "type T (a : Int, b : Int)\nlet r = T(a = 1, b = 2)\nlet u = T(r, b = 3)\nu.b"

(* `{groups, skipped}` puns in a literal as it does in a pattern. A writer
   who reaches for names in braces often wants a record with fields of
   different types, so when the values do not fit one type the error says
   what braces build and writes the type out with the literal's own names:
   a pun stays bare, as it would in the construction (#146). *)
let test_map_literal_pun () =
  ok "a pun reads the name" "import Map\nlet a = 1\nlet b = 2\n\
                             let m = {a, b}\nMap.get! \"b\" m";
  ok "beside a key with a value"
    "import Map\nlet skipped = 3\nlet m = {groups = 4, skipped}\n\
     Map.get! \"skipped\" m";
  ok "one name alone" "import Map\nlet a = 1\nMap.size {a}";
  ok "a pattern puns" "let {a, b} = {a = 1, b = 2}\na + b";
  ok "a quoted key" "import Map\nMap.size {a = 1, \"b c\" = 2}";
  let says = "`{...}` builds a Map, and all its values have one type. \
              Named fields of different types need a type" in
  expect_error "values of two types, with a pun"
    "import Map\nlet skipped = 0\nlet acc = {groups = Map.empty, skipped}\nacc"
    ("expected Map 'a, got Int. " ^ says);
  expect_error "with the literal's names"
    "import Map\nlet skipped = 0\nlet acc = {groups = Map.empty, skipped}\nacc"
    "`type T(groups: ..., skipped: ...)`, then `T(groups = ..., skipped)`";
  expect_error "at the pun's position"
    "import Map\nlet skipped = 0\nlet acc = {groups = Map.empty, skipped}\nacc"
    "3:32:";
  expect_error "and with no pun"
    "import Map\nlet acc = {groups = Map.empty, skipped = 1}\nacc"
    "expected Map 'a, got Int. `{...}` builds a Map";
  expect_error "at the value's position"
    "import Map\nlet acc = {groups = Map.empty, skipped = 1}\nacc"
    "2:42:";
  expect_error "every name, in order"
    "let a = 1\nlet m = {a,\n  b = \"x\", d = 2}\nm"
    "`type T(a: ..., b: ..., d: ...)`, then `T(a, b = ..., d = ...)`";
  (* An error inside one value is about that value, not about the map. *)
  match Runner.run_string "{a = 1, b = [1, \"x\"]}" with
  | Ok _ -> Alcotest.fail "a list of two types typechecked"
  | Error msg ->
    if contains msg "builds a Map" then
      Alcotest.failf "an error inside a value explains the map: %s" msg

let test_ocaml_try_with () =
  expect_error "try ... with cases"
    "try 1 / 0 with _ -> 0"
    "try takes no cases, so there is no 'try ... with'";
  (* The idiom the check must not catch: an unparenthesized try as a
     match scrutinee, where the `with` belongs to the match. *)
  ok "match try ... with"
    "let safe thunk = match try thunk () with | Ok v -> v | Error _ -> 0\n\
     safe (fn () -> 7)"

let test_ocaml_unbound_names () =
  expect_error "ref" "let r = ref 3"
    "there is no mutation; let binds a new name instead";
  expect_error "raise" "raise \"boom\""
    "errors are values: return an Error";
  expect_error "print_endline" "print_endline \"hi\""
    "printing is IO.println (import IO)"

(* ── Haskell ──────────────────────────────────────────────────────────────── *)

let test_haskell_lambda () =
  expect_error "backslash lambda" "let f = \\x -> x + 1"
    "a lambda is 'fn x -> ...'"

let test_haskell_cons_pattern () =
  (* The old spelling, which is now a type annotation everywhere a pattern
     is written -- so the `:` is refused where no type follows it. *)
  expect_error "(x : xs) in a match"
    "let f l = match l with | (x : xs) -> x"
    "a cons pattern is written in square brackets: [x :: xs]";
  expect_error "(x : xs) as a parameter"
    "let head (x : xs) = x"
    "a cons pattern is written in square brackets: [x :: xs]";
  (* Bare `h :: t` is read and `wand f` writes the brackets back, the way
     `fun` is read and written back as `fn`. *)
  ok "the bare cons pattern is read"
    "let f l = match l with | h :: t -> h | [] -> 0\nf [1, 2]";
  ok "and a constructor keeps its payload"
    "import Option\nlet f l = match l with | Some x :: t -> x | _ -> 0\nf [Some 5]"

(* ── Python ───────────────────────────────────────────────────────────────── *)

let test_python_keywords () =
  expect_error "and" "true and false" "the boolean operator is '&&'";
  expect_error "or" "true or false" "the boolean operator is '||'";
  expect_error "not" "not true" "boolean not is '!'"

let test_python_unbound_names () =
  expect_error "lambda" "let f = lambda\nf" "a lambda is 'fn x -> ...'";
  expect_error "len" "len [1, 2]" "List.length and String.length";
  expect_error "null" "let x = null\nx" "absence is None"

(* ── Comments ─────────────────────────────────────────────────────────────── *)

let test_c_comment () =
  expect_error "// comment" "1 // one"
    "a comment is '-- ...' to the end of the line, not '//'"

let test_hash_comment () =
  expect_error "# comment" "1 # one"
    "a comment is '-- ...' to the end of the line, not '# ...'"

(* ── String interpolation ─────────────────────────────────────────────────── *)

let test_bash_interpolation () =
  expect_error "${x}" "let x = 1\n\"v: ${x}\""
    "interpolation is %{...}, not ${...}"

let test_ruby_interpolation () =
  expect_error "#{x}" "let x = 1\n\"v: #{x}\""
    "interpolation is %{...}, not #{...}";
  ok "escaped literal #{" "\"v: \\#{x}\""

let test_backtick_literal_percent_brace () =
  (* A `...` string cannot hold a literal %{ -- the error must say where
     to go instead, because generated shell/template text will hit it. *)
  expect_error "literal %{ in backticks" "`literal %{ text`"
    "use an ordinary \"...\" string and write \\%{"

(* ── Round two: what the cold-model run surfaced ──────────────────────────── *)

let test_string_concat () =
  expect_error "^ concatenation" "\"a\" ^ \"b\""
    "string concatenation is '++', not '^'"

let test_float_operators () =
  expect_error "*." "let a = 1.5\na *. a"
    "operators are not spelled differently for Float -- there is no '*.'";
  expect_error "+." "let a = 1.5\na +. 2.0" "there is no '+.'";
  expect_error "-." "let a = 1.5\na -. 2.0" "there is no '-.'";
  expect_error "/." "let a = 1.5\na /. 2.0" "there is no '/.'"

let test_char_literal () =
  expect_error "char literal" "let c = 'x'"
    "there are no character literals: a one-character string is \"x\""

let test_begin_end () =
  expect_error "begin/end block" "let x = begin 1 end\nx"
    "expressions group with parentheses, not 'begin ... end'"

let test_foreign_members () =
  expect_error "List.iter" "import List\nList.iter (fn x -> x) [1]"
    "use List.each";
  expect_error "String.sub" "import String\nString.sub \"abc\" 0 1"
    "use String.slice";
  List.iter (fun name ->
      expect_error ("List." ^ name)
        ("import List\nList." ^ name ^ " (fn x -> [x]) [1]")
        "use List.flat_map")
    ["concat_map"; "bind"; "collect"];
  List.iter (fun (ns, names, hint) ->
      List.iter (fun name ->
          expect_error (ns ^ "." ^ name)
            (Printf.sprintf "import %s\n%s.%s 1" ns ns name) hint)
        names)
    [ "List", ["enumerate"; "mapi"; "with_index"], "use List.indexed";
      "List", ["mem"; "elem"; "includes?"; "includes"], "use List.contains?";
      "Option", ["unwrap_or"; "get_or"; "with_default"], "use Option.default";
      "String", ["drop"; "substring"; "sub"], "use String.slice";
      "IO", ["eprintln"; "eprint"; "prerr_endline"; "prerr_string"; "eprintf"],
      "use IO.println_err (or IO.print_err)";
      "List", ["nth"; "nth!"; "at"; "get_nth"],
      "use List.get (an Option) or List.get!" ];
  expect_error "FS.read_lines" "import FS\nFS.read_lines /tmp/x"
    "FS.read_file! reads the whole file";
  expect_error "int_of_string" "int_of_string \"4\""
    "Int.of_string reads an Int out of a String"

(* A constructor spelled as another language spells it. `Err` was the error
   met most often in the authoring benchmark (#129). The arm form is checked
   for where it points as well: it used to name 1:1, the start of the
   `match`, and not the line the constructor is on. *)
let test_foreign_constructors () =
  let result = "the constructors of Result are Ok and Error" in
  let option = "the constructors of Option are Some and None" in
  expect_error "Err in an expression" "Err \"x\""
    ("unknown constructor 'Err' -- " ^ result);
  expect_error "Err in a pattern"
    "match Ok 1 with\n| Ok n -> n\n| Err _ -> 0"
    ("3:3: unknown constructor 'Err' -- " ^ result);
  expect_error "Err in a nested pattern"
    "match Some (Ok 1) with\n| Some (Ok n) -> n\n| Some (Err _) -> 0\n| None -> 0"
    "3:3: unknown constructor 'Err'";
  expect_error "Just" "let x = 1\nJust x"
    ("2:1: unknown constructor 'Just' -- " ^ option);
  expect_error "Nothing in a pattern"
    "match Some 1 with\n| Some n -> n\n| Nothing -> 0"
    ("3:3: unknown constructor 'Nothing' -- " ^ option);
  expect_error "Left" "Left 1" "there is no Either";
  expect_error "Right in a pattern"
    "match Ok 1 with\n| Right n -> n\n| Error _ -> 0" "there is no Either";
  expect_error "Nil" "Nil" "the empty list is []";
  (* A file's own constructor of one of these names is no mistake. *)
  ok "a declared Err" "type E = Err String | Fine\nmatch Err \"x\" with\n| Err s -> s\n| Fine -> \"\"";
  (* A name not in the table keeps the spelling guess. *)
  expect_error "a misspelling" "type S = Ready | Done\nRaedy" "did you mean 'Ready'"

let test_discovery_pointers () =
  (* When no correction is known, the error hands over the enumerator --
     the binary is the only documentation a cold reader has. *)
  expect_error "unknown name" "frobnicate 3"
    "'wand d' lists the modules, 'wand d List' one module's members";
  expect_error "unknown member" "import String\nString.frobnicate \"x\""
    "'wand d String' lists its members"

(* ── wand's own past ──────────────────────────────────────────────────────── *)

(* The 0.17 bracket maps. Code that learned wand before the braces landed --
   or a model trained on it -- still writes `[x = 1]`, and the correction has
   to be named the same way any other dialect's is. *)
let test_bracket_maps () =
  expect_error "bracket map literal" "let m = [x = 1]\nm"
    "a map is written in braces -- {k = v}, not [k = v]";
  expect_error "bracket map pattern"
    "let f x = match x with | [k = v] -> v\nf {k = 1}"
    "a map pattern is written in braces -- {k = v}, not [k = v]";
  expect_error "bracket import destructure"
    "let [parse] = import JSON\nparse \"1\""
    "an import is destructured with braces"

(* ── Still-legal neighbours ───────────────────────────────────────────────── *)

(* Every drift check sits next to syntax that must keep working. *)
let test_neighbours_still_parse () =
  ok "cons expression" "1 :: [2, 3]";
  ok "port literal" "let p = :8080\np";
  ok "division" "10 / 2";
  ok "annotated binding" "let f x : Int = x + 1\nf 1";
  ok "literal ${ escaped" "\"cost: \\${x}\"";
  ok "shell text keeps $" "\"$HOME and $(date) survive as written\"";
  (* The float-operator and concat checks sit beside real tokens. *)
  ok "globs still lex" "let g = *.wand\ng";
  ok "type variables still lex" "let id x : 'a = x\nid 1";
  ok "subtraction still works" "5 - 2";
  ok "addition still works" "5 + 2";
  (* The bracket-map refusal keys on `ident =` after `[`; a name or an
     equality inside a list is not a map and must stay one. *)
  ok "a list of names" "let x = 1\n[x, 2]";
  ok "equality inside a list" "let x = 1\n[x == 1]"

let () =
  Alcotest.run "drift" [
    "ocaml", [
      Alcotest.test_case "cons"        `Quick test_ocaml_cons;
      Alcotest.test_case "assignment"  `Quick test_ocaml_assignment;
      Alcotest.test_case "let rec"     `Quick test_ocaml_let_rec;
      Alcotest.test_case "fun"         `Quick test_ocaml_fun_is_accepted;
      Alcotest.test_case "variant of"  `Quick test_ocaml_variant_of;
      Alcotest.test_case "record update" `Quick test_ocaml_record_update;
      Alcotest.test_case "map literal pun" `Quick test_map_literal_pun;
      Alcotest.test_case "try with"    `Quick test_ocaml_try_with;
      Alcotest.test_case "names"       `Quick test_ocaml_unbound_names;
    ];
    "haskell", [
      Alcotest.test_case "lambda"       `Quick test_haskell_lambda;
      Alcotest.test_case "cons pattern" `Quick test_haskell_cons_pattern;
    ];
    "python", [
      Alcotest.test_case "keywords" `Quick test_python_keywords;
      Alcotest.test_case "names"    `Quick test_python_unbound_names;
    ];
    "comments", [
      Alcotest.test_case "c family" `Quick test_c_comment;
      Alcotest.test_case "hash"     `Quick test_hash_comment;
    ];
    "round two", [
      Alcotest.test_case "string concat"   `Quick test_string_concat;
      Alcotest.test_case "float operators" `Quick test_float_operators;
      Alcotest.test_case "char literal"    `Quick test_char_literal;
      Alcotest.test_case "begin/end"       `Quick test_begin_end;
      Alcotest.test_case "foreign members" `Quick test_foreign_members;
      Alcotest.test_case "foreign constructors" `Quick test_foreign_constructors;
      Alcotest.test_case "discovery"       `Quick test_discovery_pointers;
    ];
    "interpolation", [
      Alcotest.test_case "bash"          `Quick test_bash_interpolation;
      Alcotest.test_case "ruby"          `Quick test_ruby_interpolation;
      Alcotest.test_case "backtick %{"   `Quick test_backtick_literal_percent_brace;
    ];
    "wand's own past", [
      Alcotest.test_case "bracket maps" `Quick test_bracket_maps;
    ];
    "neighbours", [
      Alcotest.test_case "still parse" `Quick test_neighbours_still_parse;
    ];
  ]
