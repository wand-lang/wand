open Wand

let fmt s = Formatter.format_source s

(* ── Idempotency ──────────────────────────────────────────────────────────── *)

let assert_idempotent label src =
  let once = fmt src in
  let twice = fmt once in
  Alcotest.(check string) label once twice

let test_idempotent_stdlib () =
  let dir = "../stdlib" in
  if not (Sys.file_exists dir) then
    Alcotest.failf "stdlib not found at %s (relative to test sandbox)" dir
  else
    Array.iter (fun name ->
      if Filename.check_suffix name ".wand" then
        let path = Filename.concat dir name in
        let src = In_channel.with_open_text path In_channel.input_all in
        assert_idempotent name src
    ) (Sys.readdir dir)

(* ── The output parses, at any margin ─────────────────────────────────────── *)

(* Five separate bugs shipped where `wand f` emitted source that would not
   parse, and each waited for someone to write a line long enough to wrap
   before it showed. The last two waited for test/fuzz instead, which is the
   difference: nobody has to write the line. Narrowing the margin makes every line long enough, so
   the whole corpus exercises the wrapping paths at once rather than the
   handful of places that happen to be wide today.

   Parsing is the property, not layout: what the formatter chooses at a
   20-column margin is nobody's idea of readable, but it has to be a program.
   Idempotency is checked alongside, since a second pass over source the
   first pass mangled is how the damage usually shows. *)
let corpus_dirs = ["../stdlib"; "../test/wand"; "../examples"]

(* Every margin, not a handful of them. A wrapping bug is a bug about what
   fits, so it is reachable at the widths where one particular line crosses
   the edge and nowhere else -- five sampled margins miss most of those. The
   widths this has actually caught things at are 20, 30, 40, 91, 104 and
   107, which is both ends of the range and no pattern in between.

   It costs a second or so over the whole corpus, which is the reason to
   sweep rather than sample. Above the default margin matters as much as
   below: two of those six are wider than 92. *)
let margins = List.init 99 (fun i -> i + 12)

let corpus_files () =
  List.concat_map (fun dir ->
    if not (Sys.file_exists dir) then []
    else
      Sys.readdir dir
      |> Array.to_list
      |> List.filter (fun n -> Filename.check_suffix n ".wand")
      |> List.map (fun n -> Filename.concat dir n))
    corpus_dirs

let parses out =
  match Parser.parse_program (Lexer.tokenize out) with
  | _ -> Ok ()
  | exception e -> Error (Printexc.to_string e)

(* The two the fuzzer found. Both are one shape: a keyword that has to follow
   an expression, and an expression that wrapped. The expression is over by
   the time the next line starts, so the keyword arrives with nothing to
   attach to and the parser stops at the argument below.

   Kept at the margins they were found at. A wrapping bug is a bug about what
   fits, and neither of these is reachable at 92 columns. *)
let formats_and_parses label width src =
  let fmt s = Formatter.with_width width (fun () -> Formatter.format_source s) in
  let out = fmt src in
  (match parses out with
   | Ok () -> ()
   | Error e ->
     Alcotest.failf "%s: wand f wrote source that does not parse (%s):\n%s" label e out);
  Alcotest.(check string) (label ^ ": and settles") out (fmt out)

(* `if <application that wrapped> then` -- the condition was the one part of
   an `if` that was emitted with no guard on it. *)
let test_a_wrapped_if_condition_keeps_its_then () =
  formats_and_parses "if condition" 25
    "let () = if Args.help? (Proc.args ()) then match () with | r -> \"\""

(* `match try <application that wrapped> with` -- `try` is transparent to
   whether the tail is still owed something, and was not treated as such. *)
let test_a_wrapped_try_scrutinee_keeps_its_with () =
  let ks = String.concat " " (List.init 28 (fun _ -> "k")) in
  formats_and_parses "match try" 65
    ("let k = match try thunk " ^ ks ^ " () with | O -> \"\"")

(* And the shape that must not gain parentheses for either fix: a `try` whose
   body is a `with ... ->`, which is still owed its body when its first line
   ends. This is in the corpus, so a fix that over-reaches breaks the fixed
   point rather than any test -- which is a worse way to find out. *)
let test_a_wrapped_try_with_is_left_alone () =
  let src =
    "let r =\n  try with FS.temp_dir \"d_\" as d ->\n  let x = Path.to_string d in x\n"
  in
  let out = Formatter.with_width 92 (fun () -> Formatter.format_source src) in
  if Lint.contains out "(try" then
    Alcotest.failf "a try whose body is a `with` does not need parentheses:\n%s" out

(* A `handle` with no arms. The arms carry the line break, so
   with none there is no break to carry -- but the break was written anyway,
   and the item joiner then separated that trailing newline from the next
   item. `wand f` run twice was not `wand f` run once, and the file grew a
   blank line every time. A `match` with no cases cannot reach this: the
   parser refuses it outright. Found by test/fuzz. *)
let test_an_armless_handle_settles () =
  assert_idempotent "armless handle" "handle () with\nlet x = 1\n"

(* A local multi-clause function inside a call. The continuation clause used
   to be written as the bare name aligned under the first, which only parses
   where a newline ends an expression -- so inside `( ... )`, where a newline
   continues one, it was read as more of the previous clause's body. Every
   clause repeats `let` now, which parses in all three places and is what the
   top-level emitter already wrote. Found by test/fuzz. *)
let test_local_clauses_inside_a_call_parse () =
  formats_and_parses "local clauses in a call" 92
    (String.concat "\n"
       [ "let go = fn () -> 0";
         "test \"x\" (fn t ->";
         "  let count log = 1";
         "  let count log = 2";
         "  in t.eq 0 count)"; "" ])

(* `in let f ... and g ... in e`. The `in ` opens the group three columns
   right of the keyword above it, but the group laid its own continuation
   out at that keyword's indent -- so a value that wrapped, and the `and`
   line under it, both landed left of the `let` they belong to, where the
   parser reads them as something new. Found by test/fuzz. *)
let test_an_and_group_after_in_stays_under_its_let () =
  formats_and_parses "and group after in" 40
    (String.concat "\n"
       [ "let g = h (fn ->";
         "  let d t = t";
         "  in";
         "  let e () = println \"hello there this is a fairly long one\"";
         "  and s n = n";
         "  in ())"; "" ])

(* A constructor facing a bracket that holds the whole of its argument needs
   no bracket of its own, and `bracket_holds_all` answered that by scanning
   characters -- knowing `"` and nothing else. A command literal holds both
   quotes and brackets that mean neither, so `(e `")` read as an unclosed
   string and `(e `)`)` as a bracket that closed early. Either way the
   constructor took a bracket it did not need, which a qualified name spells
   `t.(O)` -- and that does not parse at all. Found by test/fuzz. *)
let test_a_command_literal_is_not_a_bracket () =
  List.iter (fun src -> formats_and_parses "command in an argument" 92 src)
    [ "let x = t.O (e `\"`)\n";
      "let x = t.O (e `)`)\n";
      "let x = O (e `\"`)\n" ];
  (* And the brackets a constructor does need are still written: `O ()` is
     the empty field list, not unit in brackets, and `O (d).n` is not
     `(O d).n`. *)
  let out = fmt "let x = O (d).n\n" in
  if not (Lint.contains out "(O d).n") then
    Alcotest.failf "a bracket that closes early still needs its own:\n%s" out

(* A `|>` chain laid out a stage per line is read back as one
   left-associative chain, so a stage that is itself an operator needs the
   brackets `emit_binop` would have given it. `5 |> (f |> g)` came back as
   `(5 |> f) |> g` -- a different program, and one whose reprint differed
   again, which is how the fuzzer saw it. Found by test/fuzz. *)
let test_a_nested_pipeline_keeps_its_brackets () =
  let src = "let y = 5 |> (fxxxxxxxxxxxxxx |> gggggggggggggg)\n" in
  formats_and_parses "nested pipeline" 27 src;
  let out = Formatter.with_width 27 (fun () -> fmt src) in
  if not (Lint.contains out "|> (fxxxxxxxxxxxxxx") then
    Alcotest.failf "the nested pipeline lost its brackets:\n%s" out;
  (* A stage whose operator binds tighter than `|>` needs none, and must not
     gain any. *)
  let plain = "let y = 5 |> fxxxxxxxxxxxxxx |> gggggggggggggg\n" in
  let out = Formatter.with_width 27 (fun () -> fmt plain) in
  if Lint.contains out "|> (" then
    Alcotest.failf "an ordinary pipeline gained brackets:\n%s" out

(* A construction whose fields all pun is written as a list of bare names,
   and that list reads back as the payload form `T(a, b)` -- which had no
   wrapped shape, so it stayed on one line however narrow the margin. The
   named source wrapped and the bare source did not, so the two passes
   disagreed. Found by test/fuzz. *)
let test_a_bare_field_list_wraps () =
  let src = "let h = quest! Request(url = url, metho)\n" in
  formats_and_parses "bare field list" 27 src;
  let out = Formatter.with_width 27 (fun () -> fmt src) in
  if not (Lint.contains out "Request(\n") then
    Alcotest.failf "the field list should have wrapped:\n%s" out

(* `fn` binding nothing wrote two spaces before the arrow. *)
let test_a_parameterless_fn_has_one_space () =
  let out = fmt "let f = fn -> ()\n" in
  if Lint.contains out "fn  ->" then
    Alcotest.failf "a parameterless fn should not double its space:\n%s" out

(* An interior comment survives even when the rendering happens to contain
   its characters somewhere else.

   Whether a comment came through was decided by counting occurrences of its
   text in the rendered item. Here the comment is a bare `--` and the body
   holds the string `"--"`, so a rendering that had dropped the comment
   still contained its two characters, the count came out right, and the
   item was accepted. Counting is done by lexing the rendering now. Found by
   test/fuzz. *)
let test_a_string_does_not_stand_in_for_a_comment () =
  let src = "\"\"(fn->--\n(()[\"--\"]))\n" in
  let out = Formatter.with_width 43 (fun () -> fmt src) in
  let comments t =
    List.map (fun (c : Formatter.comment_tok) -> c.Formatter.c_text)
      (Formatter.all_comments t (Lexer.tokenize t))
  in
  Alcotest.(check (list string)) "the comment survives" (comments src) (comments out)

(* A field access on a numeric literal keeps the literal's brackets. `(6).o`
   written back as `6.o` is not a field access -- the lexer reads it as a
   float with nothing after the point and says so. A name that ends in a
   digit is left alone, because its token never started as a number. Found
   by test/fuzz. *)
let test_a_field_on_a_number_keeps_its_brackets () =
  formats_and_parses "field on an int" 29 "\"\"(fn->((6).o()))\n";
  (* And an application, which is the worse half: `(S 6).o` written as
     `S 6.o` is `S (6.o)` -- a different program before it is a lex error.
     The field target is rendered as an atom now, so every form that needs
     brackets in that position gets them. *)
  formats_and_parses "field on an application" 29 "\"\"(fn->((S 6).o()))\n";
  let out = fmt "let r = {x1 = 1}\nr.x1\n" in
  if Lint.contains out "(x1)" then
    Alcotest.failf "a name ending in a digit needs no brackets:\n%s" out;
  (* A literal whose lexeme runs on into the `.` keeps its brackets whatever
     it ends in. `.` is a path body character and a URL runs to the
     punctuation around it, so `(./p).log` written as `./p.log` is one token
     -- a field access turned into a path, which typechecks, so nothing
     downstream complains. These are checked as text because the property is
     that the brackets are still there: source that already had them lost
     them here too, so a round trip alone would have agreed with itself.
     Found by test/fuzz. *)
  List.iter (fun (lit, written) ->
    let out = fmt (Printf.sprintf "let v = (%s).log\nv\n" lit) in
    if not (Lint.contains out (written ^ ".log")) then
      Alcotest.failf "%s lost the brackets that make it a field access:\n%s"
        lit out)
    [ "./p",      "(./p)";
      " *.wa",    "( *.wa)";
      "http://x", "(http://x)";
      "~/h",      "(~/h)";
      "/etc/h",   "(/etc/h)";
      "$HOME",    "($HOME)" ]

(* A bracket is not written straight onto a glob. A glob literal opens with
   a star, and the two together make the sequence the lexer reads as an
   attempt at a block comment -- so `-*w J::i` came back as source that
   would not lex. Found by test/fuzz.

   Fixed once in `parenthesize`, and found again from a case body, which
   writes its own brackets. Every site that writes a bracket in front of
   emitted text goes through one helper now, so all of them are here: a bug
   that belongs to the bracket rather than to any one of its writers has to
   be tested at more than one of them.

   The last three do not wrap anything -- they open a bracket and put the
   first item after it, which is the same two characters, and each was still
   writing its own. Found by test/fuzz, three times. *)
let test_a_bracket_is_kept_off_a_glob () =
  formats_and_parses "a bracketed glob" 111 "-*w J::i\n";
  formats_and_parses "a glob in a case body" 25
    "match e with|()->(match e with|k->())(fn()->match t with|Ok(Oke)->*.wn\"%ed\";match d with|_->())\n";
  formats_and_parses "first in a tuple" 92 "let t = ( *.wa, 1)\nt\n";
  formats_and_parses "first in a block" 92 "let f x = ( *.wa; x)\nf 1\n";
  formats_and_parses "the base of a record update" 92
    "type T(g: Glob, a: Int)\nlet b = T(g = *.wa, a = 1)\nlet v = T( b, a = 2)\nv\n";
  (* `[` and `{` are not the comment opener, and a space nobody needs is a
     diff nobody meant. *)
  Alcotest.(check string) "a list gains no space"
    "let l = [*.wa]\n" (fmt "let l = [ *.wa]");
  Alcotest.(check string) "nor does a map"
    "let m = {a = *.wa}\n" (fmt "let m = {a = *.wa}")

(* A bracket written straight onto the `$` is literal command text; one
   written a space away is an expression that answers with the command.
   `$(i)` runs the command `i`; `$ (i)` runs whatever the value `i` holds.
   Both were printed as `$(...)`, which turned the second into the first
   without saying so. Found by test/fuzz. *)
let test_a_command_keeps_the_space_that_gives_it_meaning () =
  let expr_form = fmt "let s = \"ls\"\nlet g = $ (s)\ng\n" in
  if not (Lint.contains expr_form "$ (s)") then
    Alcotest.failf "an expression command loses its space:\n%s" expr_form;
  let text_form = fmt "let g = $(ls)\ng\n" in
  if not (Lint.contains text_form "$(ls)") then
    Alcotest.failf "a literal command gained a space:\n%s" text_form;
  (* `$?` has no such pair. `$? (e)` does not lex as a query, so a query's
     body is always the text and is always written tight. *)
  let query_form = fmt "let g = $?(ls)\ng\n" in
  if not (Lint.contains query_form "$?(ls)") then
    Alcotest.failf "a query gained a space:\n%s" query_form;
  (* Nor does `$*`, for the same reason. *)
  let command_form = fmt "let c = $*(ls)\nc\n" in
  if not (Lint.contains command_form "$*(ls)") then
    Alcotest.failf "a command value gained a space:\n%s" command_form

(* A float comes back as the number that was written. `%g` carries six
   significant digits and switches to an exponent past them, so `2222222.5`
   came back as `2.22222e+06` -- a different number, and one wand cannot
   read: there is no exponent form, so it lexed as `2.22222`, `e`, `+`, `6`.
   Found by test/fuzz. *)
let test_a_float_reads_back_as_itself () =
  List.iter
    (fun v ->
       let out = fmt (Printf.sprintf "let x = %s\nx\n" v) in
       let first = List.hd (String.split_on_char '\n' out) in
       Alcotest.(check string) ("the float " ^ v)
         (Printf.sprintf "let x = %s" v) first)
    [ "1.5"; "0.1"; "100.0"; "2222222.5"; "0.000001"; "3.14159265358979" ];
  (* And nothing anywhere in the output spells an exponent. *)
  let out = fmt "let x = 2222222.5\nx\n" in
  if Lint.contains out "e+" || Lint.contains out "E+" then
    Alcotest.failf "a float was written with an exponent wand cannot read:\n%s" out

(* A top-level item that opens with an operator gets brackets. A line
   opening with one continues the line above -- that is how a pipeline is
   written -- so `-1` as an item of its own, printed under a definition,
   was read as a subtraction on the next pass. Found by test/fuzz. *)
let test_an_item_opening_with_an_operator_is_bracketed () =
  assert_idempotent "an item that opens with a minus"
    "let{}=import T; -1\n";
  let out = fmt "let a = 1\n(-1)\n" in
  if not (Lint.contains out "(-1)") then
    Alcotest.failf "the brackets that make it its own statement went:\n%s" out

(* Nesting costs what it weighs, and not more.

   To decide whether a value fits on one line the emitters laid it out and
   measured it, then -- if it did not fit -- laid it out again at the indent
   it would really sit at. Both layouts asked the same question of every
   child, so the work doubled with every level. A `let ... in` chain
   fourteen deep took nearly two minutes at a forty-column margin, and the
   shapes that reach that depth are ordinary code: a file of `test` items
   inside a `with`, mutated a little.

   These are depth forty at a margin nothing fits under, which is where the
   old emitter stopped finishing. If this test ever hangs rather than fails,
   that is the bug coming back. Found by test/fuzz, which draws a margin per
   input and so kept meeting it. *)
let test_nesting_does_not_cost_exponentially () =
  let rec build n shape acc =
    if n = 0 then acc
    else
      let acc =
        match shape with
        | `App   -> Printf.sprintf "f (%s) 1" acc
        | `Let   -> Printf.sprintf "let y%d = %s in y%d" n acc n
        | `Block -> Printf.sprintf "(let z%d = %s; z%d)" n acc n
        | `List  -> Printf.sprintf "[%s, 1]" acc
      in
      build (n - 1) shape acc
  in
  List.iter
    (fun shape ->
       let src =
         Printf.sprintf "let f a b = a\nlet x = 1\nlet r = %s\nr\n"
           (build 40 shape "x")
       in
       let out = Formatter.with_width 40 (fun () -> fmt src) in
       (match parses out with
        | Ok () -> ()
        | Error e -> Alcotest.failf "deep nesting did not parse back: %s" e);
       Alcotest.(check string) "and it settles"
         out (Formatter.with_width 40 (fun () -> fmt out)))
    [ `App; `Let; `Block; `List ]

let test_output_parses_at_any_margin () =
  let files = corpus_files () in
  (* A sweep that reads nothing passes every file it never opens. *)
  Alcotest.(check bool) "found the corpus to format" true (List.length files >= 40);
  List.iter (fun path ->
    let src = In_channel.with_open_text path In_channel.input_all in
    List.iter (fun width ->
      let out = Formatter.with_width width (fun () -> fmt src) in
      (match parses out with
       | Ok () -> ()
       | Error e ->
         Alcotest.failf
           "%s formatted at a margin of %d does not parse: %s\n%s"
           (Filename.basename path) width e out);
      let twice = Formatter.with_width width (fun () -> fmt out) in
      if twice <> out then
        Alcotest.failf
          "%s formatted at a margin of %d is not a fixed point"
          (Filename.basename path) width)
      margins)
    files

let test_idempotent_snippets () =
  assert_idempotent "let binding" "let x = 1\nx + 1";
  assert_idempotent "if/else" "let f x = if x > 0 then \"pos\" else \"neg\"";
  assert_idempotent "match" "let f x = match x with\n| 0 -> \"zero\"\n| _ -> \"other\"";
  assert_idempotent "semicolon sequence" "let f x = (x + 1; x * 2)\nf 1";
  assert_idempotent "semicolon sequence wrapped"
    "let long_named_function x = (String.append x \"a considerable suffix string\"; String.append x \"another considerable suffix\"; x)\nlong_named_function \"y\""

(* ── Behavior preservation ───────────────────────────────────────────────── *)

let ok_after_format label src expected =
  let formatted = fmt src in
  match Runner.run_string formatted with
  | Ok v -> Alcotest.(check string) label expected v
  | Error msg -> Alcotest.failf "%s: formatted code failed to run: %s\nformatted:\n%s" label msg formatted

(* A string the source wrote with escaped quotes moves between backticks,
   where a quote is a quote -- unless the raw form could not reproduce it:
   a backtick in the text, a literal `%{`, or control characters whose
   spelled-out escapes are the more legible form. *)
let fmt_eq label src expected =
  Alcotest.(check string) label (expected ^ "\n") (fmt src)

(* The lines under a backtick are the string's own content. A value given a
   line of its own moves them one line down the page, and the backtick left
   above them says nothing the `=` did not. So a multi-line backtick string
   starts on the line of its `=`, as a bracket does -- which is what it is
   here. *)
let test_a_multiline_backtick_string_opens_on_the_eq_line () =
  fmt_eq "a top-level binding"
    "let a =\n  `\none\n  two`\na"
    "let a = `\none\n  two`\na";
  (* The brackets go: a chain of bindings is the body, and its statements sit
     at the indent the parser looks for a body at. What this pins is the
     string, which still starts on the line of its `=`. *)
  fmt_eq "a binding in a body, where every other wrapped value takes a line"
    "let f! () = (\n  let a =\n    `\none\n  two`;\n  a\n)\nf! ()"
    "let f! () =\n  let a = `\none\n  two`;\n  a\nf! ()";
  fmt_eq "a trailing argument"
    "t.eq \"x\"\n  `\none\n  two`"
    "(t.eq \"x\" `\none\n  two`)";
  assert_idempotent "the shape settles" "let a =\n  `\none\n  two`\na";
  (* The content is what must not move. A single-line backtick string is not
     a bracket and keeps the ordinary shape. *)
  ok_after_format "the text survives, indent and all"
    "import String\nlet a = `\n  one\n  two`\nString.length a"
    "11"

let test_escaped_quotes_prefer_backticks () =
  fmt_eq "plain string converts"
    {|let a = "say \"hi\""|} "let a = `say \"hi\"`";
  fmt_eq "interpolation converts, splice intact"
    {|let n = "ada"
let g = "greet \"%{n}\""|} "let n = \"ada\"\nlet g = `greet \"%{n}\"`";
  fmt_eq "a backtick in the text keeps the quoted form"
    {|let e = "tick ` quote \""|} {|let e = "tick ` quote \""|};
  fmt_eq "a newline keeps the quoted form"
    {|let d = "quote \" break \n"|} {|let d = "quote \" break \n"|};
  fmt_eq "a literal percent-brace keeps the quoted form"
    {|let f = "hold \%{x} quote \""|} {|let f = "hold \%{x} quote \""|};
  assert_idempotent "backtick preference is a fixed point"
    {|let a = "say \"hi\""
let g = "greet \"%{a}\""|};
  ok_after_format "the converted value is unchanged"
    {|let a = "say \"hi\""
a|}
    {|say "hi"|}

(* Braces are the only map syntax; a pattern puns where the key names its
   variable, and neither form is disturbed by a reformat. *)
let test_maps_canonicalize_to_braces () =
  fmt_eq "a punnable pattern comes back punned"
    "let f {a = a, b = c} = a\n1" "let f {a, b = c} = a\n1";
  fmt_eq "Map.empty is left as written"
    "import Map\nlet e = Map.empty\ne" "import Map\n\nlet e = Map.empty\ne";
  assert_idempotent "brace maps are a fixed point"
    "let m = {x = 1}\nlet {x} = m\nx"

(* A single constructor that is the type saying its own name again prints
   as the shorthand the parser already reads: `type Foo(fields)`. *)
let test_single_ctor_shorthand () =
  fmt_eq "long form converts"
    "type Container = Container(name: String, ready: Bool)\n1"
    "type Container(name: String, ready: Bool)\n1";
  fmt_eq "shorthand stays"
    "type Pod(name: String)\n1"
    "type Pod(name: String)\n1";
  fmt_eq "a differently named constructor keeps the long form"
    "type Opt = Wrapped(v: Int)\n1"
    "type Opt = Wrapped(v: Int)\n1";
  fmt_eq "a positional payload keeps the long form"
    "type Point = Point Int Int\n1"
    "type Point = Point Int Int\n1";
  assert_idempotent "shorthand with a type parameter"
    "type Box 'a = Box(item: 'a)\n1";
  (* A positional payload prints without its parentheses, so every type the
     bracketed form reads has to read there too. A module bound to a
     lowercase name did not: `type T(l.S)` came back as `type T = T l.S`,
     which parsed as a nullary constructor and a statement below it. Found
     by test/fuzz. *)
  assert_idempotent "a positional payload through a lowercase module"
    "type T(l.S)\n1";
  assert_idempotent "and a run of them"
    "type T(l.S Int m.U)\n1";
  assert_idempotent "shorthand too wide for one line"
    "type Wide = Wide(alpha: String, beta: String, gamma: String, delta: String, epsilon: String, zeta: String)\n1";
  ok_after_format "construction and matching still run through the shorthand"
    "type Pair = Pair(a: Int, b: Int)\nlet p = Pair(a = 1, b = 2)\nmatch p with\n| Pair(a = x, b = y) -> x + y"
    "3"

let test_behavior_preserved () =
  ok_after_format "arithmetic" "1 + 2 * 3" "7";
  ok_after_format "multi-equation function"
    "let fact 0 = 1\nlet fact n = n * fact (n - 1)\nfact 5"
    "120";
  ok_after_format "nested app needs parens"
    "let add a b = a + b\nlet f g x = g (add x 1) 2\nf add 3"
    "6";
  ok_after_format "semicolon sequence values the last statement"
    "let f x = (x + 1; x * 2)\nf 3"
    "6";
  ok_after_format "match with guard"
    {|let f x = match x with
| n when n < 0 -> "neg"
| 0 -> "zero"
| _ -> "pos"
f (-5)|}
    "neg";
  ok_after_format "tuple destructure"
    "let (a, b) = (1, 2)\na + b"
    "3";
  ok_after_format "cons pattern"
    "let f [h :: t] = h\nf [1, 2, 3]"
    "1";
  (* A match nested (unparenthesized in source) inside an outer match's
     case body: match cases only terminate at a non-`|` token, so an
     unparenthesized nested match here would swallow the outer match's
     remaining `| ...` cases into itself, changing the program's meaning. *)
  ok_after_format "match nested in match case body"
    {|let f x =
  match x with
  | Ok xs ->
    (match xs with
     | 1 -> "one"
     | _ -> "many")
  | Error _ -> "err"
f (Ok 1)|}
    "one";
  (* A recursive shorthand `let` (`let f n = ... f ... in ...`) is only
     recursive because of its exact surface syntax (see typechecker.ml's
     `Let (PVar name, Fn _, _)` special case) -- reformatting it as
     `let f = fn n -> ...` would drop that and break recursion. This is a
     regression guard for exactly that bug. *)
  ok_after_format "recursive local let stays recursive after formatting"
    "let f = fn t -> let fact n = if n <= 0 then 1 else n * fact (n - 1) in fact t\nf 5"
    "120";
  (* Local multi-equation clauses can repeat `let` (matching top-level
     syntax, parser.ml's `parse_fn_binding`) -- both clauses only give the
     right answer (120, via real 0/n dispatch) if genuinely merged into one
     recursive function; if the second `let fact` instead shadowed the first
     as a fresh nested binding, this would stack-overflow (no base case). *)
  ok_after_format "local multi-equation with repeated let stays merged after formatting"
    "let f = fn t -> let fact 0 = 1\nlet fact n = n * fact (n - 1)\nin fact t\nf 5"
    "120";
  (* `let x : T = e` reformatted via inline `e : T` syntax re-parses as
     "cons e onto T" (the parser's infix `:` in expression position always
     means cons, never ascription) rather than an annotated binding --
     `List Int` used as a bare expression additionally requires an import
     it never needed as a type, so this failed outright, not just silently. *)
  ok_after_format "value annotation survives formatting"
    "import List\nlet empty : List Int = []\nList.length empty"
    "0";
  (* Same ambiguity, at a function's return-type annotation
     (`let f x : T = body`) rather than a plain value binding. *)
  ok_after_format "function return-type annotation survives formatting"
    "let double x : Int = x * 2\ndouble 3"
    "6";
  (* A `match` nested inside another match's case is unambiguous only because
     it's parenthesized -- but the danger isn't limited to the case body being
     *directly* a Match: a `let ... in <tail match>` inside an case has the
     same "bare match at the end" shape once printed, since emit_let's
     fallback renders its tail completely unguarded. *)
  ok_after_format "match nested in a let's tail, inside another match's case"
    {|let f x =
  match x with
  | Ok xs ->
    let n = xs
    in (match n with
      | 1 -> "one"
      | _ -> "many")
  | Error _ -> "err"
f (Ok 1)|}
    "one";
  (* The same "bare match at the end" shape, reached through the three other
     forms that print their tail unguarded. Each one gave the outer match's
     remaining arms away to the nested match, which is why every case below
     asks for the answer of an arm that comes *after* the one at fault.
     Found by the daily fuzzer, under a `handle` (#22). *)
  ok_after_format "match at the end of a with body, inside another match's case"
    {|import Resource
let f x =
  match x with
  | 0 ->
    (with Resource.make (fn () -> 1) (fn _ -> ()) as n ->
      match n with
      | 1 -> "one"
      | _ -> "many")
  | _ -> "rest"
f 9|}
    "rest";
  ok_after_format "match at the end of an if's else, inside another match's case"
    {|let f x =
  match x with
  | 0 -> (if false then "no" else match x with | 0 -> "zero" | _ -> "other")
  | _ -> "rest"
f 9|}
    "rest";
  ok_after_format "match at the end of a fn body, inside another match's case"
    {|let g x =
  match x with
  | 0 -> fn _ -> (match x with | _ -> "inner")
  | _ -> fn _ -> "rest"
g 9 ()|}
    "rest";
  (* An `if` with no `else` prints its *then* branch last, so that is the
     branch the nested match sits at the end of. *)
  ok_after_format "match at the end of a one-armed if, inside another match's case"
    {|let f x =
  match x with
  | 0 -> (if false then match x with | _ -> ())
  | _ -> ()
f 9
"done"|}
    "done";
  (* A constructor pattern used as a function parameter (`let f (Some n) = ..`)
     needs its parens kept -- printed bare, `Some n` reads as two separate
     parameters instead of one destructured one. *)
  ok_after_format "constructor pattern as a function parameter"
    "type Opt = None | Some Int\nlet f None = 0\nlet f (Some n) = n\nf (Some 42)"
    "42"

(* `Ok 42.0` reformatting to `Ok 42` runs fine and *displays* the same (both
   show as "42"), so a `ok_after_format`-style behavior check can't catch it --
   only re-typechecking the formatted source tells Float and Int apart. *)
let type_after_format label src expected =
  let formatted = fmt src in
  let ty =
    Lexer.tokenize formatted
    |> Parser.parse_expr
    |> Typechecker.infer_expr
    |> Result.map Typechecker.string_of_typ
  in
  match ty with
  | Ok t    -> Alcotest.(check string) label expected t
  | Error e -> Alcotest.failf "%s: formatted code failed to typecheck: %s\nformatted:\n%s" label e formatted

let test_float_literal_type_preserved () =
  type_after_format "integral float keeps its type" "42.0" "Float";
  type_after_format "integral float in a constructor" "Ok 42.0" "Result 'a Float";
  type_after_format "non-integral float unaffected" "3.14" "Float"

(* A bare constructor absorbs a following *bracketed* expression, and only
   that:

     f None (x)   is  f (None x)
     f None x     is  (f None) x
     f None [1]   is  (f None) [1]
     f None 1     is  (f None) 1

   So a constructor argument needs brackets exactly when the text after it
   opens with `(`. This used to be decided by position -- brackets on every
   constructor that was not the last argument -- which added brackets
   nothing needed and, after a qualified name, changed the parse. `l.A S a`
   is `(l.A S) a`; `l.A (S) a` reads `S` as the payload of `l.A`. The
   formatter wrote each spelling as the other and never settled. Found by
   test/fuzz. *)
let test_constructor_argument_keeps_its_parens () =
  (* Behaviour, not text: formatted, this still returns the first argument
     rather than applying the constructor to the second. *)
  ok_after_format "constructor before another argument"
    "type Opt = None | Some Int\nlet f a b = b\nf (None) 7"
    "7";
  ok_after_format "constructor as the last argument"
    "type Opt = None | Some Int\nlet f a b = a\nf 7 None"
    "7";
  (* The brackets appear where the next argument would be absorbed. *)
  Alcotest.(check string) "a bracketed argument follows"
    "let f a b = b\nlet x = f (None) (1 + 1)\n"
    (fmt "let f a b = b\nlet x = f (None) (1 + 1)");
  (* And nowhere else. *)
  Alcotest.(check string) "last argument stays bare"
    "let f a b = a\nlet x = f 1 None\n"
    (fmt "let f a b = a\nlet x = f 1 None");
  Alcotest.(check string) "a bare argument follows"
    "let f a b = b\nlet x = f None 1\n"
    (fmt "let f a b = b\nlet x = f (None) 1");
  (* The shape that never settled. *)
  assert_idempotent "a constructor after a qualified name" "l.A S a\n";
  (* A run of them, and the head. Bracketing one constructor opens the same
     hole one place to its left, so the guard has to work backwards and has
     to reach the head: each of these came back reading two of its atoms as
     one application. Behaviour, by way of the parse -- the spelling is the
     guard's business, and only the shape has to survive. *)
  List.iter (fun src ->
    let out = fmt src in
    Alcotest.(check string) ("the spine survives: " ^ src)
      (Ast.show (Parser.parse_expr (Lexer.tokenize src)))
      (Ast.show (Parser.parse_expr (Lexer.tokenize out))))
    [ "(f S S) (B m)";      (* two constructors before a bracket *)
      "(S S S) (B m)";      (* and a constructor head above them *)
      "(l.A S) (B m)";      (* a qualified head, which absorbs harmfully *)
      "(l.A) (g x)";        (* and does so with no constructor argument at all *)
      "f None (1 + 1)" ]

(* `else ()` is the empty branch written out, and the one-armed form is the
   same expression. The formatter prints the shorter one either way. *)
let test_one_armed_if () =
  Alcotest.(check string) "an explicit empty else is dropped"
    "let f c = if c then g ()\n"
    (fmt "let f c = if c then g () else ()");
  Alcotest.(check string) "and one already written that way is left alone"
    "let f c = if c then g ()\n"
    (fmt "let f c = if c then g ()");
  Alcotest.(check string) "a branch that is not empty keeps its else"
    "let f c = if c then 1 else 2\n"
    (fmt "let f c = if c then 1 else 2")

(* A `Map` is keyed by arbitrary strings, and the parser takes a key quoted
   when it is not an identifier. Printing one bare produced source that does
   not lex -- so every map with a real-world key was destroyed by running the
   formatter over it, which is why none existed to notice. *)
let test_map_keys_that_are_not_identifiers () =
  Alcotest.(check string) "a key that needs quoting keeps them"
    "let m = {\"content-type\" = 1, \"@type\" = 2, name = 3}\n"
    (fmt "let m = {\"content-type\" = 1, \"@type\" = 2, name = 3}");
  Alcotest.(check string) "an identifier key written quoted comes back bare"
    "let m = {name = 1}\n"
    (fmt "let m = {\"name\" = 1}");
  ok_after_format "and a pattern with one still matches"
    "let f x = match x with\n| {\"a-b\" = v} -> v\n| _ -> 0\nf {\"a-b\" = 7}"
    "7"

(* Width is measured from the column the text starts at, which is not the
   indent it wraps to: a case body is written after `| Some x -> ` and so
   begins some way right of the case's own indent. Measuring from the indent
   said everything fitted, and left lines half again over the margin. *)
let longest_line s =
  String.split_on_char '\n' s
  |> List.fold_left (fun acc l -> max acc (String.length l)) 0

let check_wraps label src =
  let out = fmt src in
  if longest_line out > 92 then
    Alcotest.failf "%s: %d columns, should have wrapped:\n%s" label (longest_line out) out

let test_width_is_measured_from_the_start_column () =
  check_wraps "a case body after a wide pattern"
    "let f x =\n  match x with\n  | Some averylongconstructorpattern ->      let y = someprettylongfunction averylongconstructorpattern in y\n  | None -> 0";
  check_wraps "a lambda body inside a constructor field"
    "let t label =\n  Testing(\n    ok = fn cond -> if cond then Pass label else Fail      \"%{label}: the assertion did not hold at all\"\n  )";
  (* And what it decides still runs the same. *)
  ok_after_format "wrapping a case body preserves it"
    "type Opt = None | Some Int\nlet plus n = n + 1\nlet f x =\n  match x with\n     | Some averylongconstructorpattern -> let y = plus averylongconstructorpattern in y\n     | None -> 0\nf (Some 41)"
    "42"

(* An `if` or `match` that starts mid-line -- after `x = ` or `fn a -> ` --
   owns none of the text to its left, so when it breaks, its `else` and its
   cases step in rather than landing flush with the line that introduced
   them. An else-if chain is one ladder: the clauses all land at that same
   depth, instead of each else stepping past the one before it. *)
let test_midline_breaks_step_in () =
  fmt_eq "a mid-line else and mid-line cases step in"
    "type TestOutcome = Pass String | Fail String\nlet make label = Testing(not_ok = fn cond -> if cond then Fail \"%{label}: expected the assertion to fail here\" else Pass label, raises = fn thunk -> match try thunk () with | Ok _ -> Fail \"%{label}: expected a raise, but it completed normally\" | Error _ -> Pass label)"
    "type TestOutcome = Pass String | Fail String\nlet make label =\n  Testing(\n    not_ok = fn cond -> if cond then Fail \"%{label}: expected the assertion to fail here\"\n      else Pass label,\n    raises = fn thunk -> match try thunk () with\n      | Ok _ -> Fail \"%{label}: expected a raise, but it completed normally\"\n      | Error _ -> Pass label\n  )";
  fmt_eq "a ladder that starts its own line stays flush"
    "let grade score = let describe s = if s > 90 then \"an excellent score, top marks all around\" else if s > 75 then \"a good score, comfortably above the line\" else \"a score that needs another attempt\" in describe score"
    "let grade score =\n  let describe s =\n    if s > 90 then \"an excellent score, top marks all around\"\n    else if s > 75 then \"a good score, comfortably above the line\"\n    else \"a score that needs another attempt\";\n  describe score";
  fmt_eq "a mid-line ladder steps in once and holds"
    "let pick = (fn kind -> if kind == \"circle\" then \"a shape with no corners at all\" else if kind == \"rect\" then \"a shape with four of them\" else \"a shape nobody here has heard of\")"
    "let pick =\n  fn kind -> if kind == \"circle\" then \"a shape with no corners at all\"\n    else if kind == \"rect\" then \"a shape with four of them\"\n    else \"a shape nobody here has heard of\""

(* A manifest is a statement about the whole file rather than a line of it,
   so it stands off from the code whether or not the source did. Left to the
   source, one `wand t --fix` had just inserted stayed jammed against the
   first import and no amount of `wand f` would separate it. *)
let test_manifest_is_followed_by_a_blank_line () =
  fmt_eq "a manifest with nothing after it gains the blank line"
    "uses {FS.Read}\nimport FS\nlet read p = FS.read_file! p"
    "uses {FS.Read}\n\nimport FS\n\nlet read p = FS.read_file! p";
  fmt_eq "and one already spaced off keeps exactly one"
    "uses {FS.Read}\n\n\n\nimport FS\nlet read p = FS.read_file! p"
    "uses {FS.Read}\n\nimport FS\n\nlet read p = FS.read_file! p";
  fmt_eq "a comment after the manifest is what follows it"
    "uses {FS.Read}\n-- why this file reads\nimport FS\nlet read p = FS.read_file! p"
    "uses {FS.Read}\n\n-- why this file reads\nimport FS\n\nlet read p = FS.read_file! p"

(* A parenthesized expression that ran onto more lines closes on a line of
   its own. Trailing the last line, the bracket joins a stack of `))` that
   says nothing about which of them ends what -- and the last line of a
   `match` is its final case, where a `)` is easiest of all to misread as
   part of the case. *)
let test_a_multiline_paren_closes_on_its_own_line () =
  (* The call itself takes no brackets. Its continuation lines are indented
     past the `let`, so the application does not end at the first line end
     and nothing has to say so -- see `bracket_if_wrapped_app_at`. What this
     is about is the inner `)`, which still closes on a line of its own. *)
  fmt_eq "a handler argument closes below its last case"
    "let f thunk = check (handle thunk () with | FS!read_file _ _ -> \"caught\" | FS!write_file _ _ -> \"wrote\")"
    "let f thunk =\n  check\n    (handle thunk () with\n    | FS!read_file _ _ -> \"caught\"\n    | FS!write_file _ _ -> \"wrote\"\n    )";
  assert_idempotent "closing on its own line is a fixed point"
    "let f thunk = check (handle thunk () with | FS!read_file _ _ -> \"caught\" | FS!write_file _ _ -> \"wrote\")";
  (* One that still fits on a line keeps its bracket where it was. *)
  fmt_eq "a parenthesis that did not wrap is left alone"
    "let g x = check (x + 1)" "let g x = check (x + 1)";
  (* A binding's value that wrapped takes no brackets either, and the output
     runs: the lines under it are indented past the `let`, so they are the
     continuation they look like. The guard that used to bracket this was
     written when a newline ended a definition whatever the indent. *)
  ok_after_format "a wrapped value needs no brackets"
    "import List\nimport String\nlet total xs =\n  List.fold_left (fn acc s -> acc + String.length s) 0 xs\nString.of_int (total [\"aaaaaaaaaaaaaaaaaaaaaaaaa\", \"bbbbbbbbbbbbbbbbbbbbbbbbbb\"])"
    "51"

(* A case body wide enough to wrap has to come back as a program. It once
   did not: `wand f` turned tools/check_fmt.wand into a file that would not
   parse, which is the worst thing a formatter can do, so this runs the
   output rather than reading it.

   Brackets were the answer then, because a newline ended a definition
   whatever the indent. Indentation decides now, and the continuation sits
   past the arm, so the body carries no brackets and still runs. What is
   pinned here is the running, which is the part that matters and the part
   that did not change. *)
let test_a_wrapped_case_body_keeps_its_brackets () =
  ok_after_format "a wrapped application in a case body still parses"
    "import String\nlet f xs =\n  match xs with\n  | [] -> String.upper \"a considerable message here, quite long enough to wrap past the margin\"\n  | _ -> \"some\"\nf []"
    "A CONSIDERABLE MESSAGE HERE, QUITE LONG ENOUGH TO WRAP PAST THE MARGIN";
  ok_after_format "and the other arm is unaffected"
    "import String\nlet f xs =\n  match xs with\n  | [] -> String.upper \"a considerable message here, quite long enough to wrap past the margin\"\n  | _ -> \"some\"\nf [1]"
    "some";
  assert_idempotent "a wrapped case body is a fixed point"
    "import String\nlet f xs =\n  match xs with\n  | [] -> String.upper \"a considerable message here, quite long enough to wrap past the margin\"\n  | _ -> \"some\"\nf []";
  (* The arm below it is still an arm, and a definition after the match is
     still its own: a wrapped body reaches neither. *)
  ok_after_format "an arm and a definition below it both survive"
    "import String\nlet f xs =\n  match xs with\n  | [] -> String.upper \"a considerable message here, long enough to wrap past\"\n  | _ -> \"some\"\nlet g = 9\nf [] ++ String.of_int g"
    "A CONSIDERABLE MESSAGE HERE, LONG ENOUGH TO WRAP PAST9"

(* The block of plain imports stands off from whatever follows it, whether
   that is a destructured import or the first definition. *)
let test_import_block_is_followed_by_a_blank_line () =
  fmt_eq "a destructured import is separated from the block above it"
    "import FS\nimport Path\nlet {test} = import Test\ntest \"x\" (fn t -> t.ok true)"
    "import FS\nimport Path\n\nlet {test} = import Test\ntest \"x\" (fn t -> t.ok true)";
  fmt_eq "so is the first definition when there is no destructured import"
    "import FS\nimport Path\nlet read p = FS.read_file! p"
    "import FS\nimport Path\n\nlet read p = FS.read_file! p";
  fmt_eq "a manifest and an import block each get their own blank line"
    "uses {FS.Read}\nimport FS\nlet {test} = import Test\ntest \"x\" (fn t -> t.ok true)"
    "uses {FS.Read}\n\nimport FS\n\nlet {test} = import Test\ntest \"x\" (fn t -> t.ok true)"

(* A value that carries its own opening bracket keeps it on the line that
   introduces it, and the items carry the break. Given a line of its own the
   bracket says nothing -- the items sit at the same column either way --
   while costing a line at the top of every list, map and tuple wide enough
   to wrap. All three bracket forms follow the rule. *)
let test_bracketed_values_open_on_the_binding_line () =
  fmt_eq "a list opens on the binding's line"
    "let a_list = [\"a considerable string here\", \"another considerable string\", \"and a third one\"]"
    "let a_list = [\n  \"a considerable string here\",\n  \"another considerable string\",\n  \"and a third one\"\n]";
  fmt_eq "a map does too"
    "let a_map = {alpha = \"a considerable string\", beta = \"another considerable one\", gamma = \"third\"}"
    "let a_map = {\n  alpha = \"a considerable string\",\n  beta = \"another considerable one\",\n  gamma = \"third\"\n}";
  fmt_eq "and a tuple"
    "let a_tuple = (\"a considerable string here\", \"another considerable string\", \"and a third one\")"
    "let a_tuple = (\n  \"a considerable string here\",\n  \"another considerable string\",\n  \"and a third one\"\n)";
  (* The two positions a group body puts it in: after `in`, and as the
     body of a trailing lambda. *)
  (* The chain takes the line below the arrow and its statements share that
     indent, so the lambda's body needs no brackets and the `in` is a `;`. *)
  fmt_eq "a binding chain as a trailing lambda's body"
    "import String\nlet build = group \"the report\" (fn () -> let lines = String.lines report in [check \"a considerable assertion here\", check \"another considerable one\"])"
    "import String\n\nlet build =\n  group \"the report\" (fn () ->\n    let lines = String.lines report;\n    [check \"a considerable assertion here\", check \"another considerable one\"])"

(* An item is placed two columns in, so that is the indent it wraps to.
   Rendered at the sequence's own indent, an item's continuation lines
   landed to the left of the item itself. *)
let test_sequence_items_wrap_to_their_own_column () =
  fmt_eq "a match inside a tuple keeps its arms under it"
    "let tally first_err line = (1, match first_err with | Some e -> Some e | None -> if String.contains? \"ERROR\" line then Some line else None)"
    "let tally first_err line = (\n  1,\n  match first_err with\n  | Some e -> Some e\n  | None -> if String.contains? \"ERROR\" line then Some line else None\n)"

(* A binding's value may run onto the next line, but not as a bare
   application: the definition ends at the first line, loudly at the top
   level and silently inside a `let ... in`. Every other wrapped form carries
   an operator or a bracket that says it is not finished, so only this one
   needs the parentheses put back. *)
let test_a_wrapped_application_keeps_its_brackets () =
  ok_after_format "a top-level binding still binds what it looks like"
    "let g a b c = a + b + c\nlet x =\n  (g\n     100000\n     200000\n     300000)\nx"
    "600000";
  ok_after_format "and a local one"
    "let g a b c = a + b + c\nlet outer =\n  let x =\n    (g\n       100000\n       200000\n       300000)\n  in x\nouter"
    "600000";
  (* A form that carries its own continuation is left alone. *)
  Alcotest.(check string) "a wrapped match gains no brackets"
    "let f x =\n  match x with\n  | 0 -> \"zero\"\n  | _ -> \"other\"\n"
    (fmt "let f x =\n  match x with\n  | 0 -> \"zero\"\n  | _ -> \"other\"")

(* ── Comment preservation ────────────────────────────────────────────────── *)

let contains haystack needle =
  let hn = String.length haystack and nn = String.length needle in
  if nn = 0 then true
  else if nn > hn then false
  else begin
    let found = ref false in
    for i = 0 to hn - nn do
      if String.sub haystack i nn = needle then found := true
    done;
    !found
  end

let assert_contains label out needle =
  if not (contains out needle) then
    Alcotest.failf "%s: expected to find %S in output:\n%s" label needle out

let test_comments_preserved () =
  let src = "-- a leading comment\nlet x = 1\nx + 1" in
  assert_contains "leading comment" (fmt src) "a leading comment";
  let src2 = "let x = 1 -- trailing note\nlet y = 2\nx + y" in
  assert_contains "same-line comment" (fmt src2) "trailing note";
  let src3 = "-- documents the binding below\nlet x = 1\nx" in
  assert_contains "documentation" (fmt src3) "documents the binding below"

(* A comment inside an item's own span (between multi-equation clauses,
   or inside a function body) must stay where it was, not get silently
   relocated to after the whole item -- verified by checking the comment
   still precedes the text that followed it in the original source. *)
let assert_appears_before label out needle_before needle_after =
  let find s =
    let n = String.length out and m = String.length s in
    let pos = ref (-1) in
    (try
       for i = 0 to n - m do
         if String.sub out i m = s then (pos := i; raise Exit)
       done
     with Exit -> ());
    !pos
  in
  let before_pos = find needle_before and after_pos = find needle_after in
  if before_pos < 0 then Alcotest.failf "%s: %S not found in output:\n%s" label needle_before out;
  if after_pos < 0 then Alcotest.failf "%s: %S not found in output:\n%s" label needle_after out;
  if not (before_pos < after_pos) then
    Alcotest.failf "%s: expected %S before %S, got:\n%s" label needle_before needle_after out

let test_interior_comment_position () =
  let src = "let f 0 = \"zero\"\n-- second clause\nlet f n = \"other\"\nf 3" in
  let out = fmt src in
  assert_contains "comment between multi-equation clauses" out "second clause";
  assert_appears_before "comment stays between clauses, not after both"
    out "second clause" "let f n";
  let src2 = "let f x =\n  -- explain this\n  x + 1\nf 5" in
  let out2 = fmt src2 in
  assert_contains "comment inside function body" out2 "explain this";
  assert_appears_before "comment stays inside body, not after the function"
    out2 "explain this" "f 5"

(* A comment that follows an item on the same source line stays on that
   line. Pieces are ordered by source offset, so a comment before an item
   on the same line still introduces it. *)
let test_trailing_comment_stays_on_line () =
  let out = fmt "let x = 1  -- trailing\nlet y = 2\nx" in
  assert_contains "line comment kept" out "-- trailing";
  Alcotest.(check bool) "line comment stays on the binding's line" true
    (List.exists (fun l ->
       contains l "let x = 1" && contains l "-- trailing")
     (String.split_on_char '\n' out));
  let out3 = fmt "-- lead\nlet x = 1\nx" in
  assert_appears_before "a comment written before an item still precedes it"
    out3 "lead" "let x = 1"

(* Documentation is a run of comment lines above the definition, and the
   formatter leaves the run where it is: it never merges the lines, and it
   never puts a blank line between the run and what it documents. *)
let test_doc_run_kept_together () =
  let out = fmt "-- first line\n-- second line\nlet x = 1\nx" in
  let lines = String.split_on_char '\n' out in
  let rec adjacent = function
    | a :: b :: c :: _ when a = "-- first line" && b = "-- second line"
                            && contains c "let x = 1" -> true
    | _ :: tl -> adjacent tl
    | [] -> false
  in
  Alcotest.(check bool) "the run sits directly above the binding" true (adjacent lines)

(* A verbatim item's slice runs to the next item, absorbing any comment
   between them; if its recorded extent ignores that, a blank line gets
   inserted between a doc comment and the binding it documents. `try` is one
   of the constructs that triggers the verbatim path. *)
let test_no_blank_between_doc_and_binding () =
  let out =
    fmt "let a =\n  match try (f ()) with\n  | Ok v -> v\n  | Error _ -> 0\n\n         -- doc\nlet b = 2\nb"
  in
  let lines = String.split_on_char '\n' out in
  let rec check = function
    | a :: b :: tl ->
      if contains a "-- doc" && String.trim b = "" then
        Alcotest.failf "blank line separates the doc comment from its binding:\n%s" out
      else check (b :: tl)
    | _ -> ()
  in
  check lines

(* A record-shaped type too wide for one line widens down the page instead of
   running past the margin. *)
let test_wide_type_definition_wraps () =
  let src =
    "type Testing 'a 'b = Testing(ok: (Bool -> Int), not_ok: (Bool -> Int),      eq: ('a -> 'a -> Int), not_eq: ('a -> 'a -> Int), raises: ((Unit -> 'b) -> Int))\n     let f x = x\nf 1"
  in
  let out = fmt src in
  List.iter (fun l ->
    if String.length l > 92 then
      Alcotest.failf "formatted line exceeds the 92-column margin (%d):\n%s"
        (String.length l) l)
    (String.split_on_char '\n' out);
  assert_contains "fields kept" out "raises:";
  (* And the result still parses back to the same shape. *)
  assert_idempotent "wrapped type definition" src

(* A named field's type may be a function, written bare: the comma or the
   closing parenthesis ends the field, so the parentheses say nothing. The
   formatter has to print it bare once the parser takes it, or a writer puts
   one thing in and `wand f` gives another back. A function type in a
   parameter position keeps its parentheses, because there they say which
   type it is. *)
let test_named_field_arrow_loses_brackets () =
  let src =
    "type T 'a 'b 'e(ok: (Bool -> Int), eq: ('a -> 'a -> Int), \
     raises: ((Unit -> 'b ! 'e) -> Int ! 'e), plain: List Int)\nlet f x = x\nf 1"
  in
  let out = fmt src in
  assert_contains "the top-level arrow is bare" out "ok: Bool -> Int";
  assert_contains "and a curried one" out "eq: 'a -> 'a -> Int";
  assert_contains "a parameter keeps its parentheses" out
    "raises: (Unit -> 'b ! 'e) -> Int ! 'e";
  assert_contains "an application is untouched" out "plain: List Int";
  assert_idempotent "arrow fields" src

(* An interface declares its members the way a record declares its fields,
   and an implementation writes each binding on its own line with the `;`
   that says they are siblings. A member's doc is a run of comments above it,
   exactly as it is above a top-level binding. *)
let test_interface_and_implement_settle () =
  let src =
    "interface Ord 'a(max: 'a -> 'a -> 'a, min: 'a -> 'a -> 'a)\n\n\
     implement Ord Int =\n\
     \  -- The larger of two.\n\
     \  let max a b = if a > b then a else b;\n\
     \  let min a b = if a < b then a else b\n"
  in
  Alcotest.(check string) "written back as it was" src (fmt src);
  assert_idempotent "interface and implementation" src

(* Past the margin the members go one to a line, with the closing bracket on
   its own -- the shape a record already wraps into. *)
let test_a_wide_interface_wraps () =
  let src =
    "interface Comparing 'a(maximum: 'a -> 'a -> 'a, minimum: 'a -> 'a -> 'a, \
     clamped: 'a -> 'a -> 'a -> 'a, within?: 'a -> 'a -> 'a -> Bool)\nlet f x = x\nf 1"
  in
  let out = fmt src in
  List.iter (fun l ->
    if String.length l > 92 then
      Alcotest.failf "formatted line exceeds the 92-column margin (%d):\n%s"
        (String.length l) l)
    (String.split_on_char '\n' out);
  assert_contains "the members survive" out "within?:";
  assert_idempotent "wrapped interface" src

(* Every stage of a pipeline starts at the pipeline's own column. The first
   stage was rendered at the column the later ones use, so a first stage that
   wrapped put its interior two spaces too deep and closed its bracket out of
   line with the one it opened. *)
let test_a_pipeline_aligns_its_stages () =
  let src =
    "let blockers floor r = [\n\
     \    (r, \"still a draft\"),\n\
     \    (floor, \"below the floor, and this line is long enough to wrap it\")\n\
     \  ] |> List.filter_map (fn (bad, why) -> if bad then Some why else None)\n"
  in
  let out = fmt src in
  assert_contains "the list opens at the body indent" out "\n  [\n";
  assert_contains "its elements sit one level in" out "\n    (r, ";
  assert_contains "and it closes in line with the bracket it opened" out "\n  ]\n";
  assert_contains "the pipe joins them at the same column" out "\n  |> List.filter_map";
  assert_idempotent "a wrapped pipeline" src

let test_blank_lines () =
  let src = "let x = 1\n\n\n\nlet y = 2\nx + y" in
  let out = fmt src in
  (* collapse to at most one blank line between items *)
  if contains out "\n\n\n" then
    Alcotest.failf "expected blank-line run to collapse to one, got:\n%s" out


(* ── The shebang ─────────────────────────────────────────────────────────── *)

(* The lexer steps over `#!` and emits no token for it, so it reaches
   neither the parser nor the pieces the output is assembled from. The
   formatter dropped it, and `wand f` writes in place: formatting a script
   that runs itself stopped it running. The reference documents the form. *)
let test_shebang_survives () =
  let src = "#!/usr/bin/env wand\nuses {IO}\n\nimport IO\n\nIO.println \"hi\"\n" in
  Alcotest.(check string) "the shebang is still the first line" src (fmt src)

let test_shebang_without_a_manifest () =
  let src = "#!/usr/bin/env wand\nlet x = 1\n" in
  Alcotest.(check string) "kept with nothing else above the code" src (fmt src)

let test_shebang_settles () =
  let src = "#!/usr/bin/env wand\nlet x = 1\n" in
  Alcotest.(check string) "a second pass changes nothing" (fmt src) (fmt (fmt src))

(* A `#` anywhere but the first two bytes is not a shebang, and the lexer
   refuses it. Nothing here should invent one. *)
let test_no_shebang_gains_none () =
  let out = fmt "let x = 1\n" in
  if contains out "#!" then
    Alcotest.failf "a file with no shebang gained one:\n%s" out

(* An item that opens with an operator continues the item above it, so
   `assemble` writes back the `;` that separated the two. The item above was
   a verbatim slice, and a slice runs to the next item's offset -- so it
   already held that `;`. A second one went on after it, and the line grew a
   `;` per pass, for ever. Found by test/fuzz. *)
let test_a_separator_is_not_written_twice () =
  assert_idempotent "an operator item after a verbatim slice"
    "()--\n-let e=();-\n--\nlet k=()"

(* A comment runs to the end of its line and swallows whatever is written
   after it, so the separator above an operator item cannot go there: it
   stopped being a separator and changed the comment's own text instead.
   `prev_is_comment` guards a piece that *is* a comment; a verbatim slice
   ends in whatever the source had, a trailing comment among the rest.
   Found by test/fuzz. *)
let comments_of src =
  List.sort compare
    (List.map (fun (c : Formatter.comment_tok) -> c.Formatter.c_text)
       (Formatter.all_comments src (Lexer.tokenize src)))

let test_a_separator_is_not_written_onto_a_trailing_comment () =
  let src = "e--\n-\"\";--\n-\n--\nlet h=t" in
  let before = comments_of src and after = comments_of (fmt src) in
  Alcotest.(check (list string)) "every comment comes back as it was written"
    before after

(* A constructor takes the bracket written after it. `p.M N` is
   `App (Qualified (p, M), N)`. `p.M(N)` is `Qualified (p, App (M, N))`.
   These are two programs. The bracket puts the payload inside the module,
   so a payload written inside it is read there. Each form comes back as
   itself. Found by test/fuzz twice. The first fix wrote every bracket form
   as the loose form, which settled the instability and changed the scope. *)
let test_a_qualified_head_does_not_hide_a_constructor () =
  assert_idempotent "a qualified constructor before a bracket" "p.M(N)(9[])";
  assert_idempotent "and spelled the other way" "p.M N (9 [])";
  fmt_eq "the bracket holds the payload's scope and stays"
    {|let a = d.M(N)|}
    {|let a = d.M(N)|};
  fmt_eq "the loose form is a different program and is kept"
    {|let a = d.M N|}
    {|let a = d.M N|};
  fmt_eq "a bracket form before a second bracket keeps both apart"
    {|let a = n d.M(N)(s [])|}
    {|let a = n d.M(N) (s [])|};
  (* A qualified constructor absorbs the bracket after it with or without
     the space, so the space goes and the text says what it already meant. *)
  fmt_eq "a spaced bracket after a qualified name loses the space"
    {|let a = l.A (S) a|}
    {|let a = l.A(S) a|}

(* A literal whose lexeme runs into a following `.` keeps its brackets. A
   version was asked that by its last character, and a prerelease is
   dot-separated: `1.0.0` was bracketed and `1.0.0-a` was not, so
   `(1.0.0-a).f` came back as the single version `1.0.0-a.f` and a file that
   was a type error typechecked. Found by test/fuzz. *)
let test_a_version_keeps_its_brackets_before_a_field () =
  fmt_eq "a prerelease does not eat the dot"
    {|let a = (1.0.0-a).f|}
    {|let a = (1.0.0-a).f|};
  fmt_eq "nor does a longer one"
    {|let a = (1.0.0-alpha.1).f|}
    {|let a = (1.0.0-alpha.1).f|};
  fmt_eq "a plain version was already bracketed"
    {|let a = (1.0.0).f|}
    {|let a = (1.0.0).f|};
  (* The literals beside it are field accesses and were never at risk. *)
  fmt_eq "a duration is not bracketed"
    {|let a = 30s.f|}
    {|let a = 30s.f|};
  fmt_eq "nor a size"
    {|let a = 100MB.f|}
    {|let a = 100MB.f|}

(* ── The constructs that used to be copied verbatim ──────────────────────── *)

(* $(), $?(), try, contracts, handle and regex literals were re-emitted as
   source slices because they had no formatting rule. They have rules now,
   and these are the ways those rules can silently change a program. *)

let test_command_text_is_not_quoted () =
  (* $() holds a command, not a string. Quoting it hands the whole thing to
     the shell as one word, which is a working script turned broken. *)
  ok_after_format "a command survives formatting"
    "let out = $(echo hi)\nout"
    "hi";
  assert_contains "and is still written bare" (fmt "let x = $(git status)\nx")
    "$(git status)";
  ok_after_format "including its interpolations"
    "let n = 1\nlet out = $(echo %{n})\nout"
    "1"

let test_try_is_parenthesised_as_an_operand () =
  (* `try` reaches as far right as it can, so an operand printed bare
     swallows the operator: `(try e) == x` would become `try (e == x)`. *)
  ok_after_format "try on the left of a comparison"
    "let f () = 1\nlet r = (try (f ())) == Ok 1\nr"
    "true"

let test_contract_clauses_keep_their_indent () =
  let out = fmt "let half n =\n  requires n % 2 == 0\n  ensures result * 2 == n\n  n / 2\nhalf 10" in
  List.iter (fun needle ->
    Alcotest.(check bool)
      (Printf.sprintf "%S sits at the body's indent" needle) true
      (List.exists (fun l -> l = needle) (String.split_on_char '\n' out)))
    ["  requires n % 2 == 0"; "  ensures result * 2 == n"];
  ok_after_format "and the contract still holds" 
    "let half n =\n  requires n % 2 == 0\n  n / 2\nhalf 10"
    "5";
  (* A body on the line under the last clause has to start something new,
     and a line that opens with an operator continues the line above it
     instead. `requires p (-1)` came back as `requires p` over `-1`, which
     re-read as `requires p - 1` with no body left, and the output did not
     parse. Found by test/fuzz. *)
  ok_after_format "a body that opens with an operator keeps its brackets"
    "let f n =\n  requires n > 0\n  (-n)\nf 3"
    "-3";
  (* And the bracket goes on through `opener`, because a glob opens with a
     star: written straight onto one, `(` and `*` are what the lexer reads
     as an attempt at a block comment, and the body was saved while the
     file stopped parsing. Found by test/fuzz. *)
  ok_after_format "a body that opens with a glob keeps its brackets too"
    "let f n =\n  requires n > 0\n  **/*.wand\nf 3"
    "**/*.wand"

(* A lambda's clauses belong under its own arrow. Written after `fn ... ->`
   the first one was hugged onto that line and the rest landed at the
   enclosing indent, so the indentation said the body was not the lambda's. *)
let test_a_lambda_keeps_its_contract_under_the_arrow () =
  let out =
    fmt "let step : Int -> Int =\n  fn n ->\n    requires n > 0\n    ensures result > n\n    n + 1\nstep 3"
  in
  let lines = String.split_on_char '\n' out in
  List.iter (fun needle ->
    Alcotest.(check bool)
      (Printf.sprintf "%S sits under the arrow" needle) true
      (List.exists (fun l -> l = needle) lines))
    ["  fn n ->"; "    requires n > 0"; "    ensures result > n"; "    n + 1"];
  ok_after_format "and it still runs"
    "let step : Int -> Int =\n  fn n ->\n    requires n > 0\n    n + 1\nstep 3"
    "4";
  ok_after_format "several clauses and two parameters"
    "let pair =\n  fn a b ->\n    requires a > 0\n    requires b > 0\n    ensures result > a\n    a + b\npair 1 2"
    "3";
  (* Inside brackets the parser suspends the layout rule, so this one read
     correctly while the indentation said otherwise. *)
  ok_after_format "a lambda passed as an argument"
    "let apply f x = f x\napply\n  (fn n ->\n    requires n > 0\n    n * 2)\n  5"
    "10"

let test_handle_and_regex_round_trip () =
  ok_after_format "a handler"
    "let m () = handle $(git push) with\n| Shell!run c k -> k \"ok\"\nm ()"
    "ok";
  ok_after_format "a regex literal"
    "import Regex\nRegex.match? r/fix|bug/i \"FIXED\""
    "true"

(* Every clause of a binding repeats `let`, at the binding's own indent.
   Both spellings of the source converge, since which was written is not in
   the AST.

   The later clauses used to line up under the first one's name instead --
   a shape that says more clearly which lines belong to the binding and
   which one ends it, and which is still what a reader may prefer. It was
   given up because it is not a spelling the language always accepts: the
   bare continuation parses only where a newline ends an expression, so the
   same function inside a `( ... )` came back as a parse error. Repeating
   `let` parses at the top level, in a bare `fn` body, and inside brackets,
   and it is what the top-level emitter has always written. One spelling
   that works everywhere beat a nicer one that works in most places. *)
let test_let_clause_alignment () =
  let lines ls = String.concat "\n" ls in
  let expected =
    lines [ "let answer =";
            "  let fib 0 = 0";
            "  let fib 1 = 1";
            "  let fib n = fib (n - 1) + fib (n - 2);";
            "  fib 10";
            "";
            "answer" ]
  in
  let aligned =
    lines [ "let answer =";
            "  let fib 0 = 0";
            "      fib 1 = 1";
            "      fib n = fib (n - 1) + fib (n - 2)";
            "  in fib 10";
            "";
            "answer" ]
  in
  Alcotest.(check string) "from the repeated-let spelling"
    expected (String.trim (fmt expected));
  Alcotest.(check string) "from the aligned spelling"
    expected (String.trim (fmt aligned));
  assert_idempotent "the layout is a fixed point" expected

(* A backtick string has to come back as one. Rendered as a quoted string it
   would return escaped -- the whole point of writing it was not to escape --
   and a newline inside it would not read back at all. *)

let raw_src =
  "let inline = `{\"hello\": \"world\"}`\n\
   let block = `\n\
   one\n\
   two\n\
   `\n\
   let re = `\\d+\\s*`\n\
   let who = \"ada\"\n\
   let interp = `{\"name\": \"%{who}\"}`\n\
   inline"

let test_raw_strings_round_trip () =
  let out = fmt raw_src in
  assert_contains "quotes stay unescaped" out "`{\"hello\": \"world\"}`";
  assert_contains "backslashes stay literal" out "`\\d+\\s*`";
  assert_contains "interpolation is kept" out "`{\"name\": \"%{who}\"}`";
  Alcotest.(check bool) "no backtick text was requoted as a string literal"
    false (contains out "\"{\\\"hello");
  assert_idempotent "raw strings are a fixed point" raw_src;
  (* The value has to survive the trip, not just the shape. *)
  (match Runner.run_string (out ^ "\n") with
   | Ok _ -> ()
   | Error m -> Alcotest.failf "formatted source no longer runs: %s" m)

(* A multi-line literal keeps its layout: the newline the lexer drops after
   the opening backtick is put back, or each pass would eat a line. *)
let test_raw_multiline_keeps_its_shape () =
  let out = fmt "let b = `\none\ntwo\n`\nb" in
  assert_contains "content still starts on its own line" out "`\none\ntwo\n`"

(* An item's last line, not the line its last token began on. A raw string
   with a newline in it starts on one line and ends on another, and the
   assembly step read the start as the end -- so it saw a gap where the
   source had none and filled it with a blank line. `wand f` therefore added
   a line the second time it ran over its own output. Found by test/fuzz. *)
let test_a_multiline_literal_does_not_open_a_gap () =
  let src = "let banner = `\nhello\n`\nlet n = 1\nn\n" in
  let out = fmt src in
  let rec gap = function
    | a :: b :: tl ->
      if String.trim a = "`" && String.trim b = "" then true else gap (b :: tl)
    | _ -> false
  in
  if gap (String.split_on_char '\n' out) then
    Alcotest.failf "a blank line appeared after the literal:\n%s" out;
  assert_idempotent "a multi-line literal is a fixed point" src

(* Five sequences make the lexer stop inside a string: `%{` interpolates,
   and `%!{`, `${`, `$!{` and `#{` are each refused as the wrong spelling of
   it. A string holding one as text wrote it escaped, and it has to come
   back escaped -- `wand f` used to return three of the five bare, so
   formatting a file left source the same wand could no longer read. *)
let test_string_openers_come_back_escaped () =
  List.iter (fun (opener, escaped) ->
    let src = Printf.sprintf "let s = \"text %s here\"\ns" escaped in
    ok_after_format (opener ^ " survives") src
      (Printf.sprintf "text %s here" opener);
    assert_idempotent (opener ^ " is a fixed point") src)
    [ "%{x}",  "\\%{x}";
      "%!{x}", "\\%!{x}";
      "${x}",  "\\${x}";
      "$!{x}", "\\$!{x}";
      "#{x}",  "\\#{x}" ]

(* `(let x = 1; a; b)` and `(let x = 1 in (a; b))` say the same thing, and
   the node carries which one was written (`Ast.let_style`). So each comes
   back as itself, including the shape that has no `;` left in it:
   `(let x = 1; x + 2)` used to normalize to `in`, which turned the block
   spelling into the one the style guide keeps for naming. *)
(* ── A comment inside a definition ────────────────────────────────────────── *)

(* A comment inside an item used to make the whole item a verbatim slice of
   the source, so none of it was formatted -- one comment on the last line
   of a definition exempted every line of it, and `check_fmt` could not see
   in. The emitters write the comment above the arm or statement it sits on
   and the rest is printed as usual. *)
let test_a_comment_inside_an_item () =
  fmt_eq "an arm below a comment is still formatted"
    {|let f xs = match xs with
  | [] -> 0
  -- why the next arm is what it is
  | [h :: t] ->    h+1|}
    {|let f xs =
  match xs with
  | [] -> 0
  -- why the next arm is what it is
  | [h :: t] -> h + 1|};
  fmt_eq "and a statement below one"
    {|let f () = (
  let x = 1;
  -- what the next line is for
  g   x;
  x+1
)|}
    {|let f () = (
  let x = 1;
  -- what the next line is for
  g x;
  x + 1
)|};
  (* Two in a row, and one against the first arm. *)
  fmt_eq "several comments keep their order and their arm"
    {|let f xs = match xs with
  -- about the empty case
  | [] -> 0
  -- one
  -- two
  | [h :: t] ->  h|}
    {|let f xs =
  match xs with
  -- about the empty case
  | [] -> 0
  -- one
  -- two
  | [h :: t] -> h|};
  assert_idempotent "an item with a comment in it is a fixed point"
    {|let f xs = match xs with
  | [] -> 0
  -- why
  | [h :: t] -> h + 1|};
  ok_after_format "and it still runs"
    {|let f xs = match xs with
  | [] -> 0
  -- why
  | [h :: t] -> h + 1
f [1, 2]|} "2"

(* The two other places a comment sits inside a definition: at the very
   start of the body, and between a `let ... in` and what reads it. Both
   left the whole definition unformatted until they were reached. *)
let test_a_comment_in_a_let_in_chain () =
  fmt_eq "at the start of a definition's body"
    {|let words s =
  -- why it is done this way
  let spaced = replace  s in
  filter   spaced|}
    {|let words s =
  -- why it is done this way
  let spaced = replace s;
  filter spaced|};
  fmt_eq "between a binding and what reads it"
    {|let f () =
  let out = compute_something_here () in
  -- why the next step is needed
  let () = cleanup () in
  out|}
    {|let f () =
  let out = compute_something_here ();
  -- why the next step is needed
  let () = cleanup ();
  out|};
  assert_idempotent "a let-in chain with a comment is a fixed point"
    {|let words s =
  -- why it is done this way
  let spaced = replace s; filter spaced|}

(* A bare date is a spelling of midnight UTC, and an offset form names a
   moment in a timezone. Both keep the text they were written with: the
   value normalises where its meaning is read, and expanding the source
   would delete a spelling the language offers. *)
let test_an_instant_keeps_its_spelling () =
  fmt_eq "a bare day stays short"
    {|let d = 2024-01-15|} {|let d = 2024-01-15|};
  fmt_eq "an offset stays as written"
    {|let d = 2024-01-15T09:00:00+05:30|} {|let d = 2024-01-15T09:00:00+05:30|};
  fmt_eq "and a full UTC instant stays full"
    {|let d = 2024-01-15T00:00:00Z|} {|let d = 2024-01-15T00:00:00Z|};
  assert_idempotent "a bare day is a fixed point" {|let d = 2024-01-15|}

(* A comment that follows code on its line is about that code. Lifting it
   onto a line of its own would point it at the line below instead, so the
   item stays exactly as written -- which is what every item with a comment
   in it used to do. *)
let test_a_trailing_comment_pins_its_item () =
  fmt_eq "a trailing comment leaves the item alone"
    {|let f xs = match xs with
  | [] -> 0     -- about the empty case
  | [h :: t] ->    h+1|}
    {|let f xs = match xs with
  | [] -> 0     -- about the empty case
  | [h :: t] ->    h+1|}

(* A `let ... in` arm body that does not fit on the arrow's line put its
   continuation at the arm's own indent, level with the `|` above it, where
   it read as the next arm rather than the rest of this one. *)
let test_a_wrapping_let_in_arm_body () =
  fmt_eq "the body takes a block instead of dangling"
    {|let f xs = match xs with
  | [] -> 0
  | [h :: t] ->
    let k = some_quite_long_helper_name h in
    another_long_function k (with_more_arguments h) (and_yet_another t) 12345|}
    {|let f xs =
  match xs with
  | [] -> 0
  | [h :: t] -> (
    let k = some_quite_long_helper_name h;
    another_long_function k (with_more_arguments h) (and_yet_another t) 12345
  )|}

let test_a_block_binding_round_trips () =
  fmt_eq "a binding and two statements"
    {|let f () = (let x = 1; IO.println "a"; x + 1)|}
    {|let f () = (let x = 1; IO.println "a"; x + 1)|};
  fmt_eq "two bindings"
    {|let f () = (let x = 1; let y = 2; IO.println "a"; x + y)|}
    {|let f () = (let x = 1; let y = 2; IO.println "a"; x + y)|};
  (* A binding and the one statement that reads it. Both spellings build the
     same node -- `in` and the block's `;` bind the name over the same body
     -- so both come back the same way, and brackets are not part of it. *)
  fmt_eq "a block whose last statement is the only one"
    {|let f () = (let x = 1; x + 2)|}
    {|let f () =
  let x = 1;
  x + 2|};
  fmt_eq "and the in form beside it"
    {|let f () = let x = 1 in x + 2|}
    {|let f () =
  let x = 1;
  x + 2|};
  (* The shape that sent this to the formatter: two bindings and an `if`,
     inside a lambda, inside a call. *)
  fmt_eq "a block in a lambda in a call"
    {|let plan paths = (List.fold_right (fn p acc -> (let name = basename p; let wanted = tidy name; if wanted == name then acc else (p, wanted) :: acc)) paths [])|}
    {|let plan paths = List.fold_right (fn p acc ->
  let name = basename p;
  let wanted = tidy name;
  if wanted == name then acc else (p, wanted) :: acc
) paths []|};
  (* A binding written with `in` inside a sequence is a statement like any
     other and stays where it is. *)
  fmt_eq "the in form inside a sequence"
    {|let f () = (let x = 1 in x + 1; 9)|}
    {|let f () = (let x = 1 in x + 1; 9)|};
  (* The other spelling in the same place. A `let ... in` chain lays its
     continuation out at the indent it is handed, and level with the `fn`
     the second binding read as the statement after the lambda. On the
     `->` line the keyword also sat right of every line below it, so
     the chain takes the line under the lambda instead. *)
  fmt_eq "a let chain in a lambda wraps under it"
    {|let plan paths = (List.fold_right (fn p acc -> let name = basename p in let wanted = tidy name in if wanted == name then acc else (p, wanted) :: acc) paths [])|}
    {|let plan paths = List.fold_right (fn p acc ->
  let name = basename p;
  let wanted = tidy name;
  if wanted == name then acc else (p, wanted) :: acc
) paths []|};
  (* A `let ... in` written on the `fn -> ` line puts its keyword right of the
     value and the `in` below it, and the parser reads a line left of the
     keyword as something new: this one came back as three statements with
     the `in` at the top level. Found by test/fuzz. *)
  assert_idempotent "a wrapping with inside a let-in on the arrow line"
    {|let s = (fn -> let t = with a as d -> gggggggggg (hhhhhhhhhh "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa") "yyyyyyyyyy" in "")|};
  assert_idempotent "a block is a fixed point"
    {|let f () = (let x = 1; let y = 2; IO.println "a"; x + y)|};
  assert_idempotent "and so is a block with no sequence in it"
    {|let f () = (let x = 1; x + 2)|};
  ok_after_format "and it still runs"
    {|let f () = (let x = 1; let y = 2; x + y)
f ()|}
    "3"

(* `in`, the block's `;`, and the newline that ends a right-hand side all
   bind the name over the same body, so they build one node and come back
   one way: the `;`. Brackets are not part of it -- they are the formatter's
   to put in and take out, and a style read off them disagrees with itself
   on the next pass.

   What decides the brackets is what the chain holds. Every statement a
   binding means the `;` is doing the only thing it does outside brackets --
   ending a right-hand side and handing the rest to a body -- so the chain
   stands bare. One statement that binds nothing is joined to what follows
   by neither a `;` nor a newline, so that chain keeps the brackets that
   make it a block. *)
let test_a_newline_binding_takes_the_block_spelling () =
  fmt_eq "a statement among the bindings keeps the brackets"
    "let f () = (\n  let x = 1\n  IO.println \"a\"\n  x + 1\n)"
    {|let f () = (let x = 1; IO.println "a"; x + 1)|};
  fmt_eq "bindings alone need none"
    "let f () = (\n  let x = 1\n  let y = 2\n  x + y\n)"
    {|let f () =
  let x = 1;
  let y = 2;
  x + y|};
  fmt_eq "one expression under it, written with no brackets"
    "let f () =\n  let x = 1\n  x + 2"
    {|let f () =
  let x = 1;
  x + 2|};
  fmt_eq "and the same written with them"
    "let f () = (\n  let x = 1\n  x + 2\n)"
    {|let f () =
  let x = 1;
  x + 2|};
  fmt_eq "a statement among them, written with no brackets"
    "let f () =\n  let x = 1\n  IO.println \"a\"\n  x + 2"
    {|let f () = (let x = 1; IO.println "a"; x + 2)|};
  assert_idempotent "the printed form is a fixed point"
    {|let f () = (let x = 1; IO.println "a"; x + 1)|};
  ok_after_format "and it still runs"
    "let f () = (\n  let x = 1\n  let y = 2\n  x + y\n)\nf ()"
    "3"

(* A `with` body that opens a bracket opens it on the `->` line, the way a
   binding's value does. It used to break after the `->` and put the bracket
   on a line of its own, where it says nothing: the items sit at the same
   column either way, so the line was spent on the `(` alone. The body is
   laid out from where it lands, not from the indent, or it measures against
   room it does not have and comes back on one long line. *)
let test_a_with_body_opens_its_bracket_on_the_arrow_line () =
  fmt_eq "a block body"
    {|let a! () = with FS.temp_dir "wand_fmt_check_" as dir -> (copy_into! dir; format_in! dir; report_it dir)|}
    {|let a! () =
  with FS.temp_dir "wand_fmt_check_" as dir -> (
    copy_into! dir;
    format_in! dir;
    report_it dir
  )|};
  fmt_eq "a list body"
    {|let b! () = with FS.temp_dir "wand_fmt_check_" as dir -> [the_name_of dir, the_kind_of dir, the_size_of dir]|}
    {|let b! () =
  with FS.temp_dir "wand_fmt_check_" as dir -> [
    the_name_of dir,
    the_kind_of dir,
    the_size_of dir
  ]|};
  (* A body with no bracket of its own still takes the line below. *)
  fmt_eq "a body that opens no bracket"
    {|let c! () = with FS.temp_dir "wand_fmt_check_" as dir -> report_the_whole_thing dir dir dir dir dir dir dir|}
    {|let c! () =
  with FS.temp_dir "wand_fmt_check_" as dir ->
  report_the_whole_thing dir dir dir dir dir dir dir|};
  assert_idempotent "the bracket on the arrow line is a fixed point"
    {|let a! () =
  with FS.temp_dir "wand_fmt_check_" as dir -> (
    copy_into! dir;
    format_in! dir;
    report_it dir
  )
|}

(* A binding among a block's statements takes the block's `;`, whatever
   joined it to its body. It used to keep an `in` written by hand, so one
   block held both spellings -- tools/check_docs.wand did.

   `emit_block` decides this, not the parser. The parser sees the brackets a
   binding was written in; the printer decides whether to keep them, so where
   a binding ends up is not where it was read. `(t; (let f = e in ()))` loses
   its brackets and joins the block. Two attempts to decide it from the parse
   were unstable, both found by test/fuzz. *)
let test_an_in_among_statements_takes_the_semicolon () =
  fmt_eq "a binding under a statement"
    {|let f () = (g (); let x = 1 in x + 1)|}
    {|let f () = (g (); let x = 1; x + 1)|};
  fmt_eq "brackets of its own do not keep it out of the block"
    {|let f () = (g (); (let x = 1 in x + 1))|}
    {|let f () = (g (); let x = 1; x + 1)|};
  (* One binding and the expression that reads it: a chain of bindings, so
     the brackets it was written with are not needed and do not come back. *)
  fmt_eq "a lone binding loses the brackets"
    {|let f () = (let x = 1 in x + 1)|}
    {|let f () =
  let x = 1;
  x + 1|};
  (* Grouping brackets around one binding stay one pair. The chain brings
     its own here -- an argument is not a statement position -- and the one
     inside it is the lambda's body, written on the arrow's line and so
     needing brackets of its own. *)
  fmt_eq "a parenthesised binding as an argument"
    {|let a = t.eq 3 (let f = fn () -> let x = 1 in x + 1 in f ())|}
    {|let a = t.eq 3 (let f = fn () -> (let x = 1; x + 1); f ())|};
  (* The `in` that narrows is still the exception, in either position. *)
  fmt_eq "an in before a semicolon narrows and stays"
    {|let f () = (let x = 1 in x + 1; 9)|}
    {|let f () = (let x = 1 in x + 1; 9)|};
  fmt_eq "and under a statement too"
    {|let f () = (g (); let x = 1 in x + 1; 9)|}
    {|let f () = (g (); let x = 1 in x + 1; 9)|}

(* A binding written this way used to take everything under it into its
   body, however far left that was: `parse_body` looped on "another
   expression starts here" and the rest of the file always did. Every
   definition below went inside the one above, and a `main! ()` that ends up
   inside a function nobody calls is a script that prints nothing and exits
   0 -- `wand f` then wrote that reading back over the source. A statement
   starting left of the binding's own `let` is not part of its body. *)
(* A field that already names its variable is punned, and the pun has to read
   back as the field it came from. Two spellings do not: one identifier alone
   inside the brackets is the payload under that name (`B(n)`), and on the
   expression side a pun standing in front of a named field is the record
   update `T(r, b = 3)`. Both used to be written, and `demos/07` stopped
   typechecking after a `wand f` that nobody ran again. *)
let test_a_pun_reads_back_as_the_field_it_came_from () =
  (* Patterns: punned beside anything else, written out when alone. *)
  fmt_eq "a lone pattern field keeps its name"
    {|let f (M(a = a)) = a|}
    {|let f (M(a = a)) = a|};
  fmt_eq "two of them pun"
    {|let f (M(a = a, b = b)) = a|}
    {|let f (M(a, b)) = a|};
  (* A pattern has no update form to collide with, so a pun may stand beside
     a named field here. *)
  fmt_eq "a pun beside a named field"
    {|let f (M(a = a, b = z)) = a|}
    {|let f (M(a, b = z)) = a|};
  (* Constructions: all of them, or none. *)
  fmt_eq "a lone construction field keeps its name"
    {|let f n = B(n = n)|}
    {|let f n = B(n = n)|};
  fmt_eq "two of them pun"
    {|let f a b = M(a = a, b = b)|}
    {|let f a b = M(a, b)|};
  fmt_eq "a pun beside a named field is written out"
    {|let f a = M(a = a, b = "z")|}
    {|let f a = M(a = a, b = "z")|};
  fmt_eq "and so is one after a named field"
    {|let f b = M(a = "z", b = b)|}
    {|let f b = M(a = "z", b = b)|};
  (* The update is the spelling being kept clear of, and it is untouched. *)
  fmt_eq "an update is not a pun"
    {|let f r = M(r, b = "z")|}
    {|let f r = M(r, b = "z")|}

(* A `;` after a `match` or `handle` arm lands hard against the arm, and a
   reader has to know it closed the statement above rather than belonging to
   the arm. The parse was never in doubt -- the arms are still owed when the
   `;` arrives -- so the bracket is for the reader, and it goes on the
   binding value. A statement takes none: it stands at the block's own
   indent, where the `;` closes a line that opened with `match`. *)
let test_an_arm_before_a_semicolon_is_bracketed () =
  fmt_eq "a match as a statement is left bare"
    "let f x = (\n  match x with\n  | true -> g ()\n  | false -> ();\n  h ()\n)"
    "let f x = (\n  match x with\n  | true -> g ()\n  | false -> ();\n  h ()\n)";
  fmt_eq "a match as a binding's value"
    "let f x = (\n  let y = match x with | true -> 1 | false -> 2;\n  g y\n)"
    "let f x =\n  let y =\n    match x with\n    | true -> 1\n    | false -> 2\n  in\n  g y";
  (* The arm may be under a lambda, or anything else that ends on one. *)
  fmt_eq "a lambda whose body ends on an arm"
    "let f x = (\n  let r = fn v -> match v with | Ok w -> w | Error _ -> 0;\n  g r\n)"
    "let f x =\n  let r =\n    fn v -> match v with\n      | Ok w -> w\n      | Error _ -> 0\n  in\n  g r";
  (* The last statement has no `;` after it and takes no bracket. *)
  fmt_eq "the last statement is left alone"
    "let f x = (\n  g ();\n  match x with\n  | true -> 1\n  | false -> 2\n)"
    "let f x = (\n  g ();\n  match x with\n  | true -> 1\n  | false -> 2\n)";
  assert_idempotent "the bare statement is a fixed point"
    "let f x = (\n  match x with\n  | true -> g ()\n  | false -> ();\n  h ()\n)"

let test_a_newline_binding_stops_at_the_next_definition () =
  fmt_eq "the definition below stays its own"
    "let f () =\n  let a = 1\n  a + 1\n\nlet g () = 2"
    "let f () =\n  let a = 1;\n  a + 1\n\nlet g () = 2";
  ok_after_format "and the file below the binding still runs"
    "let f () =\n  let a = 1\n  a + 1\n\nlet g () = 2\n\nf () + g ()"
    "4"

(* `$NAME` in a string is text, so the formatter has nothing to interpret:
   it comes back as written, and is not turned into an interpolation. An
   actual environment read is `%{$USER}`, and that round-trips as itself. *)
let test_env_var_interpolation () =
  assert_contains "text is left as text" (fmt "\"user=$USER\"") "user=$USER";
  Alcotest.(check bool) "and is not made an interpolation" false
    (contains (fmt "\"user=$USER\"") "%{$USER}");
  assert_contains "a real env read survives" (fmt "\"user=%{$USER}\"") "%{$USER}"

(* ── Suite ────────────────────────────────────────────────────────────────── *)


(* A wide application breaks rather than running past the margin. The common
   shape is a trailing lambda -- `test "..." (fn t -> ...)` -- which reads
   best with its body on the next line, where a person would have put it. *)
let test_wide_application_breaks () =
  let out = fmt "import Test\ntest \"a label long enough to push this line past the margin\" (fn t -> t.eq (f (g x)) [1, 2, 3])" in
  List.iter (fun l ->
    if String.length l > 92 then
      Alcotest.failf "line runs past the margin (%d):\n%s" (String.length l) l)
    (String.split_on_char '\n' out);
  assert_contains "the lambda opens on the first line" out "(fn t ->";
  (* And the meaning survives the break. *)
  ok_after_format "a broken application still runs"
    "let apply f x = f x\nlet add a b = a + b\napply (fn n -> add n 1) 41"
    "42"

(* ── Canonicalization ────────────────────────────────────────────────────── *)

let regression = Alcotest.(check string)

let test_manifest_canonicalized () =
  regression "labels in display order, binaries sorted"
    "uses {Env, FS.Write, Shell(git, rsync)}\n\nlet x = 1\nx\n"
    (fmt "uses {Shell(rsync, git), FS.Write, Env}\nlet x = 1\nx");
  (* The typechecker's suggested line is already what fmt emits. *)
  regression "a suggested manifest is a fixed point"
    "uses {FS.Write}\n\nlet x = 1\nx\n"
    (fmt "uses {FS.Write}\nlet x = 1\nx")

let test_manifest_wraps_past_the_budget () =
  let src =
    "uses {Shell(zz-very-long-binary-name-one, yy-very-long-binary-name-two, \
     xx-very-long-binary-name-three, ww-very-long-binary-name-four), FS.Read, \
     FS.Write, Env, IO, Proc}\nlet x = 1\nx"
  in
  let out = fmt src in
  Alcotest.(check bool) "one label per line" true
    (String.length out > 0 &&
     Lint.contains out "uses {\n  Env,\n  FS.Read,\n  FS.Write,\n  IO,\n  Proc,\n  Shell(\n");
  Alcotest.(check bool) "one binary per line" true
    (Lint.contains out "    ww-very-long-binary-name-four,\n");
  assert_idempotent "wrapped manifest" src;
  (match Runner.typecheck_source ~path:"wand_fmt_wrap.wand" out with
   | Ok _ -> ()
   | Error d -> Alcotest.failf "wrapped manifest does not parse: %s" (Diag.legacy d))

let test_leading_imports_sorted () =
  regression "plain imports alphabetized, let-imports after, in source order"
    "import Env\nimport FS\nimport String\n\n\
     let u = import CSV\nlet {test} = import Test\nlet x = 1\nx\n"
    (fmt "import String\nlet u = import CSV\nimport FS\n\
          let {test} = import Test\nimport Env\nlet x = 1\nx");
  (* Let-imports are ordinary bindings: two binding the same name rebind,
     and their order is program meaning. *)
  regression "rebinding order kept"
    "let {parse} = import CSV\nlet {parse} = import TOML\nparse \"x = 1\"\n"
    (fmt "let {parse} = import CSV\nlet {parse} = import TOML\nparse \"x = 1\"");
  (* Imports past the leading region stay where they are. *)
  regression "only the leading region"
    "import String\n\nlet x = 1\nimport FS\nx\n"
    (fmt "import String\nlet x = 1\nimport FS\nx")

let test_import_region_with_comment_left_alone () =
  regression "a comment pins the region"
    "import String\n-- FS does the writing\nimport FS\n\nlet x = 1\nx\n"
    (fmt "import String\n-- FS does the writing\nimport FS\nlet x = 1\nx")

let () =
  Alcotest.run "Formatter" [
    "idempotency", [
      Alcotest.test_case "snippets" `Quick test_idempotent_snippets;
      Alcotest.test_case "stdlib"   `Quick test_idempotent_stdlib;
      Alcotest.test_case "parses at any margin" `Slow test_output_parses_at_any_margin;
      Alcotest.test_case "wrapped if condition keeps its then" `Quick
        test_a_wrapped_if_condition_keeps_its_then;
      Alcotest.test_case "wrapped try scrutinee keeps its with" `Quick
        test_a_wrapped_try_scrutinee_keeps_its_with;
      Alcotest.test_case "wrapped try with is left alone" `Quick
        test_a_wrapped_try_with_is_left_alone;
      Alcotest.test_case "armless handle settles" `Quick
        test_an_armless_handle_settles;
      Alcotest.test_case "local clauses inside a call parse" `Quick
        test_local_clauses_inside_a_call_parse;
      Alcotest.test_case "an and group after in stays under its let" `Quick
        test_an_and_group_after_in_stays_under_its_let;
      Alcotest.test_case "a command literal is not a bracket" `Quick
        test_a_command_literal_is_not_a_bracket;
      Alcotest.test_case "a nested pipeline keeps its brackets" `Quick
        test_a_nested_pipeline_keeps_its_brackets;
      Alcotest.test_case "a bare field list wraps" `Quick
        test_a_bare_field_list_wraps;
      Alcotest.test_case "parameterless fn spacing" `Quick
        test_a_parameterless_fn_has_one_space;
      Alcotest.test_case "a string is not a comment" `Quick
        test_a_string_does_not_stand_in_for_a_comment;
      Alcotest.test_case "field on a number keeps brackets" `Quick
        test_a_field_on_a_number_keeps_its_brackets;
      Alcotest.test_case "a bracket is kept off a glob" `Quick
        test_a_bracket_is_kept_off_a_glob;
      Alcotest.test_case "nesting is not exponential" `Quick
        test_nesting_does_not_cost_exponentially;
      Alcotest.test_case "a command keeps its space" `Quick
        test_a_command_keeps_the_space_that_gives_it_meaning;
      Alcotest.test_case "a float reads back as itself" `Quick
        test_a_float_reads_back_as_itself;
      Alcotest.test_case "an item opening with an operator" `Quick
        test_an_item_opening_with_an_operator_is_bracketed;
    ];
    "canonicalization", [
      Alcotest.test_case "multi-line backticks open on the = line" `Quick
        test_a_multiline_backtick_string_opens_on_the_eq_line;
      Alcotest.test_case "escaped quotes prefer backticks" `Quick test_escaped_quotes_prefer_backticks;
      Alcotest.test_case "single-constructor shorthand" `Quick test_single_ctor_shorthand;
      Alcotest.test_case "maps canonicalize to braces" `Quick test_maps_canonicalize_to_braces;
      Alcotest.test_case "manifest order"    `Quick test_manifest_canonicalized;
      Alcotest.test_case "manifest wrapping" `Quick test_manifest_wraps_past_the_budget;
      Alcotest.test_case "import block"      `Quick test_leading_imports_sorted;
      Alcotest.test_case "comment pins it"   `Quick test_import_region_with_comment_left_alone;
    ];
    "behavior preserved", [
      Alcotest.test_case "behavior" `Quick test_behavior_preserved;
      Alcotest.test_case "float literal type" `Quick test_float_literal_type_preserved;
      Alcotest.test_case "constructor argument parens" `Quick test_constructor_argument_keeps_its_parens;
      Alcotest.test_case "one-armed if" `Quick test_one_armed_if;
      Alcotest.test_case "map keys needing quotes" `Quick test_map_keys_that_are_not_identifiers;
      Alcotest.test_case "width from the start column" `Quick test_width_is_measured_from_the_start_column;
      Alcotest.test_case "mid-line breaks step in" `Quick test_midline_breaks_step_in;
      Alcotest.test_case "manifest blank line" `Quick test_manifest_is_followed_by_a_blank_line;
      Alcotest.test_case "import block blank line" `Quick test_import_block_is_followed_by_a_blank_line;
      Alcotest.test_case "wrapped case body runs" `Quick test_a_wrapped_case_body_keeps_its_brackets;
      Alcotest.test_case "multiline paren closes alone" `Quick test_a_multiline_paren_closes_on_its_own_line;
      Alcotest.test_case "bracketed values open on the binding line" `Quick
        test_bracketed_values_open_on_the_binding_line;
      Alcotest.test_case "sequence item wrap column" `Quick test_sequence_items_wrap_to_their_own_column;
      Alcotest.test_case "wrapped application brackets" `Quick test_a_wrapped_application_keeps_its_brackets;
      Alcotest.test_case "separator not written twice" `Quick
        test_a_separator_is_not_written_twice;
      Alcotest.test_case "separator not written onto a trailing comment" `Quick
        test_a_separator_is_not_written_onto_a_trailing_comment;
      Alcotest.test_case "qualified head hides no constructor" `Quick
        test_a_qualified_head_does_not_hide_a_constructor;
      Alcotest.test_case "a version before a field" `Quick
        test_a_version_keeps_its_brackets_before_a_field;
    ];
    "formerly verbatim", [
      Alcotest.test_case "command text"     `Quick test_command_text_is_not_quoted;
      Alcotest.test_case "try as operand"   `Quick test_try_is_parenthesised_as_an_operand;
      Alcotest.test_case "contract indent"  `Quick test_contract_clauses_keep_their_indent;
      Alcotest.test_case "a lambda's contract" `Quick
        test_a_lambda_keeps_its_contract_under_the_arrow;
      Alcotest.test_case "handle and regex" `Quick test_handle_and_regex_round_trip;
      Alcotest.test_case "env interpolation" `Quick test_env_var_interpolation;
      Alcotest.test_case "string openers" `Quick test_string_openers_come_back_escaped;
      Alcotest.test_case "a block binding" `Quick test_a_block_binding_round_trips;
      Alcotest.test_case "a newline binding" `Quick
        test_a_newline_binding_takes_the_block_spelling;
      Alcotest.test_case "a newline binding ends" `Quick
        test_a_newline_binding_stops_at_the_next_definition;
      Alcotest.test_case "an in among statements" `Quick
        test_an_in_among_statements_takes_the_semicolon;
      Alcotest.test_case "a with body opens its bracket on the arrow line" `Quick
        test_a_with_body_opens_its_bracket_on_the_arrow_line;
      Alcotest.test_case "a pun reads back" `Quick
        test_a_pun_reads_back_as_the_field_it_came_from;
      Alcotest.test_case "an arm before a semicolon" `Quick
        test_an_arm_before_a_semicolon_is_bracketed;
      Alcotest.test_case "a comment inside an item" `Quick test_a_comment_inside_an_item;
      Alcotest.test_case "a trailing comment pins its item" `Quick
        test_a_trailing_comment_pins_its_item;
      Alcotest.test_case "a wrapping let-in arm body" `Quick
        test_a_wrapping_let_in_arm_body;
      Alcotest.test_case "a comment in a let-in chain" `Quick
        test_a_comment_in_a_let_in_chain;
      Alcotest.test_case "an instant keeps its spelling" `Quick
        test_an_instant_keeps_its_spelling;
      Alcotest.test_case "let clause layout" `Quick test_let_clause_alignment;
      Alcotest.test_case "raw strings" `Quick test_raw_strings_round_trip;
      Alcotest.test_case "raw layout" `Quick test_raw_multiline_keeps_its_shape;
      Alcotest.test_case "multiline literal opens no gap" `Quick
        test_a_multiline_literal_does_not_open_a_gap;
      Alcotest.test_case "wide application"  `Quick test_wide_application_breaks;
    ];
    "comments", [
      Alcotest.test_case "preserved"  `Quick test_comments_preserved;
      Alcotest.test_case "interior position" `Quick test_interior_comment_position;
      Alcotest.test_case "blank lines" `Quick test_blank_lines;
      Alcotest.test_case "shebang survives" `Quick test_shebang_survives;
      Alcotest.test_case "shebang, no manifest" `Quick test_shebang_without_a_manifest;
      Alcotest.test_case "shebang settles" `Quick test_shebang_settles;
      Alcotest.test_case "no shebang invented" `Quick test_no_shebang_gains_none;
      Alcotest.test_case "trailing stays on line" `Quick test_trailing_comment_stays_on_line;
      Alcotest.test_case "doc run kept together" `Quick test_doc_run_kept_together;
      Alcotest.test_case "no blank after doc" `Quick test_no_blank_between_doc_and_binding;
      Alcotest.test_case "wide type wraps" `Quick test_wide_type_definition_wraps;
      Alcotest.test_case "named field arrow" `Quick test_named_field_arrow_loses_brackets;
      Alcotest.test_case "interface settles" `Quick test_interface_and_implement_settle;
      Alcotest.test_case "pipeline stages align" `Quick test_a_pipeline_aligns_its_stages;
      Alcotest.test_case "a wide interface wraps" `Quick test_a_wide_interface_wraps;
    ];
  ]
