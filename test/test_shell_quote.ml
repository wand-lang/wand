(* The text a `%{...}` value becomes in a command, and what a shell reads
   from that text. *)

open Wand

let wand_binary =
  let dir = Filename.dirname (Filename.dirname Sys.executable_name) in
  Filename.concat (Filename.concat dir "bin") "wand.exe"

(* The locales whose two-byte characters can end in a backslash: a lead byte
   such as 0xA4 and a `\` after it are one character to the shell. *)
let multibyte_locales =
  ["zh_TW.Big5"; "zh_CN.GBK"; "zh_CN.GB18030"; "zh_HK.Big5HKSCS"; "ja_JP.SJIS"]

let installed_locales =
  lazy (
    try
      let ic = Unix.open_process_in "locale -a 2>/dev/null" in
      let all = In_channel.input_all ic in
      ignore (Unix.close_process_in ic);
      String.split_on_char '\n' all
    with _ -> [])

let installed name = List.mem name (Lazy.force installed_locales)

(* Runs `prog` with `args` under `locale` and gives its stdout. *)
let run_in_locale locale prog args =
  let env = Array.append [| "LC_ALL=" ^ locale |] (Unix.environment ()) in
  let (ic, oc, ec) = Unix.open_process_args_full prog (Array.append [| prog |] args) env in
  close_out oc;
  let out = In_channel.input_all ic in
  ignore (In_channel.input_all ec);
  ignore (Unix.close_process_full (ic, oc, ec));
  out

let sh_in_locale locale text = run_in_locale locale "/bin/sh" [| "-c"; text |]

(* ── #168: a value inside the author's double quotes ──────────────────── *)

(* The value goes in as one single-quoted span between the author's two
   double-quoted ones. No backslash is added, so no lead byte can take one
   as its second byte. *)
let test_double_quote_text () =
  let q = Evaluator.quote_within '"' in
  Alcotest.(check string) "plain" "\"'a b'\"" (q "a b");
  Alcotest.(check string) "the four the shell reads in double quotes"
    "\"'\"\\$`'\"" (q "\"\\$`");
  Alcotest.(check string) "a single quote" "\"'it'\\''s'\"" (q "it's");
  Alcotest.(check string) "empty" "\"''\"" (q "");
  Alcotest.(check bool) "no backslash after a lead byte" false
    (let s = q "\xa4\\\"" in
     let rec go i =
       i + 1 < String.length s
       && ((s.[i] = '\xa4' && s.[i + 1] = '\\' && i + 2 < String.length s
            && s.[i + 2] = '\\') || go (i + 1)) in
     go 0)

let test_single_quote_text () =
  let q = Evaluator.quote_within '\'' in
  Alcotest.(check string) "plain" "a b" (q "a b");
  Alcotest.(check string) "a single quote" "it'\\''s" (q "it's");
  Alcotest.(check string) "double quote and backslash are text" "\"\\$" (q "\"\\$")

let hard_values = [
  "\xa4\\\"; echo PWNED #"; "\xa4\\"; "\xa4\\'; echo PWNED #";
  "\xa4\\\"$(echo PWNED)\""; "a\"b"; "$(echo PWNED)"; "`echo PWNED`"; "$HOME";
  "it's"; ""; "\\"; "x\ny";
]

(* What the shell makes of the produced text, in each multibyte locale that
   is installed. A locale that is not installed is skipped. *)
let test_double_quote_in_locales () =
  let checked = ref 0 in
  List.iter (fun locale ->
    if installed locale then begin
      incr checked;
      List.iter (fun v ->
        let dq = "printf '[%s]' \"pre " ^ Evaluator.quote_within '"' v ^ " post\"" in
        Alcotest.(check string) (locale ^ " in double quotes: " ^ String.escaped v)
          ("[pre " ^ v ^ " post]") (sh_in_locale locale dq);
        let sq = "printf '[%s]' 'pre " ^ Evaluator.quote_within '\'' v ^ " post'" in
        Alcotest.(check string) (locale ^ " in single quotes: " ^ String.escaped v)
          ("[pre " ^ v ^ " post]") (sh_in_locale locale sq);
        let nested =
          "printf '[%s]' \"<$(printf '%s' \"" ^ Evaluator.quote_within '"' v ^ "\")>\"" in
        Alcotest.(check string) (locale ^ " in $(...) in double quotes: " ^ String.escaped v)
          ("[<" ^ v ^ ">]")
          (* `$(...)` drops the newlines at the end of what it captures. *)
          (sh_in_locale locale nested))
        (List.filter (fun v -> v = "" || v.[String.length v - 1] <> '\n') hard_values)
    end) multibyte_locales;
  if !checked = 0 then
    Printf.printf "no multibyte locale is installed: skipped\n"

(* The whole path, from a script to the spawn, under Big5. *)
let test_script_in_big5 () =
  if not (installed "zh_TW.Big5") then print_endline "zh_TW.Big5 is not installed: skipped"
  else begin
    let dir = Filename.temp_dir "wand_quote" "" in
    let marker = Filename.concat dir "PWNED" in
    let script = Filename.concat dir "s.wand" in
    Out_channel.with_open_bin script (fun oc ->
      Out_channel.output_string oc (Printf.sprintf
        "uses {IO, Shell}\nimport IO\nlet v = \"\\xa4\\\\\\\"; touch %s #\"\nIO.print $(printf \"[%%s]\" \"%%{v}\")\n"
        marker));
    let out = run_in_locale "zh_TW.Big5" wand_binary [| script |] in
    Alcotest.(check bool) "the value ran nothing" false (Sys.file_exists marker);
    Alcotest.(check string) "one argument" (Printf.sprintf "[\xa4\\\"; touch %s #]" marker) out
  end

let () =
  Alcotest.run "Shell quote" [
    "inside the author's quotes (#168)", [
      Alcotest.test_case "double-quote text" `Quick test_double_quote_text;
      Alcotest.test_case "single-quote text" `Quick test_single_quote_text;
      Alcotest.test_case "multibyte locales" `Quick test_double_quote_in_locales;
      Alcotest.test_case "a script under Big5" `Quick test_script_in_big5;
    ];
  ]
