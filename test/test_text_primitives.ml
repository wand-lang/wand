(* The text primitives against plain reference versions of themselves.

   `String.words`, `String.split` and `String.lines` are
   written for speed in the evaluator. The references here are written to be
   read: each is the rule the primitive documents, and nothing else. Random
   text from a small alphabet of separators, digits and multi-byte
   characters reaches the edges -- empty fields, runs, a separator first or
   last -- far more often than real text does. *)

open Wand

let strings_of = function
  | Evaluator.VString s -> s
  | _ -> Alcotest.fail "expected a String"

let words_ref s =
  let is_space c = c = ' ' || c = '\t' || c = '\n' || c = '\r' in
  let b = Buffer.create 16 and acc = ref [] in
  let flush () =
    if Buffer.length b > 0 then (acc := Buffer.contents b :: !acc; Buffer.clear b)
  in
  String.iter (fun c -> if is_space c then flush () else Buffer.add_char b c) s;
  flush ();
  List.rev !acc

(* Left to right, the match on the left winning. *)
let split_ref delim s =
  let dl = String.length delim and sl = String.length s in
  if dl = 0 then List.init sl (fun i -> String.make 1 s.[i])
  else begin
    let acc = ref [] and start = ref 0 and i = ref 0 in
    while !i <= sl - dl do
      if String.sub s !i dl = delim then begin
        acc := String.sub s !start (!i - !start) :: !acc;
        i := !i + dl;
        start := !i
      end else incr i
    done;
    List.rev (String.sub s !start (sl - !start) :: !acc)
  end

(* A newline ends a line; what follows the last one is a line only when
   there is something there. *)
let lines_ref s =
  match split_ref "\n" s with
  | parts ->
    (match List.rev parts with
     | "" :: rest -> List.rev rest
     | _ -> parts)

let alphabet = [| "a"; "b"; " "; " "; "\t"; "\n"; "\r"; ","; "-"; "0"; "7"; "9";
                  "\xc3\xa9"; "\x0b"; "\x0c"; "aa"; "" |]

let random_text st =
  let n = Random.State.int st 12 in
  String.concat "" (List.init n (fun _ ->
    alphabet.(Random.State.int st (Array.length alphabet))))

let rounds = 20000

let test_words () =
  let st = Random.State.make [| 1 |] in
  for _ = 1 to rounds do
    let s = random_text st in
    Alcotest.(check (list string)) (Printf.sprintf "words %S" s)
      (words_ref s) (List.map strings_of (Evaluator.str_words_impl s))
  done

(* Long words and the bytes near 32. The scan steps over eight bytes at a
   time when none of them is 32 or less, so a word must be longer than
   eight bytes to reach that path, and a byte below 32 that is not a
   separator, or a byte of 33, must stop it without an error. *)
let long_alphabet = [| "a"; " "; "\t"; "\n"; "\r"; "abcdefg"; "abcdefgh";
                       "abcdefghijklmnopq"; "\x00"; "\x01"; "\x1f"; "\x0b";
                       "\x21"; "\x7f"; "\x80"; "\xff"; "\xc3\xa9";
                       String.make 70 'x' |]

let test_words_long () =
  let st = Random.State.make [| 6 |] in
  for _ = 1 to rounds do
    let n = Random.State.int st 30 in
    let s = String.concat "" (List.init n (fun _ ->
      long_alphabet.(Random.State.int st (Array.length long_alphabet)))) in
    Alcotest.(check (list string)) (Printf.sprintf "words %S" s)
      (words_ref s) (List.map strings_of (Evaluator.str_words_impl s))
  done

(* Every range of every length up to 80, around the 64 bytes where the copy
   goes back to `String.sub`, and ranges outside the text, which must raise
   as `String.sub` does. *)
let test_short_sub () =
  let s = String.init 80 (fun i -> Char.chr (i * 37 land 255)) in
  for start = -2 to 82 do
    for len = -2 to 82 do
      let expected = try Some (String.sub s start len) with Invalid_argument _ -> None in
      let got = try Some (Evaluator.short_sub s start len) with Invalid_argument _ -> None in
      Alcotest.(check (option string)) (Printf.sprintf "sub %d %d" start len) expected got
    done
  done

let test_split () =
  let st = Random.State.make [| 2 |] in
  let delims = [| ","; " "; "\n"; "aa"; "a,"; ""; "-0" |] in
  for _ = 1 to rounds do
    let s = random_text st in
    let d = delims.(Random.State.int st (Array.length delims)) in
    Alcotest.(check (list string)) (Printf.sprintf "split %S %S" d s)
      (split_ref d s) (List.map strings_of (Evaluator.str_split_impl d s))
  done

let test_lines () =
  let st = Random.State.make [| 3 |] in
  for _ = 1 to rounds do
    let s = random_text st in
    Alcotest.(check (list string)) (Printf.sprintf "lines %S" s)
      (lines_ref s) (List.map strings_of (Evaluator.str_lines_impl s))
  done

(* `Int.of_string` as it was written before it became one pass: trim, cut
   off a minus sign, ask that the rest is decimal digits, and parse. *)
let decimal_int_ref s =
  let t = String.trim s in
  let digits = if String.starts_with ~prefix:"-" t
    then String.sub t 1 (String.length t - 1) else t in
  let decimal = digits <> ""
    && String.for_all (fun c -> c >= '0' && c <= '9') digits in
  if decimal then int_of_string_opt t else None

let int_alphabet = [| "0"; "1"; "4"; "9"; "-"; "+"; " "; "\t"; "\n"; "\r"; "\x0c";
                      "\x0b"; "_"; "x"; "a"; "461168601842738790";
                      "4611686018427387903"; "4611686018427387904";
                      "-4611686018427387904"; "-4611686018427387905" |]

let test_decimal_int () =
  let st = Random.State.make [| 4 |] in
  for _ = 1 to rounds do
    let n = Random.State.int st 6 in
    let s = String.concat "" (List.init n (fun _ ->
      int_alphabet.(Random.State.int st (Array.length int_alphabet)))) in
    Alcotest.(check (option int)) (Printf.sprintf "of_string %S" s)
      (decimal_int_ref s) (Evaluator.str_decimal_int s)
  done

let test_prefix () =
  let st = Random.State.make [| 5 |] in
  for _ = 1 to rounds do
    let prefix = random_text st and s = random_text st in
    let s = if Random.State.bool st then prefix ^ s else s in
    Alcotest.(check bool) (Printf.sprintf "starts_with %S %S" prefix s)
      (String.starts_with ~prefix s) (Evaluator.str_has_prefix prefix s)
  done

let () =
  Alcotest.run "text_primitives" [
    "against references", [
      Alcotest.test_case "words" `Quick test_words;
      Alcotest.test_case "words, long" `Quick test_words_long;
      Alcotest.test_case "short_sub" `Quick test_short_sub;
      Alcotest.test_case "split" `Quick test_split;
      Alcotest.test_case "lines" `Quick test_lines;
      Alcotest.test_case "Int.of_string" `Quick test_decimal_int;
      Alcotest.test_case "starts_with?" `Quick test_prefix;
    ];
  ]
