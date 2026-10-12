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

let () =
  Alcotest.run "text_primitives" [
    "against references", [
      Alcotest.test_case "words" `Quick test_words;
      Alcotest.test_case "split" `Quick test_split;
      Alcotest.test_case "lines" `Quick test_lines;
    ];
  ]
