(* Edits applied to a known-good `.wand` source.

   The fuzzer is mutation-based on purpose. `stdlib/`, `test/wand/`,
   `examples/`, `demos/` and `tools/` are already valid wand -- every one
   of them a formatter fixed point -- so a handful of edits lands near the
   language rather than in random bytes, and a generator that builds a
   well-typed program from a type does not have to exist first. One would
   answer questions these cannot -- `docs/llm-authoring.md` names the one it
   wants -- and nothing here waits on it.

   Every mutation is named, and the names travel with a finding: a
   reproducer says how it was reached, not just what it was. *)

type t = {
  name  : string;
  apply : Random.State.t -> donor:string -> string -> string;
}

(* Fragments worth inserting: the syntax wand gives its own meaning to, the
   domain literals, and the bytes that are not text at all. A random
   printable byte finds far less than a stray `%{` does. *)
let fragments = [|
  "("; ")"; "{"; "}"; "["; "]"; "|"; "->"; "<-"; "::"; ","; "."; ":"; ";";
  "!"; "?"; "%{"; "\""; "'"; "\\"; "--"; "++"; "&&"; "||"; "=="; "=";
  "uses "; "let "; "match "; " with "; "fn "; "Ok "; "Error "; "type ";
  "import "; "0"; "1"; "-1"; "30s"; "100MB"; "*.wand"; "/var/log/app";
  "https://x"; "\n"; "\t"; " "; "\r"; "\000"; "\xc3\xa9"; "\xff";
|]

(* A ceiling on what any one mutation may produce. `repeat` can multiply a
   file, and an input nobody can read is an input nobody can shrink. *)
let max_len = 1 lsl 20

let cap s = if String.length s > max_len then String.sub s 0 max_len else s

let pick st arr = arr.(Random.State.int st (Array.length arr))

(* An offset and a length inside `s`, both valid, length at least 1. *)
let span st ?(max = 64) s =
  let len = String.length s in
  if len = 0 then (0, 0)
  else
    let off = Random.State.int st len in
    let n = 1 + Random.State.int st (min max (len - off)) in
    (off, n)

let splice_at s off n insert =
  cap (String.sub s 0 off ^ insert ^ String.sub s (off + n) (String.length s - off - n))

let lines s = String.split_on_char '\n' s
let unlines = String.concat "\n"

let flip_byte = {
  name = "flip-byte";
  apply = (fun st ~donor:_ s ->
    if s = "" then s
    else
      let i = Random.State.int st (String.length s) in
      let b = Bytes.of_string s in
      Bytes.set b i (Char.chr (Random.State.int st 256));
      Bytes.to_string b);
}

let delete_span = {
  name = "delete-span";
  apply = (fun st ~donor:_ s ->
    let (off, n) = span st s in
    if n = 0 then s else splice_at s off n "");
}

let insert_fragment = {
  name = "insert-fragment";
  apply = (fun st ~donor:_ s ->
    let off = if s = "" then 0 else Random.State.int st (String.length s + 1) in
    splice_at s off 0 (pick st fragments));
}

let overwrite_fragment = {
  name = "overwrite-fragment";
  apply = (fun st ~donor:_ s ->
    let (off, n) = span st ~max:8 s in
    if n = 0 then s else splice_at s off n (pick st fragments));
}

let duplicate_span = {
  name = "duplicate-span";
  apply = (fun st ~donor:_ s ->
    let (off, n) = span st s in
    if n = 0 then s
    else
      let piece = String.sub s off n in
      let at = Random.State.int st (String.length s + 1) in
      splice_at s at 0 piece);
}

let truncate = {
  name = "truncate";
  apply = (fun st ~donor:_ s ->
    if s = "" then s else String.sub s 0 (Random.State.int st (String.length s)));
}

let delete_line = {
  name = "delete-line";
  apply = (fun st ~donor:_ s ->
    let ls = Array.of_list (lines s) in
    if Array.length ls = 0 then s
    else
      let i = Random.State.int st (Array.length ls) in
      unlines (List.filteri (fun j _ -> j <> i) (Array.to_list ls)));
}

let duplicate_line = {
  name = "duplicate-line";
  apply = (fun st ~donor:_ s ->
    let ls = Array.of_list (lines s) in
    if Array.length ls = 0 then s
    else
      let i = Random.State.int st (Array.length ls) in
      cap (unlines (List.concat (List.mapi
        (fun j l -> if j = i then [l; l] else [l]) (Array.to_list ls)))));
}

let swap_lines = {
  name = "swap-lines";
  apply = (fun st ~donor:_ s ->
    let ls = Array.of_list (lines s) in
    let n = Array.length ls in
    if n < 2 then s
    else
      let i = Random.State.int st n and j = Random.State.int st n in
      let tmp = ls.(i) in
      ls.(i) <- ls.(j); ls.(j) <- tmp;
      unlines (Array.to_list ls));
}

(* A run of lines lifted out of another corpus file. This is what reaches
   the combinations a single-file edit cannot: a manifest from one file
   above a body from another, an import that does not match what follows. *)
let splice_donor = {
  name = "splice-donor";
  apply = (fun st ~donor s ->
    let d = Array.of_list (lines donor) in
    if Array.length d = 0 then s
    else
      let start = Random.State.int st (Array.length d) in
      let n = 1 + Random.State.int st (min 12 (Array.length d - start)) in
      let piece = unlines (Array.to_list (Array.sub d start n)) in
      let ls = lines s in
      let at = Random.State.int st (List.length ls + 1) in
      cap (unlines (List.concat (List.mapi
        (fun j l -> if j = at then [piece; l] else [l]) ls)
        @ (if at >= List.length ls then [piece] else []))));
}

(* Depth, which is the one thing a single edit never reaches. Bounded so a
   finding stays small enough to read. *)
let repeat = {
  name = "repeat";
  apply = (fun st ~donor:_ s ->
    let (off, n) = span st ~max:3 s in
    if n = 0 then s
    else
      let piece = String.sub s off n in
      let times = 2 + Random.State.int st 200 in
      let b = Buffer.create (n * times) in
      for _ = 1 to times do Buffer.add_string b piece done;
      splice_at s off n (Buffer.contents b));
}

let all = [|
  flip_byte; delete_span; insert_fragment; overwrite_fragment;
  duplicate_span; truncate; delete_line; duplicate_line; swap_lines;
  splice_donor; repeat;
|]

(* Between one and `max_edits` mutations, in order, returning the result and
   the names of what was applied. *)
let mutate ?(max_edits = 6) st ~donor src =
  let k = 1 + Random.State.int st max_edits in
  let rec go i acc s =
    if i = k then (s, List.rev acc)
    else
      let m = pick st all in
      go (i + 1) (m.name :: acc) (m.apply st ~donor s)
  in
  go 0 [] src
