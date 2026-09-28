(* Writing a YAML document from the tree a JSON value holds.

   YAML can spell one value many ways. This module picks one, so that the
   same value always gives the same text:

   - Block style: a mapping is `key: value` lines, a sequence is `- item`
     lines, each level two spaces in. A sequence under a key is indented
     under it. An empty mapping is `{}` and an empty sequence is `[]`.
   - A string is written without quotes only when no YAML reader can take it
     for anything else. Everything else is in double quotes, with JSON's
     escapes, which YAML's double-quoted style reads the same way.

   "No reader" includes YAML 1.1 readers, not only the 1.2 core schema that
   `YAML.parse` uses. kubectl reads with a 1.1 reader, which takes `on`,
   `yes`, `no`, `y` and `off` as booleans, so those are quoted too. *)

(* Words that some YAML reader takes as a boolean, a null or a number. *)
let special_words = [
  "true"; "false"; "yes"; "no"; "on"; "off"; "y"; "n";
  "null"; "~"; ".inf"; "-.inf"; "+.inf"; ".nan";
]

let is_word_char c =
  (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9')
  || String.contains "_./:@+=,-" c

(* A string that every YAML reader reads back as this string, written as it
   is. It starts with a letter, `_` or `/`, so it is not a number, not an
   indicator (`-`, `?`, `:`, `[`, `{`, `&`, `*`, `!`, `|`, `>`, `%`, `@`,
   `#`, a quote or a backquote) and not `.inf`. It holds only word characters
   and single spaces, with no `: ` in it, and `#` is not a word character, so
   it has no ` #`. It does not end with `:` or a space. *)
let plain s =
  let n = String.length s in
  n > 0
  && (let c = s.[0] in
      (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_' || c = '/')
  && s.[n - 1] <> ':' && s.[n - 1] <> ' '
  && (let ok = ref true in
      String.iteri (fun i c ->
        if c = ' ' then (if i + 1 < n && s.[i + 1] = ' ' then ok := false)
        (* `: ` starts a mapping value, so `a: b` is not one string. *)
        else if c = ':' && i + 1 < n && s.[i + 1] = ' ' then ok := false
        else if not (is_word_char c) then ok := false) s;
      !ok)
  && not (List.mem (String.lowercase_ascii s) special_words)

let quote s = if plain s then s else Yojson.Basic.to_string (`String s)

let scalar (j : Yojson.Basic.t) =
  match j with
  | `Null -> "null"
  | `Bool b -> if b then "true" else "false"
  | `Int i -> string_of_int i
  | `Float _ -> Yojson.Basic.to_string j
  | `String s -> quote s
  | `Assoc [] -> "{}"
  | `List [] -> "[]"
  | `Assoc _ | `List _ -> assert false

let indent = List.map (fun l -> "  " ^ l)

(* The lines of a mapping or a sequence, with no indent of their own. *)
let rec mapping kvs =
  List.concat_map (fun (k, v) ->
    let key = quote k in
    match v with
    | `Assoc (_ :: _ as kvs') -> (key ^ ":") :: indent (mapping kvs')
    | `List (_ :: _ as items) -> (key ^ ":") :: indent (sequence items)
    | _ -> [key ^ ": " ^ scalar v]) kvs

and sequence items =
  List.concat_map (fun v ->
    (* The first line of a nested block goes on the dash's line, and the
       rest under it, two further in. *)
    let item lines =
      match lines with
      | first :: rest -> ("- " ^ first) :: indent rest
      | [] -> []
    in
    match v with
    | `Assoc (_ :: _ as kvs) -> item (mapping kvs)
    | `List (_ :: _ as items') -> item (sequence items')
    | _ -> ["- " ^ scalar v]) items

(* One document, ending with a newline. *)
let document (j : Yojson.Basic.t) =
  let lines =
    match j with
    | `Assoc (_ :: _ as kvs) -> mapping kvs
    | `List (_ :: _ as items) -> sequence items
    | _ -> [scalar j]
  in
  String.concat "\n" lines ^ "\n"

(* Several documents in one stream, with `---` between them. *)
let documents js = String.concat "---\n" (List.map document js)
