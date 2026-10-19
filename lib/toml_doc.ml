(* A TOML document with its keys in the order the document wrote them.

   The `toml` library parses into an OCaml `Map`, so a table comes back with
   its keys sorted, and `TOML.stringify` wrote them sorted. Every other
   document wand reads keeps its order -- JSON, YAML, and `Map` itself, whose
   doc says "a map keeps its entries in the order they were first added" --
   so a TOML table of groups came back alphabetical where the same groups in
   JSON did not (#192).

   The library still does the parsing: it is what says whether the text is
   TOML. What it throws away is the order, and that is read back here from
   the text it was given, by a scan that knows only where keys are. Keys
   then come out in the order they were first written, and a key the scan
   did not see (there should be none) comes after them, sorted, which is
   what the library gave before. *)

type t =
  | Bool of bool
  | Int of int
  | Float of float
  | String of string
  | Date of float
  | Table of (string * t) list
  | Array of t list

(* The library's own rendering of a scalar: its string escapes, its float
   format and its date format, which `TOML.stringify` keeps. *)
let library_scalar v = Toml.Printer.string_of_value v

let date_text d = library_scalar (Toml.Types.TDate d)

(* ── Where the keys are ────────────────────────────────────────────────── *)

(* For each table, by its path from the top, its keys in the order they first
   appear. An element of an array of tables, or of an array of inline tables,
   shares the array's path, so their keys are one list. *)
type order = (string list, string list ref) Hashtbl.t

let record (order : order) path key =
  let keys =
    match Hashtbl.find_opt order path with
    | Some keys -> keys
    | None -> let keys = ref [] in Hashtbl.replace order path keys; keys
  in
  if not (List.mem key !keys) then keys := !keys @ [key]

exception Stop

(* A scan over text the library has already accepted, so it can be simple
   about everything but the places a key can stand: after a newline, after a
   `[` that opens a header, after a `{` or a `,` inside an inline table. A
   string, a comment and a scalar are skipped whole. *)
let scan (src : string) : order =
  let order : order = Hashtbl.create 16 in
  let n = String.length src in
  let pos = ref 0 in
  let peek k = if !pos + k < n then Some src.[!pos + k] else None in
  let starts s =
    let l = String.length s in
    !pos + l <= n && String.sub src !pos l = s
  in
  let advance k = pos := !pos + k in
  let skip_comment () =
    while !pos < n && src.[!pos] <> '\n' do advance 1 done
  in
  (* Spaces and tabs, and newlines and comments too when `lines` is set. *)
  let rec skip_space ~lines =
    match peek 0 with
    | Some (' ' | '\t' | '\r') -> advance 1; skip_space ~lines
    | Some '\n' when lines -> advance 1; skip_space ~lines
    | Some '#' when lines -> skip_comment (); skip_space ~lines
    | _ -> ()
  in
  (* A string of any of the four kinds, with its text decoded as far as a key
     needs: the escapes a key is likely to hold. A key with another escape
     only fails to match, and goes to the end. *)
  let read_string () =
    let buf = Buffer.create 16 in
    let quote = src.[!pos] in
    let triple = starts (String.make 3 quote) in
    advance (if triple then 3 else 1);
    let close () = if triple then starts (String.make 3 quote) else peek 0 = Some quote in
    while !pos < n && not (close ()) do
      (match src.[!pos] with
       | '\\' when quote = '"' && !pos + 1 < n ->
         (match src.[!pos + 1] with
          | 'n' -> Buffer.add_char buf '\n'
          | 't' -> Buffer.add_char buf '\t'
          | '"' -> Buffer.add_char buf '"'
          | '\\' -> Buffer.add_char buf '\\'
          | c -> Buffer.add_char buf '\\'; Buffer.add_char buf c);
         advance 1
       | c -> Buffer.add_char buf c);
      advance 1
    done;
    if !pos >= n then raise Stop;
    advance (if triple then 3 else 1);
    (* A run of quotes just inside a closing triple quote belongs to the
       string. *)
    while triple && peek 0 = Some quote do advance 1 done;
    Buffer.contents buf
  in
  let bare c =
    match c with
    | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '-' -> true
    | _ -> false
  in
  let read_simple_key () =
    match peek 0 with
    | Some ('"' | '\'') -> read_string ()
    | Some c when bare c ->
      let start = !pos in
      while !pos < n && bare src.[!pos] do advance 1 done;
      String.sub src start (!pos - start)
    | _ -> raise Stop
  in
  (* `a.b."c"`, as the list of its parts. *)
  let read_key () =
    let rec more acc =
      skip_space ~lines:false;
      let k = read_simple_key () in
      skip_space ~lines:false;
      if peek 0 = Some '.' then (advance 1; more (k :: acc)) else List.rev (k :: acc)
    in
    more []
  in
  (* Each part of a dotted key is a key of the table the part before it
     names. *)
  let record_path base parts =
    let rec go path = function
      | [] -> path
      | k :: rest -> record order path k; go (path @ [k]) rest
    in
    go base parts
  in
  let rec read_value path =
    skip_space ~lines:false;
    match peek 0 with
    | Some ('"' | '\'') -> ignore (read_string ())
    | Some '{' ->
      advance 1;
      let rec fields () =
        skip_space ~lines:true;
        match peek 0 with
        | Some '}' -> advance 1
        | Some ',' -> advance 1; fields ()
        | Some _ ->
          let target = record_path path (read_key ()) in
          skip_space ~lines:false;
          if peek 0 <> Some '=' then raise Stop;
          advance 1;
          read_value target;
          fields ()
        | None -> raise Stop
      in
      fields ()
    | Some '[' ->
      advance 1;
      let rec items () =
        skip_space ~lines:true;
        match peek 0 with
        | Some ']' -> advance 1
        | Some ',' -> advance 1; items ()
        | Some _ -> read_value path; items ()
        | None -> raise Stop
      in
      items ()
    | _ ->
      while !pos < n && not (List.mem src.[!pos] [','; ']'; '}'; '\n'; '#']) do advance 1 done
  in
  let current = ref [] in
  let rec statements () =
    skip_space ~lines:true;
    match peek 0 with
    | None -> ()
    | Some '[' ->
      let array = peek 1 = Some '[' in
      advance (if array then 2 else 1);
      let parts = read_key () in
      advance (if array then 2 else 1);
      current := record_path [] parts;
      statements ()
    | Some _ ->
      let target = record_path !current (read_key ()) in
      skip_space ~lines:false;
      if peek 0 <> Some '=' then raise Stop;
      advance 1;
      read_value target;
      statements ()
  in
  (try statements () with Stop | Invalid_argument _ -> ());
  order

(* ── From the library's value ──────────────────────────────────────────── *)

(* The key as written, without the quotes `Key.to_string` adds around one
   that is not bare. A bare key holds no quote, so a quoted rendering is
   always a non-bare key in quotes. *)
let raw_key k =
  let s = Toml.Types.Table.Key.to_string k in
  let l = String.length s in
  if l >= 2 && s.[0] = '"' && s.[l - 1] = '"' then String.sub s 1 (l - 2) else s

let rec array_values (arr : Toml.Types.array) : Toml.Types.value list =
  match arr with
  | Toml.Types.NodeBool bs -> List.map (fun b -> Toml.Types.TBool b) bs
  | Toml.Types.NodeInt ns -> List.map (fun n -> Toml.Types.TInt n) ns
  | Toml.Types.NodeFloat fs -> List.map (fun f -> Toml.Types.TFloat f) fs
  | Toml.Types.NodeString ss -> List.map (fun s -> Toml.Types.TString s) ss
  | Toml.Types.NodeDate ds -> List.map (fun d -> Toml.Types.TDate d) ds
  | Toml.Types.NodeTable ts -> List.map (fun t -> Toml.Types.TTable t) ts
  | Toml.Types.NodeArray ars -> List.map (fun a -> Toml.Types.TArray a) ars
  | Toml.Types.NodeEmpty -> []

and of_library (order : order) path (v : Toml.Types.value) : t =
  match v with
  | Toml.Types.TBool b -> Bool b
  | Toml.Types.TInt n -> Int n
  | Toml.Types.TFloat f -> Float f
  | Toml.Types.TString s -> String s
  | Toml.Types.TDate d -> Date d
  | Toml.Types.TArray arr -> Array (List.map (of_library order path) (array_values arr))
  | Toml.Types.TTable tbl ->
    let written = match Hashtbl.find_opt order path with Some keys -> !keys | None -> [] in
    let rank k =
      let rec go i = function
        | [] -> max_int
        | x :: rest -> if x = k then i else go (i + 1) rest
      in
      go 0 written
    in
    let pairs =
      Toml.Types.Table.to_list tbl
      |> List.map (fun (k, v) -> (raw_key k, v))
      (* `to_list` is sorted, and the sort is stable, so a key the scan did
         not see keeps the library's order among the others like it. *)
      |> List.stable_sort (fun (a, _) (b, _) -> compare (rank a) (rank b))
    in
    Table (List.map (fun (k, v) -> (k, of_library order (path @ [k]) v)) pairs)

let parse (src : string) : (t, string) result =
  match Toml.Parser.from_string src with
  | `Ok tbl -> Ok (of_library (scan src) [] (Toml.Types.TTable tbl))
  | `Error (msg, _) -> Error msg

(* ── To text ───────────────────────────────────────────────────────────── *)

let bare_key k =
  k <> "" && String.for_all (function
    | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '-' -> true
    | _ -> false) k

let key_text k = if bare_key k then k else library_scalar (Toml.Types.TString k)

let rec scalar_text v =
  match v with
  | Bool b -> library_scalar (Toml.Types.TBool b)
  | Int n -> library_scalar (Toml.Types.TInt n)
  | Float f -> library_scalar (Toml.Types.TFloat f)
  | String s -> library_scalar (Toml.Types.TString s)
  | Date d -> date_text d
  | Array vs -> "[" ^ String.concat ", " (List.map scalar_text vs) ^ "]"
  (* Only reached inside an array that is not all tables, where a section
     header cannot stand: TOML writes that table inline. *)
  | Table kvs ->
    "{" ^ String.concat ", " (List.map (fun (k, v) -> key_text k ^ " = " ^ scalar_text v) kvs) ^ "}"

let is_table_array = function
  | Array (_ :: _ as vs) -> List.for_all (function Table _ -> true | _ -> false) vs
  | _ -> false

let is_section v = match v with Table _ -> true | _ -> is_table_array v

(* A table's own values come first and its sub-tables after them, since a
   value written below a section header belongs to that section. Within each
   group the order is the table's own. *)
let rec write_table buf path kvs =
  let values, sections = List.partition (fun (_, v) -> not (is_section v)) kvs in
  List.iter (fun (k, v) ->
    Buffer.add_string buf (key_text k ^ " = " ^ scalar_text v ^ "\n")) values;
  List.iter (fun (k, v) ->
    let path' = path @ [k] in
    let header' = String.concat "." (List.map key_text path') in
    match v with
    | Table sub ->
      (* A table that holds nothing but tables needs no header of its own:
         `[a.b]` says `a` is a table. An empty one does, or it is lost. *)
      if sub = [] || not (List.for_all (fun (_, v) -> is_section v) sub) then
        Buffer.add_string buf ("[" ^ header' ^ "]\n");
      write_table buf path' sub
    | Array tables ->
      List.iter (function
        | Table sub ->
          Buffer.add_string buf ("[[" ^ header' ^ "]]\n");
          write_table buf path' sub
        | _ -> ()) tables
    | _ -> ()) sections

let to_document (kvs : (string * t) list) : string =
  let buf = Buffer.create 256 in
  write_table buf [] kvs;
  Buffer.contents buf
