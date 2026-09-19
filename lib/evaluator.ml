open Ast

module StrMap = Map.Make (String)


(* ── Constructor field name registry ─────────────────────────────────────── *)

let constr_fields : (Ctor.t, string option list) Hashtbl.t = Hashtbl.create 16

(* The defaults a constructor declares, by field name. A construction that
   leaves a field out takes its value from here, and so does a derived
   decoder reading a document with nothing under that name. *)
let constr_defaults : (Ctor.t, (string * Ast.expr) list) Hashtbl.t =
  Hashtbl.create 16

(* A pattern still carries a bare name, so a match has a name where the value
   has an identity. This says which identity a name means. Until a module can
   own a constructor, one name means one identity, and this is a lookup
   rather than a choice. *)
let ctor_of_name : (string, Ctor.t) Hashtbl.t = Hashtbl.create 16

let register_ctor c = Hashtbl.replace ctor_of_name (Ctor.name c) c

let ctor_named name =
  match Hashtbl.find_opt ctor_of_name name with
  | Some c -> c
  | None -> Ctor.Local name

(* Where a constructor stands in its declaration. `List.sort` ordered a
   variant by constructor name, so `type S = Zulu | Alpha` sorted to
   `[Alpha, Zulu]` and renaming a constructor moved values around. The
   declaration is what decides everything else about a type, and it decides
   this too. *)
let constr_index : (Ctor.t, int) Hashtbl.t = Hashtbl.create 16

let () =
  (* The built-in pairs declare their absent and failed cases first, which
     is the order they already sorted in. *)
  List.iter (fun (n, i) -> Hashtbl.replace constr_index (Ctor.Builtin n) i)
    ["None", 0; "Some", 1; "Error", 0; "Ok", 1; "ShellResult", 0;
     "HTTPRequest", 0; "HTTPResponse", 0;
     (* Declaration order, which is what the index means. *)
     "GET", 0; "POST", 1; "PUT", 2; "PATCH", 3; "DELETE", 4; "HEAD", 5];
  Hashtbl.add constr_fields (Ctor.Builtin "ShellResult")
    [Some "stdout"; Some "stderr"; Some "code"];
  (* Built in, so they are known before any file is read. *)
  Hashtbl.add constr_fields (Ctor.Builtin "Some") [None];
  Hashtbl.add constr_fields (Ctor.Builtin "None") [];
  (* What `T.parser` answers with, and what `Args.read` takes apart. *)
  Hashtbl.add constr_fields (Ctor.Builtin "CommandLine")
    [Some "spec"; Some "reader"; Some "usage"];
  (* What a request is made of, and what one answers with. `HTTP.Request`
     and `HTTP.Response` are aliases of these, so a script never writes
     these spellings. *)
  Hashtbl.add constr_fields (Ctor.Builtin "HTTPRequest")
    [Some "url"; Some "method"; Some "headers"; Some "body"; Some "timeout";
     Some "redirects"];
  Hashtbl.add constr_fields (Ctor.Builtin "HTTPResponse")
    [Some "status"; Some "headers"; Some "body"];
  (* Every field but the URL has one, so a request is written by naming what
     differs from the ordinary case. These are the same expressions the
     type's declaration carries; the typechecker holds its own copy for the
     same reason it holds the fields. *)
  Hashtbl.add constr_defaults (Ctor.Builtin "HTTPRequest")
    [ ("method",    Ast.Constr "GET");
      ("headers",   Ast.MapLit []);
      ("body",      Ast.String "");
      ("timeout",   Ast.Duration "30s");
      ("redirects", Ast.Int 5) ];
  List.iter (fun n -> Hashtbl.add constr_fields (Ctor.Builtin n) [])
    ["GET"; "POST"; "PUT"; "PATCH"; "DELETE"; "HEAD"];
  List.iter (fun n -> Hashtbl.replace ctor_of_name n (Ctor.Builtin n))
    ["ShellResult"; "CommandLine"; "HTTPRequest"; "HTTPResponse";
     "GET"; "POST"; "PUT"; "PATCH"; "DELETE"; "HEAD";
     "Some"; "None"; "Ok"; "Error"]

(* The host a URL names, as written. wand resolves no DNS: the host as
   written is the thing the manifest allows, which is the rule
   `shell_scan.ml` already states for a binary. *)
let host_of_url u =
  let after_scheme =
    match String.index_opt u ':' with
    | Some i when i + 2 < String.length u && u.[i + 1] = '/' && u.[i + 2] = '/' ->
      String.sub u (i + 3) (String.length u - i - 3)
    | _ -> u
  in
  let stop_at c s =
    match String.index_opt s c with
    | Some i -> String.sub s 0 i
    | None -> s
  in
  let host = stop_at '/' after_scheme |> stop_at '?' |> stop_at '#' in
  (* Credentials come before the host, a port after it. *)
  let host =
    match String.rindex_opt host '@' with
    | Some i -> String.sub host (i + 1) (String.length host - i - 1)
    | None -> host
  in
  stop_at ':' host

let defaults_of name =
  match Hashtbl.find_opt constr_defaults name with
  | Some ds -> ds
  | None -> []

(* Type definitions, kept for derivation.
   A decoder derived from a type has to find the decoders of the types its
   fields mention, and a type may mention itself. Those are looked up when a
   field is decoded rather than when the decoder is built, which is what lets
   `type Node (label : String, children : List Node)` have a decoder at all
   -- built eagerly, deriving one would not terminate. *)
let derivable :
  (string, Ctor.t * string list * (string option * type_expr) list) Hashtbl.t =
  Hashtbl.create 16

(* A Map holds a key once.

   Two entries with one key means one of them is unreachable: `Map.get` finds
   a single value, while `size`, `keys` and any fold see both. A document
   read in, edited and written back would carry the ghost along.

   The rule is the one an assignment already implies: the last value given
   wins, and it sits where the key first appeared. Position matters because
   a Map is written back out in the order it holds -- a config that reorders
   itself on every edit makes a diff nobody can read. *)
let map_put kvs key v =
  let rec go seen = function
    | [] -> if seen then [] else [(key, v)]
    | (k, _) :: rest when String.equal k key ->
      if seen then go seen rest else (key, v) :: go true rest
    | pair :: rest -> pair :: go seen rest
  in
  go false kvs

let map_of_pairs pairs = List.fold_left (fun acc (k, v) -> map_put acc k v) [] pairs

(* Reading a key out of a document that names it twice. The later one, for
   the same reason a Map keeps the later value: it is what an assignment
   means, and it is what the parsers everything else in the world uses do.
   Every reader here agrees on it -- `JSON.field`, a decoder, and the Map
   `get_object` hands back -- so a document cannot say different things to
   two readers of the same program. *)
let assoc_last key kvs =
  List.fold_left (fun found (k, v) -> if String.equal k key then Some v else found) None kvs

let find_field_index names label =
  let rec go i = function
    | [] -> None
    | Some n :: _ when n = label -> Some i
    | _ :: rest -> go (i + 1) rest
  in go 0 names

(* ── Values ───────────────────────────────────────────────────────────────── *)

type value =
  | VInt      of int
  | VFloat    of float
  | VString   of string
  | VBool     of bool
  | VUnit
  | VPath     of string
  | VGlob     of string
  | VDateTime of string
  | VDuration of string
  (* A URL, and the `Net(...)` bound of the file whose text wrote it. The
     bound rides here rather than on the request, because the request a
     `HTTP.get` sends is built inside the standard library while the URL is
     the part the caller wrote. A URL the run computed carries none, which
     is the case `V-NET1` reports. *)
  | VURL      of string * string list option
  | VIPv4     of string
  | VCIDR     of string
  | VPort     of int
  | VVersion  of string
  | VSize     of string
  | VRegex         of Re.re
  | VJson          of Yojson.Basic.t
  | VToml          of Toml.Types.value
  (* Restricted to string keys, YAML 1.2 core's value space is JSON's,
     so a document needs no representation of its own. It stays a
     separate wand type: what a file is decides how it is read, and a
     `JSON.get_object` on a manifest would read as though wand had
     converted the file. *)
  | VYaml          of Yojson.Basic.t
  | VTuple         of value list
  | VList          of value list
  (* A map holds its entries in a `StrMap` and remembers, per key, when it
     was first added. It was an association list, so `get` was
     `List.assoc_opt` and `set` a walk -- cost linear in the number of keys,
     paid on every operation. Tallying 200k lines over 400 keys spent 2.0s in
     here, 81% of the run, and the same shape made a keying mistake read as a
     hang rather than a wrong answer.

     The order is what the list gave for free and is documented behaviour:
     entries come back in the order their keys were first added, a key
     already present keeps its place, and a document read in, edited and
     written back keeps its shape. `m_next` is the counter that buys that
     back -- `vmap_list` sorts on it. *)
  | VMap           of vmap
  (* A module namespace. `Foo.bar` used to be `List.assoc_opt` over this
     list, so every qualified call cost one string compare per member of the
     module it named -- and the list is in reverse declaration order, so the
     function declared first in a file was the last one found.
     `String.length` took 584ns to resolve where `String.words`, declared
     last, took 74ns. The index makes both the second number. An environment
     already carries one for the same reason; this is that idea one level
     in. *)
  | VRecord        of vrecord
  | VFun           of env * pat list * expr
  (* Which constructor, and whose. `Ctor.t` rather than the bare name: two
     modules may each declare one called `Status`, and a pattern from one
     file must not match a value from the other. *)
  | VConstr        of Ctor.t * value list
  (* A request, and the `Net(...)` bound of the file that built it. The
     value inside is the ordinary `VConstr`, so every field reads as it
     would without this.

     A `Command` already carries its bound this way, and for the same
     reason: the manifest belongs to a file, the send happens somewhere
     else, and by then there is nothing left to ask. The construction site
     checks the URL it was given; the transport needs the same list again
     because a redirect's host is only known mid-flight. *)
  | VRequest       of value * string list option
  | VPartialConstr of Ctor.t * int * value list
  | VFix           of string * env * pat list * expr
  | VFixGroup      of (string * pat list * expr) list * env * string
      (* mutually-recursive function group; last string is which member
         this particular value represents *)
  | VBuiltin       of (value -> value)
  (* A resource: how to acquire, and how to give back. A description, not
     something already open -- which is what lets one be named, passed, and
     used twice. `with` is the only thing that runs it. *)
  | VResource      of value * value
  (* A stream: an inert description of a source and its stages, run only
     by a terminal operation, which opens, pulls each line through the
     stages, and closes on the way out. Like a Resource, it describes; it
     is never the open thing, which is what lets one be named, passed,
     and folded twice (each fold reads the source afresh). *)
  | VStream        of stream_desc
  (* A command: what would be run, and the bound of the file whose text
     named it. Nothing is running -- like a Resource and a Stream this
     describes, which is what lets one be named, passed and run twice. The
     bound travels with the value rather than with the call site, because it
     is the site that wrote the words that a manifest answers for. *)
  | VCommand       of string * string list option
  (* The answer a `FS!stream_lines`-family effect resumes with: the default
     handler wraps the real channel; a mock answers with a plain list and
     never sees this constructor. Runtime-internal -- it goes straight back
     to the terminal loop and no wand code ever holds one. *)
  | VLineSource    of in_channel
  (* A running subprocess, as a stream reads it: a puller over its stdout,
     and a stop that ends it. Both are closures because ending a child is
     signals, a grace period and a wait -- which belong to the default
     handler, beside the rest of the process machinery. The stop is told
     whether the stream ended early: a command wand killed because a `take`
     was satisfied did not fail, so its exit status is wand's own signal
     coming back and is not reported. Runtime-internal, like VLineSource. *)
  | VProcSource    of (unit -> value option) * (bool -> unit)
  (* Where a stream's lines go: write one, and close when they stop. The
     answer a `FS!write_lines`-family effect resumes with, the way
     `VLineSource` is the answer for reading; a mock resumes with `()` and
     the lines go nowhere.

     Closures rather than a channel, because a sink is a place lines go and
     not necessarily a file: a rehearsal answers with one that collects
     them, so the reads after the write see what would have been written.
     Runtime-internal -- no wand code holds one. *)
  (* Write a line, commit, abort. A sink that publishes what it collected --
     an atomic write of a stream -- has to be told which ending it got: a
     stream that raised at line 900 of 1000 must take its temp file away
     rather than rename it over the target, which would deliver the torn
     file the write exists to prevent. For a sink that writes straight to
     the file the two are the same call, because a run that raised half way
     really does leave a half-written file. *)
  | VLineSink      of (string -> unit) * (unit -> unit) * (unit -> unit)
  (* A decoder: how to read a value out of data that arrived untyped. It is
     handed the data and the path it stands at, so a failure can name the
     field that failed rather than only the type that did not fit. Every
     backend presents its input in JSON's shape, so one set of combinators
     serves all of them. *)
  | VDecoder       of (Yojson.Basic.t -> string list -> (value, string) result)
  (* An index over the entries behind it. An environment is a list because
     that is what binding is -- push a name in front of what was there -- and
     lookup walks it, which is fine for the handful a script defines and not
     fine for the couple of hundred builtins they all sit in front of. The
     index is dropped in front of that fixed part, so a name in it is found
     in one step instead of two hundred.

     It is a hint, not an authority: a miss keeps walking. That way an
     environment can be appended to, sliced, or carry several indexes without
     any of it having to know. *)
  | VEnvIndex      of (string, value) Hashtbl.t

(* `m_entries` maps a key to when it was first added and what it holds;
   `m_next` is the number the next new key takes. A delete leaves a gap in
   the numbering, which costs nothing: only their order is ever read. *)
and vmap = { m_entries : (int * value) StrMap.t; m_next : int }

(* `r_fields` is the members in the order they were bound, which is what
   printing and pattern matching read; `r_index` answers a lookup. They are
   built together and neither changes after, so the two cannot disagree. *)
and vrecord = { r_fields : (string * value) list;
                r_index  : (string, value) Hashtbl.t }

and env = (string * value) list

and stream_desc = { s_source : stream_source; s_stages : stream_stage list }

and stream_source =
  | SFile  of string
  | SStdin
  | SVals  of value list
  (* A command's output, line by line, and the bound of the file that wrote
     the command. The stream is still a description: nothing is spawned
     until a terminal operation runs it, and folding twice runs the command
     twice -- which re-reading a file does too, and costs more here. *)
  | SCommand of string * string list option
  (* Several files read as one. Still a single puller: it moves to the next
     file when one runs out, so the driver's shape does not change. This is
     the concatenation people actually have -- `FS.glob *.log` read as one
     log -- and it costs a source rather than a second stream. *)
  | SFiles of string list
  (* An injected puller, reachable only from OCaml -- how the tests prove
     that `take` stops pulling, which no wand-level mock can observe under
     open-granularity effects. *)
  | SPull  of (unit -> value option)

and stream_stage =
  | StMap       of value
  | StFilter    of value
  | StTake      of int
  | StFilterMap of value
  | StTakeWhile of value
  | StDrop      of int
  | StDropWhile of value
  | StIndexed
  (* The step and what to start from. Each item answers with the running
     total after it, so one item in is one item out and the starting value
     is not emitted -- a stage cannot emit before it is given anything. *)
  | StScan      of value * value
  | StChunks    of int
  | StUnique
  (* The one stage that emits several per item, which is why the driver
     carries a list rather than an option. *)
  | StFlatMap   of value

(* ── Map values ──────────────────────────────────────────────────────────── *)

(* First binding wins, exactly as walking the list did. *)
let vrecord_make r_fields =
  let r_index = Hashtbl.create (List.length r_fields * 2 + 1) in
  List.iter (fun (k, v) -> if not (Hashtbl.mem r_index k) then Hashtbl.add r_index k v)
    r_fields;
  { r_fields; r_index }

let vrecord_get label r = Hashtbl.find_opt r.r_index label

let v_none = VConstr (Ctor.Builtin "None", [])

let vmap_empty = { m_entries = StrMap.empty; m_next = 0 }

(* The entries, in the order their keys were first added. Sorting on the
   number costs O(n log n) once, where reading a key costs O(log n) every
   time -- and reads outnumber traversals in every program that has been
   measured here. *)
let vmap_list m =
  StrMap.bindings m.m_entries
  |> List.map (fun (k, (i, v)) -> (i, (k, v)))
  |> List.sort (fun (i, _) (j, _) -> compare i j)
  |> List.map snd

let vmap_get key m =
  match StrMap.find_opt key m.m_entries with
  | Some (_, v) -> Some v
  | None        -> None

(* A key already present keeps the number it was first given, so setting it
   again replaces the value and leaves it where it was. *)
let vmap_set key v m =
  match StrMap.find_opt key m.m_entries with
  | Some (i, _) -> { m with m_entries = StrMap.add key (i, v) m.m_entries }
  | None        -> { m_entries = StrMap.add key (m.m_next, v) m.m_entries;
                     m_next = m.m_next + 1 }

(* `Map.update`: one descent, where `Map.set k (f (Map.get k m)) m` walks the
   map twice and allocates an `Option` between the halves to be read once and
   thrown away. Measured over 100k rows on a five-key map, that round trip is
   260ms and this is 147ms.

   `set` is not written in terms of this. It ignores what is there, so it
   applies no function, and saying "ignore it" as a function of it costs 12%
   and reads backwards. *)
let vmap_update key f m =
  let added = ref false in
  let m_entries =
    StrMap.update key
      (function
       | Some (i, v) -> Some (i, f (Some v))
       | None        -> added := true; Some (m.m_next, f None))
      m.m_entries
  in
  { m_entries; m_next = if !added then m.m_next + 1 else m.m_next }

let vmap_delete key m = { m with m_entries = StrMap.remove key m.m_entries }
let vmap_mem key m    = StrMap.mem key m.m_entries
let vmap_size m       = StrMap.cardinal m.m_entries

let vmap_map f m =
  { m with m_entries = StrMap.map (fun (i, v) -> (i, f v)) m.m_entries }

let vmap_filter f m =
  { m with m_entries = StrMap.filter (fun _ (_, v) -> f v) m.m_entries }

(* The list's order becomes the map's, and where a key appears twice the last
   value wins and sits at the first appearance. *)
let vmap_of_list pairs =
  List.fold_left (fun acc (k, v) -> vmap_set k v acc) vmap_empty pairs

(* The key an index entry is filed under. Not a legal identifier, so no
   program can name it and no lookup can collide with it. *)
(* A default is a value written out, so the only names it can hold are
   constructors. This is those, as an environment to read one in: rebuilt
   when a declaration is registered rather than at each use, since a default
   is read every time a construction leaves its field out. *)
let ctor_env_cache : (string * value) list option ref = ref None

let forget_ctor_env () = ctor_env_cache := None

let ctor_env () =
  match !ctor_env_cache with
  | Some e -> e
  | None ->
    let e =
      Hashtbl.fold (fun c fields acc ->
        let v = match fields with
          | [] -> VConstr (c, [])
          | fs -> VPartialConstr (c, List.length fs, [])
        in
        (* Keyed by the name a default writes, which is the bare one. *)
        (Ctor.name c, v) :: acc) constr_fields []
    in
    ctor_env_cache := Some e; e

(* A regex literal is a constant, and `Re.compile` is a pure function of its
   pattern and flags -- but it ran on every evaluation of the expression,
   which inside a loop is once per iteration at about 5us a time.
   `Regex.match? r/ERROR/ line` over a 200k-line file spent a second
   compiling the same pattern 200,000 times, and the way to avoid it was to
   know to hoist the literal out by hand.

   Keyed on the pattern and the flags as written. The set of literals comes
   from the source text, so it is finite and fixed before the program runs;
   `Regex.compile` is deliberately not cached, because its argument can be
   built at run time and the table would then grow with the data.

   One table per domain. `Par` workers evaluate wand code on domains of
   their own, and a shared `Hashtbl` written from several at once is a data
   race. Compiling a pattern twice on two domains costs a little and is
   safe; sharing one table is neither. *)
let regex_literals : (string * string, Re.re) Hashtbl.t Domain.DLS.key =
  Domain.DLS.new_key (fun () -> Hashtbl.create 16)

let env_index_key = "\000index"

let index_env (base : env) : env =
  let tbl = Hashtbl.create (List.length base * 2 + 16) in
  (* First binding wins, as walking the list would. *)
  List.iter (fun (k, v) -> if not (Hashtbl.mem tbl k) then Hashtbl.add tbl k v) base;
  (env_index_key, VEnvIndex tbl) :: base

(* Every name an environment can reach, for a "did you mean" hint. *)
let rec env_names (e : env) =
  match e with
  | [] -> []
  | (k, VEnvIndex tbl) :: rest when k = env_index_key ->
    Hashtbl.fold (fun k _ acc -> k :: acc) tbl [] @ env_names rest
  | (k, _) :: rest -> k :: env_names rest

(* Re-index as a file's own definitions pile up, so a lookup never walks more
   than this many before reaching one. An index is a hint, so adding another
   in front of an older one is always safe -- the newer one simply covers
   more. *)
let index_every = 32

let rec lookup_var name (e : env) =
  match e with
  | [] -> None
  | (k, VEnvIndex tbl) :: rest when k = env_index_key ->
    (match Hashtbl.find_opt tbl name with
     | Some _ as found -> found
     | None -> lookup_var name rest)
  | (k, v) :: rest -> if String.equal k name then Some v else lookup_var name rest

(* A definition that only forwards.

   `let trim s = str_trim s` takes its argument and hands it to a builtin
   unchanged. It *is* that builtin: the closure around it exists to pass one
   value along and nothing else, and it costs 92ns on every call to do it.
   289 of the standard library's 530 definitions are written this way, so the
   closure is skipped and the name bound to the builtin itself.

   Every parameter must arrive in its own position, in order, once, and the
   head must already be a builtin. `let f a b = g b a` reorders,
   `let f a = g a a` repeats, `let lines s = str_split "\n" s` supplies an
   argument of its own, and `let empty? s = str_length s == 0` does work on
   the answer -- none of those is the same function as its head, and none of
   them qualifies. A `let f x = f x` cannot slip through either: `f` is not
   bound to a builtin when its own definition is read. *)
let forwarding_builtin env params body =
  let rec param_names acc = function
    | []            -> Some (List.rev acc)
    | PVar n :: rest -> param_names (n :: acc) rest
    | _             -> None
  in
  match param_names [] params with
  | None | Some [] -> None
  | Some ps ->
    (* Peel the applications off the body. `g a b` is `App (App (g, a), b)`,
       so the arguments come back out in the order they were written. *)
    let rec peel args e =
      match strip_located e with
      | App (f, x) ->
        (match strip_located x with
         | Var n -> peel (n :: args) f
         | _     -> None)
      | Var f -> Some (f, args)
      | _     -> None
    in
    (match peel [] body with
     | Some (head, args) when args = ps ->
       (match lookup_var head env with
        | Some (VBuiltin _ as v) -> Some v
        | _ -> None)
     | _ -> None)

(* ── Runtime error ────────────────────────────────────────────────────────── *)

exception EvalError of string

(* ── Instants ───────────────────────────────────────────────────────────── *)

(* Days from 1970-01-01 to a civil date, by Howard Hinnant's algorithm. It
   is exact for every proleptic Gregorian date and needs no table. *)
let days_from_civil y m d =
  let y = if m <= 2 then y - 1 else y in
  let era = (if y >= 0 then y else y - 399) / 400 in
  let yoe = y - era * 400 in
  let mp = (m + 9) mod 12 in
  let doy = (153 * mp + 2) / 5 + d - 1 in
  let doe = yoe * 365 + yoe / 4 - yoe / 100 + doy in
  era * 146097 + doe - 719468

(* A DateTime as seconds from the epoch, so that two spellings of one
   instant compare equal: `2024-01-15T20:00:00+05:30` and
   `2024-01-15T14:30:00Z` are the same moment.

   The lexer has already fixed the shape -- `YYYY-MM-DDTHH:MM:SS`, then
   `Z`, `+HH:MM`, `-HH:MM`, or nothing -- so the digits are where this
   expects them. A value with no offset is read as UTC. Reading it as local
   time would make one script answer differently on two machines. *)
let datetime_epoch s =
  let num at len = int_of_string (String.sub s at len) in
  let days = days_from_civil (num 0 4) (num 5 2) (num 8 2) in
  (* A bare day is that day at midnight. The lexer keeps the short
     spelling, so this is where the two forms become one meaning. *)
  if String.length s = 10 then days * 86400
  else
  let secs = days * 86400 + num 11 2 * 3600 + num 14 2 * 60 + num 17 2 in
  if String.length s <= 19 then secs
  else
    match s.[19] with
    | 'Z' -> secs
    | '+' -> secs - (num 20 2 * 3600 + num 23 2 * 60)
    | '-' -> secs + (num 20 2 * 3600 + num 23 2 * 60)
    | _   -> secs

(* The inverse: an instant written back as a `DateTime` in UTC. Every
   instant wand produces is written this way -- `Z`, whole seconds -- so a
   value that came from `Clock.now` reads like a value that was written by
   hand. The algorithm is the one `days_from_civil` reverses, from the same
   note by Howard Hinnant. *)
let civil_from_days z =
  let z = z + 719468 in
  let era = (if z >= 0 then z else z - 146096) / 146097 in
  let doe = z - era * 146097 in
  let yoe = (doe - doe / 1460 + doe / 36524 - doe / 146096) / 365 in
  let y = yoe + era * 400 in
  let doy = doe - (365 * yoe + yoe / 4 - yoe / 100) in
  let mp = (5 * doy + 2) / 153 in
  let d = doy - (153 * mp + 2) / 5 + 1 in
  let m = if mp < 10 then mp + 3 else mp - 9 in
  ((if m <= 2 then y + 1 else y), m, d)

(* Whole days since the epoch, flooring so that an instant before 1970
   lands in the day it belongs to rather than the one after. *)
let epoch_days secs = if secs >= 0 then secs / 86400 else (secs - 86399) / 86400

let seconds_into_day secs = secs - epoch_days secs * 86400

(* A day at midnight UTC, or why it is not a day. `days_from_civil` maps
   any three numbers to some day, so `2026-02-30` would come back as March
   the 2nd; converting back and comparing is what refuses it. *)
(* A `DateTime` is written with a four-digit year, so a year outside
   0..9999 has no spelling: the value would print as text nothing can read
   back. Refused here, where a caller is holding a Result to be told in. *)
let year_min = 0
let year_max = 9999

let day_at y m d =
  if y < year_min || y > year_max then
    Error (Printf.sprintf
      "%d is outside the years wand writes: %04d to %04d" y year_min year_max)
  else if m < 1 || m > 12 || d < 1 || d > 31 then
    Error (Printf.sprintf "%04d-%02d-%02d is not a day" y m d)
  else
    let days = days_from_civil y m d in
    let (y', m', d') = civil_from_days days in
    if y' = y && m' = m && d' = d then Ok days
    else Error (Printf.sprintf "%04d-%02d-%02d is not a day" y m d)

let datetime_of_epoch secs =
  let days = if secs >= 0 then secs / 86400 else (secs - 86399) / 86400 in
  let rest = secs - days * 86400 in
  let (y, m, d) = civil_from_days days in
  (* Every instant wand produces is written here, so this is the one place
     that has to answer for the four-digit year. A moved instant that leaves
     the range raises rather than printing a year nothing can read back. *)
  if y < year_min || y > year_max then
    raise (EvalError (Printf.sprintf
      "this instant falls in the year %d, outside the years wand writes: \
       %04d to %04d" y year_min year_max));
  Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02dZ"
    y m d (rest / 3600) (rest mod 3600 / 60) (rest mod 60)

(* ── Display ──────────────────────────────────────────────────────────────── *)

(* Two ways to write a value down.

   `to_text` is the value as text -- what `IO.println` writes, what `%{...}`
   splices, what goes down a command's stdin, what a CSV cell holds. A
   string is its own characters there, because that is the whole point of
   writing it out.

   `show_value` is the value as someone reads it back: the answer the REPL
   echoes, the value an error message names. A string is quoted there, at
   any depth, because without quotes the display does not say what the value
   was -- `["a, b"]` is one element and `["a", "b"]` is two, and both print
   as `[a, b]`. Quoted, what is shown is wand source again, and the escapes
   are the ones the lexer reads back. *)

(* A TOML array holds one type, so the library keeps it as a list of that
   type rather than a list of values. This puts the values back. *)
let toml_array_values (arr : Toml.Types.array) : Toml.Types.value list =
  match arr with
  | Toml.Types.NodeBool bs   -> List.map (fun b -> Toml.Types.TBool b) bs
  | Toml.Types.NodeInt ns    -> List.map (fun n -> Toml.Types.TInt n) ns
  | Toml.Types.NodeFloat fs  -> List.map (fun f -> Toml.Types.TFloat f) fs
  | Toml.Types.NodeString ss -> List.map (fun s -> Toml.Types.TString s) ss
  | Toml.Types.NodeDate ds   -> List.map (fun d -> Toml.Types.TDate d) ds
  | Toml.Types.NodeTable ts  -> List.map (fun t -> Toml.Types.TTable t) ts
  | Toml.Types.NodeArray ars -> List.map (fun a -> Toml.Types.TArray a) ars
  | Toml.Types.NodeEmpty     -> []

let quoted s =
  let buf = Buffer.create (String.length s + 2) in
  Buffer.add_char buf '"';
  String.iter (fun c ->
    match c with
    | '"'  -> Buffer.add_string buf "\\\""
    | '\\' -> Buffer.add_string buf "\\\\"
    | '\n' -> Buffer.add_string buf "\\n"
    | '\t' -> Buffer.add_string buf "\\t"
    | '\r' -> Buffer.add_string buf "\\r"
    | c    -> Buffer.add_char buf c) s;
  Buffer.add_char buf '"';
  Buffer.contents buf

let rec render ~quote v =
  let sub = render ~quote in
  match v with
  | VInt n      -> string_of_int n
  | VFloat f    -> Printf.sprintf "%g" f
  | VString s   -> if quote then quoted s else s
  | VBool b     -> string_of_bool b
  | VUnit       -> "()"
  | VPath s     -> s
  | VGlob s     -> s
  (* An instant shows as the moment it names, in UTC and in full, whichever
     of its spellings was written. A bare day is that day at midnight, and
     an offset resolves; the source keeps whatever it wrote, which is the
     formatter's business rather than this one's. *)
  | VDateTime s -> datetime_of_epoch (datetime_epoch s)
  | VDuration s -> s
  | VURL (s, _)      -> s
  | VIPv4 s     -> s
  | VCIDR s     -> s
  | VPort n     -> Printf.sprintf ":%d" n
  | VVersion s  -> s
  | VSize s     -> s
  | VRegex _    -> "<regex>"
  (* Shown as it would be written. The resolved command line is the useful
     half -- it is what a log or a plan wants -- and the `$*(...)` around it
     says this is a command rather than the text of one. *)
  | VCommand (cmd, _) -> "$*(" ^ cmd ^ ")"
  | VJson j     -> Yojson.Basic.to_string j
  | VYaml y     -> Yojson.Basic.to_string y
  (* A TOML value shows the way the rest of the language shows the same
     shapes: a table like a map, an array like a list. What it does not show
     as is a TOML document -- that is the text of the value rather than a
     look at it, so it belongs to `to_text` and to `TOML.stringify`. A
     document has newlines in it, and a display with newlines in it stops
     being one as soon as it is inside anything: a list of two tables came
     out over four lines, and the `: TOML` that says what it is landed
     after a blank. *)
  | VToml v -> render_toml ~quote v
  | VFun _ | VFix _ | VFixGroup _ | VBuiltin _ -> "<fn>"
  | VResource _ -> "<resource>"
  | VStream _ -> "<stream>"
  | VLineSource _ -> "<line source>"
  | VProcSource _ -> "<command source>"
  | VLineSink _ -> "<line sink>"
  | VDecoder _  -> "<decoder>"
  | VEnvIndex _ -> "<env index>"
  | VPartialConstr (n, _, _) -> Printf.sprintf "<%s>" (Ctor.name n)
  (* The bound is not part of what a request is; it shows as the record. *)
  | VRequest (inner, _) -> sub inner
  | VConstr (name, []) -> (Ctor.name name)
  | VConstr (name, vs) ->
    (Ctor.name name) ^ "(" ^ String.concat ", " (List.map sub vs) ^ ")"
  | VTuple vs   ->
    "(" ^ String.concat ", " (List.map sub vs) ^ ")"
  | VList vs    ->
    "[" ^ String.concat ", " (List.map sub vs) ^ "]"
  | VMap m      ->
    let kvs = vmap_list m in
    "{" ^ String.concat ", " (List.map (fun (k, v) -> k ^ " = " ^ sub v) kvs) ^ "}"
  | VRecord vr_ -> let kvs = vr_.r_fields in
    "{ " ^ String.concat ", " (List.map (fun (k, v) ->
      k ^ " = " ^ sub v) kvs) ^ " }"

and render_toml ~quote (v : Toml.Types.value) =
  let sub = render_toml ~quote in
  match v with
  | Toml.Types.TBool b   -> string_of_bool b
  | Toml.Types.TInt n    -> string_of_int n
  | Toml.Types.TFloat f  -> Printf.sprintf "%g" f
  | Toml.Types.TString s -> if quote then quoted s else s
  | Toml.Types.TTable tbl ->
    "{" ^ String.concat ", "
      (List.map (fun (k, v) ->
         Toml.Types.Table.Key.to_string k ^ " = " ^ sub v)
         (Toml.Types.Table.to_list tbl)) ^ "}"
  | Toml.Types.TArray arr ->
    (* An array showed as `<toml-array>`, which is a display that says
       nothing about the value: the one thing a reader wants from an array
       is what is in it. *)
    "[" ^ String.concat ", " (List.map sub (toml_array_values arr)) ^ "]"
  | Toml.Types.TDate _   -> "<toml-date>"

let show_value v = render ~quote:true v

(* Writing a value out is only unquoted where the value is the text: a
   string is its characters. A list of strings is not text -- the brackets
   and commas are already a display, and one that does not say where an
   element ends is no more use inside `%{...}` than it is in the REPL. *)
let to_text v =
  match v with
  | VString s                    -> s
  | VToml (Toml.Types.TString s) -> s
  (* The text of a TOML table is the document it stands for, newlines and
     all. `IO.println` of one writes real TOML; showing one does not. *)
  | VToml (Toml.Types.TTable tbl) -> Toml.Printer.string_of_table tbl
  | v                            -> show_value v

(* ── Checked Int arithmetic ───────────────────────────────────────────────── *)

(* Int is a machine word, 63 bits on a 64-bit platform, and wrapping past its
   range produced a plausible-looking wrong number rather than a complaint:
   the linear `fib 91` came back negative and every later term stayed wrong
   without anything saying so. Arithmetic whose result Int cannot represent
   now fails the way `1 / 0` does.

   A runtime error, not the Raise effect. Overflow is possible in any `+`, so
   making it an effect would put Raise on every function that adds
   two numbers, which says nothing about that function -- the same reason
   division by zero is a runtime error today. *)
let overflow op =
  raise (EvalError (Printf.sprintf
    "integer overflow in '%s': Int holds %d to %d" op min_int max_int))

(* Two operands of one sign whose result has the other went past the end. *)
let add_ovf x y =
  let s = x + y in
  if (x >= 0) = (y >= 0) && (s >= 0) <> (x >= 0) then overflow "+" else s

let sub_ovf x y =
  let d = x - y in
  if (x >= 0) <> (y >= 0) && (d >= 0) <> (x >= 0) then overflow "-" else d

(* Dividing the product back gives a different operand when it wrapped.
   `-1 * min_int` is the exception: it wraps to min_int, and dividing that
   by -1 wraps straight back, so the check has to name it. *)
let mul_ovf x y =
  let p = x * y in
  if x <> 0 && (p / x <> y || (x = -1 && y = min_int)) then overflow "*"
  else p

(* min_int / -1 is the one quotient with no representation. *)
let div_ovf x y = if x = min_int && y = -1 then overflow "/" else x / y

let neg_ovf x = if x = min_int then overflow "-" else -x

(* ── Regex repeat bound ──────────────────────────────────────────────────── *)

(* A counted repeat is expanded when the pattern is compiled, so the cost is
   the count and nothing else: `a{10000000}` takes seconds, `a{999999999}`
   takes minutes and the memory to match. Matching is a non-backtracking
   automaton, so no input makes a compiled pattern worse -- the pattern alone
   decides this, which is why it is answered where a pattern becomes a Regex.

   10,000 is past any repeat anyone writes and still compiles in the noise;
   the cost is linear, and 100,000 was already indistinguishable from zero
   when measured. `\{` is a literal brace, and so is a `{` inside a character
   class or one not followed by digits -- which is what PCRE reads them as,
   so neither is counted here. *)
let max_regex_repeat = 10_000

let regex_repeat_error pat =
  let n = String.length pat in
  let i = ref 0 and in_class = ref false and bad = ref None in
  (* The digits at `j`, as a count, with anything too long to be an int
     reported as over the bound rather than skipped. *)
  let count_at j =
    let start = !j in
    while !j < n && pat.[!j] >= '0' && pat.[!j] <= '9' do incr j done;
    if !j = start then None
    else
      let text = String.sub pat start (!j - start) in
      match int_of_string_opt text with
      | Some c -> Some c
      | None   -> Some max_int
  in
  while !i < n && !bad = None do
    (match pat.[!i] with
     | '\\' -> incr i
     | '[' when not !in_class -> in_class := true
     | ']' when !in_class -> in_class := false
     | '{' when not !in_class ->
       let j = ref (!i + 1) in
       (match count_at j with
        | None -> ()
        | Some lo ->
          let hi =
            if !j < n && pat.[!j] = ',' then (incr j; count_at j) else Some lo
          in
          if !j < n && pat.[!j] = '}' then
            let biggest = match hi with Some h -> max lo h | None -> lo in
            if biggest > max_regex_repeat then bad := Some biggest)
     | _ -> ());
    incr i
  done;
  Option.map (fun c ->
    Printf.sprintf "%s is too large -- a pattern may repeat at most %d times"
      (if c = max_int then "that repeat count"
       else Printf.sprintf "repeat count %d" c)
      max_regex_repeat)
    !bad

(* ── Algebraic effects ────────────────────────────────────────────────────── *)

type _ Effect.t += WandEffect : string * value -> value Effect.t

(* The Shell allowlist of the $()/$?() site currently being performed --
   the manifest bound of the file the site was written in, carried to the
   default handler out of band so the effect payload wand handlers match on
   stays a plain command string. Domain-local: it is set for the dynamic
   extent of one perform, and a perform is handled on the domain it was
   made on (Par's cross-domain forwarding re-threads it explicitly). None
   means the site is unbounded and nothing is checked at spawn. *)
let ambient_shell_allow : string list option Domain.DLS.key =
  Domain.DLS.new_key (fun () -> None)

(* The same, for the request being sent: the `Net(...)` bound of the file
   that built it, carried out of band so the payload a handler matches on
   stays the request itself. The transport reads it to check each redirect,
   whose host is not knowable any earlier. *)
let ambient_net_allow : string list option Domain.DLS.key =
  Domain.DLS.new_key (fun () -> None)

(* The `Net(...)` bound of the file whose top level is running. A URL
   literal carries the bound of the file it was written in; a URL the run
   computed -- `String.to_url`, `URL.join` -- has no literal to carry one,
   so it takes this one when it is made, and a request built from an
   unbounded URL at an unbounded site (the standard library's `HTTP.get`)
   is checked against it at the send. The runner sets it around a program's
   items and around each module's, so the standard library's own functions
   run under the bound of the file that called them. *)
let ambient_file_net : string list option Domain.DLS.key =
  Domain.DLS.new_key (fun () -> None)

let with_file_net allow f =
  let saved = Domain.DLS.get ambient_file_net in
  Domain.DLS.set ambient_file_net allow;
  Fun.protect ~finally:(fun () -> Domain.DLS.set ambient_file_net saved) f

let net_bound_of_manifest = function
  | Some (labels, _) -> Option.join (List.assoc_opt "Net" labels)
  | None -> None

(* A URL the run computed. It keeps the bound of the URL it was made from,
   and a URL made from text takes the running file's. *)
let computed_url ?(from = None) text =
  let bound = match from with
    | Some _ -> from
    | None -> Domain.DLS.get ambient_file_net
  in
  VURL (text, bound)

(* How long a command may run, in milliseconds, or None for as long as it
   takes. `Shell.timeout` sets it for the extent of the thunk it is given,
   and the default handler reads it when it waits for a child.

   Domain-local for the same reason the shell bound is: it belongs to the
   code that is running, not to the program. A `Par` worker that inherits
   nothing here waits without a deadline, which is the honest default --
   the worker was not the code the timeout was written around. *)
let shell_deadline : int option Domain.DLS.key =
  Domain.DLS.new_key (fun () -> None)

(* A timed-out command raises like any other failure, and `Shell.timeout`
   picks its own out of the raises it may catch by this prefix. Nothing
   else produces it, because nothing else sets a deadline. *)
let timeout_prefix = "wand:timeout"

let drop_prefix prefix s =
  let n = String.length prefix + 2 in   (* the prefix, then ": " *)
  if String.length s > n then String.sub s n (String.length s - n) else s

let perform_shell name allow payload =
  let saved = Domain.DLS.get ambient_shell_allow in
  Domain.DLS.set ambient_shell_allow allow;
  Fun.protect
    ~finally:(fun () -> Domain.DLS.set ambient_shell_allow saved)
    (fun () -> Effect.perform (WandEffect (name, payload)))

(* Raised into a handled body that a handler case answered without resuming,
   so the body unwinds and releases whatever it was holding. Private, and
   caught by the case that raised it: it is a way of running cleanup, not a
   failure anyone can see or catch. `try` re-raises what it does not
   recognise, so an abandoned region cannot be caught mid-unwind. *)
exception Abandoned

(* An abandoned region unwinds by raising `Abandoned` where the operation was
   performed. When that point is inside a `with`'s release -- a scratch
   directory removing its tree, say -- the release is running as
   `Fun.protect`'s finally, and OCaml wraps whatever the finally raises in
   `Fun.Finally_raised`. One wrapper per bracket the unwind passes through,
   so the question is what is at the bottom rather than what is on top.
   Without this a handler that declined to resume such an operation reached
   the top level as a fatal error instead of unwinding. *)
let rec is_abandoned = function
  | Abandoned -> true
  | Fun.Finally_raised e -> is_abandoned e
  | _ -> false

(* The script is stopping, carrying the code it will stop with. Raised by
   `exit` and by a signal, so that stopping unwinds the stack like anything
   else and every `with` on it releases what it holds.

   Not an EvalError, deliberately: `try` re-raises what it does not
   recognise, so a script cannot catch its own cancellation and carry on.
   The only thing that skips cleanup is a process that is destroyed rather
   than stopped -- SIGKILL, or the machine going away. *)
exception Interrupted of int

(* Set by a signal handler; read by the evaluator between steps. A signal
   handler cannot raise usefully here -- it runs wherever the program
   happens to be, which may be inside an effect handler carrying out a
   command, and an exception raised there abandons the body instead of
   unwinding it. Recording the request and raising from the evaluator's own
   stack puts the unwinding where the `with` frames are. *)
let interrupt_requested = Atomic.make 0

let request_interrupt code = Atomic.set interrupt_requested code

(* The request stands until the process ends, because every domain has to
   see it: a Par worker runs its own evaluation loop, and one that missed
   the request would keep working while the rest of the program unwound.

   Each domain raises it exactly once, recorded in domain-local state.
   Raising once is what lets cleanup run: a release is ordinary evaluation,
   and would re-raise on its first step against a request that still stood
   for this domain. A second signal is dealt with by the signal handler
   itself, which stops the process outright. *)
(* Where evaluation stands, for stamping a runtime error with a position.

   The position used to come from an exception handler wrapped around every
   `Located` node, which is the obvious way to do it and the one that costs
   the most: a handler is a stack frame, and a `Located` sits on every
   function body and every match arm, so a frame stayed behind on each one.
   Frames on the tail path never come back, so the stack grew with the call
   chain rather than with the nesting -- and every minor collection rescans
   that whole stack as roots, which makes a long recursion quadratic in its
   own depth. Recording the position instead of catching at it costs two
   stores and leaves the tail call a tail call.

   Two integers rather than the `Token.loc` they came from, because this is
   written on the way into every function body and arm: a record would be a
   pointer store, which is a write barrier, and wrapping it to say "none" is
   an allocation. Line 0 is what nowhere-in-particular is spelled as.

   Per-domain because Par's workers each evaluate their own program.

   Read only when an error is being reported. Until then it is written and
   never looked at. *)
type loc_cell =
  { mutable at_line : int; mutable at_col : int;
    (* Which file the position is in, "" for the one being run. A raise from
       inside an imported module used to report a bare line number, which
       reads as a line of the reader's own file and sends them to the wrong
       place entirely. *)
    mutable at_file : string;
    mutable depth : int }

let current_loc : loc_cell Domain.DLS.key =
  Domain.DLS.new_key (fun () ->
    { at_line = 0; at_col = 0; at_file = ""; depth = 0 })

(* A call left with work to do after it returns keeps a frame; one in tail
   position does not. So this bounds `apply` and never `apply_tail`, and a
   loop written tail-recursively still runs however long it runs.

   The bound exists because the alternative is OCaml's own `Stack overflow`,
   which cannot be caught here: on this runtime a handler that matches it --
   even one whose guard rejects it -- hangs instead of unwinding, because the
   guard runs on the stack that just ran out. Refusing before the stack is
   gone is the only way the reader gets an error they can read. *)
(* Read once, so the check itself is an integer compare and not a lookup.
   Settable because the ceiling it stands in for is not fixed: the default
   stack holds millions of frames, and a run under `OCAMLRUNPARAM=l=...` or
   a small `ulimit -s` holds far fewer. A bound above what the stack can
   carry never fires, and the fatal it exists to prevent comes back. Lower
   it to suit the stack the run actually has. *)
let max_call_depth =
  match Sys.getenv_opt "WAND_MAX_CALL_DEPTH" with
  | Some s -> (match int_of_string_opt s with
               | Some n when n > 0 -> n
               | _ -> 1_000_000)
  | None -> 1_000_000

(* The position an error is reported at is the innermost `Located` still
   being evaluated. That is what the cell holds, provided every construct
   that carries on after a subexpression finishes puts back what it found --
   otherwise a call that has already returned leaves its own body's position
   behind, and the next failure is reported against a line in whichever file
   that body came from. Tail positions are exempt: nothing carries on after
   them, so there is nothing to put back, which is the whole point. *)
let loc_cell () = Domain.DLS.get current_loc

let mark_loc (c : loc_cell) (l : Token.loc) =
  c.at_line <- l.Token.line;
  c.at_col  <- l.Token.col;
  c.at_file <- l.Token.file

(* Prefix a runtime error with where it was raised, unless it says already. *)
(* The file a run was asked for. A position in that file needs no name --
   it is the one the reader is looking at -- so only a position from
   somewhere else carries one. *)
let entry_file : string ref = ref ""

let stamp_loc msg =
  let c = loc_cell () in
  if c.at_line = 0 || Util.has_loc_prefix msg then msg
  else if c.at_file = "" || c.at_file = !entry_file then
    Printf.sprintf "%d:%d: %s" c.at_line c.at_col msg
  else
    (* Written the way the reader would type it: a file under the working
       directory by the path from there, anything else in full. An absolute
       path to a file three directories down is noise around the two numbers
       that matter. *)
    let shown =
      let cwd = Sys.getcwd () in
      let n = String.length cwd in
      if String.length c.at_file > n + 1
         && String.sub c.at_file 0 n = cwd
         && c.at_file.[n] = '/'
      then String.sub c.at_file (n + 1) (String.length c.at_file - n - 1)
      else c.at_file
    in
    Printf.sprintf "%s:%d:%d: %s" shown c.at_line c.at_col msg

(* Forget the position between runs. A session evaluates one statement after
   another, and an error raised before the next one reaches a `Located` --
   while an import is being resolved, say -- would otherwise be reported
   against the statement before it. *)
let forget_loc () =
  let c = loc_cell () in
  c.at_file <- "";
  c.at_line <- 0;
  c.at_col <- 0;
  (* Statements are evaluated one after another at depth zero. Resetting
     here keeps a session from inheriting a count that an error unwound
     past. *)
  c.depth <- 0

let interrupt_taken = Domain.DLS.new_key (fun () -> ref false)

(* Stretches where this domain must not unwind, however urgently the program
   is stopping, because something else depends on it reaching the end. Par's
   calling domain is the case: it is answering its workers' effects, and
   unwinding out of that leaves them blocked on a reply that never comes and
   never released. It takes the interrupt after they are joined. *)
let interrupts_deferred = Domain.DLS.new_key (fun () -> ref 0)

let defer_interrupts f =
  let d = Domain.DLS.get interrupts_deferred in
  incr d;
  Fun.protect ~finally:(fun () -> decr d) f

(* Forget a request that has been dealt with, for a session that carries on
   afterwards -- including this domain's record of having taken it, or the
   next request would be ignored here. A script has nothing to carry on to
   and never calls this. *)
let clear_interrupt () =
  Atomic.set interrupt_requested 0;
  Domain.DLS.get interrupt_taken := false

(* A worker that lost a race. `Par.race` sets this on the domains it is no
   longer waiting on, and the worker raises at its next checkpoint, so it
   releases what it holds on the way out.

   Domain-local and separate from the program-wide interrupt: one lost race
   is not the program stopping, and a loser must not look like Ctrl-C to
   anything else. *)
let cancelled : bool ref Domain.DLS.key = Domain.DLS.new_key (fun () -> ref false)

let cancel_this_domain flag = Domain.DLS.set cancelled flag

(* Races currently running. Nothing but a race cancels a domain, and it can
   only cancel one it is still waiting on, so while this is zero no domain
   is carrying a cancellation and there is nothing for the check below to
   find. Counted rather than flagged, because a thunk in a race may itself
   race. *)
let races_running = Atomic.make 0

let with_race_running f =
  Atomic.incr races_running;
  Fun.protect ~finally:(fun () -> Atomic.decr races_running) f

let check_interrupt_now () =
  (* Raised once, then cleared, for the reason the program-wide interrupt is
     taken once: a release is ordinary evaluation, and a flag that still
     stood would raise again on the first step of the cleanup, so nothing
     the loser held would be given back. *)
  let cancel = Domain.DLS.get cancelled in
  if !cancel then begin cancel := false; raise (Interrupted 0) end;
  let code = Atomic.get interrupt_requested in
  if code <> 0 && !(Domain.DLS.get interrupts_deferred) = 0 then begin
    let taken = Domain.DLS.get interrupt_taken in
    if not !taken then begin taken := true; raise (Interrupted code) end
  end

(* Every step of evaluation asks whether it should stop, and almost every
   time the answer is no. What that answer used to cost was two reads of
   domain-local state, which is a call and an indirection each -- more, on
   the shapes a script actually runs, than resolving all its names.

   Both reasons to stop are announced globally before any domain can see
   them, so two atomic loads decide it. Whatever they cannot rule out is
   left to the full check, which is unchanged. *)
let check_interrupt () =
  if Atomic.get interrupt_requested <> 0 || Atomic.get races_running <> 0 then
    check_interrupt_now ()

(* Structural comparison that a script can catch. OCaml's `=` and `compare`
   raise Invalid_argument when they reach a closure, and a value may carry one
   -- a function, a builtin, a compiled regex or a decoder, directly or inside
   a list, tuple, Map or constructor. That exception is not an EvalError, so
   `try` re-raises it and the interpreter dies with a fatal error on code that
   typechecked: `==`, `!=` and `List.sort` all admit function-typed operands.
   Turning it into an EvalError makes it a value the language can see. *)
let parse_dur_ms s =
  let n = String.length s in
  let i = ref 0 in
  let total = ref 0 in
  let at i prefix =
    let plen = String.length prefix in
    n >= i + plen && String.sub s i plen = prefix
  in
  (try
    while !i < n do
      let j = ref !i in
      while !j < n && s.[!j] >= '0' && s.[!j] <= '9' do incr j done;
      if !j = !i then raise Exit;
      (* A number past what an Int holds is not malformed, it is too big --
         and saying which is the difference between a reader checking their
         spelling and a reader checking their arithmetic. It used to escape
         as OCaml's own "int_of_string" and say neither. *)
      let digits = String.sub s !i (!j - !i) in
      let num =
        match int_of_string_opt digits with
        | Some v -> v
        | None ->
          raise (EvalError (Printf.sprintf
            "duration %S is too large: %s does not fit in an Int" s digits))
      in
      i := !j;
      (* Each unit's contribution and the running sum go through the checked
         arithmetic the rest of the evaluator uses. A duration whose total
         milliseconds overflow an Int used to wrap silently to a negative
         number that looked like an answer -- `9999999999999w` came back
         positive-looking nonsense. It is too big, not malformed, and says so.
         The factors are constants, so only `num * factor` and the sum can
         overflow; both are checked. *)
      let add_unit factor width =
        total := add_ovf !total (mul_ovf num factor); i := !i + width
      in
      if      at !i "min" then add_unit 60000 3
      else if at !i "ms"  then add_unit 1 2
      else if at !i "w"   then add_unit (7 * 24 * 3600000) 1
      else if at !i "d"   then add_unit (24 * 3600000) 1
      else if at !i "h"   then add_unit 3600000 1
      else if at !i "m"   then add_unit 60000 1
      else if at !i "s"   then add_unit 1000 1
      else raise Exit
    done
  with Exit -> raise (EvalError (Printf.sprintf "invalid duration: %S" s)));
  !total

(* ── The order wand gives a value ─────────────────────────────────────── *)

(* A size in bytes. A `KB` is 1000 bytes, not 1024: the spelling is the SI
   one, and SI says 1000. Reading it as 1024 would be lying about the unit
   the author wrote. Binary units would be `KiB`, which wand does not lex.

   A literal may carry a decimal (`1.5GB`), so the product is rounded to
   the nearest byte. `Int` holds 4.6 exabytes, and the largest literal the
   lexer accepts is far below that. *)
let size_bytes s =
  let n = String.length s in
  let i = ref 0 in
  while !i < n && (let c = s.[!i] in (c >= '0' && c <= '9') || c = '.') do incr i done;
  let number = float_of_string (String.sub s 0 !i) in
  let unit = String.sub s !i (n - !i) in
  let factor =
    match unit with
    | "B"  -> 1.0
    | "KB" -> 1e3
    | "MB" -> 1e6
    | "GB" -> 1e9
    | "TB" -> 1e12
    | "PB" -> 1e15
    | _ -> raise (EvalError (Printf.sprintf "invalid size: %S" s))
  in
  int_of_float (Float.round (number *. factor))

(* The readable spelling of a byte count: the largest unit that leaves at
   least one of it, to a tenth. `Size.of_bytes` answers exact bytes, so a
   sum of file sizes is a number nobody wants to read until it comes
   through here. Rounding can fill the unit -- 999_999 bytes is 1000.0KB --
   and that steps up rather than printing a thousand of something. A byte
   count below zero has no size to name, so it reads as `0B`. *)
let format_size_bytes n =
  let units = [| "B"; "KB"; "MB"; "GB"; "TB"; "PB" |] in
  let last = Array.length units - 1 in
  let rec pick i v = if i < last && v >= 1000.0 then pick (i + 1) (v /. 1000.0) else (i, v) in
  let i, v = pick 0 (float_of_int (max 0 n)) in
  let v = Float.round (v *. 10.0) /. 10.0 in
  let i, v = if i < last && v >= 1000.0 then (i + 1, v /. 1000.0) else (i, v) in
  let body =
    if Float.abs (v -. Float.round v) < 1e-9 then string_of_int (int_of_float (Float.round v))
    else Printf.sprintf "%.1f" v
  in
  body ^ units.(i)

let format_dur_ms ms =
  if ms = 0 then "0s"
  else
    let ms = abs ms in
    let buf = Buffer.create 16 in
    let add n unit =
      if n > 0 then (Buffer.add_string buf (string_of_int n); Buffer.add_string buf unit)
    in
    let rem = ref ms in
    let wk = !rem / (7*24*3600000) in rem := !rem mod (7*24*3600000);
    let dy = !rem / (24*3600000)   in rem := !rem mod (24*3600000);
    let hr = !rem / 3600000        in rem := !rem mod 3600000;
    let mn = !rem / 60000          in rem := !rem mod 60000;
    let sc = !rem / 1000           in rem := !rem mod 1000;
    let ml = !rem in
    add wk "w"; add dy "d"; add hr "h"; add mn "m"; add sc "s"; add ml "ms";
    Buffer.contents buf

(* An address as the 32-bit number it is, so `10.0.0.9` is below
   `10.0.0.10`. Text order says otherwise, which is the answer nobody
   wants. The lexer has already refused an octet above 255. *)
let ipv4_key s =
  List.fold_left (fun acc part -> acc * 256 + int_of_string part) 0
    (String.split_on_char '.' s)

(* The four octets back out of the number. *)
let ipv4_of_int n =
  Printf.sprintf "%d.%d.%d.%d"
    ((n lsr 24) land 0xFF) ((n lsr 16) land 0xFF) ((n lsr 8) land 0xFF) (n land 0xFF)

(* The prefix as a mask. `/0` is every address, and `0xFFFFFFFF lsl 32` is
   not 0 on a 63-bit int, so that case is written out rather than shifted. *)
let cidr_mask bits =
  if bits <= 0 then 0 else 0xFFFFFFFF land (0xFFFFFFFF lsl (32 - bits))

(* A network, keyed by where it starts and then how far it reaches. Compared
   as text, `10.0.0.0/8` sorted below `9.0.0.0/8` -- the same two addresses
   the other way round from what `IPv4` answers, which reads them as
   numbers. *)
let cidr_key s =
  match String.split_on_char '/' s with
  | [addr; bits] -> (ipv4_key addr, int_of_string bits)
  | _ -> raise (EvalError (Printf.sprintf "invalid CIDR: %S" s))

(* Whether a number is inside a network written as text. Used for the fixed
   ranges an address is asked about -- RFC 1918, loopback -- which are known
   here rather than passed in. *)
let in_cidr base bits n = cidr_mask bits land n = cidr_mask bits land ipv4_key base

(* Semver precedence. Numbers compare as numbers, so `1.10.0` is above
   `1.9.0`. A version with a prerelease is below the same version without
   one, and two prereleases compare identifier by identifier: a number
   against a number numerically, a number below a word, two words by their
   text, and if all of them match, the longer list wins.

   `Lexer.version_error` holds the grammar, so both spellings that used to
   reach here and are not semver -- a leading zero, and an empty prerelease
   identifier -- no longer exist by the time anything is compared. *)
(* Numbers, prerelease, build. Build metadata is taken off first: semver
   says it is ignored when determining precedence, so nothing below this
   point ever sees it -- and leaving it on would put a `+` in front of
   `int_of_string`, which is how `1.2.3+b` used to raise rather than
   compare. *)
let version_build s =
  match String.index_opt s '+' with
  | None -> (s, None)
  | Some i -> (String.sub s 0 i,
               Some (String.sub s (i + 1) (String.length s - i - 1)))

let version_parts s =
  let s, _build = version_build s in
  match String.index_opt s '-' with
  | None -> (s, None)
  | Some i -> (String.sub s 0 i,
               Some (String.sub s (i + 1) (String.length s - i - 1)))

(* One of the three numbers. Total: a value of this type has three, so the
   segment is there and is digits. *)
let version_number v i =
  match String.split_on_char '.' (fst (version_parts v)) with
  | segs -> (match List.nth_opt segs i with
             | Some seg -> (match int_of_string_opt seg with Some n -> n | None -> 0)
             | None -> 0)

let compare_prerelease a b =
  let ids t = String.split_on_char '.' t in
  let numeric t = t <> "" && String.for_all (fun c -> c >= '0' && c <= '9') t in
  let rec go xs ys =
    match xs, ys with
    | [], [] -> 0
    | [], _  -> -1          (* fewer identifiers is lower *)
    | _, []  -> 1
    | x :: xs, y :: ys ->
      let c =
        match numeric x, numeric y with
        | true, true   -> compare (int_of_string x) (int_of_string y)
        | true, false  -> -1
        | false, true  -> 1
        | false, false -> compare x y
      in
      if c <> 0 then c else go xs ys
  in
  go (ids a) (ids b)

let compare_versions a b =
  let (na, pa) = version_parts a and (nb, pb) = version_parts b in
  let nums t = List.map int_of_string (String.split_on_char '.' t) in
  let c = compare (nums na) (nums nb) in
  if c <> 0 then c
  else
    match pa, pb with
    | None,   None   -> 0
    | Some _, None   -> -1      (* a prerelease is below the release *)
    | None,   Some _ -> 1
    | Some x, Some y -> compare_prerelease x y

(* The form two paths are compared in: only the rewrites that hold whatever
   the disk contains. Repeated separators collapse, `.` segments go, and a
   trailing separator goes -- so `/a//b`, `/a/./b` and `/a/b/` are one path,
   and `./b` and `b` are one path, because they name one file against any
   working directory.

   What it does not do is what `Path.normalize` does: resolve `..`.
   `/a/c/../b` is `/a/b` only while `c` is not a symlink, and a comparison
   that answers "equal" about two files that may be different is worse than
   one that answers nothing. Case is not folded either -- whether `/A/b` and
   /a/b` are one file is a property of the filesystem, not of the text -- and
   a relative path is not made absolute, because that needs the working
   directory and a comparison operator must not perform an effect to answer.

   So `Path.normalize` keeps its own job and its own name. This is weaker on
   purpose. *)
let path_key s =
  let n = String.length s in
  (* Almost every path is already in this form, and a comparison is not the
     place to allocate for the ones that are. The scan answers without
     building anything; only a path that really has a `//`, a `.` segment or
     a trailing separator takes the second half. Sorting 20,000 paths cost
     two and a half times as much when every comparison rebuilt both. *)
  let already =
    n > 0
    && (n = 1 || s.[n - 1] <> '/')
    && (let ok = ref true in
        let i = ref 0 in
        while !ok && !i < n do
          let start = !i in
          while !i < n && s.[!i] <> '/' do incr i done;
          let len = !i - start in
          (* An empty segment is a repeated separator, except the one that
             opens an absolute path. A `.` segment is nothing, except when
             it is the whole path. *)
          if len = 0 && start <> 0 then ok := false
          else if len = 1 && s.[start] = '.' && n > 1 then ok := false;
          if !i < n then incr i
        done;
        !ok)
  in
  if already then s
  else
    let is_abs = n > 0 && s.[0] = '/' in
    let parts =
      String.split_on_char '/' s
      |> List.filter (fun p -> p <> "" && p <> ".")
    in
    let joined = String.concat "/" parts in
    if is_abs then "/" ^ joined
    (* A relative path that is all `.` segments is the directory it started
       in, and that is what `.` names. *)
    else if joined = "" then "."
    else joined

(* Comparing normalized values is the whole point: `90s` against `1min`, or
   one instant written two ways. `Date` and `Time` are fixed-width and
   zero-padded, so for those two the text order is already the right order.

   The `Ord` constraint refuses every other type, so the last case says
   what cannot arrive here rather than what to do about it. *)
let wand_order a b =
  match a, b with
  | VInt x,      VInt y      -> compare x y
  | VFloat x,    VFloat y    -> compare x y
  | VString x,   VString y   -> compare x y
  | VDuration x, VDuration y -> compare (parse_dur_ms x) (parse_dur_ms y)
  | VDateTime x, VDateTime y -> compare (datetime_epoch x) (datetime_epoch y)
  | VSize x,     VSize y     -> compare (size_bytes x) (size_bytes y)
  | VVersion x,  VVersion y  -> compare_versions x y
  | VPort x,     VPort y     -> compare x y
  | VIPv4 x,     VIPv4 y     -> compare (ipv4_key x) (ipv4_key y)
  | VCIDR x,     VCIDR y     -> compare (cidr_key x) (cidr_key y)
  | VPath x,     VPath y     -> compare (path_key x) (path_key y)
  | _ -> raise (EvalError "these values have no order")

(* Equality normalizes wherever ordering does, so the three relations agree.
   It compared the stored text before, which made `60s == 1min` false while
   `60s < 1min` and `60s > 1min` were both false as well: three answers that
   no reader can hold at once. *)
(* The types whose text is not their value, so equality and sorting have to
   read them rather than compare their spelling. `Port` is not here: it
   holds the number already. *)
let normalized = function
  | VDuration _ | VDateTime _ | VSize _ | VVersion _ | VIPv4 _ | VCIDR _
  | VPath _ -> true
  | _ -> false

(* A value that holds code. Two of these cannot be compared, and the walk
   has to say so itself: OCaml's `compare` raises only when it reaches the
   function inside, and two closures whose bodies already differ answer
   before it gets there. *)
let functional = function
  | VFun _ | VFix _ | VFixGroup _ | VBuiltin _ -> true
  | _ -> false

let rec wand_equal a b =
  match a, b with
  | _ when functional a || functional b ->
    raise (EvalError "cannot compare functions for equality")
  | _ when normalized a && normalized b ->
    (try wand_order a b = 0 with EvalError _ -> false)
  (* A value that normalizes can sit inside another value, so the walk goes
     in rather than stopping at the outside: `[60s] == [1min]` is the same
     question as `60s == 1min`. *)
  | VTuple xs, VTuple ys | VList xs, VList ys ->
    List.length xs = List.length ys && List.for_all2 wand_equal xs ys
  (* Two requests are equal when they ask for the same thing. The bound is
     an account of where they were written, not part of what they are. *)
  | VRequest (a, _), b | a, VRequest (b, _) -> wand_equal a b
  | VConstr (n1, xs), VConstr (n2, ys) ->
    n1 = n2 && List.length xs = List.length ys && List.for_all2 wand_equal xs ys
  (* Entry for entry in insertion order, which is what comparing the two
     association lists did before the representation changed. *)
  | VMap m1, VMap m2 ->
    let kvs1 = vmap_list m1 and kvs2 = vmap_list m2 in
    List.length kvs1 = List.length kvs2
    && List.for_all2 (fun (k1, v1) (k2, v2) -> k1 = k2 && wand_equal v1 v2)
         kvs1 kvs2
  | VRecord r1, VRecord r2 ->
    let kvs1 = r1.r_fields and kvs2 = r2.r_fields in
    List.length kvs1 = List.length kvs2
    && List.for_all2 (fun (k1, v1) (k2, v2) -> k1 = k2 && wand_equal v1 v2)
         kvs1 kvs2
  | _ ->
    (try a = b
     with Invalid_argument _ ->
       raise (EvalError "cannot compare functions for equality"))

(* A key that agrees with `wand_equal`: two values it calls equal always
   produce the same key. The reverse need not hold -- values that share a
   key are settled by `wand_equal` itself -- so a type whose equality is
   subtler than any cheap key can be (a `Version` and its prerelease) keys
   coarsely and still answers correctly.

   This is what keeps a membership test a hash lookup. Comparing each value
   against everything already seen would agree too, and would cost the
   square of the length. *)
let rec eq_key v =
  match v with
  (* Equality on a function is an error, but only once there is something to
     compare it against: a list of one is still a list of one. One key for
     all of them puts any two in the same bucket, so `wand_equal` is reached
     and raises whichever two they are -- rather than the answer depending
     on which values happened to share a bucket. A list holds one type, so
     nothing else can land here beside them. *)
  | VFun _ | VFix _ | VFixGroup _ | VBuiltin _ -> VUnit
  (* The types whose text is not their value, keyed by the value. *)
  | VDuration s -> VInt (parse_dur_ms s)
  | VDateTime s -> VInt (datetime_epoch s)
  | VIPv4 s     -> VInt (ipv4_key s)
  | VSize s     -> VInt (size_bytes s)
  | VPath s     -> VString (path_key s)
  (* The numbers only. Two equal versions have equal numbers, so this is a
     sound key; the prerelease is left for `wand_equal` to settle. *)
  | VVersion s ->
    (match String.split_on_char '.' (fst (version_parts s)) with
     | parts -> (try VList (List.map (fun p -> VInt (int_of_string p)) parts)
                 with Failure _ -> VString s))
  (* A value that normalizes can sit inside another one, so the walk goes
     in, exactly as `wand_equal` does. *)
  | VList xs        -> VList (List.map eq_key xs)
  | VTuple xs       -> VTuple (List.map eq_key xs)
  (* Keyed by what it asks for, so the bound cannot make two equal requests
     two different keys. *)
  | VRequest (inner, _) -> eq_key inner
  | VConstr (n, xs) -> VConstr (n, List.map eq_key xs)
  | VRecord r       -> VRecord (vrecord_make (List.map (fun (k, x) -> (k, eq_key x)) r.r_fields))
  | VMap m          -> VMap (vmap_map eq_key m)
  | v -> v


(* Two constructors of one type, by where they were declared. Falling back
   to the name covers a constructor that reached here without a declaration
   being read; within one list both come from one type, so the two never
   mix. *)
let compare_ctor c1 c2 =
  match Hashtbl.find_opt constr_index c1, Hashtbl.find_opt constr_index c2 with
  | Some i, Some j -> compare i j
  | _ -> compare (Ctor.name c1) (Ctor.name c2)

(* `List.sort` takes a list of any type, including types wand does not
   order, so it keeps structural comparison and reaches for `wand_order`
   only where wand defines one.

   The walk is written out rather than left to the runtime's own compare,
   which reads a constructor's name. It has to go all the way in: a
   constructor inside a tuple inside a list is still a constructor, and
   ordering it by name there would be the same defect one level down. *)
let rec wand_compare a b =
  match a, b with
  | _ when functional a || functional b ->
    raise (EvalError "cannot order functions")
  | _ when normalized a && normalized b -> wand_order a b
  | VConstr (c1, xs1), VConstr (c2, xs2) ->
    let c = compare_ctor c1 c2 in
    if c <> 0 then c else compare_each xs1 xs2
  | VList xs, VList ys | VTuple xs, VTuple ys -> compare_each xs ys
  | VMap m1, VMap m2 -> compare_pairs (vmap_list m1) (vmap_list m2)
  | VRecord r1, VRecord r2 ->
    compare_pairs r1.r_fields r2.r_fields
  | _ ->
    (try compare a b
     with Invalid_argument _ -> raise (EvalError "cannot order functions"))

(* Element by element, and a list that runs out first is the lesser -- the
   order the runtime's own compare gives a list, kept. *)
and compare_each xs ys =
  match xs, ys with
  | [], []             -> 0
  | [], _              -> -1
  | _, []              -> 1
  | x :: xs, y :: ys   ->
    let c = wand_compare x y in
    if c <> 0 then c else compare_each xs ys

and compare_pairs kvs1 kvs2 =
  match kvs1, kvs2 with
  | [], []                        -> 0
  | [], _                         -> -1
  | _, []                         -> 1
  | (k1, v1) :: r1, (k2, v2) :: r2 ->
    let c = compare k1 k2 in
    if c <> 0 then c
    else
      let c = wand_compare v1 v2 in
      if c <> 0 then c else compare_pairs r1 r2

(* ── Pattern matching ─────────────────────────────────────────────────────── *)

(* Whether a pattern list and a value list have the same length, decided by
   walking them together rather than measuring each. Measuring meant that
   testing `[]` against a list looked at every element of it, so each step of
   a recursive list function cost the length of what remained and traversing
   a list of n elements cost O(n^2). *)
let rec same_length ps vs =
  match ps, vs with
  | [], []           -> true
  | _ :: ps, _ :: vs -> same_length ps vs
  | _                -> false

(* How many observers are watching effects: user handlers currently in scope,
   plus one while a rehearsal or trace is running. Par consults this to decide
   whether a worker may perform its own effects. *)
let observers = Atomic.make 0

let observed f =
  ignore (Atomic.fetch_and_add observers 1);
  Fun.protect ~finally:(fun () -> ignore (Atomic.fetch_and_add observers (-1))) f

(* The observers that are a `handle` written in wand, which is the subset a
   rehearsal and a trace are not. `Par.timeout` consults this: a rehearsal
   collapsing a race still reports what the work would do, where a handler
   collapsing it takes the deadline away and says nothing. *)
let handlers = Atomic.make 0

let handled f =
  ignore (Atomic.fetch_and_add handlers 1);
  Fun.protect ~finally:(fun () -> ignore (Atomic.fetch_and_add handlers (-1))) f

(* Installs the runtime's own handlers. Set by the runner, which owns them. *)
let with_default_handler : ((unit -> value) -> value) ref = ref (fun f -> f ())

(* In a match arm, a list pattern states the whole shape: `[a, b]` is a
   two-element list and nothing else, because the arms discriminate and a
   longer list belongs to another arm. A destructuring `let` has no other
   arm -- it only binds -- so there `[a, b]` names the leading elements and
   whatever follows is ignored, the way a map pattern binds the keys it
   names and ignores the rest. [prefix] selects the binding reading. *)
(* The constructor a pattern names, whatever form it takes. *)
let pat_ctor_name (p : pat) =
  match p with
  | PConstr (n, _) | PConstrNamed (n, _) | PConstrBare (n, _) -> n
  | _ -> ""

(* Which constructor a name means here. A name bound in scope wins: that is
   how a renamed import, and a type's own name where it has one constructor,
   reach the constructor they stand for. *)
let ctor_in_scope env name =
  match lookup_var name env with
  | Some (VConstr (c, _)) | Some (VPartialConstr (c, _, _)) -> c
  | _ -> ctor_named name

let rec try_match ?(prefix = false) (p : pat) v (env : env) : env option =
  match p, v with
  | PVar name, v          -> Some ((name, v) :: env)
  | Wild, _               -> Some env
  | Int n,    VInt m      when n = m -> Some env
  | Float f,  VFloat g    when f = g -> Some env
  | String s, VString t   when s = t -> Some env
  | Bool b,   VBool c     when b = c -> Some env
  | Unit,     VUnit                  -> Some env
  | PTuple ps, VTuple vs when same_length ps vs ->
    List.fold_left2
      (fun acc p v -> match acc with
        | None     -> None
        | Some env -> try_match ~prefix p v env)
      (Some env) ps vs
  | PList ps, VList vs ->
    let rec go acc ps vs =
      match acc, ps, vs with
      | None, _, _                 -> None
      | Some env, [], []           -> Some env
      | Some env, [], _ :: _       -> if prefix then Some env else None
      | Some _, _ :: _, []         -> None
      | Some env, p :: ps, v :: vs -> go (try_match ~prefix p v env) ps vs
    in
    go (Some env) ps vs
  (* A type annotation says nothing about which values match: it is checked
     before the program runs, and the pattern under it does the matching. *)
  | PAnnot (p, _), v -> try_match ~prefix p v env
  | PCons (hp, tp), VList (v :: vs) ->
    (match try_match ~prefix hp v env with
     | None      -> None
     | Some env' -> try_match ~prefix tp (VList vs) env')
  | PCons _, VList [] -> None
  | PTuple ps, VConstr (_, vals) when same_length ps vals ->
    List.fold_left2
      (fun acc p v -> match acc with
        | None     -> None
        | Some env -> try_match ~prefix p v env)
      (Some env) ps vals
  | PConstr (name, pats), VConstr (vname, vals)
    when Ctor.equal (ctor_in_scope env name) vname && same_length pats vals ->
    List.fold_left2
      (fun acc p v -> match acc with
        | None     -> None
        | Some env -> try_match ~prefix p v env)
      (Some env) pats vals
  (* The declaration decides which of the two readings this is, exactly as
     it does when the pattern is checked. *)
  (* `one.Live`: the module's namespace holds the constructor, so its
     identity comes from there. The pattern under the qualifier then matches
     the value as any other would. *)
  (* `one.Live`: the module's namespace holds the constructor, so its identity
     comes from there. What is under the qualifier is matched against the
     value's fields directly -- resolving its bare name again would consult
     the index, where one name holds one constructor and another module may
     have registered it. *)
  | PQualified (m, inner), VConstr (vc, vals) ->
    let cname = pat_ctor_name inner in
    let owner =
      match lookup_var m env with
      | Some (VRecord vr_) -> let kvs = vr_.r_fields in
        (match List.assoc_opt cname kvs with
         | Some (VConstr (c, _)) | Some (VPartialConstr (c, _, _)) -> Some c
         (* A module's alias is not a value in its record -- a record
            constructor never is -- so the module hands back nothing under
            that name. The identity was registered when the alias was read,
            the same fallback construction takes, so `M.Response(...)`
            matches where `M.Response(...)` builds. *)
         | _ -> Hashtbl.find_opt ctor_of_name cname)
      | _ -> None
    in
    (match owner with
     | Some c when Ctor.equal c vc ->
       let fields () = match Hashtbl.find_opt constr_fields vc with
         | Some names -> names
         | None -> []
       in
       let named bindings =
         List.fold_left (fun acc (fname, p) ->
           match acc with
           | None -> None
           | Some env ->
             (match find_field_index (fields ()) fname with
              | None -> None
              | Some i ->
                (match List.nth_opt vals i with
                 | None -> None
                 | Some v -> try_match ~prefix p v env))) (Some env) bindings
       in
       (match inner with
        | PConstr (_, pats) when same_length pats vals ->
          List.fold_left2 (fun acc p v ->
            match acc with
            | None -> None
            | Some env -> try_match ~prefix p v env) (Some env) pats vals
        | PConstr (_, _) -> None
        | PConstrNamed (_, bindings) -> named bindings
        | PConstrBare (_, ids) ->
          let named_fields = List.exists (fun dn -> dn <> None) (fields ()) in
          (match Ast.constr_bare_reading ~named_fields "" ids with
           | PConstrNamed (_, bindings) -> named bindings
           | PConstr (_, pats) when same_length pats vals ->
             List.fold_left2 (fun acc p v ->
               match acc with
               | None -> None
               | Some env -> try_match ~prefix p v env) (Some env) pats vals
           | _ -> None)
        | _ -> None)
     | _ -> None)
  | PQualified (_, _), _ -> None
  | PConstrBare (name, ids), _ ->
    let named_fields =
      match Hashtbl.find_opt constr_fields (ctor_in_scope env name) with
      | Some fields -> List.exists (fun dn -> dn <> None) fields
      | None -> false
    in
    try_match ~prefix (Ast.constr_bare_reading ~named_fields name ids) v env
  | PConstrNamed (name, bindings), VConstr (vname, vals)
    when Ctor.equal (ctor_in_scope env name) vname ->
    (match Hashtbl.find_opt constr_fields vname with
     | None -> None
     | Some field_names ->
       List.fold_left (fun acc (fname, p) ->
         match acc with
         | None -> None
         | Some env ->
           (match find_field_index field_names fname with
            | None -> None
            | Some i ->
              (match List.nth_opt vals i with
               | None -> None
               | Some v -> try_match ~prefix p v env))
       ) (Some env) bindings)
  | PMap bindings, (VMap _ | VRecord _) ->
    let kvs = (match v with
      | VMap m      -> vmap_list m
      | VRecord r    -> r.r_fields
      | _           -> []) in
    List.fold_left (fun acc (key, p) ->
      match acc with
      | None -> None
      | Some env ->
        (match List.assoc_opt key kvs with
         | None   -> None
         | Some v -> try_match ~prefix p v env)
    ) (Some env) bindings
  | _ -> None

(* ── Evaluation ───────────────────────────────────────────────────────────── *)

(* One shell argument, whatever the value contains.

   Single quotes are the only quoting `sh` treats as absolutely literal:
   inside them a space does not split, a `*` does not expand, and `;`, `|`,
   backticks and `$(...)` are text. The one character they cannot carry is
   `'` itself, which is closed, escaped and reopened -- the standard
   `'\''` dance.

   An empty value becomes `''`, which is one empty argument rather than no
   argument at all. That is the difference between `rm ''` and `rm`. *)
let shell_quote v =
  let buf = Buffer.create (String.length v + 2) in
  Buffer.add_char buf '\'';
  String.iter
    (fun c -> if c = '\'' then Buffer.add_string buf "'\\''" else Buffer.add_char buf c)
    v;
  Buffer.add_char buf '\'';
  Buffer.contents buf

(* The same value where the author already opened a quote of their own.
   Wrapping it in single quotes there would quote nothing -- inside `"..."`
   a single quote is an ordinary character, so `$(echo "hi %{name}")` put
   the value straight into text the shell still expands, and a name holding
   `$(...)` ran. The value is escaped for the quote it lands in instead,
   which keeps the author's word one word and leaves nothing for the shell
   to read.

   Inside single quotes only the quote itself can end the span: it is closed,
   the character escaped outside, and the span reopened. Inside double
   quotes the four the shell still reads there take a backslash. *)
let quote_within q v =
  let buf = Buffer.create (String.length v + 8) in
  String.iter (fun c ->
    if q = '\'' then
      (if c = '\'' then Buffer.add_string buf "'\\''" else Buffer.add_char buf c)
    else begin
      if c = '"' || c = '\\' || c = '$' || c = '`' then Buffer.add_char buf '\\';
      Buffer.add_char buf c
    end) v;
  Buffer.contents buf

(* Deriving a decoder needs the decoding machinery, which is defined further
   down the file; `eval` only has to be able to reach it. *)
let derive_decoder : (string -> value) ref =
  ref (fun _ -> raise (EvalError "decoder derivation is not wired up"))

let derive_encoder : (string -> value) ref =
  ref (fun _ -> raise (EvalError "encoder derivation is not wired up"))

let derive_usage : (string -> value) ref =
  ref (fun _ -> raise (EvalError "usage derivation is not wired up"))

let derive_spec : (string -> value) ref =
  ref (fun _ -> raise (EvalError "spec derivation is not wired up"))

let derive_parser : (string -> value) ref =
  ref (fun _ -> VUnit)

let derive_reader : (string -> value) ref =
  ref (fun _ -> raise (EvalError "reader derivation is not wired up"))

(* A request is checked where it is built, against the manifest of the file
   that built it. That is earlier than the send, and better placed: the
   error lands on the line that named the host, which is the line the
   manifest is about. A URL the run decided is checked here too, because the
   URL is a field and the field has a value by now.

   `None` means the file declared bare `Net` or no manifest at all, and
   nothing is checked -- the same reading `ambient_shell_allow` gives. *)
(* The bound to check a request against: the one on the URL if it has one,
   and the construction's otherwise.

   The URL comes first because it is the part the caller wrote.
   `HTTP.get https://api.example.com/x` builds its request inside the
   standard library, whose manifest is bare `Net`, so the construction
   carries nothing and only the literal can say whose manifest applies. A
   request the caller builds itself has the same bound on both. *)
let request_bound allow value =
  match value, allow with
  | VConstr (_, VURL (_, (Some _ as from_url)) :: _), _ -> from_url
  | _, Some _ -> allow
  | _, None -> Domain.DLS.get ambient_file_net

let check_request_host allow value =
  match request_bound allow value with
  | None -> ()
  | Some allow_list ->
    (match value with
     | VConstr (_, (VURL (u, _) | VString u) :: _) ->
       let host = host_of_url u in
       if not (Narrow.allowed ~rule:Narrow.host ~allow:allow_list host) then begin
         raise (EvalError (Printf.sprintf
           "this request reaches '%s', which %s does not allow"
           host
           (Shell_scan.render_label ("Net", Some allow_list))))
       end
     | _ -> ())

let rec eval (env : env) (e : expr) : value = eval_at false env e

(* Evaluate in tail position: the value of `e` is the value of whatever
   called us, so no frame here has any work left to do. *)
and eval_tail (env : env) (e : expr) : value = eval_at true env e

(* An argument that is a nullary constructor holding a bracket it cannot
   own -- `Sha256 (x)` where the call meant `Sha256` and `x`. Answers the
   constructor and the argument it swallowed, keeping the qualifier, so the
   rebuilt call resolves the constructor exactly as the original would
   have. Nullary-ness is read off the value the name resolves to, which is
   how `Constr` is evaluated a few lines below. *)
and nullary_payload env x =
  (* A qualified constructor is looked up through its module's record, the
     way the `Qualified` case below looks it up. Reading the bare name out
     of the calling scope answers "not a constructor" and leaves the
     evaluator disagreeing with the checker. *)
  let is_nullary qual e =
    let found =
      match qual, strip_located e with
      | None, Constr name -> lookup_var name env
      | Some m, Constr name ->
        (match lookup_var m env with
         | Some (VRecord r) -> vrecord_get name r
         | _ -> None)
      | _ -> None
    in
    match found with Some (VConstr (_, [])) -> true | _ -> false
  in
  let rec unpack qual wrap e =
    match e with
    | Located (_, inner) -> unpack qual wrap inner
    | Qualified (m, inner) ->
      unpack (Some m) (fun c -> wrap (Qualified (m, c))) inner
    | App (f, arg) when is_nullary qual f -> Some (wrap f, arg)
    | _ -> None
  in
  unpack None (fun c -> c) x

and eval_at (tail : bool) (env : env) (e : expr) : value =
  check_interrupt ();
  match e with
  | Int n      -> VInt n
  | Float f    -> VFloat f
  | String s   -> VString s
  | Bool b     -> VBool b
  | Unit       -> VUnit
  | Path s     -> VPath s
  | Glob s     -> VGlob s
  | DateTime s -> VDateTime s
  | Duration s -> VDuration s
  | URL (s, allow) -> VURL (s, allow)
  | IPv4 s     -> VIPv4 s
  | CIDR s     -> VCIDR s
  | Port n     -> VPort n
  | Version s  -> VVersion s
  | Size s     -> VSize s
  | Var name ->
    (match lookup_var name env with
     | Some v -> v
     | None   ->
       raise (EvalError (Printf.sprintf "unbound variable '%s'%s"
         name (Util.hint name (env_names env)))))
  | Constr name ->
    (match lookup_var name env with
     | Some v -> v
     | None   ->
       (* A built-in constructor that carries nothing -- `GET` and the other
          methods -- is a value wherever it is named. It is not in the
          environment, because that holds what a file declared or imported,
          and nothing imports the language's own. *)
       (match Hashtbl.find_opt ctor_of_name name with
        | Some c when Hashtbl.find_opt constr_fields c = Some [] ->
          VConstr (c, [])
        | _ ->
          raise (EvalError (Printf.sprintf "unknown constructor '%s'%s"
            name (Util.hint name (env_names env))))))
  | EnvVar name ->
    (match Sys.getenv_opt name with
     | Some v -> VString v
     | None   -> raise (EvalError (Printf.sprintf
         "environment variable '%s' is not set" name)))
  | Hole ->
    raise (EvalError "cannot evaluate a hole")
  | UnOp ("-", e) ->
    (match eval env e with
     | VInt n   -> VInt (neg_ovf n)
     | VFloat f -> VFloat (-.f)
     | _        -> raise (EvalError "'-' requires a number"))
  | UnOp ("!", e) ->
    (match eval env e with
     | VBool b -> VBool (not b)
     | _       -> raise (EvalError "'!' requires a bool"))
  | UnOp (op, _) ->
    raise (EvalError (Printf.sprintf "unknown operator '%s'" op))
  | BinOp (op, a, b) -> eval_binop env op a b
  | Fn (params, body) -> VFun (env, params, body)
  | App (f, x) when nullary_payload env x <> None ->
    (* The other half of the typechecker's rule: a bracket after a
       constructor is its payload, and one that takes no payload hands the
       bracket back to the call around it. Both sides have to agree, or a
       file typechecks and then fails at run time -- which is what happened
       when only the checker knew. `test_typechecker` runs what it checks,
       so the two are pinned together there. *)
    (match nullary_payload env x with
     | Some (ctor, arg) -> eval_at tail env (App (App (f, ctor), arg))
     | None -> assert false)
  | App (f, x) ->
    let vf = eval env f in
    let vx = eval env x in
    if tail then apply_tail vf vx else apply vf vx
  | Let (p, e1, e2, _) ->
    let v1 = eval env e1 in
    let v1 = match p, v1 with
      | PVar name, VFun (fenv, params, body) ->
        VFix (name, fenv, params, body)
      | _ -> v1
    in
    eval_at tail (bind_pat ~prefix:true p v1 env) e2
  | LetRec (bindings, e2, _) ->
    let env' = List.fold_left (fun acc (name, _, _) ->
      (name, VFixGroup (bindings, env, name)) :: acc) env bindings in
    eval_at tail env' e2
  | If (cond, then_, else_) ->
    (match eval env cond with
     | VBool true  -> eval_at tail env then_
     | VBool false -> eval_at tail env else_
     | _           -> raise (EvalError "if condition must be a bool"))
  | Match (scrutinee, cases) ->
    let sv = eval env scrutinee in
    eval_match tail env sv cases
  | Tuple es  -> VTuple (List.map (eval env) es)
  | List es   -> VList  (List.map (eval env) es)
  (* `Foo.Live`: the module's namespace holds its constructors, so the
     identity comes from there rather than from the bare-name index. *)
  | Qualified (m, inner) ->
    let from_module name =
      match lookup_var m env with
      | Some (VRecord vr_) -> let kvs = vr_.r_fields in
        (match List.assoc_opt name kvs with
         | Some (VConstr (c, _)) | Some (VPartialConstr (c, _, _)) -> Some c
         | _ -> None)
      | _ -> None
    in
    let with_ident name k =
      match from_module name with
      | Some c -> k c
      | None ->
        (* A module's alias is not a value in its record -- a record
           constructor never is -- so the module has nothing to hand back
           under that name. The identity was registered when the alias was
           read, and the typechecker has already settled that this name
           belongs to this module, so the table is the right place to
           finish. *)
        (match Hashtbl.find_opt ctor_of_name name with
         | Some c -> k c
         | None ->
           raise (EvalError (Printf.sprintf
             "'%s' has no constructor '%s'" m name)))
    in
    (* The constructor after the dot carries its own extent now, so the
       shapes below are read through it. *)
    (match strip_located inner with
     | Constr name -> with_ident name (fun c ->
         match Hashtbl.find_opt constr_fields c with
         | Some (_ :: _ as fs) -> VPartialConstr (c, List.length fs, [])
         | _ -> VConstr (c, []))
     | ConstrApp (name, fields, allow) ->
       with_ident name (fun c -> eval_constr_app env c fields allow)
     | ConstrBare (name, ids) ->
       with_ident name (fun c ->
         let named_fields =
           match Hashtbl.find_opt constr_fields c with
           | Some fields -> List.exists (fun dn -> dn <> None) fields
           | None -> false
         in
         match Ast.constr_bare_construction ~named_fields name ids with
         | ConstrApp (_, fields, allow) -> eval_constr_app env c fields allow
         | other -> eval env other)
     | App (f, arg) ->
       (* `Foo.Some 3`: the constructor, then what it is applied to. *)
       apply (eval env (Qualified (m, f))) (eval env arg)
     | other -> eval env other)
  | ConstrBare (name, ids) ->
    let named_fields =
      match Hashtbl.find_opt constr_fields (ctor_named name) with
      | Some fields -> List.exists (fun dn -> dn <> None) fields
      | None -> false
    in
    eval env (Ast.constr_bare_construction ~named_fields name ids)
  | ConstrApp (name, fields, allow) ->
    eval_constr_app env (ctor_in_scope env name) fields allow
  | ConstrUpdate (name, base, fields, allow) ->
    let replacements = List.map (fun (fname, e) -> (fname, eval env e)) fields in
    (match eval env base, Hashtbl.find_opt constr_fields (ctor_named name) with
     (* Updating a request answers a request, and the bound is the one on the
        file that wrote the update -- it is the file that chose the new
        URL. *)
     | VRequest (VConstr (_, values), _), Some field_names
     | VConstr (_, values), Some field_names ->
       let c = ctor_named name in
       let built =
         VConstr (c, List.map2 (fun fname_opt v ->
           match fname_opt with
           | Some fn -> (match List.assoc_opt fn replacements with
                         | Some v' -> v'
                         | None -> v)
           | None -> v) field_names values)
       in
       (* An update may name a different host, so it is checked like a
          construction -- against the manifest of the file the update was
          written in, which is the file that chose the new URL. *)
       if c = Ctor.Builtin "HTTPRequest" then begin
         check_request_host allow built;
         VRequest (built, allow)
       end else built
     | _ -> raise (EvalError (Printf.sprintf
         "'%s' cannot be updated: it has no named fields" name)))
  | MapLit kvs ->
    VMap (vmap_of_list (List.map (fun (k, e) -> (k, eval env e)) kvs))
  | Field (e, label) ->
    (* A type's derived decoder: `Pod.decoder`. Resolved from the type's own
       definition rather than bound as a value, so it costs nothing until it
       is named and a recursive type can still have one. *)
    (* `Apps.Deployment.decoder`: the same member, reached through the module
       that declares the type. The table holds one entry per short name, so
       two modules that each declare a `Deployment` share it -- the key that
       tells them apart is the declaring module's path, which the module's
       own constructor carries. *)
    let qualified_key m inner =
      match strip_located inner with
      | Constr tname ->
        let owned =
          match lookup_var m env with
          | Some (VRecord vr) ->
            (match vrecord_get tname vr with
             | Some (VConstr (c, _)) | Some (VPartialConstr (c, _, _)) ->
               (match Ctor.modul c with
                | Some path -> Some (Module_types.canonical_type ~modul:path tname)
                | None -> None)
             | _ -> None)
          | _ -> None
        in
        (match owned with
         | Some k when Hashtbl.mem derivable k -> Some k
         | _ -> if Hashtbl.mem derivable tname then Some tname else None)
      | _ -> None
    in
    (match strip_located e, label with
     | Qualified (m, inner), ("decoder" | "encoder" | "usage" | "parser")
       when qualified_key m inner <> None ->
       let k = Option.get (qualified_key m inner) in
       (match label with
        | "decoder" -> !derive_decoder k
        | "encoder" -> !derive_encoder k
        | "usage"   -> !derive_usage k
        | _         -> !derive_parser k)
     | Constr tname, "decoder" when Hashtbl.mem derivable tname ->
       !derive_decoder tname
     | Constr tname, "encoder" when Hashtbl.mem derivable tname ->
       !derive_encoder tname
     | Constr tname, "usage" when Hashtbl.mem derivable tname ->
       !derive_usage tname
     | Constr tname, "parser" when Hashtbl.mem derivable tname ->
       !derive_parser tname
     | _ ->
    (* No VMap case: dot access on a Map is rejected by the typechecker.
       VRecord is how imported module namespaces are reached (FS.cwd). *)
    (match eval env e with
     | VRecord r ->
       (match vrecord_get label r with
        | Some v -> v
        | None   ->
          raise (EvalError (Printf.sprintf "no field '%s'%s"
            label (Util.hint label (List.map fst r.r_fields)))))
     (* A request reads as the record it wraps. *)
     | VRequest (VConstr (name, vals), _)
     | VConstr (name, vals) ->
       (match Hashtbl.find_opt constr_fields name with
        | Some names ->
          (match find_field_index names label with
           | Some i ->
             (match List.nth_opt vals i with
              | Some v -> v
              | None   -> raise (EvalError (Printf.sprintf
                  "constructor '%s' is not fully applied" (Ctor.name name))))
           | None -> raise (EvalError (Printf.sprintf
               "constructor '%s' has no field named '%s'" (Ctor.name name) label)))
        | None -> raise (EvalError (Printf.sprintf
            "constructor '%s' has no named fields" (Ctor.name name))))
     | _ -> raise (EvalError "field access on non-record")))
  | Seq (a, b) ->
    ignore (eval env a); eval_at tail env b
  | ImportExpr _ ->
    raise (EvalError "import expressions must be handled by the runner")
  | RegexLit (pat, flags) ->
    let opts = String.to_seq flags |> Seq.flat_map (function
      | 'i' -> List.to_seq [`CASELESS]
      | 'm' -> List.to_seq [`MULTILINE]
      | 's' -> List.to_seq [`DOTALL]
      | _   -> List.to_seq []) |> List.of_seq
    in
    let cache = Domain.DLS.get regex_literals in
    (match Hashtbl.find_opt cache (pat, flags) with
     | Some re -> VRegex re
     | None ->
       (match regex_repeat_error pat with
        | Some why -> raise (EvalError why)
        | None ->
          (try
             let re = Re.compile (Re.Pcre.re ~flags:opts pat) in
             Hashtbl.replace cache (pat, flags) re;
             VRegex re
           with Re.Pcre.Parse_error ->
             raise (EvalError (Printf.sprintf "invalid regex: r/%s/%s" pat flags)))))
  (* `$*(c)` builds the command and stops there. `$(c)` and `$?(c)` build
     the same command and run it -- they are `Shell.run!` and `Shell.query`
     over one, spelled short. *)
  | MkCommand (e, allow) ->
    VCommand (command_line env e allow "$*(…)", allow)
  | RunCmd (e, allow) ->
    let cmd = command_line env e allow "$(…)" in
    perform_shell "Shell!run" allow (VString cmd)
  | RunQuery (e, allow) ->
    let cmd = command_line env e allow "$?(…)" in
    perform_shell "Shell!capture" allow (VString cmd)
  | Handle (body_expr, cases) ->
    let effect_cases = List.filter_map (function
      | Ast.EffectCase (n, p, k, b) -> Some (n, p, k, b)
      | _ -> None) cases in
    let return_case = List.find_opt (function
      | Ast.ReturnCase _ -> true | _ -> false) cases in
    let apply_return v =
      match return_case with
      | None -> v
      | Some (Ast.ReturnCase (p, b)) -> eval (bind_pat p v env) b
      | Some (Ast.EffectCase _) -> assert false
    in
    observed (fun () -> handled (fun () ->
    Effect.Deep.match_with (fun () -> eval env body_expr) ()
      { Effect.Deep.
          retc = apply_return;
          exnc = raise;
          effc = fun (type a) (eff : a Effect.t) ->
            match eff with
            | WandEffect (op, arg) ->
              let rec try_cases = function
                | [] -> (None : ((a, value) Effect.Deep.continuation -> value) option)
                | (name, arg_pat, cont_name, case_body) :: rest ->
                  if name <> op then try_cases rest
                  else
                    match try_match arg_pat arg env with
                    | None -> try_cases rest
                    | Some env' ->
                      Some (fun (k : (a, value) Effect.Deep.continuation) ->
                        (* A case that answers without resuming ends the body
                           it was handling. The body may be holding something
                           that has to be given back -- a lock, a temp file --
                           and a continuation that is simply dropped runs no
                           cleanup at all, not even when it is collected. So
                           the abandoned region is unwound deliberately.

                           Cleanup runs here, inside this case, which is what
                           makes it visible: a release that performs an effect
                           of its own reaches the same handlers the acquiring
                           code saw, rather than whatever happens to be
                           installed later. Discontinuing returns the value
                           the unwinding produced; it is discarded, and the
                           case answers with its own. *)
                        let resumed = ref false in
                        let cont =
                          VBuiltin (fun v ->
                            resumed := true;
                            Effect.Deep.continue k v)
                        in
                        let answer = eval ((cont_name, cont) :: env') case_body in
                        (* Resuming consumes the continuation, so only an case
                           that never did has one left to discontinue.

                           Measured against OCaml itself, four ways, because
                           the obvious one is wrong: an case that *resumes*
                           runs cleanup and keeps its value; one that *drops*
                           the continuation runs no cleanup at all, not even
                           after a full GC; one that *discontinues* runs
                           cleanup but unwinds past the case, losing its
                           value; and one that discontinues and catches --
                           this -- gets both, because `discontinue` returns
                           to the case rather than transferring away from
                           it. *)
                        if not !resumed then
                          (try ignore (Effect.Deep.discontinue k Abandoned)
                           with e when is_abandoned e -> ());
                        answer)
              in
              try_cases effect_cases
            | _ -> None
      }))
  | RawString s -> VString s
  | Interp (parts, tail) | RawInterp (parts, tail) ->
    let buf = Buffer.create 32 in
    List.iter (fun (lit, e) ->
      Buffer.add_string buf lit;
      Buffer.add_string buf (to_text (eval env e))
    ) parts;
    Buffer.add_string buf tail;
    VString (Buffer.contents buf)
  | CmdInterp (parts, tail) ->
    let buf = Buffer.create 32 in
    List.iter (fun (lit, e, h) ->
      Buffer.add_string buf lit;
      let v = to_text (eval env e) in
      Buffer.add_string buf
        (match (h : Token.hole) with
         | Token.Source    -> v
         | Token.Arg       -> shell_quote v
         | Token.Inside q  -> quote_within q v)
    ) parts;
    Buffer.add_string buf tail;
    VString (Buffer.contents buf)
  | Contract (reqs, ens, body) ->
    List.iter (fun req ->
      match eval env req with
      | VBool true  -> ()
      | VBool false -> raise (EvalError (Printf.sprintf
          "precondition failed: %s" (Ast.show req)))
      | _ -> assert false
    ) reqs;
    let v = eval env body in
    List.iter (fun e ->
      let env' = ("result", v) :: env in
      match eval env' e with
      | VBool true  -> ()
      | VBool false -> raise (EvalError (Printf.sprintf
          "postcondition failed: %s" (Ast.show e)))
      | _ -> assert false
    ) ens;
    v
  | Try e ->
    let c = loc_cell () in
    let line = c.at_line and col = c.at_col in
    let restore v = c.at_line <- line; c.at_col <- col; v in
    restore @@
    Effect.Deep.match_with (fun () -> eval env e) ()
      { Effect.Deep.
          retc = (fun v -> VConstr (Ctor.Builtin "Ok", [v]));
          exnc = (function
            | EvalError msg -> VConstr (Ctor.Builtin "Error", [VString (Util.strip_loc_prefix msg)])
            | Failure  msg  -> VConstr (Ctor.Builtin "Error", [VString (Util.strip_loc_prefix msg)])
            | exn           -> raise exn);
          effc = fun (type a) (_ : a Effect.t) ->
            (None : ((a, value) Effect.Deep.continuation -> value) option) }
  | With (resource, p, body) ->
    (* Acquire, run, release -- and release however the body leaves: a
       value, a raise, or a handler that answered without resuming and
       unwound it. Fun.protect covers all three, the last because an
       abandoned region is torn down deliberately rather than dropped. *)
    (match eval env resource with
     | VResource (acquire, release) ->
       (* An acquire runs to the end for the same reason a release does. The
          resource becomes real partway through it -- the file exists, the
          lock is taken -- and only the value it returns lets the release
          reach that resource. An interrupt taken between those two points
          leaves something held that nothing can now give back, which is the
          leak this bracket exists to prevent. Deferred, the interrupt is
          taken on the first step of the body instead, with the release
          already installed. *)
       let held = defer_interrupts (fun () -> apply acquire VUnit) in
       Fun.protect
         (* A release runs to the end even while the program is stopping.
            It is ordinary evaluation, so without this the interrupt would
            land in the middle of the cleanup it triggered and leave the
            resource half-released -- which is worse than not having tried.
            Someone who asks twice gets an immediate stop from the signal
            handler; that is the way out of a release that hangs. *)
         ~finally:(fun () -> defer_interrupts (fun () -> ignore (apply release held)))
         (fun () -> eval (bind_pat p held env) body)
     | other ->
       raise (EvalError (Printf.sprintf
         "with expects a resource, got %s" (show_value other))))
  | Annot (_, e) -> eval_at tail env e
  | Located (loc, e) ->
    let c = loc_cell () in
    if tail then (mark_loc c loc; eval_tail env e)
    else begin
      let line = c.at_line and col = c.at_col in
      mark_loc c loc;
      let v = eval env e in
      (* Only on the way out through a value. An error on its way past wants
         the position it was raised at, not the one being returned to. *)
      c.at_line <- line;
      c.at_col <- col;
      v
    end

(* Build a record constructor's value from the fields a construction names.
   Takes the identity rather than the name, so a qualified construction can
   say which module's constructor it means. *)
(* The command line a command form names, resolved and checked. Every one of
   the three forms comes through here, so the word check and the effect are
   keyed off the literal wherever it stands -- `$*(git ...)` in a file that
   never runs it is the same site to a manifest as `$(git ...)`.

   The operation answers with the line to use, so a handler may substitute
   one; what it may not do is make a `Command` out of thin air, because
   wrapping the answer happens here rather than in wand. *)
and command_line env e allow form : string =
  let cmd = match eval env e with
    | VString s -> s
    | _ -> raise (EvalError (form ^ " requires a string"))
  in
  match perform_shell "Shell!command" allow (VString cmd) with
  | VString resolved -> resolved
  | v -> raise (EvalError (Printf.sprintf
      "a handler for Shell!command answered with %s, and a command is a        String" (show_value v)))

and eval_constr_app env c fields allow =
  let name = Ctor.name c in
  let provided = List.filter_map (fun (fname_opt, e) ->
    match fname_opt with
    | Some fname -> Some (fname, eval env e)
    | None -> None
  ) fields in
  (match Hashtbl.find_opt constr_fields c with
   | None -> raise (EvalError (Printf.sprintf "unknown constructor '%s'%s"
       name (Util.hint name (List.map fst env))))
   | Some field_names ->
     let ordered = List.map (fun fname_opt ->
       match fname_opt with
       | None -> raise (EvalError (Printf.sprintf
           "constructor '%s' has an unnamed field" name))
       | Some fn ->
         (match List.assoc_opt fn provided with
          | Some v -> v
          | None ->
            (* Left out, so the declaration says what it holds. A default is
               a value written out, so it reads in an empty environment and
               the same way at every site that omits the field. *)
            (match List.assoc_opt fn (defaults_of c) with
             | Some d -> eval (ctor_env ()) d
             | None -> raise (EvalError (Printf.sprintf
                 "constructor '%s' missing field '%s'" name fn))))
     ) field_names in
     let built = VConstr (c, ordered) in
     (* Where a request meets the manifest of the file that wrote it. The
        URL it was given is checked now; the bound travels with it, because
        a redirect's host is not known until the send. *)
     if c = Ctor.Builtin "HTTPRequest" then begin
       check_request_host allow built;
       VRequest (built, allow)
     end else built)

and apply vf vx =
  (* The call is left with work to do after this returns -- it is a builtin
     applying a function it was handed, or a caller evaluating the rest of
     an expression -- so the callee's position goes back where it was found.
     `apply_tail` is the same call with nothing to come back to. *)
  let c = loc_cell () in
  let line = c.at_line and col = c.at_col in
  if c.depth >= max_call_depth then
    raise (EvalError
      "too many nested calls -- only a call in tail position runs to any \
       depth; carry the result in an argument instead of waiting on the \
       call");
  c.depth <- c.depth + 1;
  let v = (try apply_tail vf vx with e -> c.depth <- c.depth - 1; raise e) in
  c.depth <- c.depth - 1;
  c.at_line <- line;
  c.at_col  <- col;
  v

and apply_tail vf vx =
  match vf with
  | VBuiltin f -> f vx
  | VFun (fenv, params, body) ->
    (match params with
     | []      -> raise (EvalError "function with no parameters")
     | [p]     -> eval_tail (bind_pat p vx fenv) body
     | p :: rest ->
       let env' = bind_pat p vx fenv in
       VFun (env', rest, body))
  | VFix (name, fenv, params, body) ->
    (* `vf` is the value being bound, so binding it costs nothing to build.
       Rebuilding it made a second copy of the same closure on every call,
       and going back through `apply_tail` made a `VFun` to carry it there.
       Neither outlived the call. *)
    let fenv' = (name, vf) :: fenv in
    (match params with
     | []        -> raise (EvalError "function with no parameters")
     | [p]       -> eval_tail (bind_pat p vx fenv') body
     | p :: rest -> VFun (bind_pat p vx fenv', rest, body))
  | VFixGroup (bindings, fenv, my_name) ->
    let fenv' = List.fold_left (fun acc (n, _, _) ->
      (n, VFixGroup (bindings, fenv, n)) :: acc) fenv bindings in
    let (_, params, body) = List.find (fun (n, _, _) -> n = my_name) bindings in
    apply_tail (VFun (fenv', params, body)) vx
  | VPartialConstr (name, 1, args) -> VConstr (name, args @ [vx])
  | VPartialConstr (name, n, args) -> VPartialConstr (name, n - 1, args @ [vx])
  | _ -> raise (EvalError "cannot apply a non-function")

and bind_pat ?(prefix = false) (p : pat) v (env : env) : env =
  match try_match ~prefix p v env with
  | Some env' -> env'
  | None      -> raise (EvalError "pattern match failure")

and eval_match (tail : bool) (env : env) sv cases =
  match cases with
  | [] -> raise (EvalError "non-exhaustive match")
  | (p, guard, body) :: rest ->
    (match try_match p sv env with
     | None      -> eval_match tail env sv rest
     | Some env' ->
       let passes = match guard with
         | None   -> true
         | Some g ->
           (match eval env' g with
            | VBool b -> b
            | _       -> raise (EvalError "guard must evaluate to a bool"))
       in
       if passes then eval_at tail env' body
       else eval_match tail env sv rest)

and eval_binop (env : env) op a b : value =
  match op with
  (* A sum of sizes is written in bytes, because a size holds one unit and
     `100MB + 4KB` fills two. `Size.format` is the readable spelling.

     Neither a size nor a duration has a value below zero, so a subtraction
     that would go under floors there -- the same answer `Duration.sub` and
     `Size.of_bytes` already give.

     A size is a count of bytes and a duration a count of milliseconds, both
     held in an Int, so both add through the checked add that Int addition
     uses. Raw `+` wrapped: `4000000000GB + 4000000000GB` answered with a
     negative size, which reads as under any threshold a script compares it
     against. *)
  | "+"  ->
    (match eval env a, eval env b with
     | VInt x,   VInt y   -> VInt (add_ovf x y)
     | VFloat x, VFloat y -> VFloat (x +. y)
     | VSize x,  VSize y  ->
       VSize (Printf.sprintf "%dB" (add_ovf (size_bytes x) (size_bytes y)))
     | VDuration x, VDuration y ->
       VDuration (format_dur_ms (add_ovf (parse_dur_ms x) (parse_dur_ms y)))
     (* A Duration moves an instant, from either side. The instant carries
        whole seconds, so a duration below a second moves it nowhere. *)
     | VDateTime x, VDuration d | VDuration d, VDateTime x ->
       VDateTime (datetime_of_epoch
                    (add_ovf (datetime_epoch x) (parse_dur_ms d / 1000)))
     | _ -> raise (EvalError "'+' requires matching types"))
  | "-"  ->
    (match eval env a, eval env b with
     | VInt x,   VInt y   -> VInt (sub_ovf x y)
     | VFloat x, VFloat y -> VFloat (x -. y)
     | VSize x,  VSize y  ->
       VSize (Printf.sprintf "%dB" (max 0 (size_bytes x - size_bytes y)))
     | VDuration x, VDuration y ->
       VDuration (format_dur_ms (max 0 (parse_dur_ms x - parse_dur_ms y)))
     | VDateTime x, VDuration d ->
       VDateTime (datetime_of_epoch
                    (sub_ovf (datetime_epoch x) (parse_dur_ms d / 1000)))
     (* The length between two instants. It floors at zero like every other
        Duration subtraction, so a file stamped in the future reads as no
        age rather than a negative one. *)
     | VDateTime x, VDateTime y ->
       VDuration (format_dur_ms
                    (max 0 (mul_ovf (sub_ovf (datetime_epoch x) (datetime_epoch y)) 1000)))
     | _ -> raise (EvalError "'-' requires matching types"))
  | "*"  ->
    (match eval env a, eval env b with
     | VInt x,   VInt y   -> VInt (mul_ovf x y)
     | VFloat x, VFloat y -> VFloat (x *. y)
     | _ -> raise (EvalError "'*' requires matching numeric types"))
  | "/"  ->
    (match eval env a, eval env b with
     | VInt _,   VInt 0   -> raise (EvalError "division by zero")
     | VInt x,   VInt y   -> VInt (div_ovf x y)
     | VFloat x, VFloat y -> VFloat (x /. y)
     | _ -> raise (EvalError "'/' requires matching numeric types"))
  | "%"  ->
    (match eval env a, eval env b with
     | VInt _,   VInt 0   -> raise (EvalError "modulo by zero")
     | VInt x,   VInt y   -> VInt (x mod y)
     | _ -> raise (EvalError "'%' requires Int operands"))
  | "++" ->
    (match eval env a, eval env b with
     | VString s1, VString s2 -> VString (s1 ^ s2)
     | _ -> raise (EvalError "'++' requires strings"))
  | "::" ->
    let vh = eval env a in
    (match eval env b with
     | VList vs -> VList (vh :: vs)
     | _        -> raise (EvalError "':' right side must be a list"))
  | "==" -> VBool (wand_equal (eval env a) (eval env b))
  | "!=" -> VBool (not (wand_equal (eval env a) (eval env b)))
  (* Normalized, so `90s > 1min` and two spellings of one instant compare
     as one value. The `Ord` constraint has already refused every type
     that has no order. *)
  | "<"  -> VBool (wand_order (eval env a) (eval env b) <  0)
  | ">"  -> VBool (wand_order (eval env a) (eval env b) >  0)
  | "<=" -> VBool (wand_order (eval env a) (eval env b) <= 0)
  | ">=" -> VBool (wand_order (eval env a) (eval env b) >= 0)
  | "&&" ->
    (match eval env a with
     | VBool false -> VBool false
     | VBool true  ->
       (match eval env b with
        | VBool b -> VBool b
        | _       -> raise (EvalError "'&&' requires bools"))
     | _ -> raise (EvalError "'&&' requires bools"))
  | "||" ->
    (match eval env a with
     | VBool true  -> VBool true
     | VBool false ->
       (match eval env b with
        | VBool b -> VBool b
        | _       -> raise (EvalError "'||' requires bools"))
     | _ -> raise (EvalError "'||' requires bools"))
  | "|>" ->
    let va = eval env a in
    (match b with
     | RunCmd (e, allow) ->
       let cmd = command_line env e allow "$(…)" in
       let stdin = to_text va in
       perform_shell "Shell!run" allow (VTuple [VString cmd; VString stdin])
     | RunQuery (e, allow) ->
       let cmd = command_line env e allow "$?(…)" in
       let stdin = to_text va in
       perform_shell "Shell!capture" allow (VTuple [VString cmd; VString stdin])
     | _ ->
       let vf = eval env b in
       apply vf va)
  | op -> raise (EvalError (Printf.sprintf "unknown operator '%s'" op))

(* Builtins that reach outside the program perform an effect rather than
   acting directly, so a handler can see, log, or replace what they do.
   `performing` registers the real implementation and hands back a builtin
   that performs in its place; the default handler looks the implementation
   up again. Wrapping at registration keeps the two from drifting apart. *)
let direct_impl : (string, value -> value) Hashtbl.t = Hashtbl.create 32

(* Wait, in slices, so that Ctrl-C during a sleep is taken at once rather
   than after the whole duration. A sleep is exactly the wrong thing to make
   uninterruptible.

   The slices are also what keeps this a relative wait with no clock read in
   it: each one is a fresh `nanosleep`, and the total is the sum. Reading a
   civil clock to find the remainder would make a machine that syncs its
   clock mid-sleep wake early or late.

   `Unix.sleepf` waits at least what it is given, so the total is a floor,
   which is what `Clock.sleep` promises. A zero or negative duration waits
   not at all and still performed the effect to get here. *)
let sleep_ms ms =
  let slice = 0.05 in
  let remaining = ref (float_of_int ms /. 1000.) in
  while !remaining > 0.0 do
    check_interrupt ();
    let this = if !remaining < slice then !remaining else slice in
    (try Unix.sleepf this with Unix.Unix_error (Unix.EINTR, _, _) -> ());
    remaining := !remaining -. this
  done;
  check_interrupt ()

(* Milliseconds since an arbitrary point in this run, from a clock that no
   NTP correction can move. `lib/ext/clock.c` states which clock each
   platform uses and why. *)
external elapsed_ms : unit -> int = "wand_elapsed_ms"

(* The generator behind `Random`.

   OCaml's default state is the same on every run, so a script that drew
   without seeding would answer identically every time -- which is the one
   thing the `Random` label promises it will not do. The first draw seeds
   from the environment, and `Random.seed` pins the state instead and marks
   it seeded, so a pin placed before the first draw is not overwritten by
   one. *)
let random_seeded = ref false

let random_ready () =
  if not !random_seeded then begin
    Stdlib.Random.self_init ();
    random_seeded := true
  end

(* `0 <= x < bound`. A bound below 1 answers 0 rather than raising: every
   caller in `stdlib/Random.wand` has already established a non-empty range,
   and clamping here is what keeps that module free of Raise. *)
let random_below n =
  random_ready ();
  if n < 1 then 0 else Stdlib.Random.int n

let performing name f =
  Hashtbl.replace direct_impl name f;
  VBuiltin (fun v -> Effect.perform (WandEffect (name, v)))

(* Read-only filesystem operations. They are named here and performed as
   effects below, so a trace can report what a script looked at, not only
   what it changed. *)

let fs_cwd_impl = function
  | VUnit -> VPath (Sys.getcwd ())
  | _ -> raise (EvalError "fs_cwd: expected Unit")

let fs_mtime_impl = function
  | VString p | VPath p ->
    (match (try Ok (Unix.stat p) with Unix.Unix_error (e, _, _) ->
      Error ("mtime: " ^ Unix.error_message e)) with
     | Error m -> raise (EvalError m)
     | Ok st ->
       let tm = Unix.gmtime st.Unix.st_mtime in
       VDateTime (Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02dZ"
         (tm.Unix.tm_year + 1900) (tm.Unix.tm_mon + 1) tm.Unix.tm_mday
         tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec))
  | _ -> raise (EvalError "fs_mtime: expected Path")

let fs_size_impl = function
  | VString p | VPath p ->
    (match (try Ok (Unix.stat p) with Unix.Unix_error (e, _, _) ->
      Error ("size: " ^ Unix.error_message e)) with
     | Error m -> raise (EvalError m)
     (* In bytes, the only unit `stat` gives. `Size.format` is the readable
        spelling, and a threshold is written as the literal it is:
        `size < 4KB`. *)
     | Ok st   -> VSize (Printf.sprintf "%dB" st.Unix.st_size))
  | _ -> raise (EvalError "fs_size: expected Path")

(* ── String primitive helpers ─────────────────────────────────────────────── *)

(* Does `needle` sit at position `i` of `haystack`? Compared in place.

   Every scanning loop below used to ask this with
   `String.sub haystack i nlen = needle`, which allocates a fresh short
   string at every position it looks at -- a 200k-line log answered
   `String.contains?` with about twelve million of them. Reading the bytes
   costs nothing and stops at the first one that differs. *)
let str_matches_at needle nlen haystack i =
  let rec go j = j = nlen || (haystack.[i + j] = needle.[j] && go (j + 1)) in
  go 0

let str_split_impl delim str =
  let dlen = String.length delim in
  let slen = String.length str in
  if dlen = 0 then
    List.init slen (fun i -> VString (String.make 1 str.[i]))
  else begin
    let result = ref [] in
    let start = ref 0 in
    let i = ref 0 in
    (* A one-character delimiter is the common case -- every `String.lines`
       and every split on a space or a comma -- and testing it with
       `String.sub` allocates a one-byte string per position in the input.
       Splitting a 17MB log on " " allocated 17MB of them. *)
    let matches_at =
      if dlen = 1 then
        let c = delim.[0] in
        fun i -> str.[i] = c
      else fun i -> str_matches_at delim dlen str i
    in
    while !i <= slen - dlen do
      if matches_at !i then begin
        result := VString (String.sub str !start (!i - !start)) :: !result;
        i := !i + dlen;
        start := !i
      end else
        incr i
    done;
    result := VString (String.sub str !start (slen - !start)) :: !result;
    List.rev !result
  end

(* The whitespace `String.words` separates on. Written out rather than
   `Char.is_whitespace` so that what counts is visible and fixed: these are
   the four the wand-level version replaced before it split. *)
let is_word_space = function ' ' | '\t' | '\n' | '\r' -> true | _ -> false

(* One pass, and it allocates only the words it returns.

   `String.words` used to be three `str_replace` passes to turn every
   separator into a space, a split that produced an empty string for every
   repeat, and a `List.filter` to drop them again -- so a line cost three
   full copies plus one interpreted closure call per field. Over a 200k-line
   log that was ~2.2M closure calls and about 3.2 seconds, against 73ms to
   read the same lines. *)
(* `String.lines`. A newline ends a line rather than separating two, so the
   bytes after the last one are a line only when there are some. Splitting on
   "\n" gave back one element more than there were lines, with "" at the
   end, and nearly every file ends in a newline. *)
let str_lines_impl str =
  let slen = String.length str in
  let result = ref [] and start = ref 0 in
  for i = 0 to slen - 1 do
    if str.[i] = '\n' then begin
      result := VString (String.sub str !start (i - !start)) :: !result;
      start := i + 1
    end
  done;
  if !start < slen then
    result := VString (String.sub str !start (slen - !start)) :: !result;
  List.rev !result

let str_words_impl str =
  let slen = String.length str in
  let result = ref [] in
  let i = ref 0 in
  while !i < slen do
    while !i < slen && is_word_space str.[!i] do incr i done;
    if !i < slen then begin
      let start = !i in
      while !i < slen && not (is_word_space str.[!i]) do incr i done;
      result := VString (String.sub str start (!i - start)) :: !result
    end
  done;
  List.rev !result

(* The nth whitespace-separated word, without building the others.
   `List.get! n (String.words s)` allocates a string per word, a cons per
   word and a reversal, to answer with one of them: 663ns a line against
   397ns here on a seven-field log. Same rule as `words` -- a run of
   whitespace separates once, and leading or trailing whitespace adds no
   word. *)
let str_word_impl n str =
  let slen = String.length str in
  let i = ref 0 and seen = ref 0 and result = ref None in
  while !result = None && !i < slen do
    while !i < slen && is_word_space str.[!i] do incr i done;
    if !i < slen then begin
      let start = !i in
      while !i < slen && not (is_word_space str.[!i]) do incr i done;
      if !seen = n then result := Some (String.sub str start (!i - start))
      else incr seen
    end
  done;
  (* `seen` is how many were passed, so on a miss it is the count. *)
  (!result, !seen)

let str_replace_impl old_ new_ str =
  let olen = String.length old_ in
  let slen = String.length str in
  if olen = 0 then str
  else begin
    let buf = Buffer.create slen in
    let i = ref 0 in
    while !i <= slen - olen do
      if str_matches_at old_ olen str !i then begin
        Buffer.add_string buf new_;
        i := !i + olen
      end else begin
        Buffer.add_char buf str.[!i];
        incr i
      end
    done;
    if !i < slen then
      Buffer.add_string buf (String.sub str !i (slen - !i));
    Buffer.contents buf
  end

(* The `for` here ran to the end of the string after it had its answer, so a
   needle at position 0 still read every byte of the line. Stopping at the
   first match, and comparing without allocating, took counting the matching
   lines of a 200k-line log from 456ms to 274ms. *)
let str_contains_impl needle haystack =
  let nlen = String.length needle in
  let hlen = String.length haystack in
  if nlen = 0 then true
  else if nlen > hlen then false
  else begin
    let i = ref 0 in
    let found = ref false in
    while not !found && !i <= hlen - nlen do
      if str_matches_at needle nlen haystack !i then found := true else incr i
    done;
    !found
  end

let str_trim_left s =
  let n = String.length s in
  let i = ref 0 in
  while !i < n && (let c = s.[!i] in c = ' ' || c = '\t' || c = '\n' || c = '\r') do incr i done;
  String.sub s !i (n - !i)

let str_trim_right s =
  let n = String.length s in
  let i = ref (n - 1) in
  while !i >= 0 && (let c = s.[!i] in c = ' ' || c = '\t' || c = '\n' || c = '\r') do decr i done;
  String.sub s 0 (!i + 1)

let str_repeat n s =
  let buf = Buffer.create (max 0 (String.length s * n)) in
  for _ = 1 to n do Buffer.add_string buf s done;
  Buffer.contents buf

let str_reverse s =
  let n = String.length s in
  String.init n (fun i -> s.[n - 1 - i])

(* At most [n] bytes, never ending inside a UTF-8 character. A cut lands
   badly only when the byte at [n] is a continuation byte (0b10xxxxxx),
   which means the character that began before [n] runs past it; backing up
   to that character's first byte drops it whole.

   The walk back is bounded at three, because no UTF-8 character is longer
   than four bytes. Four continuation bytes in a row is not UTF-8 at all,
   and a String is allowed to hold that -- a log line in an unknown encoding
   is the case the byte model exists for. There is no character to preserve
   there, so the plain byte cut is the answer. *)
let str_truncate n s =
  let len = String.length s in
  if n <= 0 then ""
  else if n >= len then s
  else begin
    let is_continuation i = Char.code s.[i] land 0xC0 = 0x80 in
    let rec back i steps =
      if steps = 0 || i = 0 || not (is_continuation i) then i else back (i - 1) (steps - 1)
    in
    let cut = back n 3 in
    String.sub s 0 (if is_continuation cut then n else cut)
  end

(* ── Digests, hex and base64 ─────────────────────────────────────────────

   The algorithm arrives as the string a `Hash.Algorithm` constructor maps
   to, so the closed set in the standard library is what decides which of
   these can be reached. An unknown name is unreachable from wand and is a
   raise rather than a default, because defaulting would hash with an
   algorithm nobody asked for. *)

let hash_module = function
  | "sha256" -> (module Digestif.SHA256 : Digestif.S)
  | "sha512" -> (module Digestif.SHA512 : Digestif.S)
  | "sha1"   -> (module Digestif.SHA1   : Digestif.S)
  | "md5"    -> (module Digestif.MD5    : Digestif.S)
  | a -> raise (EvalError (Printf.sprintf "unknown hash algorithm: %s" a))

let hash_string_hex algo s =
  let module H = (val hash_module algo) in
  H.to_hex (H.digest_string s)

let hash_hmac_hex algo key msg =
  let module H = (val hash_module algo) in
  H.to_hex (H.hmac_string ~key msg)

(* Hashing a file reads it in blocks and never holds it. A release archive
   is the case this exists for, and turning one into a String first would
   defeat the point of the function. *)
let hash_file_hex algo path =
  let module H = (val hash_module algo) in
  let ic = open_in_bin path in
  Fun.protect ~finally:(fun () -> close_in_noerr ic) (fun () ->
    let buf = Bytes.create 65536 in
    let rec go ctx =
      let n = input ic buf 0 (Bytes.length buf) in
      if n = 0 then ctx
      else go (H.feed_string ctx ~off:0 ~len:n (Bytes.unsafe_to_string buf))
    in
    H.to_hex (H.get (go H.empty)))

let hex_digits = "0123456789abcdef"

let hex_encode s =
  String.concat "" (List.init (String.length s) (fun i ->
    let c = Char.code s.[i] in
    Printf.sprintf "%c%c" hex_digits.[c lsr 4] hex_digits.[c land 0xf]))

let hex_value c =
  match c with
  | '0'..'9' -> Char.code c - Char.code '0'
  | 'a'..'f' -> Char.code c - Char.code 'a' + 10
  | 'A'..'F' -> Char.code c - Char.code 'A' + 10
  | _ -> -1

let hex_decode s =
  let n = String.length s in
  if n mod 2 <> 0 then Error "hex string has an odd number of digits"
  else begin
    let bad = ref None in
    let out = String.init (n / 2) (fun i ->
      let hi = hex_value s.[2 * i] and lo = hex_value s.[2 * i + 1] in
      if hi < 0 || lo < 0 then begin
        if !bad = None then
          bad := Some (Printf.sprintf "not a hex digit: %c"
                         (if hi < 0 then s.[2 * i] else s.[2 * i + 1]));
        '\000'
      end else Char.chr ((hi lsl 4) lor lo))
    in
    match !bad with Some m -> Error m | None -> Ok out
  end

(* RFC 4648. The standard alphabet is section 4 and the URL-safe one is
   section 5; they differ in the last two characters and nowhere else.
   `encode` pads, because the standard requires it. `decode` accepts input
   with or without padding, because most JWT producers omit it and a
   decoder that refused would reject the input people actually have. *)
let b64_std = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
let b64_url = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_"

let base64_encode alphabet s =
  let n = String.length s in
  let buf = Buffer.create ((n + 2) / 3 * 4) in
  let byte i = if i < n then Char.code s.[i] else 0 in
  let rec go i =
    if i < n then begin
      let b = (byte i lsl 16) lor (byte (i + 1) lsl 8) lor byte (i + 2) in
      let left = n - i in
      Buffer.add_char buf alphabet.[(b lsr 18) land 0x3f];
      Buffer.add_char buf alphabet.[(b lsr 12) land 0x3f];
      Buffer.add_char buf (if left > 1 then alphabet.[(b lsr 6) land 0x3f] else '=');
      Buffer.add_char buf (if left > 2 then alphabet.[b land 0x3f] else '=');
      go (i + 3)
    end
  in
  go 0;
  Buffer.contents buf

let base64_decode alphabet s =
  let value c =
    let rec find i = if i >= 64 then -1 else if alphabet.[i] = c then i else find (i + 1) in
    find 0
  in
  let s =
    (* Padding is optional on the way in, so it is dropped before decoding
       rather than counted. *)
    let n = ref (String.length s) in
    while !n > 0 && s.[!n - 1] = '=' do decr n done;
    String.sub s 0 !n
  in
  let n = String.length s in
  let err = ref None in
  let buf = Buffer.create (n / 4 * 3) in
  let acc = ref 0 and bits = ref 0 in
  String.iter (fun c ->
    if !err = None then begin
      let v = value c in
      if v < 0 then err := Some (Printf.sprintf "not a base64 character: %c" c)
      else begin
        acc := (!acc lsl 6) lor v;
        bits := !bits + 6;
        if !bits >= 8 then begin
          bits := !bits - 8;
          Buffer.add_char buf (Char.chr ((!acc lsr !bits) land 0xff))
        end
      end
    end) s;
  (* The character scan runs first, so `Zm9v!` is reported as the `!` it
     is rather than as a length that cannot be base64. Both are true of it;
     only one tells the caller what to fix. *)
  match !err with
  | Some m -> Error m
  | None ->
    if n mod 4 = 1 then Error "base64 input has a trailing character"
    else Ok (Buffer.contents buf)

let path_normalize s =
  let is_abs = String.length s > 0 && s.[0] = '/' in
  let is_cur = (String.length s >= 2 && s.[0] = '.' && s.[1] = '/') || s = "." in
  let parts = String.split_on_char '/' s in
  let rec process acc = function
    | [] -> List.rev acc
    | ("" | ".") :: rest -> process acc rest
    | ".." :: rest ->
      (match acc with
       | [] | ".." :: _ -> process (".." :: acc) rest
       | _ :: tl -> process tl rest)
    | p :: rest -> process (p :: acc) rest
  in
  let parts = process [] parts in
  let joined = String.concat "/" parts in
  if is_abs then "/" ^ joined
  else if is_cur then "./" ^ joined
  else joined

let exe_args_ref : string list ref = ref []

(* One dotenv source into its (key, value) pairs: `export ` stripped, blank
   and `#` lines skipped, a quoted value unquoted. Shared by
   `Env.parse_dotenv` and `Env.load_file`, so the two cannot read the same
   file differently. *)
let dotenv_pairs src =
  List.filter_map (fun line ->
    let line = String.trim line in
    let line =
      if String.length line > 7 && String.sub line 0 7 = "export " then
        String.trim (String.sub line 7 (String.length line - 7))
      else line
    in
    if line = "" || line.[0] = '#' then None
    else match String.split_on_char '=' line with
      | [] | [""] -> None
      | key :: rest ->
        let key = String.trim key in
        if key = "" then None
        else
          let raw = String.concat "=" rest in
          let value =
            let n = String.length raw in
            if n >= 2 && ((raw.[0] = '"' && raw.[n-1] = '"')
                       || (raw.[0] = '\'' && raw.[n-1] = '\'')) then
              String.sub raw 1 (n - 2)
            else raw
          in
          Some (key, value))
    (String.split_on_char '\n' src)

(* Slurp a file whole; a `Sys_error` is the caller's to catch. *)
let read_whole_file path = In_channel.with_open_text path In_channel.input_all

(* ── CSV helpers ──────────────────────────────────────────────────────────── *)

let csv_parse_string sep src =
  (* RFC 4180: quoted fields, "" = escaped quote, \r\n or \n line endings *)
  let src = (* normalise line endings *)
    let n = String.length src in
    let buf = Buffer.create n in
    let i = ref 0 in
    while !i < n do
      if src.[!i] = '\r' && !i + 1 < n && src.[!i + 1] = '\n' then
        (Buffer.add_char buf '\n'; i := !i + 2)
      else
        (Buffer.add_char buf src.[!i]; incr i)
    done;
    Buffer.contents buf
  in
  let n = String.length src in
  let rows = ref [] in
  let row  = ref [] in
  let field = Buffer.create 16 in
  let i = ref 0 in
  let sep_char = if String.length sep > 0 then sep.[0] else ',' in
  let commit_field () =
    row := Buffer.contents field :: !row;
    Buffer.clear field
  in
  let commit () =
    commit_field ();
    rows := List.rev !row :: !rows;
    row := []
  in
  while !i < n do
    let c = src.[!i] in
    if c = '"' then begin
      (* quoted field *)
      incr i;
      let continue_ = ref true in
      while !continue_ && !i < n do
        if src.[!i] = '"' then begin
          if !i + 1 < n && src.[!i + 1] = '"' then begin
            Buffer.add_char field '"'; i := !i + 2
          end else begin
            incr i; continue_ := false
          end
        end else begin
          Buffer.add_char field src.[!i]; incr i
        end
      done
    end else if c = sep_char then begin
      commit_field (); incr i
    end else if c = '\n' then begin
      commit (); incr i
    end else begin
      Buffer.add_char field c; incr i
    end
  done;
  (* commit trailing content (file may not end with newline) *)
  if Buffer.length field > 0 || !row <> [] then commit ();
  List.rev !rows

let csv_stringify_rows sep rows =
  let sep_char = if String.length sep > 0 then sep.[0] else ',' in
  let needs_quoting s =
    String.exists (fun c -> c = sep_char || c = '"' || c = '\n' || c = '\r') s
  in
  let quote_field s =
    if needs_quoting s then
      "\"" ^ String.concat "" (List.map (fun c ->
        if c = '"' then "\"\"" else String.make 1 c)
        (List.init (String.length s) (String.get s))) ^ "\""
    else s
  in
  String.concat "\n" (List.map (fun row ->
    String.concat sep (List.map quote_field row)) rows)

(* Lexing one domain literal out of a string, keeping the lexer's complaint
   when it has one. `256.0.0.1` and `:99999` are not merely unreadable: the
   lexer knows an octet is 0-255 and a port is 0-65535, and a reader that
   answers "cannot parse" has thrown away the only sentence that says what to
   do about it. Whoever gets the message -- a person or a model -- is then
   guessing at a rule the program already knows. *)
let lex_single s : (Token.t, string option) result =
  match Lexer.tokenize_plain s with
  | [tok; Token.EOF] -> Ok tok
  | _ -> Error None
  | exception Lexer.LexError (_, msg) -> Error (Some msg)

let try_lex_single s =
  match lex_single s with Ok tok -> Some tok | Error _ -> None

(* `String.to_<domain>`: read the string exactly as the lexer would, and pass
   on what the lexer said when it had something specific to say. *)
let to_domain ?shown name build s =
  let shown = Option.value shown ~default:s in
  let cannot () =
    VConstr (Ctor.Builtin "Error", [VString (Printf.sprintf "cannot parse %S as %s" shown name)])
  in
  match lex_single s with
  | Ok tok -> (match build tok with Some v -> VConstr (Ctor.Builtin "Ok", [v]) | None -> cannot ())
  | Error (Some why) -> VConstr (Ctor.Builtin "Error", [VString why])
  | Error None -> cannot ()

(* ── Globs ───────────────────────────────────────────────────────────────── *)

(* `./` in front of a pattern or a path is a way of writing "here" and not
   part of either, so it comes off both before they meet. Without it
   `FS.glob_in ./*.wand` would compare a pattern that carries it against
   entry names that do not. *)
let glob_strip_dot s =
  if String.length s > 2 && s.[0] = '.' && s.[1] = '/' then
    String.sub s 2 (String.length s - 2)
  else s

(* One compile, shared by `FS.glob` and `Glob.matches?`. Two engines given
   the same pattern would eventually answer differently, and the difference
   would show up as a walk finding a file the predicate says it should not
   have. *)
let glob_compile pat =
  match Re.compile (Re.Glob.glob ~anchored:true ~double_asterisk:true (glob_strip_dot pat)) with
  | re -> Ok re
  | exception _ ->
    Error "a character class is not closed: every `[` needs a `]` after it"

(* Everything before the first segment holding a wildcard. `./src/**/*.ts`
   has `./src`, and a pattern whose first segment is already a wildcard has
   `.`: there is no directory above it to start from. *)
let glob_base_of g =
  let segs = String.split_on_char '/' g in
  let rec take acc = function
    | [] -> List.rev acc                      (* no wildcard: all of it *)
    | seg :: rest ->
      if String.exists Lexer.is_glob_char seg then List.rev acc
      else take (seg :: acc) rest
  in
  match take [] segs with
  | [] | ["."] -> "."
  | parts -> String.concat "/" parts

(* ── URL parts ───────────────────────────────────────────────────────────── *)

(* The pieces of a URL, found once and shared by the accessors. Held as text
   in the spellings the URL itself used, so rebuilding one that was not
   changed gives the same string back. *)
type url_authority = { au_userinfo : string option; au_hostname : string; au_port : int option }

type url_parts = {
  up_scheme    : string;
  up_authority : string;
  up_path      : string;
  up_query     : string;
  up_fragment  : string option;
}

let url_split_scheme u =
  match String.index_opt u ':' with
  | Some i when i + 2 < String.length u && u.[i+1] = '/' && u.[i+2] = '/' ->
    (String.sub u 0 i, String.sub u (i + 3) (String.length u - i - 3))
  (* Unreachable for a value that passed `Lexer.url_error`, which requires the
     scheme. Kept total rather than raising: an accessor that can fail on a
     value the type says is fine is worse than one that answers "". *)
  | _ -> ("", u)

(* The first `#` ends the query and the first `?` ends the path, so they are
   taken off from the right-hand end inward. A `?` after a `#` is part of the
   fragment, and this order is what gives that. *)
let url_parts u =
  let scheme, rest = url_split_scheme u in
  let rest, fragment =
    match String.index_opt rest '#' with
    | Some i ->
      (String.sub rest 0 i,
       Some (String.sub rest (i + 1) (String.length rest - i - 1)))
    | None -> (rest, None)
  in
  let rest, query =
    match String.index_opt rest '?' with
    | Some i ->
      (String.sub rest 0 i, String.sub rest (i + 1) (String.length rest - i - 1))
    | None -> (rest, "")
  in
  let authority, path =
    match String.index_opt rest '/' with
    | Some i -> (String.sub rest 0 i, String.sub rest i (String.length rest - i))
    | None -> (rest, "")
  in
  { up_scheme = scheme; up_authority = authority; up_path = path;
    up_query = query; up_fragment = fragment }

(* Userinfo is split at the *last* `@`, because a password may hold one. The
   port is split at the last `:` only when what follows is all digits: an
   IPv6 literal is full of colons, and `[::1]:8080` has to keep them. *)
let url_authority_of text =
  let userinfo, hostport =
    match String.rindex_opt text '@' with
    | Some i ->
      (Some (String.sub text 0 i),
       String.sub text (i + 1) (String.length text - i - 1))
    | None -> (None, text)
  in
  let host_end =
    match String.rindex_opt hostport ']' with
    | Some i -> i + 1                       (* an IPv6 literal: past the bracket *)
    | None -> 0
  in
  let all_digits s =
    s <> "" && String.for_all (fun c -> c >= '0' && c <= '9') s
  in
  match String.index_from_opt hostport host_end ':' with
  | Some i ->
    let after = String.sub hostport (i + 1) (String.length hostport - i - 1) in
    if all_digits after then
      match int_of_string_opt after with
      | Some n when n >= 0 && n <= 65535 ->
        { au_userinfo = userinfo; au_hostname = String.sub hostport 0 i; au_port = Some n }
      (* Digits that are not a port. The text is not one, so it stays part of
         the host rather than being reported as a port that is out of range --
         nothing here is entitled to reject a URL that already exists. *)
      | _ -> { au_userinfo = userinfo; au_hostname = hostport; au_port = None }
    else { au_userinfo = userinfo; au_hostname = hostport; au_port = None }
  | None -> { au_userinfo = userinfo; au_hostname = hostport; au_port = None }

let url_authority u = url_authority_of (url_parts u).up_authority

(* The authority written back out. The pieces are held as they were read, so
   a URL that was not changed rebuilds to the same string it came from. *)
let url_authority_text au =
  (match au.au_userinfo with None -> "" | Some ui -> ui ^ "@")
  ^ au.au_hostname
  ^ (match au.au_port with None -> "" | Some n -> ":" ^ string_of_int n)

(* Userinfo is `username[:password]`, split at the *first* `:` -- the reverse
   of the `@` rule above, because a password may hold a `:` and a username
   may not. *)
let url_userinfo_parts au =
  (* An empty half is None, not Some "". `https://:pw@host` carries a
     password and no username, and answering Some "" for the username would
     make "has one" and "has a non-empty one" two different questions with
     the same shape. *)
  let some_unless_empty s = if s = "" then None else Some s in
  match au.au_userinfo with
  | None -> (None, None)
  | Some ui ->
    (match String.index_opt ui ':' with
     | Some i ->
       (some_unless_empty (String.sub ui 0 i),
        some_unless_empty (String.sub ui (i + 1) (String.length ui - i - 1)))
     | None -> (some_unless_empty ui, None))

let url_rebuild p =
  p.up_scheme ^ "://" ^ p.up_authority ^ p.up_path
  ^ (if p.up_query = "" then "" else "?" ^ p.up_query)
  ^ (match p.up_fragment with None -> "" | Some f -> "#" ^ f)

(* Every setter goes out through here: the new text is built and then put
   through the same grammar a literal is checked against, so no part can be
   set to something that makes the whole no longer a URL. A setter that
   cannot fail runs the check too -- it just cannot see it fail, because it
   encoded its argument first. *)
let url_checked text =
  match Lexer.url_error text with
  | None -> Ok text
  | Some why -> Error why

(* ── Percent-encoding ────────────────────────────────────────────────────── *)

let url_unreserved c =
  (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9')
  || c = '-' || c = '.' || c = '_' || c = '~'

let url_encode_text s =
  let buf = Buffer.create (String.length s) in
  String.iter
    (fun c ->
      if url_unreserved c then Buffer.add_char buf c
      else Buffer.add_string buf (Printf.sprintf "%%%02X" (Char.code c)))
    s;
  Buffer.contents buf

(* For the parts where URL punctuation is legal and meant: a fragment may
   hold a `/` or a `?` and a path is made of `/`. Only what cannot appear in
   a URL at all is escaped, so setting a part does not mangle its structure.
   Userinfo does not use this -- there `@` and `:` are the delimiters, so it
   takes the strict encoder above. *)
let url_encode_keep s =
  let buf = Buffer.create (String.length s) in
  String.iter
    (fun c ->
      if Lexer.is_url_char c then Buffer.add_char buf c
      else Buffer.add_string buf (Printf.sprintf "%%%02X" (Char.code c)))
    s;
  Buffer.contents buf

let url_hex_value c =
  if c >= '0' && c <= '9' then Some (Char.code c - Char.code '0')
  else if c >= 'a' && c <= 'f' then Some (Char.code c - Char.code 'a' + 10)
  else if c >= 'A' && c <= 'F' then Some (Char.code c - Char.code 'A' + 10)
  else None

(* Decoding for the accessors, which cannot fail: they read a URL that
   exists, and text that is not a well-formed escape is text. `%zz` in a
   query value comes back as `%zz` rather than sinking the whole read.
   `URL.decode` is the checked one, for input a caller believes is encoded. *)
let url_decode_text ~plus s =
  let buf = Buffer.create (String.length s) in
  let n = String.length s in
  let i = ref 0 in
  while !i < n do
    (match s.[!i] with
     | '%' when !i + 2 < n ->
       (match url_hex_value s.[!i + 1], url_hex_value s.[!i + 2] with
        | Some hi, Some lo -> Buffer.add_char buf (Char.chr (hi * 16 + lo)); i := !i + 2
        | _ -> Buffer.add_char buf '%')
     | '+' when plus -> Buffer.add_char buf ' '
     | c -> Buffer.add_char buf c);
    incr i
  done;
  Buffer.contents buf

let url_decode_checked s =
  let n = String.length s in
  let rec check i =
    if i >= n then Ok (url_decode_text ~plus:false s)
    else if s.[i] <> '%' then check (i + 1)
    else if i + 2 >= n then
      Error (Printf.sprintf "%S ends inside a percent-escape" s)
    else
      match url_hex_value s.[i + 1], url_hex_value s.[i + 2] with
      | Some _, Some _ -> check (i + 3)
      | _ ->
        Error (Printf.sprintf "%S is not a percent-escape: %%%c%c is not two hex digits"
                 s s.[i + 1] s.[i + 2])
  in
  check 0

(* `&` separates pairs and the first `=` splits one. A pair with no `=` has
   the empty string for its value -- `?debug` is a query that says so -- and
   an empty pair, from `?a=1&&b=2`, is dropped rather than becoming a key of
   "". *)
let url_query_pairs u =
  (url_parts u).up_query
  |> String.split_on_char '&'
  |> List.filter (fun pair -> pair <> "")
  |> List.map (fun pair ->
       let k, v =
         match String.index_opt pair '=' with
         | Some i ->
           (String.sub pair 0 i,
            String.sub pair (i + 1) (String.length pair - i - 1))
         | None -> (pair, "")
       in
       (url_decode_text ~plus:true k, VString (url_decode_text ~plus:true v)))

(* Setting one half of the userinfo and keeping the other. Both empty means
   there is no userinfo at all rather than a bare `@`, which addresses the
   same host and is not what anyone wrote. *)
let url_set_userinfo u f =
  let parts = url_parts u in
  let au = url_authority_of parts.up_authority in
  let name, pass = f (url_userinfo_parts au) in
  let ui =
    match name, pass with
    | (None | Some ""), None -> None
    | Some n, None -> Some (url_encode_text n)
    | None, Some p -> Some (":" ^ url_encode_text p)
    | Some n, Some p -> Some (url_encode_text n ^ ":" ^ url_encode_text p)
  in
  url_rebuild { parts with up_authority = url_authority_text { au with au_userinfo = ui } }

(* ── Resolving a reference ───────────────────────────────────────────────── *)

(* RFC 3986 section 5.2.4: `.` and `..` are removed by walking the segments,
   so `/a/b/../c` is `/a/c` and a `..` that would climb past the root is
   dropped rather than escaping the authority. *)
let url_remove_dot_segments path =
  let trailing_slash =
    String.length path > 0
    && (let last = path.[String.length path - 1] in last = '/')
  in
  let segs = String.split_on_char '/' path in
  let out =
    List.fold_left
      (fun acc seg ->
        match seg with
        | "" | "." -> acc
        | ".." -> (match acc with [] -> [] | _ :: rest -> rest)
        | s -> s :: acc)
      [] segs
  in
  let body = String.concat "/" (List.rev out) in
  let ends_dotty =
    match List.rev segs with "." :: _ | ".." :: _ -> true | _ -> false
  in
  "/" ^ body ^ (if (trailing_slash || ends_dotty) && body <> "" then "/" else "")

let url_resolve base reference =
  let b = url_parts base in
  let merge rel =
    (* The base's directory, which is everything up to its last `/`. A base
       with no path at all has `/` for one. *)
    let dir =
      match String.rindex_opt b.up_path '/' with
      | Some i -> String.sub b.up_path 0 (i + 1)
      | None -> "/"
    in
    url_remove_dot_segments (dir ^ rel)
  in
  let split_query_fragment rest =
    let rest, frag =
      match String.index_opt rest '#' with
      | Some i ->
        (String.sub rest 0 i, Some (String.sub rest (i + 1) (String.length rest - i - 1)))
      | None -> (rest, None)
    in
    let path, query =
      match String.index_opt rest '?' with
      | Some i ->
        (String.sub rest 0 i, String.sub rest (i + 1) (String.length rest - i - 1))
      | None -> (rest, "")
    in
    (path, query, frag)
  in
  let starts p = String.length reference >= String.length p
                 && String.sub reference 0 (String.length p) = p in
  if reference = "" then Ok base
  (* An absolute reference is a URL in its own right and is checked as one,
     so `URL.join` cannot produce something `String.to_url` would refuse. *)
  else if starts "http://" || starts "https://" then
    (match Lexer.url_error reference with
     | None -> Ok reference
     | Some why -> Error (Printf.sprintf "cannot join %S: %s" reference why))
  else if starts "//" then
    let rest = String.sub reference 2 (String.length reference - 2) in
    let authority, tail =
      match String.index_opt rest '/' with
      | Some i -> (String.sub rest 0 i, String.sub rest i (String.length rest - i))
      | None -> (rest, "")
    in
    let path, query, frag = split_query_fragment tail in
    Ok (url_rebuild { b with up_authority = authority;
                             up_path = url_remove_dot_segments path;
                             up_query = query; up_fragment = frag })
  else if starts "#" then
    Ok (url_rebuild
          { b with up_fragment = Some (String.sub reference 1 (String.length reference - 1)) })
  else if starts "?" then
    let query, frag = match String.index_opt reference '#' with
      | Some i ->
        (String.sub reference 1 (i - 1),
         Some (String.sub reference (i + 1) (String.length reference - i - 1)))
      | None -> (String.sub reference 1 (String.length reference - 1), None)
    in
    Ok (url_rebuild { b with up_query = query; up_fragment = frag })
  else
    let path, query, frag = split_query_fragment reference in
    let path = if starts "/" then url_remove_dot_segments path else merge path in
    Ok (url_rebuild { b with up_path = path; up_query = query; up_fragment = frag })

(* A port is `:8080` in wand's own notation; a document or an environment
   variable holds the bare number. Adding the colon is how the second is read
   as the first. *)
let port_text s =
  let s = String.trim s in
  if String.length s > 0 && s.[0] = ':' then s else ":" ^ s

(* ── Decoding ────────────────────────────────────────────────────────────── *)

(* Reading data that arrived untyped. Every backend -- JSON, TOML, CSV rows,
   the lines of a command's output -- presents what it read in JSON's shape,
   so there is one set of combinators rather than one per format.

   A decoder is handed the path it stands at, kept innermost-first, so a
   failure deep inside a document can say where it happened:

       .items[3].metadata.name: expected String, got Int

   Naming the field is the whole point. A decoder that reported only the
   type would leave the reader doing by hand what a scrape already made them
   do -- find which of forty fields was wrong. *)

let path_string segs =
  match segs with [] -> "" | _ -> String.concat "" (List.rev segs)

let decode_error path msg =
  match path with
  | [] -> Error msg
  | _  -> Error (path_string path ^ ": " ^ msg)

(* What to call a JSON value in a message: the wand type where there is one,
   since the reader is looking for a wand type. *)
let json_kind (j : Yojson.Basic.t) =
  match j with
  | `Null     -> "null"
  | `Bool _   -> "Bool"
  | `Int _    -> "Int"
  | `Float _  -> "Float"
  | `String _ -> "String"
  | `List _   -> "a list"
  | `Assoc _  -> "an object"

(* JSON has no spelling for infinity and none for NaN. A wand Float has all
   three -- `1.0 / 0.0` is `inf` -- so the boundary is here: a number that
   cannot be written is refused at the door it would come in through. YAML
   is read into the same value and answers the same way; YAML can spell
   `.inf`, but the value wand reads it into cannot hold one.

   It used to be refused at the far end, by Yojson, as a fatal error a
   `try` could not catch: `JSON.stringify (JSON.parse! "1e999999")` ended
   the program with exit 2, and so did `"%{j}"`. The two serializers also
   disagreed -- `stringify_pretty` wrote `Infinity`, which is not JSON. *)
let rec json_is_finite (j : Yojson.Basic.t) =
  match j with
  | `Float f  -> Float.is_finite f
  | `List xs  -> List.for_all json_is_finite xs
  | `Assoc kv -> List.for_all (fun (_, v) -> json_is_finite v) kv
  | _         -> true

let unwritable_number =
  "a number here is infinite or NaN, which a JSON value cannot hold"

let expected what path j =
  match j with
  (* A string that failed is worth quoting: the reader wants to see what was
     there, not to be told a second time that it was text. *)
  | `String s -> decode_error path (Printf.sprintf "expected %s, got %S" what s)
  | _ -> decode_error path (Printf.sprintf "expected %s, got %s" what (json_kind j))

(* A decoder reads a value out of text, and never the other way round.
   Backends that carry types -- JSON, TOML -- hand over an Int as an Int;
   backends that do not -- a CSV cell, a line of output -- hand over the
   text, and `Decode.int` reads it exactly as `String.to_int` would. So one
   decoder serves a document and a command's output both.

   The reverse is not allowed: `Decode.string` does not accept a number and
   stringify it. That direction would make `string` accept anything, which
   is the scrape it exists to replace. *)
let from_text parse (j : Yojson.Basic.t) =
  match j with `String s -> parse (String.trim s) | _ -> None

(* Every backend presents what it read in this shape. TOML carries its own
   types across; a date has no JSON of its own and comes over as the text it
   was written as, which is what `Decode.date` reads anyway. *)
let rec json_of_toml (v : Toml.Types.value) : Yojson.Basic.t =
  match v with
  | Toml.Types.TBool b   -> `Bool b
  | Toml.Types.TInt n    -> `Int n
  | Toml.Types.TFloat f  -> `Float f
  | Toml.Types.TString s -> `String s
  | Toml.Types.TDate d   -> `String (Toml.Printer.string_of_value (Toml.Types.TDate d))
  | Toml.Types.TTable tbl ->
    `Assoc (List.map (fun (k, v) ->
      (Toml.Types.Table.Key.to_string k, json_of_toml v))
      (Toml.Types.Table.to_list tbl))
  | Toml.Types.TArray arr ->
    `List (List.map json_of_toml (toml_array_values arr))

(* A domain literal decodes as itself: the string is lexed exactly as it
   would be if it had been written in the source, so `"30s"` in a document
   and `30s` in a script become the same Duration. *)
let decode_lexed name build (j : Yojson.Basic.t) path =
  match j with
  | `String s ->
    (match try_lex_single s with
     | Some tok ->
       (match build tok with
        | Some v -> Ok v
        | None -> decode_error path (Printf.sprintf "expected %s, got %S" name s))
     | None -> decode_error path (Printf.sprintf "expected %s, got %S" name s))
  | _ -> expected name path j



(* ── Par ─────────────────────────────────────────────────────────────────── *)

(* Fork-join, and nothing else. Workers never outlive the call, there is no
   handle to a running one, and these two functions are the only way to start
   any -- so there is no unstructured concurrency to build out of them.

   A worker does not handle its own effects. An effect performed on one
   domain cannot reach a handler on another, and a handler is not a value
   that can be copied there, so a worker hands each effect back to the
   calling domain and waits while it is performed there, inside whatever
   handlers the program already installed. Mocks, rehearsals and traces
   therefore reach into a worker exactly as they do anywhere else, and
   effects happen one at a time rather than racing.

   This runs where it is called rather than through an effect of its own: an
   effect would be caught by the outermost handler, and performing from
   inside that handler is outside the very handlers the work should see. *)
let par_run limit f items ~collect =
  let items = Array.of_list items in
  let n = Array.length items in
  let results = Array.make (max n 1) VUnit in
  let next = Atomic.make 0 in
  let live = Atomic.make 0 in
  let m = Mutex.create () in
  let ready = Condition.create () in
  let pending
    : (string * value * string list option * string list option) option ref =
    ref None in
  let reply : (value, exn) result option ref = ref None in

  (* One worker at a time may have a request outstanding. There is a single
     request slot and a single reply slot, and a reply carries no idea of who
     asked: with two workers waiting, either could take an answer meant for
     the other, leaving the rightful one waiting for a reply that has already
     been consumed. Held across the whole round trip, so a worker that has
     asked is the only one that can be answered. *)
  let speaking = Mutex.create () in
  let forward name arg =
    Mutex.lock speaking;
    let r =
      Fun.protect ~finally:(fun () -> Mutex.unlock speaking) (fun () ->
        Mutex.lock m;
        (* The worker's ambient allowlist rides along: the pump re-performs
           on the main domain, where the worker's domain-local value is
           invisible. *)
        pending := Some (name, arg, Domain.DLS.get ambient_shell_allow,
                         Domain.DLS.get ambient_net_allow);
        Condition.broadcast ready;
        while !reply = None do Condition.wait ready m done;
        let answer = Option.get !reply in
        reply := None;
        Condition.broadcast ready;
        Mutex.unlock m;
        answer)
    in
    match r with Ok v -> v | Error e -> raise e
  in

  let record i outcome = results.(i) <- outcome in
  let finish () =
    Mutex.lock m;
    ignore (Atomic.fetch_and_add live (-1));
    Condition.broadcast ready;
    Mutex.unlock m
  in
  let outcome_of run =
    match run () with
    | v -> if collect then VConstr (Ctor.Builtin "Ok", [v]) else VUnit
    | exception EvalError msg ->
      (* A failure becomes a value, so it says what went wrong rather than
         where, exactly as `try` does. *)
      if collect then VConstr (Ctor.Builtin "Error", [VString (Util.strip_loc_prefix msg)])
      else VUnit
  in
  (* Effects go back to the calling domain, where the handlers are. *)
  let worker_forwarding () =
    let rec loop () =
      let i = Atomic.fetch_and_add next 1 in
      if i < n then begin
        let run () =
          Effect.Deep.match_with (fun () -> apply f items.(i)) ()
            { Effect.Deep.
                retc = (fun v -> v);
                exnc = raise;
                effc = fun (type a) (eff : a Effect.t) ->
                  match eff with
                  | WandEffect (name, arg) ->
                    Some (fun (k : (a, value) Effect.Deep.continuation) ->
                      match forward name arg with
                      | v -> Effect.Deep.continue k v
                      | exception (EvalError _ as e) ->
                        Effect.Deep.discontinue k e)
                  | _ -> None }
        in
        record i (outcome_of run);
        loop ()
      end
    in
    Fun.protect ~finally:finish loop
  in
  (* Nothing is watching: the worker performs its own effects. *)
  let worker_direct () =
    let rec loop () =
      let i = Atomic.fetch_and_add next 1 in
      if i < n then begin
        record i (outcome_of (fun () -> apply f items.(i)));
        loop ()
      end
    in
    Fun.protect ~finally:finish loop
  in

  (* When nothing is watching, a worker performs its own effects on its own
     domain and the work genuinely overlaps. When a handler is in scope -- a
     mock, a rehearsal, a trace -- effects come back here instead, because a
     handler cannot be reached from another domain. That costs the overlap,
     and buys the guarantee that moving work into Par cannot escape whoever
     is watching. Nobody rehearses for speed. *)
  let watched = Atomic.get observers > 0 in
  (* Domain-local state starts empty on a new domain, so the file bound
     comes across by hand: a URL computed inside a worker is the calling
     file's as much as one computed outside it. *)
  let file_net = Domain.DLS.get ambient_file_net in
  let worker () =
    Domain.DLS.set ambient_file_net file_net;
    if watched then worker_forwarding ()
    else ignore (!with_default_handler (fun () -> worker_direct (); VUnit))
  in
  if n = 0 then (if collect then VList [] else VUnit)
  else begin
    let count = max 1 (min limit n) in
    Atomic.set live count;
    let domains = List.init count (fun _ -> Domain.spawn worker) in
    let rec pump () =
      Mutex.lock m;
      while !pending = None && Atomic.get live > 0 do Condition.wait ready m done;
      match !pending with
      | None -> Mutex.unlock m
      | Some (name, arg, allow, net_allow) ->
        pending := None;
        Mutex.unlock m;
        (* Every exception becomes the worker's answer. One that escaped here
           would leave the worker waiting on a reply that never comes, with
           `speaking` held, and the domain never joined. *)
        let answer =
          let saved = Domain.DLS.get ambient_net_allow in
          Domain.DLS.set ambient_net_allow net_allow;
          match perform_shell name allow arg with
          | v -> Domain.DLS.set ambient_net_allow saved; Ok v
          | exception e -> Domain.DLS.set ambient_net_allow saved; Error e
        in
        Mutex.lock m;
        reply := Some answer;
        Condition.broadcast ready;
        Mutex.unlock m;
        pump ()
    in
    (* Answering workers and joining them is one stretch that has to finish:
       see `defer_interrupts`. *)
    defer_interrupts (fun () ->
    pump ();
    (* Every worker is joined before this returns, interrupted or not:
       workers never outlive the call, and an interrupt is not an excuse to
       leave one running. A worker that stopped because the program is
       stopping has already released what it held; the calling domain
       raises on its own next step, from its own stack. *)
    List.iter (fun d ->
      match Domain.join d with
      | () -> ()
      | exception (Interrupted _ | Fun.Finally_raised (Interrupted _)) -> ())
      domains);
    if collect then VList (Array.to_list (Array.sub results 0 n)) else VUnit
  end

(* Run every thunk at once and answer with the first to finish.

   No worker limit, breaking `par_map`'s convention on purpose: the count is
   the length of the list and is visible at the call site, so there is
   nothing left for the caller to state. A race with a limit below the list
   length would be a staged race, which nobody wants.

   First to *finish*, not first to succeed. A loser that raises is
   discarded. A winner that raises comes back as `Error`, the way `par_map`
   puts a raise in the element's place rather than failing the call.

   Cancellation is cooperative, and it is the machinery Ctrl-C already uses.
   The winner's completion sets each loser's cancel flag; the loser raises
   at its next checkpoint, releases what it holds, and is joined. Every
   worker is joined before this returns -- workers never outlive the call,
   which is the invariant that lets `Par` have no handles and nothing to
   await. What a race bounds is when you get the answer, not when the
   machine goes quiet: a loser blocked in a subprocess finishes that
   subprocess. `Shell.timeout` inside the thunk is how to bound that.

   Under a handler, refused. An effect cannot reach a handler on another
   domain, so the branches cannot run where they were written; the race
   would collapse to its first thunk and say nothing, and a test of racing
   code would then test one branch and pass. `Par.timeout` refuses for the
   same reason.

   A rehearsal and a trace are observers as well, and are not refused: each
   reports what the work would do, and the collapse costs the report
   nothing. The race is then left-biased and deterministic -- the first
   thunk is the one that finishes first. *)
let par_race thunks =
  let items = Array.of_list thunks in
  let n = Array.length items in
  let outcome_of run =
    match run () with
    | v -> VConstr (Ctor.Builtin "Ok", [v])
    | exception EvalError msg ->
      VConstr (Ctor.Builtin "Error", [VString (Util.strip_loc_prefix msg)])
  in
  if n = 0 then
    VConstr (Ctor.Builtin "Error", [VString "race: nothing to race"])
  else if Atomic.get handlers > 0 then
    raise (EvalError
      "a race inside a handler runs its first thunk only. Move the handler \
       inside each thunk, or take it off.")
  else if Atomic.get observers > 0 then
    outcome_of (fun () -> apply items.(0) VUnit)
  else with_race_running (fun () ->
    let flags = Array.init n (fun _ -> ref false) in
    let m = Mutex.create () in
    let done_ = Condition.create () in
    let winner = ref None in
    let file_net = Domain.DLS.get ambient_file_net in
    let worker i () =
      Domain.DLS.set ambient_file_net file_net;
      cancel_this_domain flags.(i);
      let result =
        ignore (!with_default_handler (fun () ->
          let o = outcome_of (fun () -> apply items.(i) VUnit) in
          Mutex.lock m;
          if !winner = None then winner := Some o;
          Condition.broadcast done_;
          Mutex.unlock m;
          VUnit));
        ()
      in
      result
    in
    let domains = Array.to_list (Array.init n (fun i -> Domain.spawn (worker i))) in
    (* Answering nothing and joining everything is one stretch that has to
       finish, as it is in `par_run`. *)
    defer_interrupts (fun () ->
      Mutex.lock m;
      while !winner = None do Condition.wait done_ m done;
      Mutex.unlock m;
      (* Whoever is still running has lost. *)
      Array.iter (fun f -> f := true) flags;
      List.iter (fun d ->
        match Domain.join d with
        | () -> ()
        (* A loser stopped where it stood. `Fun.Finally_raised` is the same
           thing seen through a bracket it was releasing. *)
        | exception Interrupted _ -> ()
        | exception Fun.Finally_raised (Interrupted _) -> ()) domains);
    match !winner with
    | Some o -> o
    | None -> VConstr (Ctor.Builtin "Error", [VString "race: no thunk finished"]))

(* ── Streams: running a terminal operation ────────────────────────────────
   A terminal operation performs one open-granularity effect per source --
   the answer is the line source: the default handler wraps the real
   channel, a mock answers with a plain list -- then pulls internally,
   running stages and the caller's closures from the ordinary stack, and
   releases on the way out however the run ends. *)

let stdin_streamed = Atomic.make false

(* The close half takes what the read ended as: `~early:true` for a stream
   that stopped before its source ran out -- a satisfied `take`, or a stage
   that raised -- and `~early:false` for one read to exhaustion. Only a
   subprocess source has anything to do with the answer, and it is the whole
   of its exit-status rule; a file closes the same way either way. *)
let rec stream_provider (src : stream_source)
  : (unit -> value option) * (early:bool -> unit) =
  let of_vals vs =
    let rest = ref vs in
    ((fun () -> match !rest with [] -> None | v :: tl -> rest := tl; Some v),
     fun ~early:_ -> ())
  in
  let of_channel ~close ic =
    ((fun () ->
        match In_channel.input_line ic with
        | Some l -> Some (VString l)
        | None -> None),
     if close then (fun ~early:_ -> close_in_noerr ic)
     else (fun ~early:_ -> ()))
  in
  match src with
  | SVals vs -> of_vals vs
  | SPull f -> (f, fun ~early:_ -> ())
  | SFiles paths ->
    (* One open per file, each through the same operation a single file
       goes through, so a trace shows every file and a mock answers per
       file. A file is opened only when the one before it runs out. *)
    let rest = ref paths in
    let current = ref None in
    let rec next () =
      match !current with
      | Some (pull, close) ->
        (match pull () with
         | Some v -> Some v
         | None -> close ~early:false; current := None; next ())
      | None ->
        (match !rest with
         | [] -> None
         | p :: tl ->
           rest := tl;
           current := Some (stream_provider (SFile p));
           next ())
    in
    (next,
     fun ~early ->
       match !current with
       | Some (_, close) -> current := None; close ~early
       | None -> ())
  | SCommand (cmd, allow) ->
    (match perform_shell "Shell!stream" allow (VString cmd) with
     | VProcSource (pull, finish) -> (pull, fun ~early -> finish early)
     | VList vs -> of_vals vs
     | _ -> raise (EvalError
         "Shell.stream: the handler must answer with a list of lines"))
  | SFile p ->
    (match Effect.perform (WandEffect ("FS!stream_lines", VPath p)) with
     | VLineSource ic -> of_channel ~close:true ic
     | VList vs -> of_vals vs
     | _ -> raise (EvalError
         "stream_lines: the handler must answer with a list of lines"))
  | SStdin ->
    (match Effect.perform (WandEffect ("IO!stdin_lines", VUnit)) with
     | VLineSource ic ->
       (* The real stdin cannot be re-run; a mock can. The flag burns only
          on the real path, so tests replay freely. *)
       if Atomic.exchange stdin_streamed true then
         raise (EvalError
           "stdin has already been streamed once and cannot be re-run")
       else of_channel ~close:false ic
     | VList vs -> of_vals vs
     | _ -> raise (EvalError
         "stdin_lines: the handler must answer with a list of lines"))

(* A stage while a stream is running: the description plus whatever it has
   to remember. `take` counts down, `chunks` fills a buffer, `unique` keeps
   what it has seen. The description is reusable and this is not, so it is
   built per run -- which is what lets one stream be folded twice. *)
type live_stage =
  | LMap       of value
  | LFilter    of value
  | LFilterMap of value
  | LFlatMap   of value
  | LTake      of int ref
  | LTakeWhile of value
  | LDrop      of int ref
  | LDropWhile of value * bool ref      (* still dropping? *)
  | LIndexed   of int ref
  | LScan      of value * value ref
  | LChunks    of int * value list ref  (* held in reverse *)
  (* Keyed like `List.unique`: `eq_key` narrows to the values that could be
     equal, `wand_equal` decides among them. This was a `value list ref`
     scanned end to end for every item, so a stream of n distinct elements
     cost n^2/2 comparisons -- 20,000 unique lines took six seconds and
     200,000 would have taken about ten minutes. *)
  | LUnique    of (value, value list) Hashtbl.t

let run_stream_terminal (desc : stream_desc) ~(on_item : value -> unit) : unit =
  let (pull, close) = stream_provider desc.s_source in
  let stages = List.map (function
    | StMap f       -> LMap f
    | StFilter f    -> LFilter f
    | StFilterMap f -> LFilterMap f
    | StFlatMap f   -> LFlatMap f
    | StTake n      -> LTake (ref n)
    | StTakeWhile f -> LTakeWhile f
    | StDrop n      -> LDrop (ref n)
    | StDropWhile f -> LDropWhile (f, ref true)
    | StIndexed     -> LIndexed (ref 0)
    | StScan (f, init) -> LScan (f, ref init)
    | StChunks n    -> LChunks (n, ref [])
    | StUnique      -> LUnique (Hashtbl.create 64)) desc.s_stages
  in
  (* A gate that closes ends the read: nothing later can pass it, so there is
     nothing left to pull for. `take 100` of a 10GB file reads 100 lines. *)
  let stop = ref false in
  let bool_of what v =
    match v with
    | VBool b -> b
    | _ -> raise (EvalError
        (Printf.sprintf "Stream.%s: the predicate must return Bool" what))
  in
  (* One item into one stage, and out comes what that stage passes on. *)
  let step st x =
    match st with
    | LMap f -> [apply f x]
    | LFilter f -> if bool_of "filter" (apply f x) then [x] else []
    | LFilterMap f ->
      (match apply f x with
       | VConstr (c, [v]) when Ctor.name c = "Some" -> [v]
       | VConstr (c, []) when Ctor.name c = "None" -> []
       | _ -> raise (EvalError "Stream.filter_map: the function must return an Option"))
    | LFlatMap f ->
      (match apply f x with
       | VList vs -> vs
       | _ -> raise (EvalError "Stream.flat_map: the function must return a List"))
    | LTake r -> if !r <= 0 then (stop := true; []) else (decr r;
                   if !r <= 0 then stop := true; [x])
    | LTakeWhile f -> if bool_of "take_while" (apply f x) then [x]
                      else (stop := true; [])
    | LDrop r -> if !r > 0 then (decr r; []) else [x]
    | LDropWhile (f, dropping) ->
      if not !dropping then [x]
      else if bool_of "drop_while" (apply f x) then []
      else (dropping := false; [x])
    | LIndexed i -> let n = !i in incr i; [VTuple [VInt n; x]]
    | LScan (f, acc) -> acc := apply (apply f !acc) x; [!acc]
    | LChunks (n, held) ->
      held := x :: !held;
      if List.length !held >= n then
        (let full = List.rev !held in held := []; [VList full])
      else []
    | LUnique seen ->
      let k = eq_key x in
      let bucket = match Hashtbl.find seen k with
        | b -> b
        | exception Not_found -> []
      in
      if List.exists (wand_equal x) bucket then []
      else (Hashtbl.replace seen k (x :: bucket); [x])
  in
  (* What a stage still holds when the source runs out. Only `chunks` holds
     anything: a last group of fewer than n is a group, and dropping it would
     lose the tail of every file whose length is not a multiple of n. *)
  let flush st =
    match st with
    | LChunks (_, held) ->
      if !held = [] then [] else (let last = List.rev !held in held := []; [VList last])
    | _ -> []
  in
  let rec push stages vs =
    match stages with
    | [] -> List.iter on_item vs
    | st :: rest -> push rest (List.concat_map (step st) vs)
  in
  (* Innermost first: what one stage lets go still has the stages after it to
     pass through, and they may be holding something of their own. *)
  let rec flush_from stages =
    match stages with
    | [] -> ()
    | st :: rest -> push rest (flush st); flush_from rest
  in
  (* Closed twice would be wrong and closed not at all would leak, so the
     read closes itself and the bracket covers only the paths that did not
     get there. Closing inside the body rather than in the `finally` is what
     lets a source report a failure of its own -- a command that exited
     non-zero -- as an ordinary raise: an exception out of a `finally`
     arrives wrapped as `Fun.Finally_raised`, which `try` does not
     recognise. *)
  let exhausted = ref false in
  let closed = ref false in
  let finish ~early = if not !closed then (closed := true; close ~early) in
  Fun.protect ~finally:(fun () -> finish ~early:true) (fun () ->
    while not !stop do
      match pull () with
      | None -> stop := true; exhausted := true
      | Some v -> push stages [v]
    done;
    (* A stream that stopped early is not owed its buffers: `take 3` asked
       for three items, not for three and a part-filled chunk. *)
    if !exhausted then flush_from stages;
    finish ~early:(not !exhausted))

(* Open once, write each line, finish on the way out. The lines are the
   stream's own, so a source that fails or a stage that raises arrives here
   as an ordinary raise -- and which of the sink's two endings that takes is
   the whole reason it has two.

   `Fun.protect` cannot express this: it has one exit, and it could not tell
   a stream that finished from one that raised. It would also be the wrong
   shape for the reason `run_stream_terminal` already documents -- an
   exception out of a `finally` arrives wrapped as `Fun.Finally_raised`,
   which `try` does not recognise. `abort` swallows its own failures for
   that same reason: the raise a caller has to see is the stream's. *)
let write_stream_to op path desc =
  let target = match path with
    | VPath p | VString p -> VPath p
    | _ -> raise (EvalError "expected a Path")
  in
  match Effect.perform (WandEffect (op, target)) with
  | VLineSink (write_line, commit, abort) ->
    (match run_stream_terminal desc ~on_item:(fun v -> write_line (to_text v)) with
     | () -> commit (); VUnit
     | exception e -> (try abort () with _ -> ()); raise e)
  (* A mock, or a rehearsal: the lines are still pulled, so the source is
     still read and the stages still run, and nothing is written. *)
  | _ -> run_stream_terminal desc ~on_item:(fun _ -> ()); VUnit

let stream_builtins : env = [
  ("fs_stream_lines", VBuiltin (function
    | VPath p | VString p -> VStream { s_source = SFile p; s_stages = [] }
    | _ -> raise (EvalError "stream_lines: expected Path")));
  ("io_stdin_lines", VBuiltin (fun _ ->
    VStream { s_source = SStdin; s_stages = [] }));
  ("fs_stream_lines_all", VBuiltin (function
    | VList ps ->
      let path = function
        | VPath p | VString p -> p
        | _ -> raise (EvalError "stream_lines_all: expected a list of Paths")
      in
      VStream { s_source = SFiles (List.map path ps); s_stages = [] }
    | _ -> raise (EvalError "stream_lines_all: expected a list of Paths")));
  ("shell_stream", VBuiltin (function
    | VCommand (cmd, allow) ->
      VStream { s_source = SCommand (cmd, allow); s_stages = [] }
    | _ -> raise (EvalError "Shell.stream: expected a Command")));
  ("stream_of_list", VBuiltin (function
    | VList vs -> VStream { s_source = SVals vs; s_stages = [] }
    | _ -> raise (EvalError "Stream.of_list: expected List")));
  ("stream_map", VBuiltin (fun f -> VBuiltin (function
    | VStream d -> VStream { d with s_stages = d.s_stages @ [StMap f] }
    | _ -> raise (EvalError "Stream.map: expected Stream"))));
  ("stream_filter", VBuiltin (fun f -> VBuiltin (function
    | VStream d -> VStream { d with s_stages = d.s_stages @ [StFilter f] }
    | _ -> raise (EvalError "Stream.filter: expected Stream"))));
  ("stream_filter_map", VBuiltin (fun f -> VBuiltin (function
    | VStream d -> VStream { d with s_stages = d.s_stages @ [StFilterMap f] }
    | _ -> raise (EvalError "Stream.filter_map: expected Stream"))));
  ("stream_flat_map", VBuiltin (fun f -> VBuiltin (function
    | VStream d -> VStream { d with s_stages = d.s_stages @ [StFlatMap f] }
    | _ -> raise (EvalError "Stream.flat_map: expected Stream"))));
  ("stream_take_while", VBuiltin (fun f -> VBuiltin (function
    | VStream d -> VStream { d with s_stages = d.s_stages @ [StTakeWhile f] }
    | _ -> raise (EvalError "Stream.take_while: expected Stream"))));
  ("stream_drop_while", VBuiltin (fun f -> VBuiltin (function
    | VStream d -> VStream { d with s_stages = d.s_stages @ [StDropWhile f] }
    | _ -> raise (EvalError "Stream.drop_while: expected Stream"))));
  ("stream_drop", VBuiltin (function
    | VInt n -> VBuiltin (function
      | VStream d -> VStream { d with s_stages = d.s_stages @ [StDrop n] }
      | _ -> raise (EvalError "Stream.drop: expected Stream"))
    | _ -> raise (EvalError "Stream.drop: expected Int")));
  ("stream_indexed", VBuiltin (function
    | VStream d -> VStream { d with s_stages = d.s_stages @ [StIndexed] }
    | _ -> raise (EvalError "Stream.indexed: expected Stream")));
  ("stream_scan", VBuiltin (fun f -> VBuiltin (fun init -> VBuiltin (function
    | VStream d -> VStream { d with s_stages = d.s_stages @ [StScan (f, init)] }
    | _ -> raise (EvalError "Stream.scan: expected Stream")))));
  ("stream_chunks", VBuiltin (function
    | VInt n when n > 0 -> VBuiltin (function
      | VStream d -> VStream { d with s_stages = d.s_stages @ [StChunks n] }
      | _ -> raise (EvalError "Stream.chunks: expected Stream"))
    | VInt _ -> raise (EvalError "Stream.chunks: a chunk holds at least one element")
    | _ -> raise (EvalError "Stream.chunks: expected Int")));
  ("stream_unique", VBuiltin (function
    | VStream d -> VStream { d with s_stages = d.s_stages @ [StUnique] }
    | _ -> raise (EvalError "Stream.unique: expected Stream")));
  ("stream_take", VBuiltin (function
    | VInt n -> VBuiltin (function
      | VStream d -> VStream { d with s_stages = d.s_stages @ [StTake n] }
      | _ -> raise (EvalError "Stream.take: expected Stream"))
    | _ -> raise (EvalError "Stream.take: expected Int")));
  ("stream_fold", VBuiltin (fun f -> VBuiltin (fun init -> VBuiltin (function
    | VStream d ->
      let acc = ref init in
      run_stream_terminal d ~on_item:(fun x -> acc := apply (apply f !acc) x);
      !acc
    | _ -> raise (EvalError "Stream.fold_left: expected Stream")))));
  ("stream_tally", VBuiltin (function
    | VStream d ->
      let acc = ref vmap_empty in
      run_stream_terminal d ~on_item:(fun x ->
        match x with
        | VString k ->
          acc := vmap_update k
                   (function Some (VInt n) -> VInt (n + 1) | _ -> VInt 1) !acc
        | _ -> raise (EvalError "Stream.tally: expected a Stream of String"));
      VMap !acc
    | _ -> raise (EvalError "Stream.tally: expected Stream")));
  (* The write half of a stream, and a terminal operation: it opens the file
     once, pulls the source through the stages, writes each line, and closes
     however the run ends. In `FS` rather than in `Stream` because it is
     about a file, and `FS` is where a file is opened.

     One operation per open, like the read side. A handler that answers it
     without a channel -- `| FS!write_lines _ k -> k ()` -- sends the lines
     nowhere, which is what a test wants and what `--dry-run` does. *)
  ("fs_write_lines", VBuiltin (fun path -> VBuiltin (function
    | VStream d -> write_stream_to "FS!write_lines" path d
    | _ -> raise (EvalError "FS.write_lines: expected a Stream"))));
  ("fs_append_lines", VBuiltin (fun path -> VBuiltin (function
    | VStream d -> write_stream_to "FS!append_lines" path d
    | _ -> raise (EvalError "FS.append_lines: expected a Stream"))));
  ("fs_write_lines_atomic", VBuiltin (fun path -> VBuiltin (function
    | VStream d -> write_stream_to "FS!write_lines_atomic" path d
    | _ -> raise (EvalError "FS.write_lines_atomic: expected a Stream"))));
  ("stream_each", VBuiltin (fun f -> VBuiltin (function
    | VStream d ->
      run_stream_terminal d ~on_item:(fun x -> ignore (apply f x));
      VUnit
    | _ -> raise (EvalError "Stream.each: expected Stream"))));
  ("stream_to_list", VBuiltin (function
    | VStream d ->
      let acc = ref [] in
      run_stream_terminal d ~on_item:(fun x -> acc := x :: !acc);
      VList (List.rev !acc)
    | _ -> raise (EvalError "Stream.to_list: expected Stream")));
]

let stdlib_eval_env : env = [
  ("io_print",   VBuiltin (fun v -> Effect.perform (WandEffect ("IO!print",   v))));
  ("io_println", VBuiltin (fun v -> Effect.perform (WandEffect ("IO!println", v))));
  ("proc_exit",  performing "Proc!exit" (function VInt n -> raise (Interrupted n) | _ -> raise (EvalError "exit: expected Int")));
  (* Not `performing`: argv and the pid are fixed before the program starts,
     so there is no operation for a handler to intercept. *)
  ("proc_args", VBuiltin (function
    | VUnit -> VList (List.map (fun s -> VString s) !exe_args_ref)
    | _ -> raise (EvalError "proc_args: expected Unit")));
  ("proc_pid", VBuiltin (function
    | VUnit -> VInt (Unix.getpid ())
    | _ -> raise (EvalError "proc_pid: expected Unit")));
  (* A deadline on the commands a thunk runs. It is set for the extent of
     the call and taken off after, so a command outside the thunk waits as
     long as it takes.

     Only a timeout comes back as `Error`. Every other raise passes
     through: a command that exits non-zero has failed, not run late, and
     `$?()` is what asks about an exit code. *)
  ("shell_timeout", VBuiltin (function
    | VDuration d -> VBuiltin (fun thunk ->
      let saved = Domain.DLS.get shell_deadline in
      Domain.DLS.set shell_deadline (Some (parse_dur_ms d));
      let restore () = Domain.DLS.set shell_deadline saved in
      (match Fun.protect ~finally:restore (fun () -> apply thunk VUnit) with
       | v -> VConstr (Ctor.Builtin "Ok", [v])
       (* The raise carries a position by the time it gets here, and the
          marker sits after it. *)
       | exception EvalError msg
         when String.starts_with ~prefix:timeout_prefix (Util.strip_loc_prefix msg) ->
         VConstr (Ctor.Builtin "Error",
                  [VString (drop_prefix timeout_prefix
                              (Util.strip_loc_prefix msg))])))
    | _ -> raise (EvalError "Shell.timeout: expected a Duration")));
  (* Waiting is an effect, so a handler can answer it: a test that
     exercises an hour of backoff must not take an hour. *)
  ("clock_sleep", performing "Clock!sleep" (function
    | VDuration d -> sleep_ms (parse_dur_ms d); VUnit
    | _ -> raise (EvalError "Clock.sleep: expected Duration")));
  (* Reading the clock is an effect for the same reason waiting is: a
     handler can answer it, so a test pins the instant instead of arranging
     for one. UTC always -- a local reading would make one script answer
     differently on two machines. *)
  (* The reading behind `Clock.timed`. It is a primitive rather than a
     module export on purpose: two readings of a monotonic clock subtract
     soundly, and there is nothing else to do with one, so the only shape
     wand offers is the bracket. See `lib/ext/clock.c`. *)
  ("clock_elapsed", performing "Clock!timed" (function
    | VUnit -> VDuration (format_dur_ms (elapsed_ms ()))
    | _ -> raise (EvalError "Clock.timed: expected Unit")));
  ("clock_now", performing "Clock!now" (function
    | VUnit -> VDateTime (datetime_of_epoch (int_of_float (Unix.gettimeofday ())))
    | _ -> raise (EvalError "Clock.now: expected Unit")));
  (* Drawing is an effect for the same reason reading the clock is: what it
     answers is not in the program, so a caller has to be told, and a
     handler can answer it instead. *)
  ("random_below", performing "Random!int" (function
    | VInt n -> VInt (random_below n)
    | _ -> raise (EvalError "Random: expected Int")));
  ("random_float", performing "Random!float" (function
    | VUnit -> random_ready (); VFloat (Stdlib.Random.float 1.0)
    | _ -> raise (EvalError "Random.float: expected Unit")));
  ("random_seed", performing "Random!seed" (function
    | VInt n -> Stdlib.Random.init n; random_seeded := true; VUnit
    | _ -> raise (EvalError "Random.seed: expected Int")));
  ("option_get_exn", VBuiltin (function
    | VUnit -> raise (EvalError "Option.get!: called on None")
    | _ -> raise (EvalError "option_get_exn: expected Unit")));
  (* The one primitive that raises a message the standard library composed
     rather than one this file wrote. Reachable only from `stdlib/`, which
     is why wand still has no user-facing `raise`: a `!` sibling whose
     Result version does the checking has no other way to report what the
     check found. *)
  ("fail_exn", VBuiltin (function
    | VString why -> raise (EvalError why)
    | _ -> raise (EvalError "fail_exn: expected String")));
  ("read_file",  VBuiltin (fun v -> Effect.perform (WandEffect ("FS!read_file",  v))));
  ("hash_file", VBuiltin (function
    | VString algo -> VBuiltin (fun path ->
        Effect.perform (WandEffect ("Hash!file", VTuple [VString algo; path])))
    | _ -> raise (EvalError "hash_file: expected String")));
  ("write_file", VBuiltin (fun path ->
    VBuiltin (fun content ->
      Effect.perform (WandEffect ("FS!write_file", VTuple [path; content])))));
  ("write_atomic", VBuiltin (fun path ->
    VBuiltin (fun content ->
      Effect.perform (WandEffect ("FS!write_atomic", VTuple [path; content])))));
  (* Result constructors *)
  ("Ok",    VPartialConstr (Ctor.Builtin "Ok",    1, []));
  ("Error", VPartialConstr (Ctor.Builtin "Error", 1, []));
  (* The methods, spelled as the wire spells them. Nullary, so each is a
     value in scope rather than something to apply. *)
  ("GET",    VConstr (Ctor.Builtin "GET",    []));
  ("POST",   VConstr (Ctor.Builtin "POST",   []));
  ("PUT",    VConstr (Ctor.Builtin "PUT",    []));
  ("PATCH",  VConstr (Ctor.Builtin "PATCH",  []));
  ("DELETE", VConstr (Ctor.Builtin "DELETE", []));
  ("HEAD",   VConstr (Ctor.Builtin "HEAD",   []));
  (* String primitives *)
  ("str_length", VBuiltin (function
    | VString s -> VInt (String.length s)
    | _ -> raise (EvalError "str_length: expected String")));
  ("str_upper", VBuiltin (function
    | VString s -> VString (String.uppercase_ascii s)
    | _ -> raise (EvalError "str_upper: expected String")));
  ("str_lower", VBuiltin (function
    | VString s -> VString (String.lowercase_ascii s)
    | _ -> raise (EvalError "str_lower: expected String")));
  ("str_trim", VBuiltin (function
    | VString s -> VString (String.trim s)
    | _ -> raise (EvalError "str_trim: expected String")));
  ("str_slice", VBuiltin (function
    | VInt start -> VBuiltin (function
      | VInt end_ -> VBuiltin (function
        | VString s ->
          let len = String.length s in
          let start = max 0 (min start len) in
          let end_  = max start (min end_ len) in
          VString (String.sub s start (end_ - start))
        | _ -> raise (EvalError "str_slice: expected String"))
      | _ -> raise (EvalError "str_slice: expected Int"))
    | _ -> raise (EvalError "str_slice: expected Int")));
  ("str_split", VBuiltin (function
    | VString delim -> VBuiltin (function
      | VString str -> VList (str_split_impl delim str)
      | _ -> raise (EvalError "str_split: expected String"))
    | _ -> raise (EvalError "str_split: expected String")));
  ("str_word", VBuiltin (function
    | VInt n -> VBuiltin (function
      | VString str ->
        (match str_word_impl n str with
         | (Some w, _) -> VConstr (Ctor.Builtin "Some", [VString w])
         | (None, _)   -> v_none)
      | _ -> raise (EvalError "str_word: expected String"))
    | _ -> raise (EvalError "str_word: expected Int")));
  ("str_word_exn", VBuiltin (function
    | VInt n -> VBuiltin (function
      | VString str ->
        (match str_word_impl n str with
         | (Some w, _) -> VString w
         | (None, count) ->
           raise (EvalError (Printf.sprintf "no word %d: the string has %d" n count)))
      | _ -> raise (EvalError "str_word!: expected String"))
    | _ -> raise (EvalError "str_word!: expected Int")));
  ("str_words", VBuiltin (function
    | VString str -> VList (str_words_impl str)
    | _ -> raise (EvalError "str_words: expected String")));
  ("str_lines", VBuiltin (function
    | VString str -> VList (str_lines_impl str)
    | _ -> raise (EvalError "str_lines: expected String")));
  ("str_contains", VBuiltin (function
    | VString needle -> VBuiltin (function
      | VString haystack -> VBool (str_contains_impl needle haystack)
      | _ -> raise (EvalError "str_contains: expected String"))
    | _ -> raise (EvalError "str_contains: expected String")));
  ("str_starts_with", VBuiltin (function
    | VString prefix -> VBuiltin (function
      | VString s ->
        VBool (String.starts_with ~prefix s)
      | _ -> raise (EvalError "str_starts_with: expected String"))
    | _ -> raise (EvalError "str_starts_with: expected String")));
  ("str_ends_with", VBuiltin (function
    | VString suffix -> VBuiltin (function
      | VString s ->
        VBool (String.ends_with ~suffix s)
      | _ -> raise (EvalError "str_ends_with: expected String"))
    | _ -> raise (EvalError "str_ends_with: expected String")));
  ("str_replace", VBuiltin (function
    | VString old_ -> VBuiltin (function
      | VString new_ -> VBuiltin (function
        | VString s -> VString (str_replace_impl old_ new_ s)
        | _ -> raise (EvalError "str_replace: expected String"))
      | _ -> raise (EvalError "str_replace: expected String"))
    | _ -> raise (EvalError "str_replace: expected String")));
  ("str_trim_left", VBuiltin (function
    | VString s -> VString (str_trim_left s)
    | _ -> raise (EvalError "str_trim_left: expected String")));
  ("str_trim_right", VBuiltin (function
    | VString s -> VString (str_trim_right s)
    | _ -> raise (EvalError "str_trim_right: expected String")));
  ("str_repeat", VBuiltin (function
    | VInt n -> VBuiltin (function
      | VString s -> VString (str_repeat n s)
      | _ -> raise (EvalError "str_repeat: expected String"))
    | _ -> raise (EvalError "str_repeat: expected Int")));
  ("str_reverse", VBuiltin (function
    | VString s -> VString (str_reverse s)
    | _ -> raise (EvalError "str_reverse: expected String")));
  ("str_truncate", VBuiltin (function
    | VInt n -> VBuiltin (function
      | VString s -> VString (str_truncate n s)
      | _ -> raise (EvalError "str_truncate: expected String"))
    | _ -> raise (EvalError "str_truncate: expected Int")));
  ("hash_hex", VBuiltin (function
    | VString algo -> VBuiltin (function
      | VString data -> VString (hash_string_hex algo data)
      | _ -> raise (EvalError "hash_hex: expected String"))
    | _ -> raise (EvalError "hash_hex: expected String")));
  ("hmac_hex", VBuiltin (function
    | VString algo -> VBuiltin (function
      | VString key -> VBuiltin (function
        | VString msg -> VString (hash_hmac_hex algo key msg)
        | _ -> raise (EvalError "hmac_hex: expected String"))
      | _ -> raise (EvalError "hmac_hex: expected String"))
    | _ -> raise (EvalError "hmac_hex: expected String")));
  ("hex_decode", VBuiltin (function
    | VString s ->
      (match hex_decode s with
       | Ok b    -> VConstr (Ctor.Builtin "Ok", [VString b])
       | Error m -> VConstr (Ctor.Builtin "Error", [VString m]))
    | _ -> raise (EvalError "hex_decode: expected String")));
  ("hex_encode", VBuiltin (function
    | VString s -> VString (hex_encode s)
    | _ -> raise (EvalError "hex_encode: expected String")));
  ("base64_encode", VBuiltin (function
    | VString s -> VString (base64_encode b64_std s)
    | _ -> raise (EvalError "base64_encode: expected String")));
  ("base64_encode_url", VBuiltin (function
    | VString s -> VString (base64_encode b64_url s)
    | _ -> raise (EvalError "base64_encode_url: expected String")));
  ("base64_decode", VBuiltin (function
    | VString s ->
      (match base64_decode b64_std s with
       | Ok b    -> VConstr (Ctor.Builtin "Ok", [VString b])
       | Error m -> VConstr (Ctor.Builtin "Error", [VString m]))
    | _ -> raise (EvalError "base64_decode: expected String")));
  ("base64_decode_url", VBuiltin (function
    | VString s ->
      (match base64_decode b64_url s with
       | Ok b    -> VConstr (Ctor.Builtin "Ok", [VString b])
       | Error m -> VConstr (Ctor.Builtin "Error", [VString m]))
    | _ -> raise (EvalError "base64_decode_url: expected String")));
  (* Constant time in the length it compares, which is what an HMAC check
     needs. The lengths themselves are compared first and leak, and a digest
     length is public. *)
  ("ct_equal", VBuiltin (function
    | VString a -> VBuiltin (function
      | VString b -> VBool (String.length a = String.length b && Eqaf.equal a b)
      | _ -> raise (EvalError "ct_equal: expected String"))
    | _ -> raise (EvalError "ct_equal: expected String")));
  ("str_bytes", VBuiltin (function
    | VString s ->
      VList (List.init (String.length s) (fun i -> VString (String.make 1 s.[i])))
    | _ -> raise (EvalError "str_bytes: expected String")));
  ("int_to_str", VBuiltin (function
    | VInt n -> VString (string_of_int n)
    | _ -> raise (EvalError "int_to_str: expected Int")));
  ("float_of_int", VBuiltin (function
    | VInt n -> VFloat (float_of_int n)
    | _ -> raise (EvalError "float_of_int: expected Int")));
  (* Round half away from zero, the arithmetic reading of "round": -2.5
     rounds to -3, as Float.round's doc states. *)
  ("float_round", VBuiltin (function
    | VFloat f -> VInt (int_of_float (Float.round f))
    | _ -> raise (EvalError "float_round: expected Float")));
  ("float_floor", VBuiltin (function
    | VFloat f -> VInt (int_of_float (Float.floor f))
    | _ -> raise (EvalError "float_floor: expected Float")));
  ("float_ceil", VBuiltin (function
    | VFloat f -> VInt (int_of_float (Float.ceil f))
    | _ -> raise (EvalError "float_ceil: expected Float")));
  ("float_abs", VBuiltin (function
    | VFloat f -> VFloat (Float.abs f)
    | _ -> raise (EvalError "float_abs: expected Float")));
  (* A width is a printing decision, so this answers a String. Rounding the
     Float instead would answer a value that cannot hold the answer: no
     Float is exactly 0.1, and `%.*f` is the only place the digits are
     decided once. A negative width reads as none. *)
  ("float_format", VBuiltin (function
    | VInt digits -> VBuiltin (function
      | VFloat f -> VString (Printf.sprintf "%.*f" (max 0 digits) f)
      | _ -> raise (EvalError "float_format: expected Float"))
    | _ -> raise (EvalError "float_format: expected Int")));
  ("str_to_int", VBuiltin (function
    | VString s ->
      (match int_of_string_opt (String.trim s) with
       | Some n -> VConstr (Ctor.Builtin "Ok",    [VInt n])
       | None   -> VConstr (Ctor.Builtin "Error", [VString (Printf.sprintf "cannot parse %S as Int" s)]))
    | _ -> raise (EvalError "str_to_int: expected String")));
  ("str_to_float", VBuiltin (function
    | VString s ->
      (match float_of_string_opt (String.trim s) with
       | Some f -> VConstr (Ctor.Builtin "Ok",    [VFloat f])
       | None   -> VConstr (Ctor.Builtin "Error", [VString (Printf.sprintf "cannot parse %S as Float" s)]))
    | _ -> raise (EvalError "str_to_float: expected String")));
  ("str_to_bool", VBuiltin (function
    | VString s ->
      (match String.lowercase_ascii (String.trim s) with
       | "true"  -> VConstr (Ctor.Builtin "Ok",    [VBool true])
       | "false" -> VConstr (Ctor.Builtin "Ok",    [VBool false])
       | _       -> VConstr (Ctor.Builtin "Error", [VString (Printf.sprintf "cannot parse %S as Bool" s)]))
    | _ -> raise (EvalError "str_to_bool: expected String")));
  ("str_to_path", VBuiltin (function
    | VString s -> VPath s
    | _ -> raise (EvalError "str_to_path: expected String")));
  (* Checked against the URL grammar rather than by re-lexing. The lexer ends
     a URL literal at `,` and `;` because they are the punctuation around it,
     and routing this through the scanner made that writing rule into a rule
     about URLs: `https://x/a?b=1,2` is a legal URL that no expression could
     produce. `Lexer.url_error` is what the literal is checked against too. *)
  (* A glob from text, so a pattern out of argv, an environment variable or
     a config file can be one. There was no way to build a Glob at all
     before this: the literal was the only source, and it cannot spell a
     pattern beginning with a bare name -- `src/**/*.ts` reads as a variable
     called `src`, and only `./src/**/*.ts` lexes. *)
  ("str_to_glob", VBuiltin (function
    | VString s ->
      let text = String.trim s in
      (match Lexer.glob_error text with
       | Some why ->
         VConstr (Ctor.Builtin "Error", [VString
           (Printf.sprintf "cannot parse %S as Glob: %s" s why)])
       | None ->
         (* Compiled here and thrown away, to learn whether it compiles. An
            unclosed class is not a glob, and answering one that raises the
            first time it is matched moves the failure away from the text
            that caused it. *)
         (match glob_compile text with
          | Ok _ -> VConstr (Ctor.Builtin "Ok", [VGlob text])
          | Error why ->
            VConstr (Ctor.Builtin "Error", [VString
              (Printf.sprintf "cannot parse %S as Glob: %s" s why)])))
    | _ -> raise (EvalError "str_to_glob: expected String")));
  (* Whether a path is one the pattern selects, without going to the disk to
     find out. The matcher is the one `FS.glob` walks a directory with, so
     the two cannot disagree about a pattern -- which is the whole reason
     this is a builtin rather than the rules written again in wand. *)
  ("glob_matches", VBuiltin (function
    | VGlob pat -> VBuiltin (function
      | VPath p | VString p ->
        (match glob_compile pat with
         | Ok re -> VBool (Re.execp re (glob_strip_dot p))
         (* Unreachable: a Glob exists only if it compiled. *)
         | Error _ -> VBool false)
      | _ -> raise (EvalError "glob_matches: expected Path"))
    | _ -> raise (EvalError "glob_matches: expected Glob")));
  ("glob_to_str", VBuiltin (function
    | VGlob g -> VString g
    | _ -> raise (EvalError "glob_to_str: expected Glob")));
  (* The directory part before the first wildcard: where a walk has to start,
     and above which is ground the pattern cannot reach. *)
  ("glob_base", VBuiltin (function
    | VGlob g -> VPath (glob_base_of g)
    | _ -> raise (EvalError "glob_base: expected Glob")));
  ("str_to_url", VBuiltin (function
    | VString s ->
      let text = String.trim s in
      (match Lexer.url_error text with
       | None -> VConstr (Ctor.Builtin "Ok", [computed_url text])
       | Some why -> VConstr (Ctor.Builtin "Error", [VString
           (Printf.sprintf "cannot parse %S as URL: %s" s why)]))
    | _ -> raise (EvalError "str_to_url: expected String")));
  ("str_to_ipv4", VBuiltin (function
    | VString s -> to_domain "IPv4" (function Token.IPv4 v -> Some (VIPv4 v) | _ -> None) s
    | _ -> raise (EvalError "str_to_ipv4: expected String")));
  ("str_to_cidr", VBuiltin (function
    | VString s -> to_domain "CIDR" (function Token.CIDR v -> Some (VCIDR v) | _ -> None) s
    | _ -> raise (EvalError "str_to_cidr: expected String")));
  (* Both spellings read: `:8080` is wand's own notation, and the bare number
     is what an environment variable, a config file or a flag holds. A caller
     that has just read one should not have to add a colon to it, and
     `Decode.port` accepts both for the same reason. *)
  (* Port primitives. The colon is the literal's punctuation and stays in
     every string a port makes -- `"host%{:8080}"` is `host:8080`, which is
     the address anyone wants. The number is what a command wants for an
     argument of its own, and this is where it comes from. *)
  ("port_to_int", VBuiltin (function
    | VPort n -> VInt n
    | _ -> raise (EvalError "port_to_int: expected Port")));
  ("port_of_int", VBuiltin (function
    | VInt n ->
      if n >= 0 && n <= 65535 then VConstr (Ctor.Builtin "Ok", [VPort n])
      else VConstr (Ctor.Builtin "Error", [VString
        (Printf.sprintf "invalid port %d: must be 0-65535" n)])
    | _ -> raise (EvalError "port_of_int: expected Int")));
  ("str_to_port", VBuiltin (function
    | VString s ->
      to_domain ~shown:s "Port"
        (function Token.Port v -> Some (VPort v) | _ -> None) (port_text s)
    | _ -> raise (EvalError "str_to_port: expected String")));
  (* Checked against the version grammar rather than by re-lexing, as
     `str_to_url` is. The literal cannot spell build metadata -- `+` is the
     addition operator -- and text holding a version usually holds the `v`
     of a git tag as well. Both are read here; see `Lexer.version_error`. *)
  ("str_to_version", VBuiltin (function
    | VString s ->
      let text = Lexer.version_text s in
      (match Lexer.version_error text with
       | None -> VConstr (Ctor.Builtin "Ok", [VVersion text])
       | Some why -> VConstr (Ctor.Builtin "Error", [VString
           (Printf.sprintf "cannot parse %S as Version: %s" s why)]))
    | _ -> raise (EvalError "str_to_version: expected String")));
  ("str_to_size", VBuiltin (function
    | VString s -> to_domain "Size" (function Token.Size v -> Some (VSize v) | _ -> None) s
    | _ -> raise (EvalError "str_to_size: expected String")));
  (* ── Addresses and networks ─────────────────────────────────────────── *)

  ("ipv4_octets", VBuiltin (function
    | VIPv4 a ->
      (match String.split_on_char '.' a with
       | [w; x; y; z] ->
         VTuple (List.map (fun p -> VInt (int_of_string p)) [w; x; y; z])
       (* Unreachable: the literal and `String.to_ipv4` both require four. *)
       | _ -> raise (EvalError ("ipv4_octets: not four octets: " ^ a)))
    | _ -> raise (EvalError "ipv4_octets: expected IPv4")));
  (* The address as the number it is. Every question about a network -- what
     it contains, where it ends, how many it holds -- is arithmetic on this,
     and doing it on the four octets separately is how off-by-one errors get
     in one octet at a time. *)
  ("ipv4_to_int", VBuiltin (function
    | VIPv4 a -> VInt (ipv4_key a)
    | _ -> raise (EvalError "ipv4_to_int: expected IPv4")));
  ("ipv4_of_int", VBuiltin (function
    | VInt n ->
      if n >= 0 && n <= 4294967295 then
        VConstr (Ctor.Builtin "Ok", [VIPv4 (ipv4_of_int n)])
      else
        VConstr (Ctor.Builtin "Error", [VString
          (Printf.sprintf "invalid IPv4 %d: an address is 0 to 4294967295" n)])
    | _ -> raise (EvalError "ipv4_of_int: expected Int")));
  ("ipv4_to_str", VBuiltin (function
    | VIPv4 a -> VString a
    | _ -> raise (EvalError "ipv4_to_str: expected IPv4")));
  (* The RFC 1918 ranges, and nothing else. `private?` is a question about
     routability, and these three are what the RFC sets aside; a caller
     asking a different question -- link-local, multicast -- is asking about
     a range this does not claim to cover. *)
  ("ipv4_is_private", VBuiltin (function
    | VIPv4 a ->
      let n = ipv4_key a in
      VBool (in_cidr "10.0.0.0" 8 n || in_cidr "172.16.0.0" 12 n
             || in_cidr "192.168.0.0" 16 n)
    | _ -> raise (EvalError "ipv4_is_private: expected IPv4")));
  ("ipv4_is_loopback", VBuiltin (function
    | VIPv4 a -> VBool (in_cidr "127.0.0.0" 8 (ipv4_key a))
    | _ -> raise (EvalError "ipv4_is_loopback: expected IPv4")));

  (* Whether the network holds the address. The reason this module exists:
     the type has been here since the beginning and the one question anyone
     asks of a network could only be answered by taking the text apart. *)
  ("cidr_contains", VBuiltin (function
    | VCIDR net -> VBuiltin (function
      | VIPv4 a ->
        let base, bits = cidr_key net in
        VBool (cidr_mask bits land ipv4_key a = cidr_mask bits land base)
      | _ -> raise (EvalError "cidr_contains: expected IPv4"))
    | _ -> raise (EvalError "cidr_contains: expected CIDR")));
  (* The base address with the host bits cleared. `10.0.0.5/8` is a network
     written from an address inside it -- which is how a `ip addr` line
     spells one -- and this is the network it names. *)
  ("cidr_network", VBuiltin (function
    | VCIDR net ->
      let base, bits = cidr_key net in
      VIPv4 (ipv4_of_int (cidr_mask bits land base))
    | _ -> raise (EvalError "cidr_network: expected CIDR")));
  ("cidr_prefix", VBuiltin (function
    | VCIDR net -> VInt (snd (cidr_key net))
    | _ -> raise (EvalError "cidr_prefix: expected CIDR")));
  (* The ends of the range, host bits all clear and all set. Not "the first
     usable host" and "the broadcast address": those are conventions of
     particular network sizes, and a /31 on a point-to-point link has
     neither. These are the two addresses the prefix actually bounds.

     The first of them is the network address, so `first` and `network` are
     two names a reader reaches for over one answer. *)
  ("cidr_first", VBuiltin (function
    | VCIDR net ->
      let base, bits = cidr_key net in
      VIPv4 (ipv4_of_int (cidr_mask bits land base))
    | _ -> raise (EvalError "cidr_first: expected CIDR")));
  ("cidr_last", VBuiltin (function
    | VCIDR net ->
      let base, bits = cidr_key net in
      VIPv4 (ipv4_of_int ((cidr_mask bits land base) lor (0xFFFFFFFF - cidr_mask bits)))
    | _ -> raise (EvalError "cidr_last: expected CIDR")));
  ("cidr_count", VBuiltin (function
    | VCIDR net ->
      let _, bits = cidr_key net in
      VInt (1 lsl (32 - bits))
    | _ -> raise (EvalError "cidr_count: expected CIDR")));
  ("cidr_to_str", VBuiltin (function
    | VCIDR net -> VString net
    | _ -> raise (EvalError "cidr_to_str: expected CIDR")));
  ("cidr_of_parts", VBuiltin (function
    | VIPv4 a -> VBuiltin (function
      | VInt bits ->
        if bits >= 0 && bits <= 32 then
          VConstr (Ctor.Builtin "Ok", [VCIDR (Printf.sprintf "%s/%d" a bits)])
        else
          VConstr (Ctor.Builtin "Error", [VString
            (Printf.sprintf "invalid CIDR prefix %d: must be 0-32" bits)])
      | _ -> raise (EvalError "cidr_of_parts: expected Int"))
    | _ -> raise (EvalError "cidr_of_parts: expected IPv4")));

  (* ── Taking a version apart ─────────────────────────────────────────── *)

  (* The numbers are three by the time a value exists -- `Lexer.version_error`
     and the literal both require it -- so these are total. *)
  ("version_major", VBuiltin (function
    | VVersion v -> VInt (version_number v 0)
    | _ -> raise (EvalError "version_major: expected Version")));
  ("version_minor", VBuiltin (function
    | VVersion v -> VInt (version_number v 1)
    | _ -> raise (EvalError "version_minor: expected Version")));
  ("version_patch", VBuiltin (function
    | VVersion v -> VInt (version_number v 2)
    | _ -> raise (EvalError "version_patch: expected Version")));
  ("version_prerelease", VBuiltin (function
    | VVersion v ->
      (match snd (version_parts v) with
       | Some p -> VConstr (Ctor.Builtin "Some", [VString p])
       | None   -> v_none)
    | _ -> raise (EvalError "version_prerelease: expected Version")));
  ("version_build", VBuiltin (function
    | VVersion v ->
      (match snd (version_build v) with
       | Some b -> VConstr (Ctor.Builtin "Some", [VString b])
       | None   -> v_none)
    | _ -> raise (EvalError "version_build: expected Version")));
  (* A release, as opposed to something on the way to one. Build metadata
     does not make a version unstable: it names the build, not the state. *)
  ("version_is_stable", VBuiltin (function
    | VVersion v -> VBool (snd (version_parts v) = None)
    | _ -> raise (EvalError "version_is_stable: expected Version")));
  (* The three numbers alone. What a prerelease is on the way to, and the
     value to compare when the prerelease is not the question. *)
  ("version_core", VBuiltin (function
    | VVersion v -> VVersion (fst (version_parts v))
    | _ -> raise (EvalError "version_core: expected Version")));
  ("version_to_str", VBuiltin (function
    | VVersion v -> VString v
    | _ -> raise (EvalError "version_to_str: expected Version")));
  (* A Result, because a negative number is not a version component and
     `of_parts (0 - 1) 0 0` is a mistake rather than a version. *)
  ("version_of_parts", VBuiltin (function
    | VInt major -> VBuiltin (function
      | VInt minor -> VBuiltin (function
        | VInt patch ->
          if major >= 0 && minor >= 0 && patch >= 0 then
            VConstr (Ctor.Builtin "Ok",
              [VVersion (Printf.sprintf "%d.%d.%d" major minor patch)])
          else
            VConstr (Ctor.Builtin "Error", [VString
              (Printf.sprintf "invalid version %d.%d.%d: the numbers cannot be negative"
                 major minor patch)])
        | _ -> raise (EvalError "version_of_parts: expected Int"))
      | _ -> raise (EvalError "version_of_parts: expected Int"))
    | _ -> raise (EvalError "version_of_parts: expected Int")));

  (* Each bump clears everything below it, including the prerelease and the
     build: `1.2.3-rc.1` bumped at the minor is `1.3.0`, not `1.3.0-rc.1`,
     because the prerelease named a run-up to 1.2.3 and there is nothing
     left of it. *)
  ("version_bump_major", VBuiltin (function
    | VVersion v -> VVersion (Printf.sprintf "%d.0.0" (version_number v 0 + 1))
    | _ -> raise (EvalError "version_bump_major: expected Version")));
  ("version_bump_minor", VBuiltin (function
    | VVersion v ->
      VVersion (Printf.sprintf "%d.%d.0" (version_number v 0) (version_number v 1 + 1))
    | _ -> raise (EvalError "version_bump_minor: expected Version")));
  (* The one exception. A prerelease is below the release it names, so
     `1.2.3-rc.1` is already on the way to 1.2.3 and bumping the patch
     answers 1.2.3 rather than 1.2.4 -- skipping the release the prerelease
     was for would step over it. *)
  ("version_bump_patch", VBuiltin (function
    | VVersion v ->
      let core = fst (version_parts v) in
      if snd (version_parts v) <> None then VVersion core
      else
        VVersion (Printf.sprintf "%d.%d.%d"
                    (version_number v 0) (version_number v 1) (version_number v 2 + 1))
    | _ -> raise (EvalError "version_bump_patch: expected Version")));

  (* Setting the tail. A Result: a prerelease is part of the grammar rather
     than text to escape -- there is no encoding that turns `rc 1` into an
     identifier -- so text that is not one is reported. *)
  ("version_with_prerelease", VBuiltin (fun pre -> VBuiltin (function
    | VVersion v ->
      let core, _ = version_parts v in
      let _, build = version_build v in
      let tail = match build with None -> "" | Some b -> "+" ^ b in
      let text = match pre with
        | VConstr (Ctor.Builtin "Some", [VString p]) -> core ^ "-" ^ p ^ tail
        | VConstr (Ctor.Builtin "None", []) -> core ^ tail
        | _ -> raise (EvalError "version_with_prerelease: expected Option String")
      in
      (match Lexer.version_error text with
       | None -> VConstr (Ctor.Builtin "Ok", [VVersion text])
       | Some why -> VConstr (Ctor.Builtin "Error", [VString why]))
    | _ -> raise (EvalError "version_with_prerelease: expected Version"))));
  ("version_with_build", VBuiltin (fun bld -> VBuiltin (function
    | VVersion v ->
      let head, _ = version_build v in
      let text = match bld with
        | VConstr (Ctor.Builtin "Some", [VString b]) -> head ^ "+" ^ b
        | VConstr (Ctor.Builtin "None", []) -> head
        | _ -> raise (EvalError "version_with_build: expected Option String")
      in
      (match Lexer.version_error text with
       | None -> VConstr (Ctor.Builtin "Ok", [VVersion text])
       | Some why -> VConstr (Ctor.Builtin "Error", [VString why]))
    | _ -> raise (EvalError "version_with_build: expected Version"))));

  (* ── Taking a URL apart ─────────────────────────────────────────────── *)

  (* One parse, read by every accessor. A URL is
     `scheme://[userinfo@]host[:port][/path][?query][#fragment]`, and the
     boundaries are found in RFC 3986's order: the authority ends at the
     first `/`, `?` or `#`, the path at the first `?` or `#`, the query at
     the `#`. Doing it in that order is what keeps a `?` inside a fragment,
     or a `/` inside a query, from being read as the start of something.

     The value is already known to be a URL -- `Lexer.url_error` passed
     before it could be built -- so this does not validate, and every
     accessor is total. A part that is absent is "" or None, not an error:
     `https://x` has no port and no fragment, and that is ordinary. *)
  ("url_scheme", VBuiltin (function
    | VURL (u, _) -> VString (fst (url_split_scheme u))
    | _ -> raise (EvalError "url_scheme: expected URL")));
  (* `hostname` is the domain alone and `host` is the domain with the port,
     which is the split the web platform uses. Naming the bare one `host`
     would read correctly to anyone who had not met the other spelling and
     silently wrongly to everyone who had. *)
  ("url_hostname", VBuiltin (function
    | VURL (u, _) -> VString (url_authority u).au_hostname
    | _ -> raise (EvalError "url_hostname: expected URL")));
  ("url_host", VBuiltin (function
    | VURL (u, _) ->
      let a = url_authority u in
      VString (a.au_hostname
               ^ (match a.au_port with None -> "" | Some n -> ":" ^ string_of_int n))
    | _ -> raise (EvalError "url_host: expected URL")));
  (* The credentials before the `@`, percent-decoded, as every accessor here
     decodes -- what they are for is being passed to something that wants the
     password rather than its wire spelling. None when the URL carries none;
     `https://:pw@host` has a password and no username, which is why these
     are two Options and not one. *)
  ("url_username", VBuiltin (function
    | VURL (u, _) ->
      (match fst (url_userinfo_parts (url_authority u)) with
       | Some n -> VConstr (Ctor.Builtin "Some", [VString (url_decode_text ~plus:false n)])
       | None   -> v_none)
    | _ -> raise (EvalError "url_username: expected URL")));
  ("url_password", VBuiltin (function
    | VURL (u, _) ->
      (match snd (url_userinfo_parts (url_authority u)) with
       | Some n -> VConstr (Ctor.Builtin "Some", [VString (url_decode_text ~plus:false n)])
       | None   -> v_none)
    | _ -> raise (EvalError "url_password: expected URL")));
  (* Scheme, host and port -- what two URLs have to share to be same-origin,
     and the reason this is one function rather than three read together.
     The userinfo is not part of it: credentials do not change what is being
     addressed.

     Written as the URL wrote it. A default port is not dropped and nothing
     is lowercased, because this module keeps the text it was given -- so
     this answers whether two URLs *say* the same origin, and normalizing
     first is the caller's business. *)
  ("url_origin", VBuiltin (function
    | VURL (u, _) ->
      let a = url_authority u in
      VString (fst (url_split_scheme u) ^ "://" ^ a.au_hostname
               ^ (match a.au_port with None -> "" | Some n -> ":" ^ string_of_int n))
    | _ -> raise (EvalError "url_origin: expected URL")));
  (* An Option rather than a default, because the default depends on the
     scheme and this module is not the place that knows it: 443 is right for
     https and wrong for http, and a caller passing the port to something
     else needs to know whether the URL said one at all. *)
  ("url_port", VBuiltin (function
    | VURL (u, _) ->
      (match (url_authority u).au_port with
       | Some n -> VConstr (Ctor.Builtin "Some", [VPort n])
       | None   -> v_none)
    | _ -> raise (EvalError "url_port: expected URL")));
  (* A Path, so it composes with the module that already knows about
     segments and extensions. `https://x` has no path and answers `/`, which
     is what it addresses. *)
  ("url_path", VBuiltin (function
    | VURL (u, _) ->
      let p = (url_parts u).up_path in
      VPath (if p = "" then "/" else p)
    | _ -> raise (EvalError "url_path: expected URL")));
  (* The query as its pairs, percent-decoded. `+` reads as a space here and
     nowhere else: a query string is form-encoded in practice, and a caller
     asking for `q` wants what was typed rather than the wire spelling.
     `URL.decode` is the one that leaves `+` alone.

     A repeated key keeps its last value, which is what a Map can hold.
     `URL.query_list` answers all of them in order. *)
  ("url_query", VBuiltin (function
    | VURL (u, _) -> VMap (vmap_of_list (url_query_pairs u))
    | _ -> raise (EvalError "url_query: expected URL")));
  ("url_query_list", VBuiltin (function
    | VURL (u, _) ->
      VList (List.map (fun (k, v) -> VTuple [VString k; v]) (url_query_pairs u))
    | _ -> raise (EvalError "url_query_list: expected URL")));
  ("url_fragment", VBuiltin (function
    | VURL (u, _) ->
      (match (url_parts u).up_fragment with
       | Some f -> VConstr (Ctor.Builtin "Some", [VString (url_decode_text ~plus:false f)])
       | None   -> v_none)
    | _ -> raise (EvalError "url_fragment: expected URL")));
  ("url_to_str", VBuiltin (function
    | VURL (u, _) -> VString u
    | _ -> raise (EvalError "url_to_str: expected URL")));

  (* ── Building one ───────────────────────────────────────────────────── *)

  (* Replacing the query rather than adding to it, and encoding what it is
     given. A caller holding a `Map String String` has the values as they
     mean them; turning those into a query string is exactly where the
     encoding has to happen, and doing it here is what stops a `&` in a
     value from becoming a separator. *)
  ("url_with_query", VBuiltin (function
    | VMap m -> VBuiltin (function
      | VURL (u, from) ->
        let kvs = vmap_list m in
        let parts = url_parts u in
        let q =
          String.concat "&"
            (List.map
               (fun (k, v) ->
                 let text =
                   match v with
                   | VString t -> t
                   | _ -> raise (EvalError "url_with_query: expected Map String String")
                 in
                 url_encode_text k ^ "=" ^ url_encode_text text)
               kvs)
        in
        computed_url ~from (url_rebuild { parts with up_query = q })
      | _ -> raise (EvalError "url_with_query: expected URL"))
    | _ -> raise (EvalError "url_with_query: expected Map")));

  (* Every pair, in the order given, so a repeated key can be written as well
     as read. `with_query` takes a Map and a Map holds one value per key, so
     without this the module could read a `?x=1&x=2` it had no way to build. *)
  ("url_with_query_list", VBuiltin (function
    | VList pairs -> VBuiltin (function
      | VURL (u, from) ->
        let parts = url_parts u in
        let q =
          String.concat "&"
            (List.map
               (function
                 | VTuple [VString k; VString v] ->
                   url_encode_text k ^ "=" ^ url_encode_text v
                 | _ -> raise (EvalError "url_with_query_list: expected List (String, String)"))
               pairs)
        in
        computed_url ~from (url_rebuild { parts with up_query = q })
      | _ -> raise (EvalError "url_with_query_list: expected URL"))
    | _ -> raise (EvalError "url_with_query_list: expected List (String, String)")));

  (* ── Setting a part ─────────────────────────────────────────────────── *)

  (* A Result where the argument is text that has to be a URL part in its own
     right -- a scheme that is not http, or a host holding a space, does not
     become one by being encoded, and quietly escaping it would answer a URL
     addressing something else. Where the argument can be encoded without
     changing what it means, the setter is total instead. *)
  ("url_with_scheme", VBuiltin (function
    | VString sch -> VBuiltin (function
      | VURL (u, from) ->
        let parts = url_parts u in
        (match url_checked (url_rebuild { parts with up_scheme = sch }) with
         | Ok text -> VConstr (Ctor.Builtin "Ok", [computed_url ~from text])
         | Error why -> VConstr (Ctor.Builtin "Error", [VString why]))
      | _ -> raise (EvalError "url_with_scheme: expected URL"))
    | _ -> raise (EvalError "url_with_scheme: expected String")));
  ("url_with_hostname", VBuiltin (function
    | VString h -> VBuiltin (function
      | VURL (u, from) ->
        let parts = url_parts u in
        let au = url_authority_of parts.up_authority in
        let text =
          url_rebuild { parts with
            up_authority = url_authority_text { au with au_hostname = h } }
        in
        (match url_checked text with
         | Ok text -> VConstr (Ctor.Builtin "Ok", [computed_url ~from text])
         | Error why -> VConstr (Ctor.Builtin "Error", [VString why]))
      | _ -> raise (EvalError "url_with_hostname: expected URL"))
    | _ -> raise (EvalError "url_with_hostname: expected String")));
  (* An Option, so the port can be taken off as well as set. A Port is
     already a port, so nothing here can fail. *)
  ("url_with_port", VBuiltin (function
    | port -> VBuiltin (function
      | VURL (u, from) ->
        let n = match port with
          | VConstr (Ctor.Builtin "Some", [VPort n]) -> Some n
          | VConstr (Ctor.Builtin "None", []) -> None
          | _ -> raise (EvalError "url_with_port: expected Option Port")
        in
        let parts = url_parts u in
        let au = url_authority_of parts.up_authority in
        computed_url ~from (url_rebuild { parts with
                up_authority = url_authority_text { au with au_port = n } })
      | _ -> raise (EvalError "url_with_port: expected URL"))));
  (* A Path is made of `/`, so its separators are kept and only what cannot
     appear in a URL is escaped. A path that does not start with `/` gets
     one: a URL path is absolute, and `with_path (Path.of_string "a/b")`
     meaning `/a/b` is the only reading that addresses anything. *)
  ("url_with_path", VBuiltin (function
    | VPath p -> VBuiltin (function
      | VURL (u, from) ->
        let p = if p = "" then "/" else if p.[0] = '/' then p else "/" ^ p in
        let parts = url_parts u in
        computed_url ~from (url_rebuild { parts with up_path = url_encode_keep p })
      | _ -> raise (EvalError "url_with_path: expected URL"))
    | _ -> raise (EvalError "url_with_path: expected Path")));
  (* An Option, because an empty fragment is not a missing one: `page#`
     names the top of the page and `page` does not. *)
  ("url_with_fragment", VBuiltin (function
    | frag -> VBuiltin (function
      | VURL (u, from) ->
        let f = match frag with
          | VConstr (Ctor.Builtin "Some", [VString f]) -> Some (url_encode_keep f)
          | VConstr (Ctor.Builtin "None", []) -> None
          | _ -> raise (EvalError "url_with_fragment: expected Option String")
        in
        computed_url ~from (url_rebuild { (url_parts u) with up_fragment = f })
      | _ -> raise (EvalError "url_with_fragment: expected URL"))));
  (* The credentials, strictly encoded: `@` and `:` are the delimiters of the
     authority, so a password holding one has to be escaped or it moves the
     host. Setting one leaves the other alone -- `https://:pw@host` is a URL
     with a password and no username, and dropping the password because the
     username went away would lose something the caller did not touch. *)
  ("url_with_username", VBuiltin (fun name -> VBuiltin (function
    | VURL (u, from) ->
      let n = match name with
        | VConstr (Ctor.Builtin "Some", [VString n]) -> Some n
        | VConstr (Ctor.Builtin "None", []) -> None
        | _ -> raise (EvalError "url_with_username: expected Option String")
      in
      computed_url ~from (url_set_userinfo u (fun (_, pw) -> (n, pw)))
    | _ -> raise (EvalError "url_with_username: expected URL"))));
  ("url_with_password", VBuiltin (fun pass -> VBuiltin (function
    | VURL (u, from) ->
      let p = match pass with
        | VConstr (Ctor.Builtin "Some", [VString p]) -> Some p
        | VConstr (Ctor.Builtin "None", []) -> None
        | _ -> raise (EvalError "url_with_password: expected Option String")
      in
      computed_url ~from (url_set_userinfo u (fun (n, _) -> (n, p)))
    | _ -> raise (EvalError "url_with_password: expected URL"))));

  (* Resolving a reference against a base, RFC 3986 section 5. An absolute
     reference replaces the base outright, `//host/x` keeps only the scheme,
     `/x` keeps the authority, `?q` and `#f` keep everything to their left,
     and a relative path is merged against the base's directory and then has
     its `.` and `..` removed. Concatenating strings gets every one of those
     wrong, which is why this is here rather than in a script. *)
  ("url_join", VBuiltin (function
    | VString r -> VBuiltin (function
      | VURL (base, from) ->
        (match url_resolve base r with
         | Ok u -> VConstr (Ctor.Builtin "Ok", [computed_url ~from u])
         | Error why -> VConstr (Ctor.Builtin "Error", [VString why]))
      | _ -> raise (EvalError "url_join: expected URL"))
    | _ -> raise (EvalError "url_join: expected String")));

  (* Percent-encoding, and its inverse. Everything outside RFC 3986's
     unreserved set is encoded, so the result is safe as one path segment or
     one query value -- `/` and `&` included, since a caller encoding a value
     does not want it read as structure. *)
  ("url_encode", VBuiltin (function
    | VString s -> VString (url_encode_text s)
    | _ -> raise (EvalError "url_encode: expected String")));
  (* A Result: `%zz` is not an escape, and text that claims to be encoded and
     is not is a caller's mistake rather than something to pass along. *)
  ("url_decode", VBuiltin (function
    | VString s ->
      (match url_decode_checked s with
       | Ok t -> VConstr (Ctor.Builtin "Ok", [VString t])
       | Error why -> VConstr (Ctor.Builtin "Error", [VString why]))
    | _ -> raise (EvalError "url_decode: expected String")));

  (* ── Taking an instant apart ────────────────────────────────────────── *)

  (* Every one of these reads the value's own digits through
     `datetime_epoch`, so an instant written with an offset answers for the
     UTC moment it names rather than for the text it was written in:
     `2026-08-22T20:00:00+05:30` is the 22nd at 14:30 UTC, and `hour`
     answers 14. *)
  ("dt_year", VBuiltin (function
    | VDateTime s ->
      let (y, _, _) = civil_from_days (epoch_days (datetime_epoch s)) in VInt y
    | _ -> raise (EvalError "DateTime.year: expected DateTime")));
  ("dt_month", VBuiltin (function
    | VDateTime s ->
      let (_, m, _) = civil_from_days (epoch_days (datetime_epoch s)) in VInt m
    | _ -> raise (EvalError "DateTime.month: expected DateTime")));
  ("dt_day", VBuiltin (function
    | VDateTime s ->
      let (_, _, d) = civil_from_days (epoch_days (datetime_epoch s)) in VInt d
    | _ -> raise (EvalError "DateTime.day: expected DateTime")));
  ("dt_hour", VBuiltin (function
    | VDateTime s -> VInt (seconds_into_day (datetime_epoch s) / 3600)
    | _ -> raise (EvalError "DateTime.hour: expected DateTime")));
  ("dt_minute", VBuiltin (function
    | VDateTime s -> VInt (seconds_into_day (datetime_epoch s) mod 3600 / 60)
    | _ -> raise (EvalError "DateTime.minute: expected DateTime")));
  ("dt_second", VBuiltin (function
    | VDateTime s -> VInt (seconds_into_day (datetime_epoch s) mod 60)
    | _ -> raise (EvalError "DateTime.second: expected DateTime")));
  (* ISO 8601: Monday is 1 and Sunday is 7. 1970-01-01 was a Thursday, so
     the epoch day itself is 4. *)
  ("dt_weekday", VBuiltin (function
    | VDateTime s ->
      let d = epoch_days (datetime_epoch s) in
      VInt (((d + 3) mod 7 + 7) mod 7 + 1)
    | _ -> raise (EvalError "DateTime.weekday: expected DateTime")));
  (* Midnight UTC of the day the instant falls in. *)
  ("dt_day_start", VBuiltin (function
    | VDateTime s ->
      VDateTime (datetime_of_epoch (epoch_days (datetime_epoch s) * 86400))
    | _ -> raise (EvalError "DateTime.day_start: expected DateTime")));
  (* The one builder. A day that is not a day is refused rather than
     silently shifted, so `2026 2 30` does not answer March the 2nd: the
     round trip through `civil_from_days` is what catches it. *)
  ("dt_on", VBuiltin (function
    | VTuple [VInt y; VInt m; VInt d] ->
      (match day_at y m d with
       | Ok days -> VConstr (Ctor.Builtin "Ok", [VDateTime (datetime_of_epoch (days * 86400))])
       | Error msg -> VConstr (Ctor.Builtin "Error", [VString msg]))
    | _ -> raise (EvalError "DateTime.on: expected three Ints")));
  (* The raising sibling, over the same answer, so the two say the same
     thing about the same day. *)
  ("dt_on_exn", VBuiltin (function
    | VTuple [VInt y; VInt m; VInt d] ->
      (match day_at y m d with
       | Ok days -> VDateTime (datetime_of_epoch (days * 86400))
       | Error msg -> raise (EvalError msg))
    | _ -> raise (EvalError "DateTime.on!: expected three Ints")));
  ("dt_date_string", VBuiltin (function
    | VDateTime s -> VString (String.sub (datetime_of_epoch (datetime_epoch s)) 0 10)
    | _ -> raise (EvalError "DateTime.date_string: expected DateTime")));
  ("dt_time_string", VBuiltin (function
    | VDateTime s ->
      VString (String.sub (datetime_of_epoch (datetime_epoch s)) 11 8)
    | _ -> raise (EvalError "DateTime.time_string: expected DateTime")));

  ("str_to_datetime", VBuiltin (function
    | VString s -> to_domain "DateTime" (function Token.DateTime v -> Some (VDateTime v) | _ -> None) s
    | _ -> raise (EvalError "str_to_datetime: expected String")));
  ("str_to_duration", VBuiltin (function
    | VString s -> to_domain "Duration" (function Token.Duration v -> Some (VDuration v) | _ -> None) s
    | _ -> raise (EvalError "str_to_duration: expected String")));
  (* FS primitives *)
  ("fs_exists",  performing "FS!exists?" (function
    | VPath p -> VBool (Sys.file_exists p)
    | _ -> raise (EvalError "fs_exists: expected Path")));
  ("fs_is_file", performing "FS!file?" (function
    | VPath p -> VBool (Sys.file_exists p && not (Sys.is_directory p))
    | _ -> raise (EvalError "fs_is_file: expected Path")));
  ("fs_is_dir",  performing "FS!dir?" (function
    | VPath p -> VBool (Sys.file_exists p && Sys.is_directory p)
    | _ -> raise (EvalError "fs_is_dir: expected Path")));
  ("fs_mkdir",   VBuiltin (fun v -> Effect.perform (WandEffect ("FS!mkdir", v))));
  ("fs_ls",      VBuiltin (fun v -> Effect.perform (WandEffect ("FS!list_dir",      v))));
  ("fs_remove",  VBuiltin (fun v -> Effect.perform (WandEffect ("FS!delete",  v))));
  ("fs_append",  VBuiltin (fun path ->
    VBuiltin (fun content ->
      Effect.perform (WandEffect ("FS!append", VTuple [path; content])))));
  ("fs_create",  VBuiltin (fun v -> Effect.perform (WandEffect ("FS!create_file",  v))));
  ("fs_temp_file", VBuiltin (fun prefix ->
    VBuiltin (fun suffix ->
      Effect.perform (WandEffect ("FS!temp_file", VTuple [prefix; suffix])))));
  ("fs_temp_dir", VBuiltin (fun prefix ->
    Effect.perform (WandEffect ("FS!temp_dir", prefix))));
  ("fs_lock",   VBuiltin (fun v -> Effect.perform (WandEffect ("FS!lock", v))));
  ("fs_lock_wait", VBuiltin (fun budget ->
    VBuiltin (fun path ->
      Effect.perform (WandEffect ("FS!lock_wait", VTuple [path; budget])))));
  ("fs_unlock", VBuiltin (fun v -> Effect.perform (WandEffect ("FS!unlock", v))));
  ("fs_delete_tree", VBuiltin (fun v ->
    Effect.perform (WandEffect ("FS!delete_tree", v))));
  ("fs_rename",  VBuiltin (fun old_ ->
    VBuiltin (fun new_ ->
      Effect.perform (WandEffect ("FS!rename", VTuple [old_; new_])))));
  ("fs_copy",    VBuiltin (fun src ->
    VBuiltin (fun dst ->
      Effect.perform (WandEffect ("FS!copy", VTuple [src; dst])))));
  ("fs_copy_tree", VBuiltin (fun src ->
    VBuiltin (fun dst ->
      Effect.perform (WandEffect ("FS!copy_tree", VTuple [src; dst])))));
  ("fs_cwd",     VBuiltin (fun v -> Effect.perform (WandEffect ("FS!cwd", v))));
  ("fs_mtime",   VBuiltin (fun v -> Effect.perform (WandEffect ("FS!mtime", v))));
  ("fs_size",    VBuiltin (fun v -> Effect.perform (WandEffect ("FS!size", v))));
  ("fs_glob",    VBuiltin (fun pattern ->
    VBuiltin (fun dir ->
      Effect.perform (WandEffect ("FS!glob", VTuple [pattern; dir])))));
  ("fs_glob_impl", VBuiltin (function
    | VString pat | VGlob pat ->
      VBuiltin (function
        | VString base | VPath base ->
          (* The same compile `Glob.matches?` uses, so a walk and the
             predicate cannot disagree about one pattern. *)
          let re =
            match glob_compile pat with
            | Ok re -> re
            | Error why -> raise (EvalError ("fs_glob: " ^ why))
          in
          let absolute = String.length pat > 0 && pat.[0] = '/' in
          let is_link p =
            match Unix.lstat p with
            | { Unix.st_kind = Unix.S_LNK; _ } -> true
            | _ -> false
            | exception Unix.Unix_error _ -> false
          in
          (* A symlink is an entry like any other -- it can match, and is
             answered with as itself -- but the walk does not go through it.
             Walking through one left the base directory the caller named:
             a link inside `./data` pointing at `/etc` had
             `FS.glob_in **.conf ./data` answering with files `./data` does
             not contain, which is not what the argument says. A link back
             to an ancestor was worse -- the walk went round it until the
             path outgrew what the system would take.

             The base itself is followed, since naming it is what asks for
             it. *)
          let rec collect ~walk_link path rel acc =
            let subject = if absolute then path else rel in
            if (not walk_link) && is_link path then
              (if Re.execp re subject then VPath path :: acc else acc)
            else if not (Sys.file_exists path) then acc
            else if Sys.is_directory path then begin
              let entries = Sys.readdir path in
              Array.sort String.compare entries;
              Array.fold_left (fun a name ->
                let child_path = Filename.concat path name in
                let child_rel  = if rel = "" then name
                                 else rel ^ "/" ^ name in
                collect ~walk_link:false child_path child_rel a) acc entries
            end else if Re.execp re subject then VPath path :: acc
            else acc
          in
          (* A directory the walk cannot read is the filesystem answering,
             not wand failing: it arrives as `Sys_error`, which no `try`
             can hold. Said as a wand error instead, so a caller can catch
             it like any other. *)
          let results =
            match collect ~walk_link:true base "" [] with
            | acc -> List.rev acc
            | exception Sys_error why -> raise (EvalError ("fs_glob: " ^ why))
          in
          VList results
        | _ -> raise (EvalError "fs_glob: second argument must be Path"))
    | _ -> raise (EvalError "fs_glob: first argument must be Glob or String")));
  (* Duration primitives *)
  ("dur_zero",    VDuration "0s");
  ("dur_seconds", VBuiltin (function
    | VInt n -> VDuration (format_dur_ms (mul_ovf n 1000))
    | _ -> raise (EvalError "dur_seconds: expected Int")));
  ("dur_minutes", VBuiltin (function
    | VInt n -> VDuration (format_dur_ms (mul_ovf n 60000))
    | _ -> raise (EvalError "dur_minutes: expected Int")));
  ("dur_hours",   VBuiltin (function
    | VInt n -> VDuration (format_dur_ms (mul_ovf n 3600000))
    | _ -> raise (EvalError "dur_hours: expected Int")));
  ("dur_days",    VBuiltin (function
    | VInt n -> VDuration (format_dur_ms (mul_ovf n 86400000))
    | _ -> raise (EvalError "dur_days: expected Int")));
  ("dur_weeks",   VBuiltin (function
    | VInt n -> VDuration (format_dur_ms (mul_ovf n 604800000))
    | _ -> raise (EvalError "dur_weeks: expected Int")));
  ("dur_add", VBuiltin (function
    | VDuration a -> VBuiltin (function
      | VDuration b -> VDuration (format_dur_ms (add_ovf (parse_dur_ms a) (parse_dur_ms b)))
      | _ -> raise (EvalError "dur_add: expected Duration"))
    | _ -> raise (EvalError "dur_add: expected Duration")));
  ("dur_sub", VBuiltin (function
    | VDuration a -> VBuiltin (function
      | VDuration b -> VDuration (format_dur_ms (max 0 (parse_dur_ms a - parse_dur_ms b)))
      | _ -> raise (EvalError "dur_sub: expected Duration"))
    | _ -> raise (EvalError "dur_sub: expected Duration")));
  ("dur_scale", VBuiltin (function
    | VInt n -> VBuiltin (function
      | VDuration d -> VDuration (format_dur_ms (mul_ovf n (parse_dur_ms d)))
      | _ -> raise (EvalError "dur_scale: expected Duration"))
    | _ -> raise (EvalError "dur_scale: expected Int")));
  ("dur_format", VBuiltin (function
    | VDuration d -> VString (format_dur_ms (parse_dur_ms d))
    | _ -> raise (EvalError "dur_format: expected Duration")));
  ("dur_to_ms", VBuiltin (function
    | VDuration d -> VInt (parse_dur_ms d)
    | _ -> raise (EvalError "dur_to_ms: expected Duration")));
  (* Size primitives *)
  ("size_to_bytes", VBuiltin (function
    | VSize s -> VInt (size_bytes s)
    | _ -> raise (EvalError "size_to_bytes: expected Size")));
  ("size_of_bytes", VBuiltin (function
    | VInt n -> VSize (Printf.sprintf "%dB" (max 0 n))
    | _ -> raise (EvalError "size_of_bytes: expected Int")));
  ("size_format", VBuiltin (function
    | VSize s -> VString (format_size_bytes (size_bytes s))
    | _ -> raise (EvalError "size_format: expected Size")));
  (* Regex primitives *)
  ("regex_match", VBuiltin (function
    | VRegex re -> VBuiltin (function
      | VString s -> VBool (Re.execp re s)
      | _ -> raise (EvalError "regex_match: expected String"))
    | _ -> raise (EvalError "regex_match: expected Regex")));
  ("regex_capture", VBuiltin (function
    | VRegex re -> VBuiltin (function
      | VString s ->
        (match Re.exec_opt re s with
         | None   -> VList []
         | Some g ->
           let all = Re.Group.all g in
           VList (Array.to_list (Array.map (fun s -> VString s) all)))
      | _ -> raise (EvalError "regex_capture: expected String"))
    | _ -> raise (EvalError "regex_capture: expected Regex")));
  ("regex_replace", VBuiltin (function
    | VRegex re -> VBuiltin (function
      | VString repl -> VBuiltin (function
        | VString s ->
          (match Re.exec_opt re s with
           | None   -> VString s
           | Some g ->
             let (b, e) = Re.Group.offset g 0 in
             VString (String.sub s 0 b ^ repl ^ String.sub s e (String.length s - e)))
        | _ -> raise (EvalError "regex_replace: expected String"))
      | _ -> raise (EvalError "regex_replace: expected String repl"))
    | _ -> raise (EvalError "regex_replace: expected Regex")));
  ("regex_replace_all", VBuiltin (function
    | VRegex re -> VBuiltin (function
      | VString repl -> VBuiltin (function
        | VString s -> VString (Re.replace_string re ~by:repl s)
        | _ -> raise (EvalError "regex_replace_all: expected String"))
      | _ -> raise (EvalError "regex_replace_all: expected String repl"))
    | _ -> raise (EvalError "regex_replace_all: expected Regex")));
  ("regex_split", VBuiltin (function
    | VRegex re -> VBuiltin (function
      | VString s -> VList (List.map (fun s -> VString s) (Re.split re s))
      | _ -> raise (EvalError "regex_split: expected String"))
    | _ -> raise (EvalError "regex_split: expected Regex")));
  ("regex_find_all", VBuiltin (function
    | VRegex re -> VBuiltin (function
      | VString s ->
        VList (List.map (fun g -> VString (Re.Group.get g 0)) (Re.all re s))
      | _ -> raise (EvalError "regex_find_all: expected String"))
    | _ -> raise (EvalError "regex_find_all: expected Regex")));
  (* Pair an acquire with a release. The only way to build a resource, and
     the pair is built in one place so the two halves cannot drift apart. *)
  ("resource_make", VBuiltin (fun acquire ->
    VBuiltin (fun release -> VResource (acquire, release))));
  ("regex_compile", VBuiltin (function
    | VString pat ->
      (match regex_repeat_error pat with
       | Some why -> VConstr (Ctor.Builtin "Error", [VString why])
       | None ->
         (try VConstr (Ctor.Builtin "Ok", [VRegex (Re.compile (Re.Pcre.re pat))])
          with Re.Pcre.Parse_error ->
            VConstr (Ctor.Builtin "Error", [VString (Printf.sprintf "invalid regex: %s" pat)])))
    | _ -> raise (EvalError "regex_compile: expected String")));
  (* Path primitives — pure string operations on VPath values *)
  ("path_join", VBuiltin (function
    | VPath p1 | VString p1 -> VBuiltin (function
      | VPath p2 | VString p2 -> VPath (path_normalize (Filename.concat p1 p2))
      | _ -> raise (EvalError "path_join: expected Path"))
    | _ -> raise (EvalError "path_join: expected Path")));
  ("path_parent", VBuiltin (function
    | VPath s | VString s -> VPath (Filename.dirname s)
    | _ -> raise (EvalError "path_parent: expected Path")));
  ("path_basename", VBuiltin (function
    | VPath s | VString s -> VPath (Filename.basename s)
    | _ -> raise (EvalError "path_basename: expected Path")));
  ("path_extension", VBuiltin (function
    | VPath s | VString s -> VString (Filename.extension s)
    | _ -> raise (EvalError "path_extension: expected Path")));
  ("path_with_extension", VBuiltin (function
    | VString ext -> VBuiltin (function
      | VPath s | VString s ->
        (* `Path.extension` answers with the dot -- ".txt" -- so that
           spelling has to go back in unchanged. Somebody writing the
           extension the way it is said, "md", means the same thing, and
           pasting it straight on turned /a/b.txt into /a/bmd: a path with
           no extension at all, silently. An empty extension takes the
           extension off. *)
        let stem = Filename.remove_extension s in
        let dotted =
          if ext = "" then ""
          else if ext.[0] = '.' then ext
          else "." ^ ext
        in
        VPath (stem ^ dotted)
      | _ -> raise (EvalError "path_with_extension: expected Path"))
    | _ -> raise (EvalError "path_with_extension: expected String ext")));
  ("path_is_absolute", VBuiltin (function
    | VPath s | VString s ->
      VBool (String.length s > 0 && s.[0] = '/')
    | _ -> raise (EvalError "path_is_absolute: expected Path")));
  ("path_is_relative", VBuiltin (function
    | VPath s | VString s ->
      VBool (String.length s = 0 || s.[0] <> '/')
    | _ -> raise (EvalError "path_is_relative: expected Path")));
  ("path_normalize", VBuiltin (function
    | VPath s | VString s -> VPath (path_normalize s)
    | _ -> raise (EvalError "path_normalize: expected Path")));
  ("path_to_string", VBuiltin (function
    | VPath s | VString s -> VString s
    | _ -> raise (EvalError "path_to_string: expected Path")));
  ("path_of_string", VBuiltin (function
    | VString s -> VPath s
    | _ -> raise (EvalError "path_of_string: expected String")));
  ("path_components", VBuiltin (function
    | VPath s | VString s ->
      let parts = String.split_on_char '/' s |> List.filter (fun p -> p <> "") in
      VList (List.map (fun p -> VString p) parts)
    | _ -> raise (EvalError "path_components: expected Path")));
  (* IO primitives *)
  ("io_print_err",   VBuiltin (fun v -> Effect.perform (WandEffect ("IO!print_err",   v))));
  ("io_println_err", VBuiltin (fun v -> Effect.perform (WandEffect ("IO!println_err", v))));
  ("io_read_line",   VBuiltin (fun v -> Effect.perform (WandEffect ("IO!read_line",   v))));
  ("io_read_all",    VBuiltin (fun v -> Effect.perform (WandEffect ("IO!read_all",    v))));
  ("io_flush",       VBuiltin (fun v -> Effect.perform (WandEffect ("IO!flush",       v))));
  (* Par primitives. *)
  ("par_map", VBuiltin (fun limit ->
    VBuiltin (fun f ->
      VBuiltin (fun xs ->
        match limit, xs with
        | VInt n, VList items -> par_run n f items ~collect:true
        | _ -> raise (EvalError "par_map: expected a limit and a list")))));
  (* `Par.timeout` is a race between the work and a sleeper, and `par_race`
     refuses inside a handler. This guard runs first so the message is about
     the deadline: the sleeper is a branch, a branch cannot run where the
     handler is, and work that only the deadline would have stopped would
     run forever. Refused, with the reason, rather than hanging a test suite
     with no message.

     A rehearsal and a trace are observers too, and are not refused: a
     rehearsal collapsing the race still reports what the work would do.

     This is a run-time error, not the `Raise` effect, as division by zero
     is -- the signature says nothing about it because no wand code can
     answer it. *)
  ("par_deadline_guard", VBuiltin (fun _ ->
    if Atomic.get handlers > 0 then
      raise (EvalError
        "a deadline inside a handler never fires. Move the handler inside \
         the thunk -- `Par.timeout d (fn () -> with_clock (fn () -> ...))` \
         -- or take it off.")
    else VUnit));
  ("par_race", VBuiltin (function
    | VList thunks -> par_race thunks
    | _ -> raise (EvalError "par_race: expected a list of thunks")));
  ("par_each", VBuiltin (fun limit ->
    VBuiltin (fun f ->
      VBuiltin (fun xs ->
        match limit, xs with
        | VInt n, VList items -> par_run n f items ~collect:false
        | _ -> raise (EvalError "par_each: expected a limit and a list")))));
  (* Running a command that was already built. The bound travels with the
     value: what a spawn is checked against is the manifest of the file
     whose text named the words, not the one that happened to call `run!`.
     A command built in a `Shell(git)` file stays a git command wherever it
     is passed. *)
  ("shell_run", VBuiltin (function
    | VCommand (cmd, allow) -> perform_shell "Shell!run" allow (VString cmd)
    | _ -> raise (EvalError "shell_run: expected a Command")));
  ("net_http", VBuiltin (fun request ->
    (* The bound rides beside the perform rather than inside the payload, so
       a mock still matches `| Net!http request k` on a plain request. *)
    let (payload, allow) =
      match request with
      | VRequest (inner, allow) -> (inner, request_bound allow inner)
      | other -> (other, request_bound None other)
    in
    let saved = Domain.DLS.get ambient_net_allow in
    Domain.DLS.set ambient_net_allow allow;
    Fun.protect
      ~finally:(fun () -> Domain.DLS.set ambient_net_allow saved)
      (fun () -> Effect.perform (WandEffect ("Net!http", payload)))));
  ("net_download", VBuiltin (fun url ->
    VBuiltin (fun dest ->
      let allow = match url with VURL (_, a) -> a | _ -> None in
      let saved = Domain.DLS.get ambient_net_allow in
      Domain.DLS.set ambient_net_allow allow;
      Fun.protect
        ~finally:(fun () -> Domain.DLS.set ambient_net_allow saved)
        (fun () ->
          Effect.perform (WandEffect ("Net!download", VTuple [url; dest]))))));
  ("shell_query", VBuiltin (function
    | VCommand (cmd, allow) -> perform_shell "Shell!capture" allow (VString cmd)
    | _ -> raise (EvalError "shell_query: expected a Command")));
  (* Process primitives *)
  (* Env primitives *)
  ("env_read_dotenv", performing "Env!read" (function
    | VString src | VPath src ->
      VList (List.map (fun (k, v) -> VTuple [VString k; VString v])
               (dotenv_pairs src))
    | _ -> raise (EvalError "env_read_dotenv: expected String or Path")));
  ("env_load_file", VBuiltin (function
    | VString path | VPath path ->
      (* Read through the same effect a plain read does, so a trace shows
         the file and a mock can substitute it. *)
      let src =
        match Effect.perform (WandEffect ("FS!read_file", VString path)) with
        | VString s -> s
        | _ -> raise (EvalError "env_load_file: expected file contents")
      in
      (* One env_set per variable, so a rehearsal names each one rather than
         reporting that a file was loaded. *)
      List.iter (fun (k, v) ->
        ignore (Effect.perform
          (WandEffect ("Env!set", VTuple [VString k; VString v]))))
        (dotenv_pairs src);
      VUnit
    | _ -> raise (EvalError "env_load_file: expected Path")));

  ("env_get_exn", performing "Env!get" (function
    | VString name ->
      (match Sys.getenv_opt name with
       | Some v -> VString v
       | None   -> raise (EvalError ("env: variable not set: " ^ name)))
    | _ -> raise (EvalError "env_get_exn: expected String")));
  (* Changing the environment is a mutation like any other, so it goes
     through an interceptable effect rather than straight to putenv. A
     rehearsal that still edited the environment would be worse than none. *)
  ("env_set", VBuiltin (fun name ->
    VBuiltin (fun value ->
      (* A name is what stands left of the `=` in the environment, so a name
         holding one names something else: `Env.set "A=B" "x"` set `A` to
         `B=x`, and an empty name added an entry nothing can read. Refused
         here rather than passed on, so a rehearsal refuses it too. *)
      (match name with
       | VString n ->
         if n = "" then
           raise (EvalError "Env.set: the name is empty")
         else if String.contains n '=' then
           raise (EvalError (Printf.sprintf
             "Env.set: %S holds '=', which separates a name from its value"
             n))
       | _ -> ());
      Effect.perform (WandEffect ("Env!set", VTuple [name; value])))));
  ("env_clear", VBuiltin (fun name ->
    Effect.perform (WandEffect ("Env!clear", name))));
  ("env_all", performing "Env!all" (function
    | VUnit ->
      let pairs = Array.to_list (Unix.environment ()) |> List.filter_map (fun s ->
        match String.split_on_char '=' s with
        | [] | [""] -> None
        | name :: rest -> Some (VTuple [VString name; VString (String.concat "=" rest)]))
      in
      VList pairs
    | _ -> raise (EvalError "env_all: expected Unit")));
  ("env_home", performing "Env!home" (function
    | VUnit ->
      (match Sys.getenv_opt "HOME" with
       | Some h -> VPath h
       | None   -> raise (EvalError "env: HOME not set"))
    | _ -> raise (EvalError "env_home: expected Unit")));
  ("env_user", performing "Env!user" (function
    | VUnit ->
      let user =
        match Sys.getenv_opt "USER" with
        | Some u -> u
        | None   -> (try Unix.getlogin () with _ -> "")
      in
      VString user
    | _ -> raise (EvalError "env_user: expected Unit")));
  (* CSV primitives *)
  ("csv_parse", VBuiltin (function
    | VString sep -> VBuiltin (function
      | VString src ->
        let rows = csv_parse_string sep src in
        VList (List.map (fun row -> VList (List.map (fun s -> VString s) row)) rows)
      | _ -> raise (EvalError "csv_parse: expected String content"))
    | _ -> raise (EvalError "csv_parse: expected String separator")));
  ("csv_stringify", VBuiltin (function
    | VString sep -> VBuiltin (function
      | VList rows ->
        let str_rows = List.map (function
          | VList fields -> List.map (function
            | VString s -> s
            | v -> to_text v) fields
          | _ -> raise (EvalError "csv_stringify: rows must be List (List String)")) rows
        in
        VString (csv_stringify_rows sep str_rows)
      | _ -> raise (EvalError "csv_stringify: expected List of rows"))
    | _ -> raise (EvalError "csv_stringify: expected String separator")));
  (* JSON primitives *)
  ("json_null",  VJson `Null);
  ("json_of_bool",   VBuiltin (function VBool b  -> VJson (`Bool b)   | _ -> raise (EvalError "json_of_bool: expected Bool")));
  ("json_of_int",    VBuiltin (function VInt n   -> VJson (`Int n)    | _ -> raise (EvalError "json_of_int: expected Int")));
  ("json_of_float",  VBuiltin (function
    | VFloat f when Float.is_finite f -> VJson (`Float f)
    | VFloat _ -> raise (EvalError ("json_of_float: " ^ unwritable_number))
    | _ -> raise (EvalError "json_of_float: expected Float")));
  ("json_of_string", VBuiltin (function VString s -> VJson (`String s) | _ -> raise (EvalError "json_of_string: expected String")));
  ("json_of_list",   VBuiltin (function
    | VList vs ->
      let items = List.map (function VJson j -> j | _ -> raise (EvalError "json_of_list: elements must be JSON")) vs in
      VJson (`List items)
    | _ -> raise (EvalError "json_of_list: expected List")));
  (* An object from a Map, in the order the Map holds. `JSON.get_object` gives
     back a Map, so this is its inverse.

     A key the Map holds twice is written once, at its first position. A Map
     can hold a repeated key -- `[a = 1, a = 9]` has two entries and
     `Map.get` finds the first -- and a document naming the same key twice is
     read differently by different parsers. Writing the one that can be read
     back is the only answer that round-trips. *)
  ("json_of_map",    VBuiltin (function
    | VMap kvs_m -> let kvs = vmap_list kvs_m in
      (* A Map holds a key once, so what comes out names each key once too. *)
      VJson (`Assoc (List.map (fun (k, v) -> match v with
        | VJson j -> (k, j)
        | _ -> raise (EvalError "json_of_map: values must be JSON")) kvs))
    | _ -> raise (EvalError "json_of_map: expected Map")));
  ("json_is_null",   VBuiltin (function VJson `Null -> VBool true | VJson _ -> VBool false | _ -> raise (EvalError "json_is_null: expected JSON")));
  ("json_get_bool",  VBuiltin (function
    | VJson (`Bool b) -> VConstr (Ctor.Builtin "Ok", [VBool b])
    | VJson j -> VConstr (Ctor.Builtin "Error", [VString ("expected bool, got " ^ Yojson.Basic.to_string j)])
    | _ -> raise (EvalError "json_get_bool: expected JSON")));
  ("json_get_int",   VBuiltin (function
    | VJson (`Int n) -> VConstr (Ctor.Builtin "Ok", [VInt n])
    | VJson j -> VConstr (Ctor.Builtin "Error", [VString ("expected int, got " ^ Yojson.Basic.to_string j)])
    | _ -> raise (EvalError "json_get_int: expected JSON")));
  ("json_get_float", VBuiltin (function
    | VJson (`Float f) -> VConstr (Ctor.Builtin "Ok", [VFloat f])
    | VJson (`Int n)   -> VConstr (Ctor.Builtin "Ok", [VFloat (float_of_int n)])
    | VJson j -> VConstr (Ctor.Builtin "Error", [VString ("expected float, got " ^ Yojson.Basic.to_string j)])
    | _ -> raise (EvalError "json_get_float: expected JSON")));
  ("json_get_string", VBuiltin (function
    | VJson (`String s) -> VConstr (Ctor.Builtin "Ok", [VString s])
    | VJson j -> VConstr (Ctor.Builtin "Error", [VString ("expected string, got " ^ Yojson.Basic.to_string j)])
    | _ -> raise (EvalError "json_get_string: expected JSON")));
  ("json_get_array", VBuiltin (function
    | VJson (`List vs) -> VConstr (Ctor.Builtin "Ok", [VList (List.map (fun j -> VJson j) vs)])
    | VJson j -> VConstr (Ctor.Builtin "Error", [VString ("expected array, got " ^ Yojson.Basic.to_string j)])
    | _ -> raise (EvalError "json_get_array: expected JSON")));
  ("json_get_object", VBuiltin (function
    | VJson (`Assoc kvs) ->
      VConstr (Ctor.Builtin "Ok", [VMap (vmap_of_list (List.map (fun (k, j) -> (k, VJson j)) kvs))])
    | VJson j -> VConstr (Ctor.Builtin "Error", [VString ("expected object, got " ^ Yojson.Basic.to_string j)])
    | _ -> raise (EvalError "json_get_object: expected JSON")));
  ("json_field", VBuiltin (fun key ->
    VBuiltin (function
      | VJson (`Assoc kvs) ->
        let k = (match key with VString s -> s | _ -> raise (EvalError "json_field: key must be String")) in
        (match assoc_last k kvs with
         | Some j -> VConstr (Ctor.Builtin "Ok", [VJson j])
         | None   -> VConstr (Ctor.Builtin "Error", [VString ("no field: " ^ k)]))
      | VJson j -> VConstr (Ctor.Builtin "Error", [VString ("expected object, got " ^ Yojson.Basic.to_string j)])
      | _ -> raise (EvalError "json_field: expected JSON"))));
  (* ── YAML ──────────────────────────────────────────────────────────────
     Read-only. `Yaml_read` resolves scalars against the 1.2 core schema
     rather than letting the library resolve them the 1.1 way, which is what
     keeps a workflow's `on:` a string and a compose file's `restart: no` a
     word. *)
  ("yaml_parse", VBuiltin (function
    | VString s ->
      (match Yaml_read.parse s with
       | Ok y when not (json_is_finite y) ->
         VConstr (Ctor.Builtin "Error", [VString unwritable_number])
       | Ok y      -> VConstr (Ctor.Builtin "Ok", [VYaml y])
       | Error msg -> VConstr (Ctor.Builtin "Error", [VString msg]))
    | _ -> raise (EvalError "yaml_parse: expected String")));
  ("yaml_parse_exn", VBuiltin (function
    | VString s ->
      (match Yaml_read.parse s with
       | Ok y when not (json_is_finite y) ->
         raise (EvalError ("yaml_parse: " ^ unwritable_number))
       | Ok y      -> VYaml y
       | Error msg -> raise (EvalError ("yaml_parse: " ^ msg)))
    | _ -> raise (EvalError "yaml_parse_exn: expected String")));
  ("yaml_parse_all", VBuiltin (function
    | VString s ->
      (match Yaml_read.parse_all s with
       | Ok ys when not (List.for_all json_is_finite ys) ->
         VConstr (Ctor.Builtin "Error", [VString unwritable_number])
       | Ok ys     -> VConstr (Ctor.Builtin "Ok", [VList (List.map (fun y -> VYaml y) ys)])
       | Error msg -> VConstr (Ctor.Builtin "Error", [VString msg]))
    | _ -> raise (EvalError "yaml_parse_all: expected String")));
  ("yaml_parse_all_exn", VBuiltin (function
    | VString s ->
      (match Yaml_read.parse_all s with
       | Ok ys when not (List.for_all json_is_finite ys) ->
         raise (EvalError ("yaml_parse_all: " ^ unwritable_number))
       | Ok ys     -> VList (List.map (fun y -> VYaml y) ys)
       | Error msg -> raise (EvalError ("yaml_parse_all: " ^ msg)))
    | _ -> raise (EvalError "yaml_parse_all_exn: expected String")));
  ("yaml_is_mapping", VBuiltin (function
    | VYaml (`Assoc _) -> VBool true | VYaml _ -> VBool false
    | _ -> raise (EvalError "yaml_is_mapping: expected YAML")));
  ("yaml_is_sequence", VBuiltin (function
    | VYaml (`List _) -> VBool true | VYaml _ -> VBool false
    | _ -> raise (EvalError "yaml_is_sequence: expected YAML")));
  ("yaml_is_null", VBuiltin (function
    | VYaml `Null -> VBool true | VYaml _ -> VBool false
    | _ -> raise (EvalError "yaml_is_null: expected YAML")));
  ("yaml_get_bool", VBuiltin (function
    | VYaml (`Bool b) -> VConstr (Ctor.Builtin "Ok", [VBool b])
    | VYaml y -> VConstr (Ctor.Builtin "Error", [VString ("expected bool, got " ^ Yojson.Basic.to_string y)])
    | _ -> raise (EvalError "yaml_get_bool: expected YAML")));
  ("yaml_get_int", VBuiltin (function
    | VYaml (`Int n) -> VConstr (Ctor.Builtin "Ok", [VInt n])
    | VYaml y -> VConstr (Ctor.Builtin "Error", [VString ("expected int, got " ^ Yojson.Basic.to_string y)])
    | _ -> raise (EvalError "yaml_get_int: expected YAML")));
  ("yaml_get_float", VBuiltin (function
    | VYaml (`Float f) -> VConstr (Ctor.Builtin "Ok", [VFloat f])
    | VYaml (`Int n)   -> VConstr (Ctor.Builtin "Ok", [VFloat (float_of_int n)])
    | VYaml y -> VConstr (Ctor.Builtin "Error", [VString ("expected float, got " ^ Yojson.Basic.to_string y)])
    | _ -> raise (EvalError "yaml_get_float: expected YAML")));
  ("yaml_get_string", VBuiltin (function
    | VYaml (`String s) -> VConstr (Ctor.Builtin "Ok", [VString s])
    | VYaml y -> VConstr (Ctor.Builtin "Error", [VString ("expected string, got " ^ Yojson.Basic.to_string y)])
    | _ -> raise (EvalError "yaml_get_string: expected YAML")));
  ("yaml_get_sequence", VBuiltin (function
    | VYaml (`List ys) -> VConstr (Ctor.Builtin "Ok", [VList (List.map (fun y -> VYaml y) ys)])
    | VYaml y -> VConstr (Ctor.Builtin "Error", [VString ("expected sequence, got " ^ Yojson.Basic.to_string y)])
    | _ -> raise (EvalError "yaml_get_sequence: expected YAML")));
  ("yaml_get_mapping", VBuiltin (function
    | VYaml (`Assoc kvs) ->
      VConstr (Ctor.Builtin "Ok", [VMap (vmap_of_list (List.map (fun (k, y) -> (k, VYaml y)) kvs))])
    | VYaml y -> VConstr (Ctor.Builtin "Error", [VString ("expected mapping, got " ^ Yojson.Basic.to_string y)])
    | _ -> raise (EvalError "yaml_get_mapping: expected YAML")));
  ("yaml_field", VBuiltin (fun key ->
    VBuiltin (function
      | VYaml (`Assoc kvs) ->
        let k = (match key with VString s -> s | _ -> raise (EvalError "yaml_field: key must be String")) in
        (match assoc_last k kvs with
         | Some y -> VConstr (Ctor.Builtin "Ok", [VYaml y])
         | None   -> VConstr (Ctor.Builtin "Error", [VString ("no field: " ^ k)]))
      | VYaml y -> VConstr (Ctor.Builtin "Error", [VString ("expected mapping, got " ^ Yojson.Basic.to_string y)])
      | _ -> raise (EvalError "yaml_field: expected YAML"))));
  ("yaml_field_exn", VBuiltin (fun key ->
    VBuiltin (function
      | VYaml (`Assoc kvs) ->
        let k = (match key with VString s -> s | _ -> raise (EvalError "yaml_field!: key must be String")) in
        (match assoc_last k kvs with
         | Some y -> VYaml y
         | None   -> raise (EvalError ("no field: " ^ k)))
      | VYaml y -> raise (EvalError ("expected mapping, got " ^ Yojson.Basic.to_string y))
      | _ -> raise (EvalError "yaml_field!: expected YAML"))));
  ("json_parse", VBuiltin (function
    | VString s ->
      (try
         let j = Yojson.Basic.from_string s in
         if json_is_finite j then VConstr (Ctor.Builtin "Ok", [VJson j])
         else VConstr (Ctor.Builtin "Error", [VString unwritable_number])
       with Yojson.Json_error msg -> VConstr (Ctor.Builtin "Error", [VString msg]))
    | _ -> raise (EvalError "json_parse: expected String")));
  ("json_parse_exn", VBuiltin (function
    | VString s ->
      (try
         let j = Yojson.Basic.from_string s in
         if json_is_finite j then VJson j
         else raise (EvalError ("json_parse: " ^ unwritable_number))
       with Yojson.Json_error msg -> raise (EvalError ("json_parse: " ^ msg)))
    | _ -> raise (EvalError "json_parse_exn: expected String")));
  ("json_field_exn", VBuiltin (fun key ->
    VBuiltin (function
      | VJson (`Assoc kvs) ->
        let k = (match key with VString s -> s | _ -> raise (EvalError "json_field_exn: key must be String")) in
        (match assoc_last k kvs with
         | Some j -> VJson j
         | None   -> raise (EvalError ("json_field_exn: no field: " ^ k)))
      | VJson j -> raise (EvalError ("json_field_exn: expected object, got " ^ Yojson.Basic.to_string j))
      | _ -> raise (EvalError "json_field_exn: expected JSON"))));
  ("json_stringify", VBuiltin (function
    | VJson j -> VString (Yojson.Basic.to_string j)
    | _ -> raise (EvalError "json_stringify: expected JSON")));
  ("json_stringify_pretty", VBuiltin (function
    | VJson j -> VString (Yojson.Basic.pretty_to_string j)
    | _ -> raise (EvalError "json_stringify_pretty: expected JSON")));
  (* TOML primitives *)
  ("toml_parse", VBuiltin (function
    | VString s ->
      (match Toml.Parser.from_string s with
       | `Ok tbl  -> VConstr (Ctor.Builtin "Ok", [VToml (Toml.Types.TTable tbl)])
       | `Error (msg, _) -> VConstr (Ctor.Builtin "Error", [VString msg]))
    | _ -> raise (EvalError "toml_parse: expected String")));
  ("toml_parse_exn", VBuiltin (function
    | VString s ->
      (match Toml.Parser.from_string s with
       | `Ok tbl  -> VToml (Toml.Types.TTable tbl)
       | `Error (msg, _) -> raise (EvalError ("toml_parse: " ^ msg)))
    | _ -> raise (EvalError "toml_parse_exn: expected String")));
  ("toml_stringify", VBuiltin (function
    | VToml (Toml.Types.TTable tbl) -> VString (Toml.Printer.string_of_table tbl)
    | VToml _ -> raise (EvalError "toml_stringify: value must be a TOML table")
    | _ -> raise (EvalError "toml_stringify: expected TOML")));
  ("toml_is_table", VBuiltin (function
    | VToml (Toml.Types.TTable _) -> VBool true
    | VToml _ -> VBool false
    | _ -> raise (EvalError "toml_is_table: expected TOML")));
  ("toml_is_array", VBuiltin (function
    | VToml (Toml.Types.TArray _) -> VBool true
    | VToml _ -> VBool false
    | _ -> raise (EvalError "toml_is_array: expected TOML")));
  ("toml_get_bool", VBuiltin (function
    | VToml (Toml.Types.TBool b) -> VConstr (Ctor.Builtin "Ok", [VBool b])
    | VToml _ -> VConstr (Ctor.Builtin "Error", [VString "expected bool"])
    | _ -> raise (EvalError "toml_get_bool: expected TOML")));
  ("toml_get_int", VBuiltin (function
    | VToml (Toml.Types.TInt n) -> VConstr (Ctor.Builtin "Ok", [VInt n])
    | VToml _ -> VConstr (Ctor.Builtin "Error", [VString "expected int"])
    | _ -> raise (EvalError "toml_get_int: expected TOML")));
  ("toml_get_float", VBuiltin (function
    | VToml (Toml.Types.TFloat f) -> VConstr (Ctor.Builtin "Ok", [VFloat f])
    | VToml (Toml.Types.TInt n)   -> VConstr (Ctor.Builtin "Ok", [VFloat (float_of_int n)])
    | VToml _ -> VConstr (Ctor.Builtin "Error", [VString "expected float"])
    | _ -> raise (EvalError "toml_get_float: expected TOML")));
  ("toml_get_string", VBuiltin (function
    | VToml (Toml.Types.TString s) -> VConstr (Ctor.Builtin "Ok", [VString s])
    | VToml _ -> VConstr (Ctor.Builtin "Error", [VString "expected string"])
    | _ -> raise (EvalError "toml_get_string: expected TOML")));
  ("toml_get_array", VBuiltin (function
    | VToml (Toml.Types.TArray arr) ->
      let items = match arr with
        | Toml.Types.NodeBool bs   -> List.map (fun b -> VToml (Toml.Types.TBool b)) bs
        | Toml.Types.NodeInt ns    -> List.map (fun n -> VToml (Toml.Types.TInt n)) ns
        | Toml.Types.NodeFloat fs  -> List.map (fun f -> VToml (Toml.Types.TFloat f)) fs
        | Toml.Types.NodeString ss -> List.map (fun s -> VToml (Toml.Types.TString s)) ss
        | Toml.Types.NodeDate ds   -> List.map (fun d -> VToml (Toml.Types.TDate d)) ds
        | Toml.Types.NodeTable ts  -> List.map (fun t -> VToml (Toml.Types.TTable t)) ts
        | Toml.Types.NodeArray _   -> []
        | Toml.Types.NodeEmpty     -> []
      in
      VConstr (Ctor.Builtin "Ok", [VList items])
    | VToml _ -> VConstr (Ctor.Builtin "Error", [VString "expected array"])
    | _ -> raise (EvalError "toml_get_array: expected TOML")));
  ("toml_get_table", VBuiltin (function
    | VToml (Toml.Types.TTable tbl) ->
      let pairs = Toml.Types.Table.to_list tbl in
      let vmap = VMap (vmap_of_list (List.map (fun (k, v) ->
        (Toml.Types.Table.Key.to_string k, VToml v)) pairs)) in
      VConstr (Ctor.Builtin "Ok", [vmap])
    | VToml _ -> VConstr (Ctor.Builtin "Error", [VString "expected table"])
    | _ -> raise (EvalError "toml_get_table: expected TOML")));
  ("toml_field", VBuiltin (fun key ->
    VBuiltin (function
      | VToml (Toml.Types.TTable tbl) ->
        let k = (match key with VString s -> s | _ -> raise (EvalError "toml_field: key must be String")) in
        (match Toml.Types.Table.find_opt (Toml.Types.Table.Key.of_string k) tbl with
         | Some v -> VConstr (Ctor.Builtin "Ok", [VToml v])
         | None   -> VConstr (Ctor.Builtin "Error", [VString ("no key: " ^ k)]))
      | VToml _ -> VConstr (Ctor.Builtin "Error", [VString "expected table"])
      | _ -> raise (EvalError "toml_field: expected TOML"))));
  ("toml_field_exn", VBuiltin (fun key ->
    VBuiltin (function
      | VToml (Toml.Types.TTable tbl) ->
        let k = (match key with VString s -> s | _ -> raise (EvalError "toml_field_exn: key must be String")) in
        (match Toml.Types.Table.find_opt (Toml.Types.Table.Key.of_string k) tbl with
         | Some v -> VToml v
         | None   -> raise (EvalError ("toml_field_exn: no key: " ^ k)))
      | VToml _ -> raise (EvalError "toml_field_exn: expected table")
      | _ -> raise (EvalError "toml_field_exn: expected TOML"))));
  (* List primitives *)
  ("list_get", VBuiltin (function
    | VInt n -> VBuiltin (function
      | VList xs ->
        let rec nth i = function
          | []     -> v_none
          | x :: _ when i = 0 -> VConstr (Ctor.Builtin "Some", [x])
          | _ :: t -> nth (i - 1) t
        in
        if n < 0 then v_none
        else nth n xs
      | _ -> raise (EvalError "list_get: expected List"))
    | _ -> raise (EvalError "list_get: expected Int index")));
  ("list_tally", VBuiltin (function
    | VList xs ->
      VMap (List.fold_left (fun m x ->
        match x with
        | VString k ->
          vmap_update k (function Some (VInt n) -> VInt (n + 1) | _ -> VInt 1) m
        | _ -> raise (EvalError "List.tally: expected a List of String"))
        vmap_empty xs)
    | _ -> raise (EvalError "List.tally: expected List")));
  ("list_get_exn", VBuiltin (function
    | VInt n -> VBuiltin (function
      | VList xs ->
        let rec nth i = function
          | []     -> raise (EvalError (Printf.sprintf "list_get!: index %d out of bounds" n))
          | x :: _ when i = 0 -> x
          | _ :: t -> nth (i - 1) t
        in
        if n < 0 then raise (EvalError (Printf.sprintf "list_get!: index %d out of bounds" n))
        else nth n xs
      | _ -> raise (EvalError "list_get!: expected List"))
    | _ -> raise (EvalError "list_get!: expected Int index")));
  ("list_sort", VBuiltin (function
    | VList xs -> VList (List.sort wand_compare xs)
    | _ -> raise (EvalError "list_sort: expected List")));
  ("list_sort_by", VBuiltin (fun f ->
    VBuiltin (function
      | VList xs ->
        (* The key was computed inside the comparator, so sorting n elements
           applied `f` about 2n log n times where n would do -- seven million
           interpreted calls to order 200k rows. Compute each key once, sort
           the pairs, drop the keys. `f` now runs exactly once per element,
           left to right. *)
        let keyed = List.map (fun x -> (apply f x, x)) xs in
        let sorted =
          List.stable_sort (fun (ka, _) (kb, _) -> wand_compare ka kb) keyed in
        VList (List.map snd sorted)
      | _ -> raise (EvalError "list_sort_by: expected List"))));
  ("list_unique", VBuiltin (function
    | VList xs ->
      (* Membership is equality, and equality is what `==` answers with.
         This read the stored value instead, so `List.unique [60s, 1min]`
         kept both while `60s == 1min` was true -- and a list of one instant
         written two ways came back holding it twice.

         `eq_key` narrows the search to the values that could be equal;
         `wand_equal` decides among them. *)
      let seen : (value, value list) Hashtbl.t = Hashtbl.create 16 in
      VList (List.filter (fun x ->
        let k = eq_key x in
        let bucket = match Hashtbl.find seen k with
          | b -> b
          | exception Not_found -> []
        in
        if List.exists (wand_equal x) bucket then false
        else (Hashtbl.replace seen k (x :: bucket); true)) xs)
    | _ -> raise (EvalError "list_unique: expected List")));
  ("list_range", VBuiltin (function
    | VInt lo -> VBuiltin (function
      | VInt hi ->
        let rec go i acc =
          if i < lo then acc else go (i - 1) (VInt i :: acc)
        in
        VList (go hi [])
      | _ -> raise (EvalError "list_range: expected Int"))
    | _ -> raise (EvalError "list_range: expected Int")));
  ("list_flatten", VBuiltin (function
    | VList xss ->
      VList (List.concat_map (function
        | VList xs -> xs
        | _ -> raise (EvalError "list_flatten: expected List of Lists")) xss)
    | _ -> raise (EvalError "list_flatten: expected List")));
  ("list_concat", VBuiltin (function
    | VList xs -> VBuiltin (function
      | VList ys -> VList (xs @ ys)
      | _ -> raise (EvalError "list_concat: expected List"))
    | _ -> raise (EvalError "list_concat: expected List")));
]

(* ── Map builtins ─────────────────────────────────────────────────────────── *)

let map_builtins : env = [
  ("map_empty",  VMap vmap_empty);
  ("map_get", VBuiltin (function
    | VString key -> VBuiltin (function
      | VMap m ->
        (match vmap_get key m with
         | Some v -> VConstr (Ctor.Builtin "Some", [v])
         | None   -> v_none)
      | _ -> raise (EvalError "map_get: expected Map"))
    | _ -> raise (EvalError "map_get: expected String key")));
  ("map_get_exn", VBuiltin (function
    | VString key -> VBuiltin (function
      | VMap m ->
        (match vmap_get key m with
         | Some v -> v
         | None   -> raise (EvalError ("map key not found: " ^ key)))
      | _ -> raise (EvalError "map_get!: expected Map"))
    | _ -> raise (EvalError "map_get!: expected String key")));
  ("map_set", VBuiltin (function
    | VString key -> VBuiltin (fun v -> VBuiltin (function
      | VMap m -> VMap (vmap_set key v m)
      | _ -> raise (EvalError "map_set: expected Map")))
    | _ -> raise (EvalError "map_set: expected String key")));
  (* `absent` is what the function sees when the key is not there, so the
     function never meets an `Option` and none is built. A key already
     present keeps its place, as `set` leaves it. *)
  ("map_update", VBuiltin (function
    | VString key -> VBuiltin (fun dflt -> VBuiltin (fun f -> VBuiltin (function
      | VMap m ->
        VMap (vmap_update key
                (fun cur -> apply f (match cur with Some v -> v | None -> dflt)) m)
      | _ -> raise (EvalError "map_update: expected Map"))))
    | _ -> raise (EvalError "map_update: expected String key")));
  ("map_delete", VBuiltin (function
    | VString key -> VBuiltin (function
      | VMap m -> VMap (vmap_delete key m)
      | _ -> raise (EvalError "map_delete: expected Map"))
    | _ -> raise (EvalError "map_delete: expected String key")));
  ("map_has", VBuiltin (function
    | VString key -> VBuiltin (function
      | VMap m -> VBool (vmap_mem key m)
      | _ -> raise (EvalError "map_has?: expected Map"))
    | _ -> raise (EvalError "map_has?: expected String key")));
  ("map_keys", VBuiltin (function
    | VMap m -> VList (List.map (fun (k, _) -> VString k) (vmap_list m))
    | _ -> raise (EvalError "map_keys: expected Map")));
  ("map_values", VBuiltin (function
    | VMap m -> VList (List.map snd (vmap_list m))
    | _ -> raise (EvalError "map_values: expected Map")));
  ("map_size", VBuiltin (function
    | VMap m -> VInt (vmap_size m)
    | _ -> raise (EvalError "map_size: expected Map")));
  ("map_to_list", VBuiltin (function
    | VMap m -> VList (List.map (fun (k, v) -> VTuple [VString k; v]) (vmap_list m))
    | _ -> raise (EvalError "map_to_list: expected Map")));
  ("map_from_list", VBuiltin (function
    | VList pairs ->
      let kvs = List.map (function
        | VTuple [VString k; v] -> (k, v)
        | _ -> raise (EvalError "map_from_list: expected list of (String, value) tuples")) pairs
      in
      VMap (vmap_of_list kvs)
    | _ -> raise (EvalError "map_from_list: expected List")));
  ("map_merge", VBuiltin (function
    | VMap a -> VBuiltin (function
      | VMap b ->
        (* The right-hand value wins, and a key already on the left keeps its
           place there -- merging a change into a document should not shuffle
           the document. *)
        VMap (List.fold_left (fun acc (k, v) -> vmap_set k v acc) a (vmap_list b))
      | _ -> raise (EvalError "map_merge: expected Map"))
    | _ -> raise (EvalError "map_merge: expected Map")));
  ("map_map", VBuiltin (function
    | f -> VBuiltin (function
      | VMap m -> VMap (vmap_map (fun v -> apply f v) m)
      | _ -> raise (EvalError "map_map: expected Map"))));
  ("map_filter", VBuiltin (function
    | f -> VBuiltin (function
      | VMap m ->
        VMap (vmap_filter (fun v ->
          match apply f v with VBool b -> b | _ -> false) m)
      | _ -> raise (EvalError "map_filter: expected Map"))));
]

(* ── Decoder builtins ─────────────────────────────────────────────────────── *)

(* ── Derived decoders ─────────────────────────────────────────────────────
   A single-constructor type whose fields are named already says everything a
   decoder needs: the field names, and what each one holds.

   Built here when a decoder is *used*, never when a type is defined. Building
   at the definition is the obvious move and the expensive one: a type that
   mentions itself would not terminate, a pair that mention each other needs
   a fixpoint over a dependency graph, a type mentioned before it is defined
   needs topological ordering, and every built decoder becomes a value that
   has to cross the module boundary. Deferring costs none of that and buys no
   less: eager construction adds no decoding power whatever, it is the same
   feature built the expensive way. `Pod.decoder`
   reads exactly that, so adding a field to the type adds it to the decoder
   and there is no second copy to go stale.

   Only the flat record is covered. A document whose keys are nested, spelled
   differently, or need validating is what a hand-written decoder is for --
   derivation removes the boilerplate ones, not the interesting ones. *)

(* Why a type has no derived decoder, in a sentence that says what to do. *)
exception Not_derivable of string

(* The head of an applied type and its arguments: `Tree 'a` is ("Tree", ['a]). *)
let rec type_spine te =
  match te with
  | TEApp (f, a) -> let (head, args) = type_spine f in (head, args @ [a])
  | TEName n -> (Some n, [])
  | _ -> (None, [])

(* `venv` binds a type's parameters to the decoders supplied for them, so a
   generic type is read by a decoder that takes one decoder per parameter --
   `Box.decoder : Decoder 'a -> Decoder (Box 'a)`. It is threaded rather than
   global because a type may mention itself with different arguments. *)
(* The evaluator's half of `cmdline_shape`: a field whose type is a record is
   the flags, and the other one is what was written without a flag in front
   of it. The typechecker has already refused anything else, so this only has
   to tell the two apart. *)
let cmdline_parts fields =
  let named = List.filter_map (fun (n, te) ->
    match n with Some n -> Some (n, te) | None -> None) fields in
  match List.partition (fun (_, te) ->
    match te with
    | Ast.TEName n -> Hashtbl.mem derivable n
    | _ -> false) named with
  | [], _ -> None
  | (fname, Ast.TEName ftype) :: _, (aname, ate) :: _ ->
    Some (fname, ftype, aname, ate)
  | _ -> None

(* How many arguments the field that reads them will take, and what each one
   is. A `List` takes any number, an `Option` one or none, anything else
   exactly one -- which is the check `probe-args.wand` used to write by
   hand. *)
let argument_arity (te : Ast.type_expr) =
  match te with
  | Ast.TEApp (Ast.TEName "List", inner) -> (`Many, inner)
  | Ast.TEApp (Ast.TEName "Option", inner) -> (`Maybe, inner)
  | te -> (`One, te)

let rec decoder_of_type_expr venv (te : type_expr) :
  (Yojson.Basic.t -> string list -> (value, string) result) =
  let named name =
    match name with
    | "Int"      -> scalar_decoder "decode_int"
    | "Float"    -> scalar_decoder "decode_float"
    | "String"   -> scalar_decoder "decode_string"
    | "Bool"     -> scalar_decoder "decode_bool"
    | "Path"     -> scalar_decoder "decode_path"
    | "Glob"     -> scalar_decoder "decode_glob"
    | "Duration" -> scalar_decoder "decode_duration"
    | "URL"      -> scalar_decoder "decode_url"
    | "Size"     -> scalar_decoder "decode_size"
    | "Version"  -> scalar_decoder "decode_version"
    | "Date"     -> scalar_decoder "decode_date"
    | "Time"     -> scalar_decoder "decode_time"
    | "DateTime" -> scalar_decoder "decode_datetime"
    | "IPv4"     -> scalar_decoder "decode_ipv4"
    | "CIDR"     -> scalar_decoder "decode_cidr"
    | "Port"     -> scalar_decoder "decode_port"
    | tname ->
      (* Another named type: looked up when a field is decoded, so a type may
         mention itself. *)
      (fun j path -> derived_decoder tname [] j path)
  in
  match te with
  | TEName name -> named name
  (* A decoder is derived from the declaration, which the module registered
     under the type's own name. *)
  | TEQual (_, name) -> named name
  | TEVar (v, _) ->
    (match List.assoc_opt v venv with
     | Some d -> d
     | None ->
       raise (Not_derivable (Printf.sprintf
         "a decoder cannot be derived for the type variable '%s" v)))
  | TEApp (TEName "List", inner) ->
    let elem = decoder_of_type_expr venv inner in
    (fun j path ->
      match j with
      | `List items ->
        let rec go i acc = function
          | [] -> Ok (VList (List.rev acc))
          | x :: rest ->
            (match elem x (Printf.sprintf "[%d]" i :: path) with
             | Ok v      -> go (i + 1) (v :: acc) rest
             | Error msg -> Error msg)
        in
        go 0 [] items
      | _ -> expected "a list" path j)
  | TETuple _ ->
    raise (Not_derivable
      "a field holds a tuple, which has no field names to read it by")
  | TEFun _ ->
    raise (Not_derivable "a field holds a function, which no document contains")
  | TEApp (TEName "Map", inner) ->
    let elem = decoder_of_type_expr venv inner in
    (fun j path ->
      match j with
      | `Assoc kvs ->
        let rec go acc = function
          | [] -> Ok (VMap (vmap_of_list (List.rev acc)))
          | (k, v) :: rest ->
            (match elem v (("." ^ k) :: path) with
             | Ok x      -> go ((k, x) :: acc) rest
             | Error msg -> Error msg)
        in
        go [] kvs
      | _ -> expected "an object" path j)
  | TEApp _ ->
    (* A generic type applied to something: `Tree 'a`, `Paged Pod`. Its own
       parameters are bound to decoders for the arguments, built here in the
       environment the mention stands in. *)
    (match type_spine te with
     | (Some tname, args) when Hashtbl.mem derivable tname ->
       let arg_decoders = List.map (decoder_of_type_expr venv) args in
       (fun j path -> derived_decoder tname arg_decoders j path)
     | _ -> raise (Not_derivable "a field holds a type no decoder is known for"))

(* A builtin decoder, by the name it is registered under. *)
and scalar_decoder name j path =
  match List.assoc_opt name !decode_registry with
  | Some (VDecoder d) -> d j path
  | _ -> raise (EvalError ("derive: no builtin decoder " ^ name))

and derived_decoder tname arg_decoders j path =
  match Hashtbl.find_opt derivable tname with
  | None ->
    Error (Printf.sprintf "no decoder for type '%s'" tname)
  | Some (ctor, params, fields) ->
    let venv =
      try List.combine params arg_decoders
      with Invalid_argument _ ->
        raise (EvalError (Printf.sprintf
          "'%s' takes %d type argument(s)" tname (List.length params)))
    in
    let defaults = defaults_of ctor in
    let rec go acc = function
      | [] -> Ok (VConstr (ctor, List.rev acc))
      | (fname, te) :: rest ->
        let key = match fname with Some n -> n | None -> "" in
        (match read_field venv ~defaults key te j path with
         | Ok v      -> go (v :: acc) rest
         | Error msg -> Error msg)
    in
    (match j with
     | `Assoc _ -> go [] fields
     | _ -> expected "an object" path j)

(* One field of a derived decoder. A field whose type is an `Option` may be
   absent -- that is what the type says -- and every other field may not,
   unless it declares a default: a field left out of the document is the
   same case as a field left out of a construction, and takes the same
   value. A document has null where the language has nothing, and
   `Decode.optional` already reads absent and null alike, so a default
   answers for both. *)
and read_field venv ?(defaults = []) key te j path =
  let kvs = match j with `Assoc kvs -> kvs | _ -> [] in
  let absent () =
    match List.assoc_opt key defaults with
    | Some d -> Some (eval (ctor_env ()) d)
    | None -> None
  in
  match te with
  (* A flag is present or absent, and `Bool` is the type with a word for
     absent, so a document without the key reads as `false` -- the same
     reading `Option` has always had, in the type that a command line
     actually uses for it. Without this a `Bool` field had to carry a
     default to be usable at all, and `Args.parse_with ["verbose"]` failed
     on every line that left `--verbose` off. *)
  | TEName "Bool" when assoc_last key kvs = None ->
    (match absent () with
     | Some v -> Ok v
     | None -> Ok (VBool false))
  | TEApp (TEName "Option", inner) ->
    let d = decoder_of_type_expr venv inner in
    (match assoc_last key kvs with
     | None | Some `Null ->
       (match absent () with
        | Some v -> Ok v
        | None -> Ok (v_none))
     | Some v ->
       (match d v (("." ^ key) :: path) with
        | Ok x      -> Ok (VConstr (Ctor.Builtin "Some", [x]))
        | Error msg -> Error msg))
  | _ ->
    let d = decoder_of_type_expr venv te in
    let here = ("." ^ key) :: path in
    (match assoc_last key kvs with
     | Some `Null | None ->
       (match absent () with
        | Some v -> Ok v
        | None ->
          (match assoc_last key kvs with
           | Some v -> d v here
           | None -> decode_error here "no such field"))
     | Some v -> d v here)

(* ── Derived encoders ─────────────────────────────────────────────────────
   The other direction, and a much smaller thing: encoding cannot fail, so
   there is no error to thread and no path to carry. That is why an encoder
   is an ordinary function `'a -> JSON` rather than a type of its own --
   `JSON` and its constructors already exist, and a second abstraction beside
   `Decoder` would earn nothing.

   It works from the value rather than from the type, since a value carries
   its own tag and cannot disagree with itself. A field holding `None` is
   left out rather than written as null: both read back as `None`, and a
   config is tidier without the empty keys. *)
(* Encoding walks the field types when a type variable is involved, so an
   encoder passed in for a parameter is the one that runs. Everywhere else it
   falls through to the value, which carries its own tag. *)
and json_of_typed venv (te : type_expr) (v : value) : Yojson.Basic.t =
  match te, v with
  | TEVar (name, _), _ ->
    (match List.assoc_opt name venv with
     | Some f ->
       (match apply f v with
        | VJson j -> j
        | other   -> json_of_value other)
     | None -> json_of_value v)
  | TEApp (TEName "Option", _), VConstr (Ctor.Builtin "None", []) -> `Null
  | TEApp (TEName "Option", inner), VConstr (Ctor.Builtin "Some", [x]) -> json_of_typed venv inner x
  | TEApp (TEName "List", inner), VList vs ->
    `List (List.map (json_of_typed venv inner) vs)
  | TEApp (TEName "Map", inner), VMap m ->
    `Assoc (List.map (fun (k, x) -> (k, json_of_typed venv inner x)) (vmap_list m))
  | _, VConstr (_, _) ->
    (match type_spine te with
     | (Some tname, args) when Hashtbl.mem derivable tname ->
       let arg_encoders =
         List.map (fun a ->
           VBuiltin (fun x -> VJson (json_of_typed venv a x))) args
       in
       (match encoded_with tname arg_encoders v with
        | VJson j -> j
        | other -> json_of_value other)
     | _ -> json_of_value v)
  | _ -> json_of_value v

and json_of_value (v : value) : Yojson.Basic.t =
  match v with
  | VInt n    -> `Int n
  | VFloat f when not (Float.is_finite f) ->
    raise (EvalError ("JSON.of: " ^ unwritable_number))
  | VFloat f  -> `Float f
  | VString s -> `String s
  | VBool b   -> `Bool b
  | VUnit     -> `Null
  | VPath s | VDuration s | VURL (s, None) | VSize s | VVersion s
  | VDateTime s | VIPv4 s | VCIDR s | VGlob s -> `String s
  (* A port reads back from either spelling, so it goes out as the number a
     document would have held. *)
  | VPort n -> `Int n
  | VList vs -> `List (List.map json_of_value vs)
  | VMap kvs_m -> let kvs = vmap_list kvs_m in `Assoc (List.map (fun (k, v) -> (k, json_of_value v)) kvs)
  | VJson j -> j
  | VConstr (Ctor.Builtin "None", []) -> `Null
  | VConstr (Ctor.Builtin "Some", [x]) -> json_of_value x
  | VConstr (ctor, vals) ->
    (match Hashtbl.find_opt constr_fields ctor with
     | Some names when List.length names = List.length vals ->
       let pairs =
         List.concat (List.map2 (fun n v ->
           match n, v with
           | Some _, VConstr (Ctor.Builtin "None", []) -> []   (* absent, not null *)
           | Some name, v -> [(name, json_of_value v)]
           | None, _ -> []) names vals)
       in
       `Assoc pairs
     | _ ->
       raise (EvalError (Printf.sprintf
         "cannot encode '%s': it has no named fields" (Ctor.name ctor))))
  | _ -> raise (EvalError "cannot encode this value as JSON")

and encoded_with tname arg_encoders v =
  match Hashtbl.find_opt derivable tname with
  | None -> raise (EvalError (Printf.sprintf "no encoder for type '%s'" tname))
  | Some (_, params, fields) ->
    let venv =
      try List.combine params arg_encoders
      with Invalid_argument _ ->
        raise (EvalError (Printf.sprintf
          "'%s' takes %d type argument(s)" tname (List.length params)))
    in
    (match v with
     | VConstr (_, vals) when List.length vals = List.length fields ->
       let pairs =
         List.concat (List.map2 (fun (fname, te) x ->
           match fname, x with
           (* A field holding None is left out rather than written as null. *)
           | Some _, VConstr (Ctor.Builtin "None", []) -> []
           | Some name, x -> [(name, json_of_typed venv te x)]
           | None, _ -> []) fields vals)
       in
       VJson (`Assoc pairs)
     | _ -> VJson (json_of_value v))

(* What `T.encoder` is worth: a function from the type, after one encoder per
   parameter. Encoding cannot fail, so these are plain functions to JSON. *)
and encoder_value tname =
  match Hashtbl.find_opt derivable tname with
  | None -> raise (EvalError (Printf.sprintf "no encoder for type '%s'" tname))
  | Some (_, params, _) ->
    let rec collect n acc =
      if n = 0 then VBuiltin (fun v -> encoded_with tname (List.rev acc) v)
      else VBuiltin (fun f -> collect (n - 1) (f :: acc))
    in
    collect (List.length params) []

(* What `T.decoder` is worth. A type with no parameters is a decoder; one
   with parameters is a function taking a decoder for each, in the order the
   type declares them -- `Box.decoder : Decoder 'a -> Decoder (Box 'a)`. *)
(* The decoder that reads a command line, as opposed to a document. `Args`
   builds one flat object -- the flags by name, and everything written
   without a flag under `_` -- and a type that describes a whole command line
   is two levels: its record field is those flags, and its other field is
   what was under `_`. So the mapping is here rather than in `Args`, which
   would have to be told the field names to do it, and rather than in
   `T.decoder`, which reads a document and has to keep doing so. *)
and reader_value tname =
  match Hashtbl.find_opt derivable tname with
  | None -> raise (EvalError (Printf.sprintf "no reader for type '%s'" tname))
  | Some (ctor, _, fields) ->
    (match cmdline_parts fields with
     (* No record field: every field is a flag, which is what the decoder
        already reads. *)
     | None -> decoder_value tname
     | Some (fname, ftype, aname, ate) ->
       VDecoder (fun j path ->
         (* The flags are read from the same object, not from a key of their
            own: `--port` is at the top of what `Args` built. *)
         match derived_decoder ftype [] j path with
         | Error msg -> Error msg
         | Ok flags ->
           let written =
             match j with
             | `Assoc kvs ->
               (match assoc_last "_" kvs with
                | Some (`List vs) -> vs
                | _ -> [])
             | _ -> []
           in
           let (arity, inner) = argument_arity ate in
           let d = decoder_of_type_expr [] inner in
           let here = ("." ^ aname) :: path in
           let read_one v = d v here in
           let build v = Ok (VConstr (ctor,
             List.map (fun (n, _) ->
               if n = Some fname then flags else v) fields))
           in
           (match arity, written with
            | `Many, vs ->
              let rec go acc = function
                | [] -> Ok (VList (List.rev acc))
                | v :: rest ->
                  (match read_one v with
                   | Ok x -> go (x :: acc) rest
                   | Error msg -> Error msg)
              in
              (match go [] vs with Ok l -> build l | Error msg -> Error msg)
            | `Maybe, [] -> build (v_none)
            | `Maybe, [v] ->
              (match read_one v with
               | Ok x -> build (VConstr (Ctor.Builtin "Some", [x]))
               | Error msg -> Error msg)
            | `Maybe, vs ->
              decode_error here (Printf.sprintf
                "expected at most one %s, got %d" aname (List.length vs))
            | `One, [v] ->
              (match read_one v with Ok x -> build x | Error msg -> Error msg)
            | `One, [] ->
              decode_error here (Printf.sprintf "expected a %s" aname)
            | `One, vs ->
              decode_error here (Printf.sprintf
                "expected one %s, got %d" aname (List.length vs)))))

and decoder_value tname =
  match Hashtbl.find_opt derivable tname with
  | None -> raise (EvalError (Printf.sprintf "no decoder for type '%s'" tname))
  | Some (_, params, _) ->
    let rec collect n acc =
      if n = 0 then VDecoder (fun j path -> derived_decoder tname (List.rev acc) j path)
      else
        VBuiltin (fun d ->
          match d with
          | VDecoder inner -> collect (n - 1) (inner :: acc)
          | _ -> raise (EvalError (tname ^ ".decoder: expected a Decoder")))
    in
    collect (List.length params) []

and decode_registry : env ref = ref []

let as_decoder who = function
  | VDecoder d -> d
  | _ -> raise (EvalError (who ^ ": expected a Decoder"))

(* Backends that produce one record per row or per line share this: each
   record is decoded at its own index, so a failure says which row it was
   in before it says what was wrong with it. *)
let decode_each inner items =
  let rec go i acc = function
    | [] -> VConstr (Ctor.Builtin "Ok", [VList (List.rev acc)])
    | x :: rest ->
      (match inner x [Printf.sprintf "[%d]" i] with
       | Ok v      -> go (i + 1) (v :: acc) rest
       | Error msg -> VConstr (Ctor.Builtin "Error", [VString msg]))
  in
  go 0 [] items

let decode_builtins : env = [
  ("decode_int", VDecoder (fun j path ->
    match j, from_text int_of_string_opt j with
    | `Int n, _   -> Ok (VInt n)
    | _, Some n   -> Ok (VInt n)
    | _           -> expected "Int" path j));
  ("decode_float", VDecoder (fun j path ->
    match j, from_text float_of_string_opt j with
    (* A whole number in a document is an Int to a parser and a Float to
       whoever wrote it. Reading one as a Float is not a coercion the other
       way round: nothing is lost. *)
    | `Float f, _ -> Ok (VFloat f)
    | `Int n, _   -> Ok (VFloat (float_of_int n))
    | _, Some f   -> Ok (VFloat f)
    | _           -> expected "Float" path j));
  ("decode_string", VDecoder (fun j path ->
    match j with `String s -> Ok (VString s) | _ -> expected "String" path j));
  ("decode_bool", VDecoder (fun j path ->
    let as_bool s = match String.lowercase_ascii s with
      | "true"  -> Some true
      | "false" -> Some false
      | _       -> None
    in
    match j, from_text as_bool j with
    | `Bool b, _ -> Ok (VBool b)
    | _, Some b  -> Ok (VBool b)
    | _          -> expected "Bool" path j));
  (* The two ends of `and_then`: a decoder that reads nothing and answers,
     and one that refuses. Without them `and_then` has nothing to return. *)
  ("decode_succeed", VBuiltin (fun v -> VDecoder (fun _ _ -> Ok v)));
  ("decode_fail", VBuiltin (function
    | VString msg -> VDecoder (fun _ path -> decode_error path msg)
    | _ -> raise (EvalError "decode_fail: expected String")));
  ("decode_field", VBuiltin (function
    | VString key -> VBuiltin (fun d ->
      let inner = as_decoder "decode_field" d in
      VDecoder (fun j path ->
        match j with
        | `Assoc kvs ->
          let here = ("." ^ key) :: path in
          (match assoc_last key kvs with
           | Some v -> inner v here
           | None   -> decode_error here "no such field")
        | _ -> expected "an object" path j))
    | _ -> raise (EvalError "decode_field: key must be String")));
  (* Absence is an answer; a value that will not decode is not.
     `one_of [field name inner, succeed None]` is the version that writes
     itself, and it is wrong: it turns a renamed or retyped field into None
     as readily as a missing one, which is the silent null this whole layer
     exists to replace. Absence is decided here, where it can be told apart
     from failure, and a present field is decoded exactly as `field` would.
     A null is absence written down, so it answers None too. *)
  ("decode_optional", VBuiltin (function
    | VString key -> VBuiltin (fun d ->
      let inner = as_decoder "decode_optional" d in
      VDecoder (fun j path ->
        match j with
        | `Assoc kvs ->
          (match assoc_last key kvs with
           | None | Some `Null -> Ok (v_none)
           | Some v ->
             (match inner v (("." ^ key) :: path) with
              | Ok x      -> Ok (VConstr (Ctor.Builtin "Some", [x]))
              | Error msg -> Error msg))
        | _ -> expected "an object" path j))
    | _ -> raise (EvalError "decode_optional: key must be String")));
  (* An object whose keys are data rather than field names -- a label map,
     per-host counts, anything keyed by a name the program does not know in
     advance. The keys become the Map's keys; a failure names the key it was
     under, exactly as a field would. *)
  ("decode_dict", VBuiltin (fun d ->
    let inner = as_decoder "decode_dict" d in
    VDecoder (fun j path ->
      match j with
      | `Assoc kvs ->
        let rec go acc = function
          | [] -> Ok (VMap (vmap_of_list (List.rev acc)))
          | (k, v) :: rest ->
            (match inner v (("." ^ k) :: path) with
             | Ok x      -> go ((k, x) :: acc) rest
             | Error msg -> Error msg)
        in
        go [] kvs
      | _ -> expected "an object" path j)));
  (* A value that may be null, where no field lookup is involved -- an
     element of a list, say. `optional` is the field-level sibling: it
     answers whether the field is *there*, which is a question only a lookup
     can ask. This one answers whether the value is null. *)
  ("decode_nullable", VBuiltin (fun d ->
    let inner = as_decoder "decode_nullable" d in
    VDecoder (fun j path ->
      match j with
      | `Null -> Ok (v_none)
      | _ ->
        (match inner j path with
         | Ok v      -> Ok (VConstr (Ctor.Builtin "Some", [v]))
         | Error msg -> Error msg))));
  ("decode_list", VBuiltin (fun d ->
    let inner = as_decoder "decode_list" d in
    VDecoder (fun j path ->
      match j with
      | `List items ->
        (* The first element that fails stops the list: a decoder answers
           with a value or with the one thing that went wrong. *)
        let rec go i acc = function
          | [] -> Ok (VList (List.rev acc))
          | x :: rest ->
            (match inner x (Printf.sprintf "[%d]" i :: path) with
             | Ok v      -> go (i + 1) (v :: acc) rest
             | Error msg -> Error msg)
        in
        go 0 [] items
      | _ -> expected "a list" path j)));
  ("decode_map2", VBuiltin (fun f -> VBuiltin (fun da -> VBuiltin (fun db ->
    let a = as_decoder "decode_map2" da in
    let b = as_decoder "decode_map2" db in
    VDecoder (fun j path ->
      match a j path with
      | Error msg -> Error msg
      | Ok va ->
        (match b j path with
         | Error msg -> Error msg
         | Ok vb -> Ok (apply (apply f va) vb)))))));
  ("decode_and_then", VBuiltin (fun f -> VBuiltin (fun d ->
    let inner = as_decoder "decode_and_then" d in
    VDecoder (fun j path ->
      match inner j path with
      | Error msg -> Error msg
      | Ok v -> (as_decoder "decode_and_then" (apply f v)) j path))));
  ("decode_one_of", VBuiltin (function
    | VList ds ->
      let inners = List.map (as_decoder "decode_one_of") ds in
      VDecoder (fun j path ->
        (* Every alternative's complaint is kept. One of them is the reason
           the data is not what was expected, and which one is not for the
           decoder to guess. *)
        let rec go tried = function
          | [] ->
            decode_error path
              ("no alternative matched" ^
               (match List.rev tried with
                | [] -> ""
                | msgs -> ": " ^ String.concat "; " msgs))
          | d :: rest ->
            (match d j path with
             | Ok v      -> Ok v
             | Error msg -> go (msg :: tried) rest)
        in
        go [] inners)
    | _ -> raise (EvalError "decode_one_of: expected List")));
  (* A domain literal decodes as itself: `"30s"` in a document lexes exactly
     as `30s` in a script, so the boundary produces the same Duration the
     rest of the program is written against. *)
  ("decode_path", VDecoder (fun j path ->
    match j with `String s -> Ok (VPath s) | _ -> expected "Path" path j));
  (* A glob is checked, where a path is not: any text names a file, and not
     any text is a pattern. *)
  ("decode_glob", VDecoder (fun j path ->
    match j with
    | `String s ->
      let text = String.trim s in
      (match Lexer.glob_error text with
       | Some why -> decode_error path (Printf.sprintf "expected Glob, got %S: %s" s why)
       | None ->
         (match glob_compile text with
          | Ok _ -> Ok (VGlob text)
          | Error why -> decode_error path (Printf.sprintf "expected Glob, got %S: %s" s why)))
    | _ -> expected "Glob" path j));
  ("decode_duration", VDecoder (decode_lexed "Duration"
    (function Token.Duration v -> Some (VDuration v) | _ -> None)));
  (* Not `decode_lexed`: see `str_to_url`. A decoder reads text the program
     did not write, so the punctuation rule for literals has even less
     business here -- a `,` in a query string is ordinary in a document and
     used to fail the decode. *)
  ("decode_url", VDecoder (fun j path ->
    match j with
    | `String s ->
      let text = String.trim s in
      (match Lexer.url_error text with
       | None -> Ok (VURL (text, None))
       | Some why -> decode_error path (Printf.sprintf "expected URL, got %S: %s" s why))
    | _ -> expected "URL" path j));
  ("decode_size", VDecoder (decode_lexed "Size"
    (function Token.Size v -> Some (VSize v) | _ -> None)));
  (* Not `decode_lexed`: see `str_to_version`. A lock file or a tag list is
     exactly where `v1.2.3` and build metadata turn up. *)
  ("decode_version", VDecoder (fun j path ->
    match j with
    | `String s ->
      let text = Lexer.version_text s in
      (match Lexer.version_error text with
       | None -> Ok (VVersion text)
       | Some why -> decode_error path (Printf.sprintf "expected Version, got %S: %s" s why))
    | _ -> expected "Version" path j));
  ("decode_datetime", VDecoder (decode_lexed "DateTime"
    (function Token.DateTime v -> Some (VDateTime v) | _ -> None)));
  ("decode_ipv4", VDecoder (decode_lexed "IPv4"
    (function Token.IPv4 v -> Some (VIPv4 v) | _ -> None)));
  ("decode_cidr", VDecoder (decode_lexed "CIDR"
    (function Token.CIDR v -> Some (VCIDR v) | _ -> None)));
  (* A port is written `:8080` in a script. In a document it is usually the
     number on its own, which is what a config file or an API contains, and
     sometimes the script's own form. All three go through the lexer in the
     end, so what a decoder accepts is exactly what could have been written
     in the source -- one rule rather than two that drift apart. *)
  ("decode_port", VDecoder (fun j path ->
    (* Out of range is the one case where the number matters more than its
       type, and the lexer has the sentence for it: "expected Port, got Int"
       would be describing what is right about it. *)
    let read text =
      match lex_single (port_text text) with
      | Ok (Token.Port n)  -> Ok (VPort n)
      | Error (Some why)   -> decode_error path why
      | _ ->
        (match j with
         | `Int n -> decode_error path (Printf.sprintf "expected Port, got %d" n)
         | _ -> expected "Port" path j)
    in
    match j with
    | `Int n    -> read (string_of_int n)
    | `String s -> read s
    | _ -> expected "Port" path j));
  ("json_decode", VBuiltin (fun d ->
    let inner = as_decoder "json_decode" d in
    VBuiltin (function
      | VJson j ->
        (match inner j [] with
         | Ok v      -> VConstr (Ctor.Builtin "Ok", [v])
         | Error msg -> VConstr (Ctor.Builtin "Error", [VString msg]))
      | _ -> raise (EvalError "json_decode: expected JSON"))));
  ("toml_decode", VBuiltin (fun d ->
    let inner = as_decoder "toml_decode" d in
    VBuiltin (function
      | VToml t ->
        (match inner (json_of_toml t) [] with
         | Ok v      -> VConstr (Ctor.Builtin "Ok", [v])
         | Error msg -> VConstr (Ctor.Builtin "Error", [VString msg]))
      | _ -> raise (EvalError "toml_decode: expected TOML"))));
  (* No conversion, unlike TOML's: a YAML document already is the value the
     decoders run over. *)
  ("yaml_decode", VBuiltin (fun d ->
    let inner = as_decoder "yaml_decode" d in
    VBuiltin (function
      | VYaml y ->
        (match inner y [] with
         | Ok v      -> VConstr (Ctor.Builtin "Ok", [v])
         | Error msg -> VConstr (Ctor.Builtin "Error", [VString msg]))
      | _ -> raise (EvalError "yaml_decode: expected YAML"))));
  (* One record per line, and the line is text: a command's output has no
     types of its own, so `Decode.int` reads the digits. *)
  ("shell_lines", VBuiltin (fun d ->
    let inner = as_decoder "shell_lines" d in
    VBuiltin (function
      | VString s ->
        (* $() strips the trailing newline, so a non-empty capture has one
           line per record and an empty capture has none -- not one empty
           line, which is the mistake every hand-written count makes. *)
        let lines = if String.trim s = "" then [] else String.split_on_char '\n' s in
        decode_each inner (List.map (fun l -> `String l) lines)
      | _ -> raise (EvalError "shell_lines: expected String"))));
  ("shell_decode", VBuiltin (fun d ->
    let inner = as_decoder "shell_decode" d in
    VBuiltin (function
      | VString s ->
        (match inner (`String (String.trim s)) [] with
         | Ok v      -> VConstr (Ctor.Builtin "Ok", [v])
         | Error msg -> VConstr (Ctor.Builtin "Error", [VString msg]))
      | _ -> raise (EvalError "shell_decode: expected String"))));
  (* A CSV's first row names the columns, so a row arrives as an object and
     is read by field name like anything else. A file without a header is
     what `CSV.parse` is for. *)
  ("csv_rows", VBuiltin (fun d ->
    let inner = as_decoder "csv_rows" d in
    VBuiltin (function
      | VString s ->
        (match csv_parse_string "," s with
         | [] -> VConstr (Ctor.Builtin "Ok", [VList []])
         | header :: rows ->
           let as_object row =
             let rec pair hs cs = match hs, cs with
               | [], _ | _, [] -> []
               | h :: hs', c :: cs' -> (h, `String c) :: pair hs' cs'
             in
             `Assoc (pair header row)
           in
           decode_each inner (List.map as_object rows))
      | _ -> raise (EvalError "csv_rows: expected String"))));
]

(* Derivation reaches the builtin decoders by name, so it needs them after
   they are defined rather than while they are being defined. *)
let () = decode_registry := decode_builtins
let () = derive_decoder := decoder_value
let () = derive_encoder := encoder_value

(* ── Derived usage ────────────────────────────────────────────────────────
   What a command line reading this type looks like. `Args` turns argv into a
   document and a decoder reads it, so the flags are the fields, and the same
   declaration that decides how one is read decides how it is written down.
   The line that used to be a string beside the type could disagree with it;
   this cannot. *)

(* The type as a placeholder for what the flag takes. A flag's value arrives
   as a word, so what a reader needs is the name of the thing that word has
   to be. *)
let rec usage_type_name (te : Ast.type_expr) =
  match te with
  | Ast.TEName n -> n
  | Ast.TEQual (_, n) -> n
  | Ast.TEVar (v, None) -> "'" ^ v
  | Ast.TEVar (v, Some c) -> "'" ^ v ^ ": " ^ c
  | Ast.TEApp (f, a) -> usage_type_name f ^ " " ^ usage_type_name a
  | Ast.TETuple ts ->
    "(" ^ String.concat ", " (List.map usage_type_name ts) ^ ")"
  | Ast.TEFun _ -> "function"

let rec usage_value tname =
  match Hashtbl.find_opt derivable tname with
  | None -> raise (EvalError (Printf.sprintf "no usage for type '%s'" tname))
  | Some (ctor, _, fields) ->
    (* A type that describes a whole command line prints its flags, then
       what it takes without one. *)
    (match cmdline_parts fields with
     | Some (_, ftype, aname, ate) ->
       let flags = match usage_value ftype with VString s -> s | _ -> "" in
       let arg = match fst (argument_arity ate) with
         | `Many  -> Printf.sprintf "<%s>..." aname
         | `Maybe -> Printf.sprintf "[<%s>]" aname
         | `One   -> Printf.sprintf "<%s>" aname
       in
       VString (if flags = "" then arg else flags ^ " " ^ arg)
     | None ->
    let defaults = defaults_of ctor in
    let part (fname, te) =
      match fname with
      | None -> ""
      | Some name ->
        let default = List.assoc_opt name defaults in
        (match te with
         (* A flag with nothing after it: `Args.parse_with` reads it as
            present-or-absent, so there is no value to show. *)
         | Ast.TEName "Bool" -> "[--" ^ name ^ "]"
         (* The type already says this one may be left out. Its default, if
            it has one, is `Some` something, which is a spelling of the
            language rather than of a command line. *)
         | Ast.TEApp (Ast.TEName "Option", inner) ->
           Printf.sprintf "[--%s <%s>]" name (usage_type_name inner)
         | _ ->
           (match default with
            | Some d ->
              Printf.sprintf "[--%s %s]" name (to_text (eval (ctor_env ()) d))
            | None -> Printf.sprintf "--%s <%s>" name (usage_type_name te)))
    in
    VString (String.concat " " (List.filter (fun p -> p <> "")
      (List.map part fields))))

let () = derive_reader := reader_value

let () = derive_usage := usage_value

(* The three, in the order `CommandLine` declares them. They are still
   derived one at a time; what changed is that a caller takes them as one
   value and cannot pair one type's with another's. *)
let () = derive_parser := (fun tname ->
  VConstr (Ctor.Builtin "CommandLine",
           [!derive_spec tname; reader_value tname; usage_value tname]))

(* Everything a command line needs said about a flag that its own text cannot
   say. A `Bool` takes no value, and a `List` collects rather than replacing:
   `--tag a --tag b` is two tags, where `--name a --name b` is one name,
   written twice. Both are facts about the type, and neither is visible in
   argv -- a flag written once looks the same either way.

   Fields that need nothing said are left out, so the map is empty for a type
   whose flags all take one value. *)
let rec spec_value tname =
  match Hashtbl.find_opt derivable tname with
  | None -> raise (EvalError (Printf.sprintf "no spec for type '%s'" tname))
  | Some (_, _, fields) ->
    (* The flags of a whole command line are the record's, so the account of
       them is the record's too. *)
    match cmdline_parts fields with
    | Some (_, ftype, _, _) -> spec_value ftype
    | None ->
    VMap (vmap_of_list (List.filter_map (fun (fname, te) ->
      match fname, te with
      | Some name, Ast.TEName "Bool" -> Some (name, VString "switch")
      | Some name, Ast.TEApp (Ast.TEName "List", _) ->
        Some (name, VString "repeated")
      | _ -> None) fields))

let () = derive_spec := spec_value

(* Any wand value as TOML. TOML has no way to write a bare scalar, so the
   top level must be a table -- a map or a record -- and anything else says
   so rather than producing a document nothing can read.

   An array in this representation is homogeneous: the library holds one
   node per element type, so a list wand allows through its own `List 'a`
   is already uniform, and an empty one is `NodeEmpty`. *)
let rec toml_of_value (v : value) : Toml.Types.value =
  match v with
  | VInt n    -> Toml.Types.TInt n
  | VFloat f  -> Toml.Types.TFloat f
  | VString s -> Toml.Types.TString s
  | VBool b   -> Toml.Types.TBool b
  | VPath s | VDuration s | VURL (s, None) | VSize s | VVersion s
  | VDateTime s | VIPv4 s | VCIDR s | VGlob s -> Toml.Types.TString s
  | VPort n -> Toml.Types.TInt n
  | VConstr (Ctor.Builtin "Some", [x]) -> toml_of_value x
  | VList vs -> Toml.Types.TArray (toml_array vs)
  | VMap kvs_m -> let kvs = vmap_list kvs_m in Toml.Types.TTable (toml_table kvs)
  | VRecord vr_ -> let kvs = vr_.r_fields in Toml.Types.TTable (toml_table kvs)
  | VConstr (ctor, vals) ->
    (match Hashtbl.find_opt constr_fields ctor with
     | Some names when List.length names = List.length vals ->
       let pairs =
         List.concat (List.map2 (fun n v ->
           match n, v with
           | Some _, VConstr (Ctor.Builtin "None", []) -> []   (* absent, not empty *)
           | Some name, v -> [(name, v)]
           | None, _ -> []) names vals)
       in
       Toml.Types.TTable (toml_table pairs)
     | _ ->
       raise (EvalError (Printf.sprintf
         "cannot write '%s' as TOML: it has no named fields" (Ctor.name ctor))))
  | _ -> raise (EvalError "cannot write this value as TOML")

and toml_table kvs =
  List.fold_left (fun tbl (k, v) ->
    (* A key that is absent is left out rather than written empty: TOML has
       no null, so the two would not read back the same. *)
    match v with
    | VConstr (Ctor.Builtin "None", []) -> tbl
    | _ -> Toml.Types.Table.add (Toml.Min.key k) (toml_of_value v) tbl)
    Toml.Types.Table.empty kvs

and toml_array vs =
  match List.map toml_of_value vs with
  | [] -> Toml.Types.NodeEmpty
  | Toml.Types.TInt _ :: _ as ts ->
    Toml.Types.NodeInt (List.map (function Toml.Types.TInt n -> n | _ -> 0) ts)
  | Toml.Types.TFloat _ :: _ as ts ->
    Toml.Types.NodeFloat (List.map (function Toml.Types.TFloat f -> f | _ -> 0.) ts)
  | Toml.Types.TBool _ :: _ as ts ->
    Toml.Types.NodeBool (List.map (function Toml.Types.TBool b -> b | _ -> false) ts)
  | Toml.Types.TString _ :: _ as ts ->
    Toml.Types.NodeString (List.map (function Toml.Types.TString s -> s | _ -> "") ts)
  | Toml.Types.TTable _ :: _ as ts ->
    Toml.Types.NodeTable (List.map (function Toml.Types.TTable t -> t | _ -> Toml.Types.Table.empty) ts)
  | Toml.Types.TArray _ :: _ as ts ->
    Toml.Types.NodeArray (List.map (function Toml.Types.TArray a -> a | _ -> Toml.Types.NodeEmpty) ts)
  | _ -> raise (EvalError "cannot write this list as a TOML array")

(* The top of a TOML document is a table, so a scalar is refused here rather
   than written into something no parser would accept back. *)
let toml_document v =
  match toml_of_value v with
  | Toml.Types.TTable _ as t -> t
  | _ ->
    raise (EvalError
      "a TOML document is a table: write a map or a record, not a bare value")

(* Any wand value as JSON, in one call, so a structure does not have to be
   converted a piece at a time. `json_of_value` already walks numbers, text,
   every domain type, lists, maps, options and records; what it cannot write
   is a function, a resource or a stream, and that is an `Error` rather than
   a raise. Registered here because it is defined after the table above. *)
let serialise_builtins : env = [
  ("json_of", VBuiltin (fun v ->
    match json_of_value v with
    | j -> VConstr (Ctor.Builtin "Ok", [VJson j])
    | exception EvalError m -> VConstr (Ctor.Builtin "Error", [VString m])));
  ("json_of_exn", VBuiltin (fun v -> VJson (json_of_value v)));
  ("toml_of", VBuiltin (fun v ->
    match toml_document v with
    | t -> VConstr (Ctor.Builtin "Ok", [VToml t])
    | exception EvalError m -> VConstr (Ctor.Builtin "Error", [VString m])));
  ("toml_of_exn", VBuiltin (fun v -> VToml (toml_document v)));
]

let stdlib_eval_env =
  stdlib_eval_env @ map_builtins @ decode_builtins @ stream_builtins
  (* `Option`'s constructors are built in, so a module reaches them the way
     it reaches a builtin function rather than by importing the module that
     used to declare the type. *)
  @ [ ("Some", VPartialConstr (Ctor.Builtin "Some", 1, []));
      ("None", v_none) ]
  @ serialise_builtins

(* Every function a file calls comes from a module it imported. These two
   are constructors of a built-in type, so there is no module to import
   them from. *)
let base_eval_env : env = [
  ("Ok",      VPartialConstr (Ctor.Builtin "Ok",    1, []));
  ("Error",   VPartialConstr (Ctor.Builtin "Error", 1, []));
  (* `Option` is built in, so its constructors are here beside `Result`'s
     rather than arriving with an import. *)
  ("Some",    VPartialConstr (Ctor.Builtin "Some",  1, []));
  ("None",    v_none);
]
