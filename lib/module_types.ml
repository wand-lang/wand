(* ── Import path resolution ───────────────────────────────────────────────── *)

(* Pure import path handling, shared by `Runner` and `Evaluator`. Lives below
   `Evaluator` in the dependency graph -- it touches only
   `Ast`/`Lexer`/`Parser`, never `Evaluator.value` -- so both can depend on it
   without a cycle. *)

(* What module loading answers with when it refuses: a path that is not
   there, a name this binary does not carry, a symbol a module does not
   export, a pattern that cannot destructure one.

   An exception of its own rather than `Failure`, because `diag_of_exn`
   turns any `Failure` into `E-FAIL` -- no code, no position -- and every
   one of these is a diagnostic a person meets by mistyping an import, not
   an internal error. Reported as `E-IMPORT`.

   The location travels separately and is attached by the caller: the import
   site is a position in the file being checked, which is not something this
   module can see. `ImportError` is what the refusing site raises;
   `ImportErrorAt` is what the import walk re-raises once it knows where.
   Found by test/fuzz. *)
exception ImportError of string
exception ImportErrorAt of Token.loc * string

let add_ext p = if Filename.check_suffix p ".wand" then p else p ^ ".wand"

(* The file a command was handed, made absolute. `add_ext` is for an import,
   which names a module rather than a file -- `import ./util` reaches
   `util.wand`. A command is handed a path, and an executable script with a
   shebang and no extension is a path that is there, so the name as written
   wins whenever there is a file under it. *)
let entry_path path =
  let full =
    if Filename.is_relative path
    then Filename.concat (Sys.getcwd ()) path
    else path
  in
  let is_file p =
    match Sys.is_directory p with
    | false -> true
    | true -> false
    | exception Sys_error _ -> false
  in
  if is_file full then full
  else if Filename.is_relative path
  then Filename.concat (Sys.getcwd ()) (add_ext path)
  else add_ext path

(* Where a module's source comes from. The standard library is carried in
   the binary, so it has no path and no directory to be found in; a user
   module is a file. Keeping the two apart in the type is what lets one
   loader serve both without a filesystem lookup standing in for "is this
   the standard library". *)
type source =
  | Embedded of string (* a standard library module, by name *)
  | File of string (* a path on disk *)

(* The name a module is known by while loading: the import cache, the cycle
   check and the compile cache's dependency keys are all keyed on this. An
   embedded module's key is a path that cannot exist, so it can never
   collide with a user file -- and `Filename.dirname` of it gives the base
   directory its own imports resolve against, which for a stdlib module is
   never consulted, because everything it imports is another stdlib module. *)
let stdlib_base_dir = "<stdlib>"

(* A type's canonical name: the module that declares it, and the name it was
   declared under. Two modules that each declare `Status` therefore declare
   two types, and a file that writes the short name says which through its
   imports. A file that is run declares types nothing else can name, so its
   own keep their short names. *)
let canonical_type ~modul name = modul ^ "#" ^ name

(* A declaration as it travels: every type it names is canonical, so
   rebuilding a constructor's type anywhere resolves to the same type. A file
   that reads the declaration never writes these names.

   `names` is the declaring module's own map from what it writes to what
   that means -- its own types, and `M.Meta` for an import it bound as `M`.
   Reading the declaration against the importer's map instead made
   `metadata : M.Meta` mean whatever the importer called `M`, or nothing:
   a file that bound the same module as `Meta` got "unknown type 'M.Meta'". *)
let canonicalise_tdef (names : (string * string) list) (tdef : Ast.type_def) =
  let rec te (t : Ast.type_expr) : Ast.type_expr =
    match t with
    | Ast.TEName n ->
      (match List.assoc_opt n names with
       | Some c -> Ast.TEName c
       | None -> t)
    | Ast.TEQual (m, n) ->
      (match List.assoc_opt (m ^ "." ^ n) names with
       | Some c -> Ast.TEName c
       | None -> t)
    | Ast.TEVar _ -> t
    | Ast.TEApp (f, a) -> Ast.TEApp (te f, te a)
    | Ast.TETuple ts -> Ast.TETuple (List.map te ts)
    | Ast.TEFun (a, b, e) -> Ast.TEFun (te a, te b, e)
  in
  match tdef with
  | Ast.Alias (n, ps, t) -> Ast.Alias (n, ps, te t)
  | Ast.Variants (n, ps, ctors) ->
    Ast.Variants (n, ps,
      List.map (fun (c : Ast.ctor_def) ->
        { c with Ast.fields = List.map (fun (f, t) -> (f, te t)) c.Ast.fields })
        ctors)

(* One file, one key. A module reached as `../../x/a.wand` and as
   `/abs/x/./a.wand` is the same module, and its types are the same types, so
   the spelling a file happened to use cannot be part of its identity. *)
let normalise path =
  match Unix.realpath path with
  | p -> p
  | exception _ ->
    (* The file may not exist yet, or the platform may not answer. Take out
       what can be taken out without asking the filesystem. *)
    let parts = String.split_on_char '/' path in
    let rec go acc = function
      | [] -> List.rev acc
      | "." :: rest -> go acc rest
      | ".." :: rest ->
        (match acc with
         | _ :: tl -> go tl rest
         | [] -> go [".."] rest)
      | "" :: rest when acc <> [] -> go acc rest
      | p :: rest -> go (p :: acc) rest
    in
    let joined = String.concat "/" (go [] parts) in
    if String.length path > 0 && path.[0] = '/' then "/" ^ joined else joined

let key_of = function
  | Embedded name -> Filename.concat stdlib_base_dir (name ^ ".wand")
  | File path -> normalise path

(* `WAND_STDLIB` points at a standard library to use instead of the embedded
   one. It is a development override -- run a built binary against a working
   tree -- rather than how the library is normally found. Empty counts as
   unset: an empty value almost always arrives from a shell interpolating a
   variable that held nothing, and reading that as "use the directory named
   by nothing" breaks every run for a reason nobody can see. *)
let stdlib_override () =
  match Sys.getenv_opt "WAND_STDLIB" with
  | Some dir when String.trim dir <> "" -> Some (String.trim dir)
  | _ -> None

let embedded_module name =
  match List.assoc_opt name Stdlib_embed.table with
  | Some _ -> Some name
  | None ->
    (* Case is a fallback, not the rule: `import List` is how the module is
       written, and `import list` finds it rather than reporting that no
       such module exists. Spelled out here because a table has no case
       rules of its own, and leaving it to the platform is how a script
       comes to run on one machine and not another. *)
    let lower = String.lowercase_ascii name in
    List.find_map
      (fun (n, _) -> if String.lowercase_ascii n = lower then Some n else None)
      Stdlib_embed.table

let resolve_stdlib name =
  match stdlib_override () with
  | Some dir ->
    let exact = Filename.concat dir (name ^ ".wand") in
    if Sys.file_exists exact then File exact
    else
      let lower =
        Filename.concat dir (String.lowercase_ascii name ^ ".wand")
      in
      if Sys.file_exists lower then File lower
      else
        (* Pointing at the wrong directory is the likely mistake, and it is
           worth saying so: the override is the only thing standing between
           the run and a standard library that is known to be present. *)
        raise (ImportError
          (Printf.sprintf
             "no standard library module named '%s': WAND_STDLIB is set to \
              %S, which has no %s.wand. Unset it to use the standard library \
              built into this binary."
             name dir name))
  | None ->
    (match embedded_module name with
     | Some n -> Embedded n
     | None ->
       raise (ImportError
         (Printf.sprintf
            "no standard library module named '%s'. This binary carries: %s."
            name
            (String.concat ", " (List.map fst Stdlib_embed.table)))))

let resolve_import base_dir = function
  | Ast.StdlibModule name -> resolve_stdlib name
  | Ast.ModuleURL url ->
    (try File (Package.resolve_url ~base_dir url)
     with Package.Unresolved msg -> raise (ImportError msg))
  | Ast.ModuleAlias name ->
    (try File (Package.resolve_alias ~base_dir name)
     with Package.Unresolved msg -> raise (ImportError msg))
  | Ast.UserPath path ->
    let file =
      if Filename.is_relative path
      then Filename.concat base_dir (add_ext path)
      else add_ext path
    in
    (try Package.check_private_path ~base_dir file
     with Package.Unresolved msg -> raise (ImportError msg));
    File file

let read_source = function
  | Embedded name ->
    (match List.assoc_opt name Stdlib_embed.table with
     | Some src -> src
     | None -> raise (ImportError ("no standard library module named '" ^ name ^ "'")))
  | File path ->
    (try In_channel.with_open_text path In_channel.input_all
     with Sys_error msg -> raise (ImportError ("cannot import '" ^ path ^ "': " ^ msg)))

(* Whether a directory is a standard library, asked of a directory someone
   already named rather than searched for. A directory holding every module
   this binary carries is one; a directory that happens to contain a file
   called `List.wand` is not. That distinction is the whole reason the
   upward search is gone, so it is not re-introduced here. *)
let is_stdlib_dir dir =
  Stdlib_embed.table <> []
  && List.for_all
       (fun (name, _) -> Sys.file_exists (Filename.concat dir (name ^ ".wand")))
       Stdlib_embed.table

let namespace_name_of kind =
  match Parser.import_name kind with
  | Some n -> n
  | None ->
    let p = Parser.import_text kind in
    raise (ImportError (Printf.sprintf
      "`import %s` has no name to bind; write `let name = import %s`" p p))

let strip_located = Ast.strip_located

let import_kind_of e = match strip_located e with
  | Ast.ImportExpr k -> Some k
  | _ -> None

let local_tenv_of prog =
  List.filter_map (function
    | Ast.TLType (((Ast.Variants (n, _, _) | Ast.Alias (n, _, _)) as tdef), _) ->
      Some (n, tdef)
    | _ -> None) prog.Ast.items

let is_private name = String.length name > 0 && name.[0] = '_'
