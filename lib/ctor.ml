(* Which constructor a value is, and which declaration it came from.

   The identity used to be the bare name. Two modules that each declare
   `Status` therefore shared one identity: they collided in the evaluator's
   tables, and a pattern from one file matched a value from the other. That
   is why `Foo.Status` and `Bar.Status` cannot both be used in one file.

   A variant rather than a record, so an identity reads the same in a pattern
   as in an expression -- `VConstr (Ctor.Builtin "Ok", [v])` matches and
   builds. *)

type t =
  (* `Ok`, `Error`, `Some`, `None`, `ShellResult`: the language's own, which
     no module declares and no file may redeclare. *)
  | Builtin of string
  (* Declared in the file being run. A script is not a module, so nothing
     else can name its types, and the bare name is identity enough. *)
  | Local of string
  (* Declared in a module, keyed by the module's path rather than by whatever
     the importing file calls it. Two files that alias one module
     differently still agree about its constructors. *)
  | Owned of string * string

(* A declared constructor is keyed by its type as well as its name --
   `PullPolicy.Always` -- since two types in one module may each have an
   `Always`. The key is the identity; the name is what a reader and a
   document see, and is what follows the last dot. *)
let bare key =
  match String.rindex_opt key '.' with
  | Some i -> String.sub key (i + 1) (String.length key - i - 1)
  | None -> key

let name = function
  | Builtin n -> n
  | Local k | Owned (_, k) -> bare k

(* `Type.Ctor`, or the bare name for a built-in. *)
let key = function
  | Builtin n -> n
  | Local k | Owned (_, k) -> k

let make_key ~type_name name = type_name ^ "." ^ name

let modul = function
  | Builtin _ | Local _ -> None
  | Owned (m, _) -> Some m

let equal (a : t) (b : t) = a = b

(* What a reader sees: the constructor as it was written. Two constructors of
   one name print alike, which is what a value has always looked like. *)
let to_string c = name c
