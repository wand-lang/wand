open Ast

(* Every module under `stdlib/`. The list drives three things that must agree
   with what is on disk: which names `wand d` will import to answer about,
   which ones `wand d` lists, and which unknown name gets "did you forget
   to import" instead of "unknown constructor". A module missing from here
   still imports and runs -- it just becomes invisible to the tools, which is
   how `Test` went a long time with unreachable doc strings. *)
let stdlib_module_names =
  [ "List"; "String"; "Path"; "FS"; "IO"; "Float"; "Duration"; "Env"; "Map";
    "Regex"; "JSON"; "TOML"; "CSV"; "Option"; "Par"; "Resource"; "Stream";
    "Proc"; "Decode"; "Shell"; "Test"; "Args"; "Clock"; "Size"; "Port";
    "DateTime"; "Result"; "URL"; "Version"; "Glob"; "IPv4"; "CIDR";
    "Random"; "Int"; "Hash"; "Digest"; "Base64"; "HTTP"; "YAML" ]

(* A module that was one and is not. The name is not unknown to anyone
   holding a script from an earlier release, so the error says what to write
   instead of it. *)
let moved_module_names =
  [ "Ord", "the comparisons sit on each ordered type. Write 'Int.max', \
            'Duration.min', 'Path.between?' etc. `Ord` is the interface they \
            implement, not a module" ]

(* ── Types ────────────────────────────────────────────────────────────────── *)

type typ =
  | TInt | TFloat | TString | TBool | TUnit
  | TPath | TGlob | TDateTime | TDuration
  | TURL | TIPv4 | TCIDR | TPort | TVersion | TSize
  | TVar    of tv
  | TFun    of typ * typ * Effect_set.t  (* arg, result, effects of calling *)
  | TTuple  of typ list
  | TList   of typ
  | TResult of typ * typ  (* error type, value type *)
  | TMap    of typ
  | TApp    of typ * typ  (* user-defined generic type application *)
  (* The name an alias was written with, over the type it names. An alias is
     transparent -- `unify` strips this and reads what is underneath -- so
     the two are one type and never disagree. It is carried only so a
     message can show the name the reader wrote: `Point (= (Int, Int))`.

     It sits where the annotation was, and is never merged with another, so
     two aliases of one type cannot argue over which name to print. *)
  | TAlias  of string * typ list * typ
  | TRegex
  (* A command, and never what it did: a description of one subprocess, made
     by `$*(cmd)` and run by `Shell.run!`, `Shell.query` and their kin. The
     quoting is done by the literal, so a `Command` can only be built where
     its words were written -- there is no function from a String to one,
     which is what makes a function taking one safe. *)
  | TCommand
  (* A resource: how to acquire an 'a and give it back, and what doing
     either performs. The effects are carried rather than hidden -- a bracket
     that concealed its own effects would let a file take a lock and
     report a signature that never mentions it. *)
  | TResource of Effect_set.t * typ
  (* A stream: an inert description of a source and its stages, whose
     effects are what a terminal operation performs when it reads the source. Like
     a Resource, it describes; it is never the open thing. *)
  | TStream of Effect_set.t * typ
  (* How to read an 'a out of data that arrived untyped. No effects: decoding is
     a function from something already read, so a decoder that could perform
     effects would be a second way to run a script's I/O. *)
  | TDecoder of typ
  | TJson
  | TToml
  | TYaml
  | TName of string
  (* The type of a module that implements an interface: the contract's name
     and what its parameters were bound to. `Ord Int` is the type of a module
     providing `max` and `min` at `Int`.

     It is a type for a *module*, not for a value of the type it orders.
     Nothing is dispatched from it: the caller passes the module, so this is
     the annotation a parameter carries and nothing more. *)
  | TIface of string * typ list
  (* A module used as a value, carrying the interfaces it claims. Conformance
     is nominal -- `implement` is what makes a module fit, and a module with
     the right members by accident does not -- so what travels here is the
     claims rather than the members.

     They ride in the module's own type environment, so an imported module
     brings them with it and a cached one keeps them, with no table to hold
     consistent. *)
  | TModule of (string * typ list) list

and tv = {
  id  : int;
  mutable def : typ option;
  (* What this variable may become. The evaluator dispatches on the value's
     tag, so a constrained variable stays generalized: there is no
     defaulting, and `let double x = x + x` works at both numeric types.

     The constraints nest: every `Num` is an `Add`, and every `Add` is an
     `Ord`. So a flag per constraint cannot express them -- unifying a
     `Num` variable with an `Ord` one has to yield `Num` -- and a variant
     can. *)
  constrained : constrained;
}

(* `Free` may become anything. `Num` is `Int` or `Float`, the `Num` in
   `Num -> Num`. `Add` is what `+` and `-` take: the numbers, and the two
   quantities that add without leaving their type. `Ord` is any type wand
   can order, which is the widest set. *)
and constrained = Free | Ord | Add | Num

(* Builtin signatures are written with `@->` so the tables stay readable.
   Every builtin's *own* latent effect is filled in separately; the arrow
   itself carries no effect until it is seeded. *)
let ( @-> ) a b = TFun (a, b, Effect_set.unknown ())

(* `effs es a b` is "a to b, performing es". Only the arrow that is
   actually applied last carries them: reading a file happens when the path
   *and* the contents have arrived, not when the first argument does. *)
let effs es a b = TFun (a, b, Effect_set.of_list es)

(* ── Fresh variable generation ────────────────────────────────────────────── *)

let next_id = ref 0
let holes : typ list ref = ref []

(* The type of every expression discarded by a `(e1; e2)` sequence, with the
   location of the discarded expression. The lint that catches a thrown-away
   `Result` needs it, for the same reason `expr_item_types` exists for bare
   top-level statements. *)
let seq_discard_types : (Token.loc * typ) list ref = ref []

(* The type a `let _ = ...` binds, with the location of the bound
   expression. A binder that names nothing is how a file says a failure does
   not matter; where the value is Unit there is no failure to dismiss, and
   the lint that says so needs the type to tell the two apart. *)
let wild_let_types : (Token.loc * typ) list ref = ref []

(* `effect_of_operation` and `operation_types` are derived from the
   operations table, which needs `fresh` -- see "Effect operations" below. *)

(* Effects performed by whatever is currently being inferred.

   Evaluation is strict and left to right, so the effects of an expression
   are just the union of everything evaluated along the way -- which an
   accumulator expresses directly, rather than threading a set out of all
   forty-nine cases of `infer` and unioning it back together by hand.

   Only two places touch it: applying a function adds that function's latent
   effects, and inferring a lambda's body scopes them, because defining a
   function performs nothing. *)
(* A scope's effects start undetermined rather than empty: a function that
   performs nothing of its own still passes on whatever its arguments do,
   and an open set is what lets that be generalised into effect
   polymorphism. *)
let current_eff : Effect_set.t ref = ref (Effect_set.unknown ())

let performs r = current_eff := Effect_set.absorb ~ambient:!current_eff r

(* Run `f` with its own effect accumulator, returning what it performed
   alongside its result and leaving the enclosing accumulator untouched. *)
let scoped_eff f =
  let saved = !current_eff in
  current_eff := Effect_set.unknown ();
  let result = (try f () with e -> current_eff := saved; raise e) in
  let inner = !current_eff in
  current_eff := saved;
  (result, inner)

(* Name of the function whose body is being inferred, so a match that came
   from a multi-equation definition can report failures in terms of that
   definition rather than the desugared match it became. *)
let current_fn : string option ref = ref None

(* Innermost wins: a local `let k` inside `h` reports as `k`, since the
   message now quotes the equation as written and a name from an enclosing
   definition would make that quote a fiction. Restored on the way out,
   whether the body checked or raised. *)
let with_current_fn name f =
  let saved = !current_fn in
  current_fn := Some name;
  Fun.protect ~finally:(fun () -> current_fn := saved) f

(* Local binders -- parameters, `let ... in` names, pattern variables --
   with the index of the top-level item that binds them. Expressions carry
   no positions, so "which occurrence is the cursor on" cannot be answered,
   but "this name, inside this item" is enough for an editor hover on names
   the top-level scope never sees. Recorded head-first, so within an item
   the most recent (innermost) binding of a shadowed name is found first.
   The typs keep refining in place as unification proceeds; read them only
   after inference finishes. *)
let local_binders : (int * (string * typ)) list ref = ref []
let current_item = ref (-1)
let record_local name t =
  local_binders := (!current_item, (name, t)) :: !local_binders

let fresh_as c =
  let id = !next_id in
  incr next_id;
  TVar { id; def = None; constrained = c }

let fresh ()     = fresh_as Free
let fresh_num () = fresh_as Num
let fresh_add () = fresh_as Add
let fresh_ord () = fresh_as Ord

(* The types wand orders. `Int`, `Float` and `String` were ordered before
   there was a constraint; the temporal four join them here.

   Ordering compares normalized values, never the stored string. A
   `DateTime` carries an offset, so two spellings of one instant must
   compare equal. A `Duration` is a sum of units. `Date` and `Time` are
   fixed-width and zero-padded, so for those two the string order is the
   right order -- stated because it was checked, not assumed. *)
let is_ordered = function
  | TInt | TFloat | TString
  | TDuration | TDateTime
  | TSize | TVersion | TPort | TIPv4 | TCIDR
  (* Compared in the form that holds whatever the disk contains: repeated
     separators, `.` segments and a trailing separator do not change which
     file a path names. `..` is left alone, because resolving it needs to
     know whether a component is a symlink. *)
  | TPath -> true
  | _ -> false

(* The types `+` and `-` take. A `Size` and a `Duration` add to their own
   type and to nothing else, which is what separates them from the rest of
   the ordered set: two dates do not add, and a date plus a duration is a
   date, an operator with three types in it.

   `*` and `/` are not here. Scaling a quantity by a number is that same
   three-type shape, and it already has a name in `Duration.scale`. *)
let is_additive = function
  | TInt | TFloat | TSize | TDuration -> true
  | _ -> false

(* Which of two constraints a unified variable keeps: the narrower one.
   `Num` is inside `Add`, `Add` is inside `Ord`, and all of them are inside
   `Free`. *)
let narrower a b =
  match a, b with
  | Num, _ | _, Num -> Num
  | Add, _ | _, Add -> Add
  | Ord, _ | _, Ord -> Ord
  | Free, Free -> Free

(* What an intercepted operation carries, and what resuming it has to supply.

   A handler case binds the operation's payload and calls its continuation,
   and both used to be inferred as fresh variables -- so a case could read a
   path as a String, or resume a read with an Int, and find out only when it
   ran. That is the one place in the language where a value crossed a
   boundary unchecked, in the construct whose whole purpose is standing at a
   boundary.

   An operation carries what its *raising* builtin returns, not what the
   `Result`-returning wrapper does: `FS.mtime` is `try mtime!`, so the effect
   supplies a `DateTime` and the `try` around it makes the `Result`.

   `Shell!run` and `Shell!capture` are left out on purpose: each carries
   either a command, or a command and the stdin threaded into it, so there
   is no single payload type to give them. Declaring one would be worse than
   declaring none -- a handler would be told the shape was a String and
   handed a tuple whenever someone piped into the command. *)
(* ── Effect operations ───────────────────────────────────────────────────── *)

(* Every operation a handler can catch, in one table: the effect catching it
   accounts for, what it carries, and what a script writes to perform it.
   The three used to be a match apiece, which meant nothing could enumerate
   them -- an editor could not answer what `FS!` might become, and the
   answers could disagree without anything noticing. They are read from here
   now, so they cannot drift, and `operation_names` hands the list to
   completion.

   `op_performers` is the part a reader wants and no analysis can supply:
   `FS!read_file` is what `JSON.read_file` performs as much as `FS.read_file`,
   and `Shell!run` is performed by `$(...)`, which is syntax rather than a
   name. It is checked rather than trusted -- test/wand/test_operations.wand
   calls every entry under a handler for its operation and fails if the
   handler does not fire. The lists name what a script calls directly; a
   function that reaches an operation through another (`FS.glob` asks where
   it is standing, so it performs `FS!cwd`) is not repeated here. *)
type operation = {
  op_name : string;
  op_effect : Effect_set.eff;
  (* Payload and resume types, behind a thunk: two operations answer with a
     fresh type variable, and every mention has to get its own or the first
     handler in a file would pin the rest. `None` leaves both open. *)
  op_types : unit -> (typ * typ) option;
  op_performers : string list;
}

let operations : operation list =
  let path = TPath and str = TString in
  let t p r = fun () -> Some (p, r) in
  let open Effect_set in
  [
    (* Reading. *)
    { op_name = "FS!read_file"; op_effect = FsRead; op_types = t path str;
      (* Every reader that parses a file reads it through this one operation,
         so a test mocks reading once rather than once per format. *)
      op_performers = ["FS.read_file"; "FS.read_file!"; "JSON.read_file";
                       "JSON.read_file!"; "CSV.read_file"; "CSV.read_file!";
                       "TOML.read_file"; "TOML.read_file!"; "Env.read!"] };
    { op_name = "FS!stream_lines"; op_effect = FsRead; op_types = t path (TList str);
      op_performers = ["FS.stream_lines"; "FS.stream_lines_all"] };
    (* The algorithm rides along with the path, so a handler that mocks this
       answers per file rather than per file and algorithm. *)
    { op_name = "Hash!file"; op_effect = FsRead;
      op_types = t (TTuple [str; path]) str;
      op_performers = ["Hash.file"; "Hash.file!"] };
    { op_name = "FS!list_dir"; op_effect = FsRead; op_types = t path (TList path);
      op_performers = ["FS.list_dir"; "FS.list_dir!"] };
    { op_name = "FS!glob"; op_effect = FsRead;
      op_types = t (TTuple [TGlob; path]) (TList path);
      op_performers = ["FS.glob"; "FS.glob_in"] };
    { op_name = "FS!exists?"; op_effect = FsRead; op_types = t path TBool;
      op_performers = ["FS.exists?"] };
    { op_name = "FS!file?"; op_effect = FsRead; op_types = t path TBool;
      op_performers = ["FS.file?"] };
    { op_name = "FS!dir?"; op_effect = FsRead; op_types = t path TBool;
      op_performers = ["FS.dir?"] };
    { op_name = "FS!mtime"; op_effect = FsRead; op_types = t path TDateTime;
      op_performers = ["FS.mtime"; "FS.mtime!"] };
    { op_name = "FS!size"; op_effect = FsRead; op_types = t path TInt;
      op_performers = ["FS.size"; "FS.size!"] };
    { op_name = "FS!cwd"; op_effect = FsRead; op_types = t TUnit path;
      op_performers = ["FS.cwd"] };
    (* Writing. *)
    (* Opening a file to write a stream into it. One operation per open, as
       reading has: the lines go through the channel it answers with, so a
       trace shows the file once rather than once per line. A handler that
       answers `k ()` sends them nowhere. *)
    { op_name = "FS!write_lines"; op_effect = FsWrite; op_types = t path TUnit;
      op_performers = ["FS.write_lines"; "FS.write_lines!"] };
    { op_name = "FS!append_lines"; op_effect = FsWrite; op_types = t path TUnit;
      op_performers = ["FS.append_lines"; "FS.append_lines!"] };
    (* The streaming half of `FS!write_atomic`. One operation per open, as
       the other two sinks have, and the same payload: the lines go through
       the sink it answers with, so a trace shows the file once rather than
       once per line. What differs is the ending -- the sink publishes when
       the stream finishes and takes its temp file away when the stream
       raises. *)
    { op_name = "FS!write_lines_atomic"; op_effect = FsWrite;
      op_types = t path TUnit;
      op_performers = ["FS.write_lines_atomic"; "FS.write_lines_atomic!"] };
    { op_name = "FS!write_file"; op_effect = FsWrite;
      op_types = t (TTuple [path; str]) TUnit;
      op_performers = ["FS.write_file"; "FS.write_file!"] };
    (* Publishing a file whole. The write and the rename are one operation
       because a handler that saw them apart could answer one and not the
       other, which is the half-written file the function exists to prevent.
       Reading the target's mode is part of it, so the builtin's type carries
       `FS.Read` as well; the label a handler discharges is the write. *)
    { op_name = "FS!write_atomic"; op_effect = FsWrite;
      op_types = t (TTuple [path; str]) TUnit;
      op_performers = ["FS.write_atomic"; "FS.write_atomic!"] };
    { op_name = "FS!append"; op_effect = FsWrite;
      op_types = t (TTuple [path; str]) TUnit;
      op_performers = ["FS.append"; "FS.append!"] };
    { op_name = "FS!create_file"; op_effect = FsWrite; op_types = t path TUnit;
      op_performers = ["FS.create_file"; "FS.create_file!"] };
    { op_name = "FS!delete"; op_effect = FsWrite; op_types = t path TUnit;
      op_performers = ["FS.delete"; "FS.delete!"] };
    { op_name = "FS!delete_tree"; op_effect = FsWrite; op_types = t path TUnit;
      (* A scratch directory being released performs this as well, which is
         why the bracket is listed beside the two calls. *)
      op_performers = ["FS.delete_tree"; "FS.delete_tree!"; "FS.temp_dir"] };
    { op_name = "FS!copy_tree"; op_effect = FsWrite;
      op_types = t (TTuple [path; path]) TUnit;
      op_performers = ["FS.copy_tree"; "FS.copy_tree!"] };
    { op_name = "FS!mkdir"; op_effect = FsWrite; op_types = t path TUnit;
      op_performers = ["FS.mkdir"; "FS.mkdir!"] };
    { op_name = "FS!rename"; op_effect = FsWrite;
      op_types = t (TTuple [path; path]) TUnit;
      op_performers = ["FS.rename"; "FS.rename!"] };
    { op_name = "FS!copy"; op_effect = FsWrite;
      op_types = t (TTuple [path; path]) TUnit;
      op_performers = ["FS.copy"; "FS.copy!"] };
    { op_name = "FS!temp_file"; op_effect = FsWrite;
      op_types = t (TTuple [str; str]) path;
      op_performers = ["FS.temp_file"] };
    { op_name = "FS!temp_dir"; op_effect = FsWrite; op_types = t str path;
      op_performers = ["FS.temp_dir"] };
    (* Taking a lock creates a file and holds it, so the label is the write.
       The answer is an `Option`: `None` is the one failure a caller
       branches on -- another process holds it -- and everything else is a
       raise. A double stands in for a lock by answering this. *)
    { op_name = "FS!lock"; op_effect = FsWrite;
      op_types = t path (TApp (TName "Option", path));
      op_performers = ["FS.lock"; "FS.lock!"] };
    (* The waiting acquire. Its own operation rather than a loop written in
       wand over `FS!lock` and `Clock.sleep`: a rehearsal withholds a sleep,
       so such a loop would spin against a wall-clock deadline for the whole
       budget. One operation is also what lets a rehearsal decline to wait.

       It carries `Clock` as well as the write, because it sleeps, so a
       script that waits for a lock says so in its manifest and one that
       does not, does not. *)
    { op_name = "FS!lock_wait"; op_effect = FsWrite;
      op_types = t (TTuple [path; TDuration]) (TApp (TName "Option", path));
      op_performers = ["FS.lock_wait"; "FS.lock_wait!"] };
    { op_name = "FS!unlock"; op_effect = FsWrite; op_types = t path TUnit;
      op_performers = ["FS.lock"; "FS.lock!"; "FS.lock_wait"; "FS.lock_wait!"] };
    (* Reaching a host. One operation, because the operation count is the
       mocking surface: four would be four ways for a test to believe it was
       sealed and not be. `HTTP.get`, `HTTP.post` and the rest are written
       in wand over this one, so one handler case covers the module.

       Named for the protocol rather than for the label, so a listening
       socket or a raw read is a second operation under the same label
       rather than an eleventh label. `Net` is the reach; the operation says
       how. *)
    { op_name = "Net!http"; op_effect = Net;
      op_types = t (TName "HTTPRequest") (TName "HTTPResponse");
      op_performers = ["HTTP.request"; "HTTP.get"; "HTTP.post";
                       "HTTP.download"; "HTTP.upload"] };
    (* Fetching to a file is its own operation, because a download must not
       become a value on the way past: a 2GB artifact held in a `String` is
       the thing `HTTP.download` exists to avoid, and one operation cannot
       both answer with a body and write one. Both are `Net`, and the
       doubles answer both, so a test that seals one seals the module. *)
    { op_name = "Net!download"; op_effect = Net;
      op_types = t (TTuple [TURL; TPath]) TUnit;
      op_performers = ["HTTP.download"; "HTTP.download!"] };
    (* The program's own streams. *)
    { op_name = "IO!print"; op_effect = IO;
      (* Printing takes whatever it is given. *)
      op_types = (fun () -> Some (fresh (), TUnit));
      op_performers = ["IO.print"] };
    { op_name = "IO!println"; op_effect = IO;
      op_types = (fun () -> Some (fresh (), TUnit));
      op_performers = ["IO.println"] };
    { op_name = "IO!print_err"; op_effect = IO;
      op_types = (fun () -> Some (fresh (), TUnit));
      op_performers = ["IO.print_err"] };
    { op_name = "IO!println_err"; op_effect = IO;
      op_types = (fun () -> Some (fresh (), TUnit));
      op_performers = ["IO.println_err"] };
    { op_name = "IO!read_line"; op_effect = IO; op_types = t TUnit str;
      op_performers = ["IO.read_line"; "IO.read_line!"] };
    { op_name = "IO!read_all"; op_effect = IO; op_types = t TUnit str;
      op_performers = ["IO.read_all"; "IO.read_all!"] };
    { op_name = "IO!flush"; op_effect = IO; op_types = t TUnit TUnit;
      op_performers = ["IO.flush"] };
    { op_name = "IO!stdin_lines"; op_effect = IO; op_types = t TUnit (TList str);
      op_performers = ["IO.stdin_lines"] };
    (* The environment. *)
    (* `Env.get` is `try get!`, so the operation supplies the `String` that
       `env_get_exn` returns and the `try` around it makes the `Option`. A
       handler that resumed with anything else used to typecheck, and
       `Env.get` then answered `Some 42`. *)
    { op_name = "Env!get"; op_effect = Env; op_types = t str str;
      op_performers = ["Env.get"; "Env.get!"] };
    { op_name = "Env!set"; op_effect = Env; op_types = t (TTuple [str; str]) TUnit;
      op_performers = ["Env.set"; "Env.load!"] };
    { op_name = "Env!clear"; op_effect = Env; op_types = t str TUnit;
      op_performers = ["Env.clear"] };
    { op_name = "Env!all"; op_effect = Env; op_types = t TUnit (TList (TTuple [str; str]));
      op_performers = ["Env.all"] };
    { op_name = "Env!home"; op_effect = Env; op_types = t TUnit path;
      op_performers = ["Env.home"] };
    { op_name = "Env!user"; op_effect = Env; op_types = t TUnit str;
      op_performers = ["Env.user"] };
    { op_name = "Env!read"; op_effect = Env; op_types = t str (TList (TTuple [str; str]));
      op_performers = ["Env.read!"] };
    (* Subprocesses. `Shell!run` and `Shell!capture` carry either a command,
       or a command and the stdin threaded into it, so there is no single
       payload type to give them. Declaring one would be worse than declaring
       none -- a handler would be told the shape was a String and handed a
       tuple whenever someone piped into the command. *)
    (* Making one. It carries the resolved command line and answers with the
       line to build the `Command` from, so a handler can read every command
       a file names -- including one it never runs -- and substitute it.
       Wrapping the answer is the literal's own job, in OCaml: there is no
       way in wand to turn a String into a `Command`, and that is what makes
       `Shell.run!` safe where a function over a String would not be. *)
    { op_name = "Shell!command"; op_effect = Shell; op_types = t str str;
      op_performers = ["$*(...)"] };
    (* Reading one as it goes. The payload is the command line and the
       answer is its output lines, so a mock supplies a list exactly as it
       does for `FS!stream_lines`. *)
    { op_name = "Shell!stream"; op_effect = Shell; op_types = t str (TList str);
      op_performers = ["Shell.stream"] };
    { op_name = "Shell!run"; op_effect = Shell; op_types = (fun () -> None);
      op_performers = ["$(...)"; "Shell.run"; "Shell.run!"] };
    { op_name = "Shell!capture"; op_effect = Shell; op_types = (fun () -> None);
      op_performers = ["$?(...)"; "Shell.query"] };
    (* These two have builtins behind them and are answered by `--dry-run`
       and `--trace`, but nothing a script can write reaches them: the
       builtins are not bound in a script's scope and no module exports
       them. A handler case for either is legal and will never fire. *)
    { op_name = "Shell!run_quiet"; op_effect = Shell; op_types = t str TUnit;
      op_performers = [] };
    { op_name = "Shell!exit_code"; op_effect = Shell; op_types = t str TInt;
      op_performers = [] };
    (* Ending the process. Nothing follows an exit, so a case that resumes
       one may say it resumes with anything. `Proc.args` is not here: argv
       is fixed before the program starts, so there is nothing to intercept
       and nothing to bound. *)
    { op_name = "Proc!exit"; op_effect = Proc;
      op_types = (fun () -> Some (TInt, fresh ()));
      op_performers = ["Proc.exit"] };
    (* Waiting. A handler that answers this one supplies a clock: it decides
       how long a sleep takes, which is what makes a deadline testable
       instead of slow. *)
    { op_name = "Clock!sleep"; op_effect = Clock;
      op_types = t TDuration TUnit;
      op_performers = ["Clock.sleep"] };
    (* Reading the clock. A handler that answers this one pins the instant,
       so a test of "older than thirty days" needs neither a real file nor a
       wait. *)
    { op_name = "Clock!now"; op_effect = Clock;
      op_types = t TUnit TDateTime;
      op_performers = ["Clock.now"] };
    (* The reading `Clock.timed` brackets work with. A handler that answers
       it decides what the work appears to have taken. *)
    { op_name = "Clock!timed"; op_effect = Clock;
      op_types = t TUnit TDuration;
      op_performers = ["Clock.timed"] };
    (* Drawing. A handler that answers these decides what a run draws, which
       is what makes a shuffle or a jittered backoff testable rather than
       merely unlikely to differ. `Random!int` carries its exclusive
       bound, so a handler can answer in range without knowing the caller. *)
    { op_name = "Random!int"; op_effect = Random;
      op_types = t TInt TInt;
      op_performers = ["Random.int"; "Random.choose"; "Random.shuffle";
                       "Random.hex"] };
    { op_name = "Random!float"; op_effect = Random;
      op_types = t TUnit TFloat;
      op_performers = ["Random.float"; "Random.chance?"] };
    (* Pinning the seed. It is an operation like the others so that a
       handler can refuse it: a test that pins a seed and a handler that
       answers every draw are two ways to the same place, and a handler
       that has taken over the draws should not be stepped around. *)
    { op_name = "Random!seed"; op_effect = Random;
      op_types = t TInt TUnit;
      op_performers = ["Random.seed"] };
  ]

let operation_index : (string, operation) Hashtbl.t = Hashtbl.create 64
let () = List.iter (fun o -> Hashtbl.replace operation_index o.op_name o) operations

let find_operation name = Hashtbl.find_opt operation_index name

(* Every operation, in table order: FS reads, FS writes, streams, the
   environment, subprocesses, exit. What an editor offers after `FS!`. *)
let operation_names () = List.map (fun o -> o.op_name) operations

(* The operations a named function performs, which is what a handler case
   over it would say. An effect set names the label -- `{FS.Read}` -- and a
   label covers several operations, so the signature alone does not tell you
   what to intercept. `wand d` prints this beside the type for that reason:
   the answer was only in the reference's table before, which is a document
   away from the question. *)
let operations_performed_by name =
  List.filter_map
    (fun o -> if List.mem name o.op_performers then Some o.op_name else None)
    operations

(* Which effect an intercepted operation accounts for. A handler case names
   the builtin operation it catches, and catching it is what removes the
   corresponding effect from the handled expression. *)
let effect_of_operation name =
  Option.map (fun o -> o.op_effect) (find_operation name)

let operation_types op : (typ * typ) option =
  match find_operation op with Some o -> o.op_types () | None -> None

(* ── Repr (follow unification links) ─────────────────────────────────────── *)

(* An alias name over what it names, with the name taken off. Everything
   that asks what a type *is* goes through here, so an alias needs no case
   of its own anywhere; only printing looks under the name. *)
let rec strip_alias t = match t with TAlias (_, _, u) -> strip_alias u | _ -> t

let rec repr t =
  match t with
  | TVar tv -> (match tv.def with
    | None    -> t
    | Some t' ->
      let t'' = repr t' in
      tv.def <- Some t'';
      t'')
  | TAlias (_, _, u) -> repr u
  | _ -> t

(* ── string_of_typ ────────────────────────────────────────────────────────── *)

(* Every effect variable in `t`, with repeats, so display can tell one that
   links two places apart from one that is merely undetermined. *)
let rec collect_evars t =
  match repr t with
  | TFun (a, b, r) ->
    collect_evars a @ collect_evars b @ Effect_set.free_vars r
  | TTuple ts   -> List.concat_map collect_evars ts
  | TList t     -> collect_evars t
  | TResult (e, t) -> collect_evars e @ collect_evars t
  | TResource (r, t) -> Effect_set.free_vars r @ collect_evars t
  | TStream (r, t) -> Effect_set.free_vars r @ collect_evars t
  | TDecoder t  -> collect_evars t
  | TMap t      -> collect_evars t
  | TApp (f, a) -> collect_evars f @ collect_evars a
  | TIface (_, args) -> List.concat_map collect_evars args
  | TModule claims -> List.concat_map (fun (_, a) -> List.concat_map collect_evars a) claims
  | _           -> []

(* Does this type need brackets where it stands as an argument? Anything
   that prints as an application does: without them `List (Map Int)` comes
   back as `List Map Int`, which reads as List given two arguments and is
   not the type -- nor, since arity is checked, a type at all.

   One predicate because there were six copies of the list and they had
   drifted apart. `TList` had lost `TMap`, and none of them had ever held
   `TDecoder`, `TStream` or `TResource`: `List (Decoder Int)` printed as
   `List Decoder Int`, and a stream inside a list printed
   `List Stream {FS.Read, Raise | ..} String`, where even the effect set
   reads as an argument. A tuple brings its own brackets and needs none. *)
let needs_brackets = function
  | TFun _ | TList _ | TResult _ | TMap _ | TApp _
  | TDecoder _ | TStream _ | TResource _ -> true
  | _ -> false

let string_of_typ t =
  let counter = ref 0 in
  let names : (int, string) Hashtbl.t = Hashtbl.create 4 in
  let name_of id =
    match Hashtbl.find_opt names id with
    | Some n -> n
    | None   ->
      (* Past 'z the names wrap and take a number rather than running on
         through the byte after 'z: 'a1, 'b1, and so on. Adding the counter
         to 'a printed punctuation from the 27th variable on -- `'{`, then
         bytes that are not characters at all -- and raised `Char.chr` on
         the 159th, so a type wide enough turned `wand t` into a backtrace.
         A type that wide is one line of wand: a function of 159 arguments,
         or a tuple of 159 holes. Found by test/fuzz. *)
      let n =
        let letter = Char.chr (Char.code 'a' + !counter mod 26) in
        let wrap = !counter / 26 in
        if wrap = 0 then Printf.sprintf "'%c" letter
        else Printf.sprintf "'%c%d" letter wrap
      in
      incr counter; Hashtbl.add names id n; n
  in
  let linking_sets = collect_evars t in
  let var_names : (int, string) Hashtbl.t = Hashtbl.create 4 in
  let var_counter = ref 0 in
  let var_name_of rid =
    match Hashtbl.find_opt var_names rid with
    | Some n -> n
    | None   ->
      (* Written like a type variable, because that is what it is: one that
         ranges over effects rather than types. *)
      let n = if !var_counter = 0 then "'e"
              else Printf.sprintf "'e%d" !var_counter in
      incr var_counter; Hashtbl.add var_names rid n; n
  in
  (* Left to right, because a constraint is written where its variable is
     first met and `List.map` does not say which end it starts from. *)
  let map_lr f xs = List.rev (List.fold_left (fun acc x -> f x :: acc) [] xs) in
  let annotated : (int, unit) Hashtbl.t = Hashtbl.create 4 in
  (* A variable about to show its constraint is two words, so it takes
     brackets in an argument position: `List ('a: Ord)`, not `List 'a: Ord`,
     which is not a type anyone could write back. *)
  let shows_a_constraint t =
    match repr t with
    | TVar tv -> tv.constrained <> Free && not (Hashtbl.mem annotated tv.id)
    | _ -> false
  in
  let wants_brackets t = needs_brackets t || shows_a_constraint t in
  let rec go t =
    (* The one place an alias is looked at rather than through. It shows
       with what it names, so the name the reader wrote is in the message
       and no message can claim two identical types differ. *)
    match t with
    | TAlias (n, args, u) ->
      let applied =
        if args = [] then n
        else n ^ " " ^ String.concat " " (map_lr go args) in
      Printf.sprintf "%s (= %s)" applied (go u)
    | _ ->
    match repr t with
    | TAlias (_, _, u) -> go u   (* `repr` strips it; here for the compiler *)
    | TInt      -> "Int"      | TFloat    -> "Float"    | TString   -> "String"
    | TBool     -> "Bool"     | TUnit     -> "Unit"
    | TPath     -> "Path"     | TGlob     -> "Glob"
    | TDateTime -> "DateTime" | TDuration -> "Duration"
    | TURL      -> "URL"      | TIPv4     -> "IPv4"     | TCIDR     -> "CIDR"
    | TPort     -> "Port"     | TVersion  -> "Version"  | TSize     -> "Size"
    | TRegex    -> "Regex"
    | TCommand  -> "Command"
    | TJson     -> "JSON"
    | TToml     -> "TOML"
    | TYaml     -> "YAML"
    (* A type carries the module that declares it. A reader wrote the short
       name, so that is what a message shows. *)
    | TName n   ->
      (match String.rindex_opt n '#' with
       | Some i -> String.sub n (i + 1) (String.length n - i - 1)
       | None -> n)
    (* A constrained variable is named like any other, and carries its
       constraint where it is first met: `'a: Ord -> 'a -> 'a` says the three
       are one type, which `Ord -> Ord -> Ord` could not, and says it about
       each variable separately, which is what two of them need. *)
    | TVar tv   ->
      let constraint_name = match tv.constrained with
        | Num  -> Some "Num"
        | Add  -> Some "Add"
        | Ord  -> Some "Ord"
        | Free -> None
      in
      (match constraint_name with
       | None -> name_of tv.id
       | Some c ->
         let n = name_of tv.id in
         if Hashtbl.mem annotated tv.id then n
         else (Hashtbl.add annotated tv.id (); n ^ ": " ^ c))
    | TFun (a, b, eff) ->
      (* A set prints when it says something. Known effects always do. A set
         variable does only when it appears more than once, because then it
         is linking argument to result -- `List.map`'s says the list is
         processed with whatever effects the given function has. A variable
         appearing once means "undetermined", which is not information. *)
      let labels = Effect_set.labels_of eff in
      let var_name =
        match Effect_set.free_vars eff with
        | [rid] when List.length (List.filter (( = ) rid) linking_sets) > 1 ->
          Some (var_name_of rid)
        | _ -> None
      in
      let names =
        String.concat ", "
          (List.map Effect_set.name_of (Effect_set.EffSet.elements labels))
      in
      let suffix =
        match Effect_set.EffSet.is_empty labels, var_name with
        | true,  None   -> ""
        | true,  Some v -> " ! " ^ v
        (* An unnamed tail is one that appears once: undetermined, and so
           not worth printing beside the effects that are known. *)
        | false, None   -> " ! {" ^ names ^ "}"
        | false, Some v -> " ! {" ^ names ^ " | " ^ v ^ "}"
      in
      let sa = match repr a with TFun _ -> "(" ^ go a ^ ")" | _ -> go a in
      let rendered_b = go b in
      (* Each arrow of a curried function has its own effects, but partial
         application ties them to the same one, so a chain would otherwise
         repeat itself: `a -> b -> c ! e ! e`. Print it once. *)
      let ends_with s suf =
        let n = String.length s and m = String.length suf in
        m <= n && String.sub s (n - m) m = suf
      in
      let suffix = if suffix <> "" && ends_with rendered_b suffix then "" else suffix in
      sa ^ " -> " ^ rendered_b ^ suffix
    | TTuple ts ->
      "(" ^ String.concat ", " (map_lr go ts) ^ ")"
    | TList t ->
      let s = if wants_brackets (repr t) then "(" ^ go t ^ ")" else go t in
      "List " ^ s
    | TResult (e, t) ->
      let wrap x = if wants_brackets (repr x) then "(" ^ go x ^ ")" else go x in
      "Result " ^ wrap e ^ " " ^ wrap t
    | TResource (r, t) ->
      let wrap x = if wants_brackets (repr x) then "(" ^ go x ^ ")" else go x in
      "Resource " ^ Effect_set.to_string r ^ " " ^ wrap t
    | TStream (r, t) ->
      let wrap x = if wants_brackets (repr x) then "(" ^ go x ^ ")" else go x in
      "Stream " ^ Effect_set.to_string r ^ " " ^ wrap t
    | TDecoder t ->
      let s = if wants_brackets (repr t) then "(" ^ go t ^ ")" else go t in
      "Decoder " ^ s
    | TMap t ->
      let s = if wants_brackets (repr t) then "(" ^ go t ^ ")" else go t in
      "Map " ^ s
    | TApp (f, a) ->
      let sa = if wants_brackets (repr a) then "(" ^ go a ^ ")" else go a in
      go f ^ " " ^ sa
    (* `Ord Int` -- the interface and what its parameters were bound to,
       written the way the annotation is written. *)
    | TIface (n, []) -> n
    | TIface (n, args) ->
      n ^ " " ^ String.concat " "
        (List.map (fun a ->
           if wants_brackets (repr a) then "(" ^ go a ^ ")" else go a) args)
    (* A module has no spelling in a signature -- an interface is what a
       parameter is annotated with -- so it prints as what it is, and says
       what it answers to. *)
    | TModule [] -> "a module implementing nothing"
    | TModule claims ->
      "a module implementing "
      ^ String.concat ", "
          (List.map (fun (n, args) ->
             if args = [] then n
             else n ^ " " ^ String.concat " " (List.map go args)) claims)
  in
  go t

(* ── Occurs check ─────────────────────────────────────────────────────────── *)

let rec occurs (tv : tv) t =
  match repr t with
  | TVar tv'    -> tv' == tv
  | TFun (a, b, _) -> occurs tv a || occurs tv b
  | TTuple ts   -> List.exists (occurs tv) ts
  | TList t     -> occurs tv t
  | TResult (e, t) -> occurs tv e || occurs tv t
  | TResource (_, t) -> occurs tv t
  | TStream (_, t) -> occurs tv t
  | TDecoder t  -> occurs tv t
  | TMap t      -> occurs tv t
  | TApp (f, a) -> occurs tv f || occurs tv a
  | TIface (_, args) -> List.exists (occurs tv) args
  | TModule claims -> List.exists (fun (_, a) -> List.exists (occurs tv) a) claims
  | _           -> false

(* ── Unification ──────────────────────────────────────────────────────────── *)

(* When an error can be corrected mechanically, the correction rides beside
   the exception rather than only inside its prose -- set just before the
   raise, collected by the entry points into the structured error, applied by
   `wand t --fix` and offered by the editor. A manifest line and a missing
   import are both this. Never set for widening to bare `Shell`: erasing a
   narrowing the author wrote is not a correction a tool should make on its
   own. *)
let pending_fix : Diag.fix option ref = ref None

let take_pending_fix () =
  let f = !pending_fix in
  pending_fix := None;
  f

exception TypeError of string

(* The located form. `TypeError` is raised where only the fact is known;
   the nearest enclosing `Located` promotes it to `TypeErrorAt`, which then
   passes through outer handlers untouched -- so the position that wins is
   the innermost one, and it travels as data rather than as a "3:5: "
   prefix spliced into the message. *)
exception TypeErrorAt of Token.loc * string

(* Raise at a location when there is one. A declaration checked after the
   file is read has no expression to blame, so it carries the location it was
   written at; a built-in definition has none, and falls back to the
   unlocated form. *)
let fail_at_opt (loc : Token.loc option) msg =
  match loc with
  | Some l -> raise (TypeErrorAt (l, msg))
  | None   -> raise (TypeError msg)

let loc_of_expr (e : expr) : Token.loc option =
  match e with Located (l, _) -> Some l | _ -> None

(* The two types that did not fit, raised by the one case below that has
   nothing better to say. `unify` turns it into a message: which of the two
   the reader expected is known at the call site, not here. Every other
   failure in this function already knows what to say and says it. *)
exception Mismatch of typ * typ

let rec unify_ t1 t2 =
  match repr t1, repr t2 with
  | TInt,      TInt      | TFloat,    TFloat    | TString,  TString
  | TBool,     TBool     | TUnit,     TUnit
  | TPath,     TPath     | TGlob,     TGlob
  | TDateTime, TDateTime | TDuration, TDuration
  | TURL,      TURL      | TIPv4,     TIPv4     | TCIDR,    TCIDR
  | TPort,     TPort     | TVersion,  TVersion  | TSize,    TSize  -> ()
  | TRegex,    TRegex    -> ()
  | TJson,     TJson     -> ()
  | TToml,     TToml     -> ()
  | TYaml,     TYaml     -> ()
  | TCommand,  TCommand  -> ()
  | TName n1, TName n2 when n1 = n2 -> ()
  | TVar tv1, TVar tv2 when tv1 == tv2 -> ()
  (* Two variables: link so that the narrower constraint survives on
     whichever remains undefined. *)
  | TVar tv1, TVar tv2 ->
    let keep = narrower tv1.constrained tv2.constrained in
    if keep = tv1.constrained then tv2.def <- Some (TVar tv1)
    else if keep = tv2.constrained then tv1.def <- Some (TVar tv2)
    else begin
      (* Neither side already carries it, so a third variable does. *)
      let v = fresh_as keep in
      tv1.def <- Some v; tv2.def <- Some v
    end
  | TVar tv, t | t, TVar tv ->
    (* The value would have to hold itself: `x x`, or a list that is its
       own element. There is no such type, and naming the two sides does not
       help, because one of them is inside the other. *)
    if occurs tv t then
      raise (TypeError (Printf.sprintf
        "this value would have to contain itself: %s appears inside its own \
         type" (string_of_typ (TVar tv))))
    else if (tv.constrained = Num || tv.constrained = Add) && t = TString then
      raise (TypeError "strings concatenate with '++', not '+'")
    else if tv.constrained = Num && t <> TInt && t <> TFloat then
      raise (TypeError (Printf.sprintf
        "expected a number, got %s -- * and / work on Int and Float"
        (string_of_typ t)))
    else if tv.constrained = Add && not (is_additive t) then
      raise (TypeError (Printf.sprintf
        "%s does not add -- + and - work on Int, Float, Size and Duration"
        (string_of_typ t)))
    (* The ordered set grows as types gain a normalizer, so the message
       names the type that is not in it rather than listing the set. The
       reference has the list. *)
    else if tv.constrained = Ord && not (is_ordered t) then
      raise (TypeError (Printf.sprintf
        "%s is not ordered, so it cannot be compared with < > <= >="
        (string_of_typ t)))
    else tv.def <- Some t
  (* The two members of Num, mixed: name the conversions rather than only
     the mismatch, since this is the first error a Float-using script
     meets. *)
  | (TInt, TFloat) | (TFloat, TInt) ->
    raise (TypeError
      "Int and Float do not mix -- Float.of_int and Float.round \
       convert between them")
  | TFun (a1, res1, eff1), TFun (a2, res2, eff2) ->
    unify_ a1 a2; unify_ res1 res2;
    (* Two functions are the same only if calling them does the same. A
       conflict in effects is a type error like any other. *)
    Effect_set.unify eff1 eff2
  | TTuple ts1, TTuple ts2 when List.length ts1 = List.length ts2 ->
    List.iter2 unify_ ts1 ts2
  | TList t1,   TList t2   -> unify_ t1 t2
  | TResult (e1, t1), TResult (e2, t2) -> unify_ e1 e2; unify_ t1 t2
  | TResource (r1, t1), TResource (r2, t2) ->
    Effect_set.unify r1 r2; unify_ t1 t2
  | TStream (r1, t1), TStream (r2, t2) ->
    Effect_set.unify r1 r2;
    unify_ t1 t2
  | TDecoder t1, TDecoder t2 -> unify_ t1 t2
  | TMap t1,    TMap t2    -> unify_ t1 t2
  | TApp (f1, a1), TApp (f2, a2) -> unify_ f1 f2; unify_ a1 a2
  | TIface (n1, a1), TIface (n2, a2)
    when n1 = n2 && List.length a1 = List.length a2 ->
    List.iter2 unify_ a1 a2
  (* A module meeting an interface. Conformance is nominal, so this asks
     what the module claimed rather than what members it happens to have:
     `implement` is the whole of what makes a module fit. *)
  | (TModule claims, TIface (n, args)) | (TIface (n, args), TModule claims) ->
    let matching =
      List.filter (fun (cn, cargs) ->
        cn = n && List.length cargs = List.length args) claims
    in
    (match matching with
     | [] ->
       raise (TypeError (Printf.sprintf
         "this module does not implement %s, which it would have to declare \
          with 'implement %s %s' in its own file%s"
         n n
         (String.concat " " (List.map string_of_typ args))
         (if claims = [] then ""
          else Printf.sprintf ". It implements %s"
                 (String.concat ", " (List.map fst claims)))))
     | (_, cargs) :: _ -> List.iter2 unify_ cargs args)
  | TModule c1, TModule c2 when c1 == c2 -> ()
  | t1, t2 -> raise (Mismatch (t1, t2))

(* What a failed unification says.

   `unify a b` has no fixed direction -- both orders appear at call sites --
   so on its own it can only name the two types and leave the reader to work
   out which one is theirs. Where the call site does know, it says so with
   `unify_expected`, and the message reads `expected Glob, got Path`. The
   orientation survives the recursion: the left type stays on the left all
   the way down, so the innermost pair is oriented like the outermost, and
   the message names the two types that actually differ rather than the two
   whole types they sit in.

   The word "unify" belongs to the algorithm, not to the script, so it
   appears in no message. *)
let effects_conflict_message ?expected ?got allowed found =
  let extra = Effect_set.extra ~allowed ~found in
  match expected, got, extra with
  | Some e, Some g, (_ :: _ as labels) ->
    Printf.sprintf "%s allows %s, but %s performs %s"
      e (Effect_set.to_string allowed) g (String.concat ", " labels)
  | Some e, Some g, [] ->
    Printf.sprintf "%s allows %s, and %s performs %s"
      e (Effect_set.to_string allowed) g (Effect_set.to_string found)
  | _ ->
    Printf.sprintf "these effects do not match: %s and %s"
      (Effect_set.to_string allowed) (Effect_set.to_string found)

(* `<stdlib>/Test.wand#Testing` reads as `Test.Testing`: the module a reader
   would name it through, not the path a loader found it at. Used only where
   two types would otherwise print the same. *)
let qualified_display t =
  match repr t with
  | TName n ->
    (match String.rindex_opt n '#' with
     | None -> n
     | Some i ->
       let path = String.sub n 0 i in
       let short = String.sub n (i + 1) (String.length n - i - 1) in
       Filename.remove_extension (Filename.basename path) ^ "." ^ short)
  | _ -> string_of_typ t

(* Two types that print the same, told apart by the module each came from. *)
let disambiguate a b =
  let (sa, sb) = (string_of_typ a, string_of_typ b) in
  if sa = sb then (qualified_display a, qualified_display b) else (sa, sb)

let unify t1 t2 =
  try unify_ t1 t2 with
  | Mismatch (a, b) ->
    let (sa, sb) = disambiguate a b in
    raise (TypeError (Printf.sprintf "%s and %s are not the same type" sa sb))
  | Effect_set.Conflict (a, b) ->
    raise (TypeError (effects_conflict_message a b))

(* For a site that knows which side the reader wrote: an annotation, a
   condition, an argument, an arm of a `match`. *)
let unify_expected ~expected ~got =
  try unify_ expected got with
  | Mismatch (a, b) ->
    let (sa, sb) = disambiguate a b in
    raise (TypeError (Printf.sprintf "expected %s, got %s" sa sb))
  | Effect_set.Conflict (a, b) ->
    raise (TypeError (effects_conflict_message ~expected:"the type"
                        ~got:"the body" a b))

(* ── Schemes and environment ──────────────────────────────────────────────── *)

type scheme =
  | Mono of typ
  | Poly of int list * int list * typ   (* type vars, effect vars, body *)
  (* A module: what it holds, and what it claims to implement. The claims
     are beside the members rather than among them -- a claim is not
     something a file can select, and every listing of a module's surface
     would otherwise have to remember to leave it out. *)
  | Namespace of env * (string * typ list) list

and env = (string * scheme) list

(* A module's claims ride in its own environment, under a name no file can
   write -- the space is what puts it out of reach -- so an import brings
   them along and a cached module keeps them, with no table to hold
   consistent. `split_claims` lifts them out where the module becomes a
   namespace, which is why no listing has to know about them. *)
let implements_key = "implements claims"

(* Names a reader of another language reaches for. wand has no training-data
   presence, so a model writing it drifts toward OCaml, Python, Ruby, and
   bash; an unbound-variable error that names the wand spelling is what
   makes the edit-typecheck loop converge instead of circle. *)
let foreign_name_hint = function
  | "not" -> Some "boolean not is '!'"
  | "ref" | "mutable" ->
    Some "there is no mutation; let binds a new name instead"
  | "raise" | "throw" ->
    Some "errors are values: return an Error, call a !-suffixed \
          function to raise, or wrap a call with try"
  | "print" | "println" | "printf" | "puts" | "print_endline"
  | "print_string" | "print_newline" | "print_int" | "print_float"
  | "console" ->
    Some "printing is IO.println (import IO)"
  | "echo" ->
    Some "IO.println prints a line (import IO); $(echo ...) runs the command"
  | "lambda" -> Some "a lambda is 'fn x -> ...'"
  | "elif" -> Some "write 'else if'"
  | "len" -> Some "List.length and String.length measure things"
  | "nil" | "null" -> Some "absence is None, matched with 'match ... with'"
  | "is" -> Some "comparison is '=='; a missing value is matched: \
                  'match x with | None -> ...'"
  | "begin" -> Some "expressions group with parentheses, not \
                     'begin ... end'"
  | "int_of_string" -> Some "String.to_int reads an Int out of a String \
                             (as a Result); String.of_int goes the other way"
  | "string_of_int" | "string_of_float" ->
    Some "interpolation makes strings of anything: \"%{n}\""
  | "read_lines" -> Some "FS.read_file! reads a whole file, and \
                          String.lines splits it (import FS and String)"
  | "list_dir" | "readdir" | "read_dir" ->
    Some "directories are read with FS.list_dir! (import FS)"
  | _ -> None

(* The same idea one level down: a member looked up on the right module by
   its name in another language's standard library. Checked before the
   edit-distance guess, which can mislead here -- List.iter's nearest
   neighbour by spelling is 'filter', but its meaning is 'each'. *)
let foreign_member_hint ns member =
  match ns, member with
  | "List", "iter" -> Some "use List.each"
  | "List", "iteri" -> Some "use List.each, with List.indexed for positions"
  | "String", "split_on_char" -> Some "use String.split -- the separator is a String"
  | "String", "sub" -> Some "use String.slice"
  | "FS", "read_lines" -> Some "FS.read_file! reads the whole file; \
                                String.lines splits it"
  (* `Ord` was a module of four functions until 0.79.0, and is a module
     declaring an interface again from 0.80.0. A script written against the
     old one names a member that is not there, and the answer is the same as
     it was: the comparisons sit on the type. *)
  | "Ord", ("max" | "min" | "clamp" | "between?") ->
    Some "the comparisons sit on each ordered type; write 'Int.max', \
          'Duration.min', 'Path.between?' etc. `Ord` declares the interface \
          they answer to"
  | "Shell", ("run" | "run!" | "exec" | "exec!") ->
    Some "commands run with $(...); Shell only reads their output \
          (Shell.decode, Shell.lines)"
  | _ -> None

(* Named in unbound-name errors that have nothing better to offer: the
   binary can enumerate what exists, and a reader who has never seen wand
   has no other way to learn that. *)
let discovery_hint =
  " -- 'wand d' lists the modules, 'wand d List' one module's members"

let unbound_message name (env : env) =
  let hint = match foreign_name_hint name with
    | Some h -> " -- " ^ h
    | None ->
      (match Util.hint name (List.map fst env) with
       | "" -> discovery_hint
       | h -> h)
  in
  Printf.sprintf "unbound variable '%s'%s" name hint

let lookup name (env : env) =
  match List.assoc_opt name env with
  | Some s -> s
  | None   -> raise (TypeError (unbound_message name env))

(* Every unbound name the check met, in the order it met them, each once.
   A file with six of them took six runs to clear -- a person reread it
   between each, and a tool driving `wand t` in a loop paid a round trip.

   This is the one error a check can carry on past. The name is given a
   fresh variable, which unifies with anything, so nothing below reports a
   consequence of the miss as a mistake of its own. Every other error still
   stops where it stood: a type that did not fit leaves the wrong type
   behind, and going on from one invents mistakes the file does not have. *)
let unbound_names : (Token.loc option * string) list ref = ref []
let unbound_seen : (string, unit) Hashtbl.t = Hashtbl.create 8

(* Where the check is, for the errors it records rather than raises. A
   raised one is located by the `Located` it passes on the way out; a
   recorded one never leaves, so it reads the position here. *)
let cur_loc : Token.loc option ref = ref None

let forget_unbound () =
  unbound_names := [];
  Hashtbl.reset unbound_seen;
  cur_loc := None

(* Raise the first name the check went past, so every caller sees the one
   error it has always seen. `infer_program_full_with_own` reads the rest off
   `unbound_names` and reports them alongside; nothing else has to change to
   stay correct. *)
let raise_first_unbound () =
  match !unbound_names with
  | [] -> ()
  | (Some loc, msg) :: _ -> raise (TypeErrorAt (loc, msg))
  | (None, msg) :: _ -> raise (TypeError msg)

(* ── Free type variables ──────────────────────────────────────────────────── *)

let rec free_tvars t =
  match repr t with
  | TVar tv     -> [tv.id]
  | TFun (a, b, _) -> free_tvars a @ free_tvars b
  | TTuple ts   -> List.concat_map free_tvars ts
  | TList t     -> free_tvars t
  | TResult (e, t) -> free_tvars e @ free_tvars t
  | TResource (_, t) -> free_tvars t
  | TStream (_, t) -> free_tvars t
  | TDecoder t  -> free_tvars t
  | TMap t      -> free_tvars t
  | TApp (f, a) -> free_tvars f @ free_tvars a
  | TIface (_, args) -> List.concat_map free_tvars args
  | TModule claims -> List.concat_map (fun (_, a) -> List.concat_map free_tvars a) claims
  | _           -> []

let rec free_evars_typ t =
  match repr t with
  | TFun (a, b, r) ->
    free_evars_typ a @ free_evars_typ b @ Effect_set.free_vars r
  | TTuple ts   -> List.concat_map free_evars_typ ts
  | TList t     -> free_evars_typ t
  | TResult (e, t) -> free_evars_typ e @ free_evars_typ t
  | TResource (r, t) -> Effect_set.free_vars r @ free_evars_typ t
  | TStream (r, t) -> Effect_set.free_vars r @ free_evars_typ t
  | TDecoder t  -> free_evars_typ t
  | TMap t      -> free_evars_typ t
  | TApp (f, a) -> free_evars_typ f @ free_evars_typ a
  | TIface (_, args) -> List.concat_map free_evars_typ args
  | TModule claims ->
    List.concat_map (fun (_, a) -> List.concat_map free_evars_typ a) claims
  | _           -> []

let free_tvars_scheme = function
  | Mono t           -> free_tvars t
  | Poly (ids, _, t) -> List.filter (fun id -> not (List.mem id ids)) (free_tvars t)
  | Namespace _      -> []

let free_evars_scheme = function
  | Mono t            -> free_evars_typ t
  | Poly (_, evar_ids, t) -> List.filter (fun id -> not (List.mem id evar_ids)) (free_evars_typ t)
  | Namespace _       -> []

let free_tvars_env (env : env) =
  List.concat_map (fun (_, s) -> free_tvars_scheme s) env

let free_evars_env (env : env) =
  List.concat_map (fun (_, s) -> free_evars_scheme s) env

(* ── Generalization and instantiation ────────────────────────────────────── *)

let generalize (env : env) t =
  let env_free = free_tvars_env env in
  let quantify =
    free_tvars t
    |> List.sort_uniq compare
    |> List.filter (fun id -> not (List.mem id env_free))
  in
  let env_free_evars = free_evars_env env in
  let quantify_evars =
    free_evars_typ t
    |> List.sort_uniq compare
    |> List.filter (fun id -> not (List.mem id env_free_evars))
  in
  if quantify = [] && quantify_evars = [] then Mono t
  else Poly (quantify, quantify_evars, t)

(* ── Reading a scheme back from a cache ───────────────────────────────────
   A scheme carries unification variables, and `instantiate` tells them apart
   by their integer id. Those ids were issued by whichever process wrote the
   scheme down, so two entries read back could each hold a variable numbered
   7 and be treated as the same variable the moment they meet.

   Everything read back is therefore renumbered into ids this process has
   issued. Sharing is preserved within an entry -- two occurrences of the
   same variable stay the same variable -- because ids are consistent inside
   a single entry even when they collide across entries. *)
let refresh_scheme (sch : scheme) : scheme =
  let tmap : (int, typ) Hashtbl.t = Hashtbl.create 16 in
  let evar_map : (int, Effect_set.evar) Hashtbl.t = Hashtbl.create 16 in
  let tvar_for (tv : tv) =
    match Hashtbl.find_opt tmap tv.id with
    | Some t -> t
    | None ->
      let t = fresh_as tv.constrained in
      Hashtbl.add tmap tv.id t; t
  in
  let var_for (v : Effect_set.evar) =
    match Hashtbl.find_opt evar_map v.Effect_set.id with
    | Some v' -> v'
    | None ->
      let v' = Effect_set.fresh_var () in
      Hashtbl.add evar_map v.Effect_set.id v'; v'
  in
  let effects r =
    let (Effect_set.Set (labels, tail)) = Effect_set.repr r in
    match tail with
    | None -> Effect_set.Set (labels, None)
    | Some v -> Effect_set.Set (labels, Some (var_for v))
  in
  let rec go t =
    match repr t with
    | TVar tv -> tvar_for tv
    | TFun (a, b, r) -> TFun (go a, go b, effects r)
    | TTuple ts -> TTuple (List.map go ts)
    | TList t -> TList (go t)
    | TResult (e, t) -> TResult (go e, go t)
    | TResource (r, t) -> TResource (effects r, go t)
    | TStream (r, t) -> TStream (effects r, go t)
    | TDecoder t -> TDecoder (go t)
    | TMap t -> TMap (go t)
    | TApp (f, a) -> TApp (go f, go a)
    | TIface (n, args) -> TIface (n, List.map go args)
    | TModule claims -> TModule (List.map (fun (n, a) -> (n, List.map go a)) claims)
    | other -> other
  in
  let id_of t = match repr t with TVar tv -> Some tv.id | _ -> None in
  match sch with
  | Namespace _ -> sch
  | Mono t -> Mono (go t)
  | Poly (ids, evar_ids, t) ->
    let body = go t in
    let ids' = List.filter_map (fun id ->
      (* A quantified id was replaced while walking the body; one that
         never appears in the body constrains nothing, so a plain fresh
         variable stands in for it. *)
      match Hashtbl.find_opt tmap id with
      | Some t -> id_of t
      | None ->
        let t = fresh () in
        Hashtbl.add tmap id t; id_of t) ids in
    let evar_ids' =
      List.map (fun rid ->
        match Hashtbl.find_opt evar_map rid with
        | Some v -> v.Effect_set.id
        | None ->
          let v = Effect_set.fresh_var () in
          Hashtbl.add evar_map rid v; v.Effect_set.id) evar_ids
    in
    Poly (ids', evar_ids', body)

let instantiate = function
  (* A module used as a value. What it is, for typing purposes, is what it
     says it implements: that is the only question anything asks of it, since
     a parameter is annotated with an interface and never with a module. A
     module claiming nothing is a value nothing accepts. *)
  | Namespace (_, claims) -> TModule claims
  | Mono t -> t
  | Poly (ids, evar_ids, t) ->
    (* Replacements are made per variable rather than precomputed per id,
       so a constrained variable's replacement carries the same constraint
       -- the record being replaced is what knows. *)
    let tbl : (int, typ) Hashtbl.t = Hashtbl.create 8 in
    let subst_for (tv : tv) =
      match Hashtbl.find_opt tbl tv.id with
      | Some t' -> t'
      | None ->
        let t' = fresh_as tv.constrained in
        Hashtbl.add tbl tv.id t'; t'
    in
    let evar_subst = List.map (fun id -> (id, Effect_set.fresh_var ())) evar_ids in
    let rec inst t =
      match repr t with
      | TVar tv ->
        if List.mem tv.id ids then subst_for tv else TVar tv
      | TFun (a, b, r) -> TFun (inst a, inst b, Effect_set.subst evar_subst r)
      | TTuple ts   -> TTuple (List.map inst ts)
      | TList t     -> TList (inst t)
      | TResult (e, t) -> TResult (inst e, inst t)
      | TResource (r, t) -> TResource (Effect_set.subst evar_subst r, inst t)
      | TStream (r, t) -> TStream (Effect_set.subst evar_subst r, inst t)
      | TDecoder t  -> TDecoder (inst t)
      | TMap t      -> TMap (inst t)
      | TApp (f, a) -> TApp (inst f, inst a)
      (* An interface's arguments are types like any other, and a module's
         claims carry types too. Left out of the walk, a signature written
         `Ord 'a -> 'a -> 'a` came back as `Ord 'a -> 'b -> 'b`: the
         argument kept the quantified variable while the rest was
         refreshed. *)
      | TIface (n, args) -> TIface (n, List.map inst args)
      | TModule claims ->
        TModule (List.map (fun (n, a) -> (n, List.map inst a)) claims)
      | t           -> t
    in
    inst t

(* ── Type definitions ─────────────────────────────────────────────────────── *)

type typedef_env = (string * type_def) list

(* `bound` fixes what a type's parameters stand for, so a field of an applied
   generic type comes back with the arguments substituted in: the `v` of a
   `Box Int` is an `Int`, not a fresh variable. Anything unbound gets one,
   which is what a standalone type expression needs. *)
(* The type names in scope for the file being checked. A name that is not
   one of them used to become an opaque `TName` that nothing could construct
   or unify with, so `type Pod (status : Status)` with no `Status` anywhere
   was accepted, and a misspelt `Itn` was a type in its own right. The
   mistake surfaced later as "expected Int, got Status", pointing at the
   use rather than at the declaration that invented the name.

   None outside a program: `type_of_te` is also called where there is no
   file to draw a set from, and refusing every name there would be wrong. *)
let known_type_names : string list option ref = ref None

(* The aliases in scope, by name. Set alongside `known_type_names` and for
   the same reason: `type_of_te` runs where there is no file to draw them
   from, and an empty table there simply resolves nothing. *)
let known_aliases : (string * (string list * Ast.type_expr)) list ref = ref []

(* How many type arguments each declared type takes, keyed as the file
   writes it. Aliases carry theirs in `known_aliases` and are checked where
   they are applied; this is every other declared type. Empty where there is
   no file, in which case a declared name's arity is simply not known and
   only the builtins above are checked. *)
let known_type_arities : (string * int) list ref = ref []

(* The names `type_of_te_bound` resolves without consulting a file: the
   primitives, and the four that only ever appear applied to an argument. *)
let builtin_type_names =
  [ "Int"; "Float"; "String"; "Bool"; "Unit"; "Path"; "Glob";
    "DateTime"; "Duration"; "URL"; "IPv4"; "CIDR";
    "Port"; "Version"; "Size"; "JSON"; "TOML"; "YAML"; "Command";
    "List"; "Map"; "Result"; "Option"; "Decoder" ]

let builtin_type_name n = List.mem n builtin_type_names

(* How many type arguments a builtin takes. A name that takes none is not a
   type constructor at all, so `Int Int` is the same mistake as `Map String
   String` and gets the same answer. `Num`, `Add` and `Ord` are written in
   signatures without being declared anywhere, so they are named here too. *)
let builtin_type_arity = function
  | "List" | "Map" | "Option" | "Decoder" -> Some 1
  | "Result"                              -> Some 2
  | "Num" | "Add" | "Ord"                 -> Some 0
  | n when builtin_type_name n            -> Some 0
  | _                                     -> None

(* What a type name written in a file means. A type declared in a module is
   keyed by the module as well as by its name, so two modules that each
   declare `Status` declare two types. A file writes a short name -- its own,
   one it selected from an import, or `Foo.Status` -- and this says which
   canonical name that is.

   Set around inference alongside `known_type_names`, and empty where there
   is no file, in which case a name means itself. *)
let type_name_map : (string * string) list ref = ref []

let canonical_type_name n =
  match List.assoc_opt n !type_name_map with
  | Some c -> c
  | None -> n

(* The short name a canonical one was declared under, for a message. *)
let short_type_name n =
  match String.rindex_opt n '#' with
  | Some i -> String.sub n (i + 1) (String.length n - i - 1)
  | None -> n

let with_type_name_map m f =
  let saved = !type_name_map in
  type_name_map := m;
  Fun.protect ~finally:(fun () -> type_name_map := saved) f

let with_known_type_names names f =
  let saved = !known_type_names in
  known_type_names := Some names;
  Fun.protect ~finally:(fun () -> known_type_names := saved) f

(* An effect name as a manifest or a signature writes it. *)
let eff_of_written name =
  match Effect_set.of_name name with
  | Some e -> e
  | None ->
    raise (TypeError (Printf.sprintf "unknown effect '%s'%s" name
      (Util.hint name (List.map Effect_set.name_of Effect_set.all))))

(* Which effect variables a written type named. They are the ones safe to
   generalise: a written `'e` says how the effects of one part of a type
   relate to another, and instantiating it afresh per use is what makes that
   relationship hold. An effect variable nobody wrote is inference's own, and
   generalising it throws away what inference learned -- see `ctor_schemes`. *)
let written_evars : int list ref = ref []

(* A written type applied to the wrong number of arguments.

   `Map String String` was built as an application of an already-saturated
   `Map`, which is not a type at all. It was accepted, and then failed
   wherever it was met -- a field default reported "expected Map String
   String, got Map 'a", blaming the default, which was correct. `List Int
   Int`, `Option Int Int`, a bare `Map`, and a declared `Box 'a` given two
   arguments or none went the same way.

   An alias is checked where it is applied, in `apply_alias`. This is that
   check for the builtins and for declared types, run over the written type
   before it is read, so the error names the type that is wrong rather than
   the value that later met it. A name whose arity is unknown -- a type
   variable in head position, or any name where there is no file to declare
   it -- is left alone: this reports what it is sure of. *)
(* Every interface in scope, by the name a file writes for it. A contract
   declares no value, so this is the whole of what an interface is: a list of
   members and their types, read where a module claims it and where a
   parameter is annotated with it. *)
let iface_defs : (string, Ast.interface_def) Hashtbl.t = Hashtbl.create 8

(* `Ord` is declared here rather than in a file, because it belongs to no
   module: it is what the eleven ordered types implement, and a type is
   ordered whether or not anything was imported. So it is written bare, the
   way a built-in type is, and there is nothing to qualify it with.

   It shares its name with the constraint `<` and `>` check, and deliberately
   -- both say "this type is ordered". They are two mechanisms for where an
   implementation comes from, never two properties: an operator has nowhere
   to take a module, so the constraint is what it checks; a caller passes the
   module, which is what this is. They cannot be confused syntactically
   either, since a constraint is only ever written after `'a:`. *)
let builtin_ifaces : (string * Ast.interface_def) list =
  let v n = Ast.TEVar (n, None) in
  let arrow a b = Ast.TEFun (a, b, None) in
  let a = v "a" in
  [ ("Ord",
     { Ast.if_name = "Ord";
       if_params = ["a"];
       if_members =
         [ ("max",      arrow a (arrow a a));
           ("min",      arrow a (arrow a a));
           ("clamp",    arrow a (arrow a (arrow a a)));
           ("between?", arrow a (arrow a (arrow a (Ast.TEName "Bool")))) ] }) ]

(* A member's own effects: the ones on the innermost arrow of its type, which
   is where the parser puts them. The arguments are the left of each arrow on
   the way there. *)
let rec member_spine (te : Ast.type_expr) =
  match te with
  | Ast.TEFun (a, b, eff) ->
    let (args, inner) = member_spine b in
    (a :: args, (match inner with None -> eff | some -> some))
  | _ -> ([], None)

(* Every effect variable written anywhere inside a type. *)
let rec written_eff_vars (te : Ast.type_expr) =
  match te with
  | Ast.TEFun (a, b, eff) ->
    written_eff_vars a @ written_eff_vars b
    @ (match eff with
       | Some { Ast.te_var = Some v; _ } -> [v]
       | _ -> [])
  | Ast.TEApp (a, b) -> written_eff_vars a @ written_eff_vars b
  | Ast.TETuple ts -> List.concat_map written_eff_vars ts
  | Ast.TEName _ | Ast.TEQual _ | Ast.TEVar _ -> []

(* A member's effects bound what any module implementing it may perform, and
   a variable nothing determines bounds nothing. Where the variable also
   names an argument's effects the caller settles it, which is the ordinary
   higher-order shape and stays legal.

   Without this a module performing Shell answered to a member declared
   `! 'e`, and the caller was told nothing: `'e` was read from the
   declaration and never met the implementation, so a file bounded
   `uses {IO}` ran a subprocess and typechecked clean. The same shape written
   as a plain parameter is caught, because there the real function's type
   binds the variable. *)
let undetermined_member_effect (te : Ast.type_expr) =
  let (args, result_eff) = member_spine te in
  match result_eff with
  | Some { Ast.te_var = Some v; _ }
    when not (List.exists (fun a -> List.mem v (written_eff_vars a)) args) ->
    Some v
  | _ -> None

let is_iface name = Hashtbl.mem iface_defs name

(* The members an interface declares, for the listings that mark which of a
   module's bindings answer to one. None where the interface is not in
   scope, which marks nothing rather than guessing. *)
let iface_member_names name =
  match Hashtbl.find_opt iface_defs name with
  | Some i -> Some (List.map fst i.Ast.if_members)
  | None -> None

let split_claims (own : env) : env * (string * typ list) list =
  let claims =
    match List.assoc_opt implements_key own with
    | Some (Mono (TModule cs)) -> cs
    | _ -> []
  in
  (List.filter (fun (n, _) -> n <> implements_key) own, claims)

(* The qualified spellings of an interface written bare. An imported one is
   reached only through the module that declares it, so a bare name that
   names one is answered with the spellings that work rather than with a
   complaint about an unknown type. Several modules may declare the name,
   and each is a different contract, so all of them are offered. *)
let qualified_ifaces_named n =
  let suffix = "." ^ n in
  let k = String.length suffix in
  List.sort compare
    (Hashtbl.fold (fun key _ acc ->
       if String.length key > k
       && String.sub key (String.length key - k) k = suffix
       then key :: acc else acc) iface_defs [])


let check_te_arity (te : type_expr) =
  let arity_of name =
    match List.assoc_opt name !known_aliases with
    | Some (params, _) -> Some (List.length params)
    | None ->
      (match List.assoc_opt name !known_type_arities with
       | Some n -> Some n
       | None   -> builtin_type_arity name)
  in
  let check name got =
    (* An interface is not a type, and its arity is its own: it is checked
       where the annotation is read, against what the interface declares. A
       name that is one only when qualified is left alone too, or `Ord` --
       which is also a constraint -- would be answered about the constraint
       rather than told how to reach the interface. *)
    if is_iface name || qualified_ifaces_named name <> [] then () else
    match arity_of name with
    | Some want when want <> got ->
      raise (TypeError (
        if want = 0 then
          Printf.sprintf "'%s' takes no type arguments, but is given %d"
            name got
        else
          Printf.sprintf "'%s' takes %d type argument%s, not %d"
            name want (if want = 1 then "" else "s") got))
    | _ -> ()
  in
  let rec peel acc = function
    | TEApp (f, a) -> peel (a :: acc) f
    | h            -> (h, acc)
  in
  let rec go te =
    match te with
    (* Peeled whole. The spine of a partial application is not a type on its
       own, so walking into it would ask `Result String` for its arity and
       find it one short of what the reader wrote. *)
    | TEApp _ ->
      let (head, args) = peel [] te in
      (match head with
       | TEName n      -> check n (List.length args)
       | TEQual (m, n) -> check (m ^ "." ^ n) (List.length args)
       | _             -> ());
      List.iter go args
    | TEName n        -> check n 0
    | TEQual (m, n)   -> check (m ^ "." ^ n) 0
    | TEFun (a, b, _) -> go a; go b
    | TETuple ts      -> List.iter go ts
    | TEVar _         -> ()
  in
  go te

let type_of_te_bound_with_vars (bound : (string * typ) list) (te : type_expr)
  : typ * (string * typ) list =
  check_te_arity te;
  let vars : (string, typ) Hashtbl.t = Hashtbl.create 4 in
  List.iter (fun (n, t) -> Hashtbl.replace vars n t) bound;
  (* One table per written type, so `'e` twice in one signature is one
     variable -- which is the entire point of being able to write it. *)
  let evars : (string, Effect_set.t) Hashtbl.t = Hashtbl.create 4 in
  let effects_of (spec : te_effects option) =
    match spec with
    | None -> Effect_set.unknown ()
    | Some { te_labels; te_var } ->
      let base =
        match te_var with
        | None -> Effect_set.pure
        | Some v ->
          (match Hashtbl.find_opt evars v with
           | Some r -> r
           | None ->
             let r = Effect_set.unknown () in
             Hashtbl.replace evars v r;
             written_evars := free_evars_typ (TFun (TUnit, TUnit, r)) @ !written_evars;
             r)
      in
      List.fold_left (fun acc name -> Effect_set.add (eff_of_written name) acc)
        base te_labels
  in
  (* An alias being resolved, so `type A = A` and a ring of them stop rather
     than run forever. *)
  let resolving = ref [] in
  (* An alias applied to its arguments. The parameters are bound to the
     argument types for as long as the target is being read, so `'a` in
     `type Pair 'a = ('a, 'a)` is the type at the use site rather than a
     fresh variable. Whatever those names meant outside is put back after:
     an alias's `'a` is its own, not the enclosing signature's. *)
  let rec apply_alias name args =
    let (params, target) = List.assoc name !known_aliases in
    let want = List.length params and got = List.length args in
    if want <> got then
      raise (TypeError (Printf.sprintf
        "'%s' takes %d type argument%s, not %d"
        name want (if want = 1 then "" else "s") got));
    let arg_ts = List.map go args in
    let saved = List.map (fun p -> (p, Hashtbl.find_opt vars p)) params in
    List.iter2 (fun p t -> Hashtbl.replace vars p t) params arg_ts;
    (* Both spellings, because an alias declared in a module is reachable
       under the short name the file writes and under the canonical one the
       table is keyed by; a ring that changes spelling half way round has to
       stop the same as one that does not. *)
    let marks = [name; canonical_type_name name] in
    resolving := marks @ !resolving;
    let t = go target in
    resolving := List.filteri (fun i _ -> i >= List.length marks) !resolving;
    List.iter (fun (p, prev) ->
      match prev with
      | Some t -> Hashtbl.replace vars p t
      | None   -> Hashtbl.remove vars p) saved;
    TAlias (name, arg_ts, t)
  and go = function
    (* `Foo.Status`: the import records the type under this key as well as
       under its bare name. The key says the qualifier is right; the type is
       the one the bare name gives, so the two spellings are one type. Step 4
       makes them tell two modules' types apart. *)
    | TEQual (m, n) when is_iface (m ^ "." ^ n) ->
      let name = m ^ "." ^ n in
      let idef = Hashtbl.find iface_defs name in
      let want = List.length idef.Ast.if_params in
      if want <> 0 then
        raise (TypeError (Printf.sprintf
          "'%s' takes %d type argument%s, and this names none: write '%s %s'"
          name want (if want = 1 then "" else "s") name
          (String.concat " " (List.map (fun p -> "'" ^ p) idef.Ast.if_params))));
      TIface (name, [])
    (* `Foo.Status`: the module says which `Status` this is. *)
    | TEQual (m, n) ->
      (match !known_type_names with
       | Some names when not (List.mem (m ^ "." ^ n) names) ->
         raise (TypeError (Printf.sprintf "unknown type '%s.%s'" m n))
       | _ -> ());
      let canon = canonical_type_name (m ^ "." ^ n) in
      (* An alias is transparent whichever way it is named. Written bare it
         goes through the branch below; written `M.Alias` it arrived here
         and stayed an opaque name, so `HTTP.Response` in a signature and
         `HTTP.Response(...)` in a pattern were two different types. *)
      if List.mem_assoc canon !known_aliases && not (List.mem canon !resolving)
      then apply_alias canon []
      else TName canon
    (* `ord.Ord Int`: the interface, said with the module that declares it.
       Read before the cases that take a qualified name for a type. *)
    | TEApp _ as te when (
        let rec head = function
          | TEApp (f, _) -> head f
          | TEQual (m, n) -> Some (m ^ "." ^ n)
          | _ -> None
        in
        match head te with Some n -> is_iface n | None -> false) ->
      let rec peel acc = function
        | TEApp (f, a) -> peel (a :: acc) f
        | TEQual (m, n) -> (m ^ "." ^ n, acc)
        | _ -> assert false
      in
      let (name, args) = peel [] te in
      let idef = Hashtbl.find iface_defs name in
      let want = List.length idef.Ast.if_params and got = List.length args in
      if want <> got then
        raise (TypeError (Printf.sprintf
          "'%s' takes %d type argument%s, and this names %d"
          name want (if want = 1 then "" else "s") got));
      TIface (name, List.map go args)
    (* An imported interface has no bare form, so a bare name that is one
       says what to write. Read before every case that would take the name
       for a type -- `Ord` is also a constraint, and answering about that
       tells the reader nothing they can act on. *)
    | TEName n when qualified_ifaces_named n <> [] ->
      raise (TypeError (Printf.sprintf
        "'%s' is an interface, and one is reached through the module that \
         declares it: write %s" n
        (String.concat " or "
           (List.map (fun q -> "'" ^ q ^ "'") (qualified_ifaces_named n)))))
    (* An interface names a module's contract, not a type, so it is read
       before any of the cases that take a bare name for one. *)
    | TEName n when is_iface n ->
      let idef = Hashtbl.find iface_defs n in
      let want = List.length idef.Ast.if_params in
      if want <> 0 then
        raise (TypeError (Printf.sprintf
          "'%s' takes %d type argument%s, and this names none: write '%s %s'"
          n want (if want = 1 then "" else "s") n
          (String.concat " " (List.map (fun p -> "'" ^ p) idef.Ast.if_params))));
      TIface (n, [])
    | TEName name when List.mem_assoc name !known_aliases
                    && not (List.mem name !resolving) ->
      (* The name is kept over what it names, for the message. Everything
         that reads the type goes through `repr`, which takes it off. *)
      apply_alias name []
    | TEName name when List.mem name !resolving ->
      raise (TypeError (Printf.sprintf
        "'%s' is an alias for itself; an alias names a type that already \
         exists" name))
    | TEName name ->
      (match name with
       (* Each written `Num` is a fresh numeric variable; use-sites link
          them. This is what keeps `:t` output paste-able: a printed Num
          never claims more linkage than the code enforces. `Ord` is the
          same, one constraint wider. *)
       | "Num"      -> fresh_num ()
       | "Add"      -> fresh_add ()
       | "Ord"      -> fresh_ord ()
       | "Int"      -> TInt      | "Float"    -> TFloat
       | "String"   -> TString   | "Bool"     -> TBool
       | "Unit"     -> TUnit     | "Path"     -> TPath     | "Glob"     -> TGlob
       | "DateTime" -> TDateTime | "Duration" -> TDuration
       | "URL"      -> TURL      | "IPv4"     -> TIPv4
       | "CIDR"     -> TCIDR     | "Port"     -> TPort
       | "Version"  -> TVersion  | "Size"     -> TSize
       | "JSON"     -> TJson
       | "TOML"     -> TToml
       | "YAML"     -> TYaml
       | "Command"  -> TCommand
       (* A canonical name resolves to itself: it is not something a file
          writes, it is what a declaration that travelled says. *)
       | n when String.contains n '#' -> TName n
       | n          ->
         (match !known_type_names with
          | Some known when not (builtin_type_name n || List.mem n known) ->
            (* An interface is reached through the module that declares it,
               so a bare name that is one names the spelling that works
               rather than saying the type is unknown. Several modules may
               declare the name, and each is a different contract, so all of
               them are offered. *)
            raise (TypeError (
              match qualified_ifaces_named n with
              | [] -> Printf.sprintf "unknown type '%s'%s"
                        n (Util.hint n (known @ builtin_type_names))
              | qs -> Printf.sprintf
                  "'%s' is an interface, and one is reached through the \
                   module that declares it: write %s"
                  n (String.concat " or " (List.map (fun q -> "'" ^ q ^ "'") qs))))
          (* The name a file writes; the type is the one it stands for. *)
          | _ -> TName (canonical_type_name n)))
    (* The constraint is applied to the variable wherever it is written,
       rather than only where the variable is first created: a written type
       is read as a tree, and which branch of an arrow is walked first is
       not something the reader chose. Written `'a -> 'a: Ord`, the bare
       mention would otherwise make the variable and the constraint would
       reach it too late to bind. *)
    | TEVar (name, constraint_) ->
      let constrained = function
        | "Num" -> fresh_num ()
        | "Add" -> fresh_add ()
        | "Ord" -> fresh_ord ()
        | other ->
          raise (TypeError (Printf.sprintf
            "'%s' is not a constraint; the constraints are Num, Add and Ord"
            other))
      in
      (match Hashtbl.find_opt vars name with
       | Some t ->
         (match constraint_ with
          | None   -> ()
          | Some c -> unify t (constrained c));
         t
       | None ->
         let t =
           match constraint_ with
           | None   -> fresh ()
           | Some c -> constrained c
         in
         Hashtbl.add vars name t; t)
    | TEFun (a, b, eff) -> TFun (go a, go b, effects_of eff)
    | TETuple ts    -> TTuple (List.map go ts)
    (* `Ord Int` in a parameter's annotation: the type of a module providing
       what `Ord` declares, at `Int`. Read before the generic application
       below, which would take `Ord` for a type. *)
    | TEApp _ as te when (
        let rec head = function
          | TEApp (f, _) -> head f
          | TEName n -> Some n
          | _ -> None
        in
        match head te with Some n -> is_iface n | None -> false) ->
      let rec peel acc = function
        | TEApp (f, a) -> peel (a :: acc) f
        | TEName n -> (n, acc)
        | _ -> assert false
      in
      let (name, args) = peel [] te in
      let idef = Hashtbl.find iface_defs name in
      let want = List.length idef.Ast.if_params and got = List.length args in
      if want <> got then
        raise (TypeError (Printf.sprintf
          "'%s' takes %d type argument%s, and this names %d"
          name want (if want = 1 then "" else "s") got));
      TIface (name, List.map go args)
    (* A parameterised alias applied to its arguments, written bare or
       written with the module it came from. `Args.Parser Opts` reached the
       generic application below, which read the head on its own -- an alias
       with no arguments, one short of what it takes. *)
    | TEApp _ as te when (
        let rec head = function
          | TEApp (f, _) -> head f
          | TEName n -> Some n
          | TEQual (m, n) -> Some (canonical_type_name (m ^ "." ^ n))
          | _ -> None
        in
        match head te with
        | Some n -> List.mem_assoc n !known_aliases && not (List.mem n !resolving)
        | None -> false) ->
      let rec peel acc = function
        | TEApp (f, a) -> peel (a :: acc) f
        | TEName n -> (n, acc)
        | TEQual (m, n) -> (canonical_type_name (m ^ "." ^ n), acc)
        | _ -> assert false
      in
      let (name, args) = peel [] te in
      apply_alias name args
    | TEApp (TEName "List", arg)   -> TList   (go arg)
    | TEApp (TEName "Decoder", arg) -> TDecoder (go arg)
    | TEApp (TEApp (TEName "Result", e), a) -> TResult (go e, go a)
    | TEApp (TEName "Result", _) ->
      raise (TypeError "Result now takes two type arguments: Result <ErrorType> <ValueType>")
    | TEApp (TEName "Map", arg)    -> TMap    (go arg)
    | TEApp (f, arg) -> TApp (go f, go arg)
  in
  let t = go te in
  (* The variables this written type named itself -- not the ones it was
     handed -- which is what `check_written_vars` has to be able to ask
     about afterwards. *)
  let named =
    Hashtbl.fold (fun name v acc ->
      if List.mem_assoc name bound then acc else (name, v) :: acc) vars []
  in
  (t, named)

let type_of_te_bound bound te = fst (type_of_te_bound_with_vars bound te)

(* The members an implementation has to answer for, with the interface's
   parameters bound to what the claim named them at: `implement Ord Int`
   binds `'a` to `Int`, so `max` comes out `Int -> Int -> Int`. That is
   instantiation, not dispatch -- nothing is looked up from a value. *)
let iface_members (idef : Ast.interface_def) (args : typ list) =
  let bound = List.combine idef.Ast.if_params args in
  List.map (fun (n, te) -> (n, type_of_te_bound bound te)) idef.Ast.if_members

let type_of_te (te : type_expr) : typ = type_of_te_bound [] te

(* A written type variable is a promise to whoever reads the signature:
   `'a -> 'a` says the function works for any type at all. Unifying the
   annotation with the inferred type cannot keep that promise on its own,
   because a variable unifies with `Int` as readily as with anything else --
   so `let f : 'a -> 'a = fn x -> x + 1` was accepted, and the signature
   said a String would do.

   Once the unification has happened, each written variable has to still be
   a variable -- nothing about the body decided what it is -- and two of
   them have to still be different, since `'a -> 'b` over `fn x -> x` claims
   the result is unrelated to the argument, which is more than the body
   does. An annotation narrower than the body stays fine: `Int -> Int` over
   the identity names no variable and promises nothing.

   The effects half of this was fixed when written effects arrived: a
   written `! {Shell}` is a closed set, and a body performing more does not
   unify with it. *)
let check_written_vars (written : (string * typ) list) =
  List.iter (fun (name, v) ->
    match repr v with
    | TVar _ -> ()
    | t ->
      raise (TypeError (Printf.sprintf
        "the annotation says '%s, which stands for any type, but the body \
         works only for %s" name (string_of_typ t)))) written;
  let rec distinct = function
    | [] -> ()
    | (n1, v1) :: rest ->
      List.iter (fun (n2, v2) ->
        match repr v1, repr v2 with
        | TVar a, TVar b when a == b ->
          raise (TypeError (Printf.sprintf
            "the annotation says '%s and '%s are separate types, but the \
             body makes them the same" n1 n2))
        | _ -> ()) rest;
      distinct rest
  in
  distinct written

(* `key` is the name the definition is registered under, which for a module's
   type is its canonical one. The name inside the declaration is the short one
   a reader wrote, and is not what the type is called elsewhere. *)
let ctor_schemes ?key (tdef : type_def) : (string * scheme) list =
  match tdef with
  (* An alias is another name for a type, not a new one, so it brings no
     constructor with it. *)
  | Alias _ -> []
  | Variants (tname, params, ctors) ->
    let var_table = List.map (fun p -> (p, fresh ())) params in
    (* The declaration carries the short name; the type it builds is the
       canonical one, which is what every other reference resolves to. *)
    let result =
      let name = match key with Some k -> k | None -> canonical_type_name tname in
      List.fold_left (fun acc (_, v) -> TApp (acc, v)) (TName name) var_table
    in
    (* One table for the whole declaration, so `'e` written twice in a field
       is one variable and the relationship it states actually holds. *)
    let evars : (string, Effect_set.t) Hashtbl.t = Hashtbl.create 4 in
    let written = ref [] in
    let effects_of (spec : te_effects option) =
      match spec with
      | None -> Effect_set.unknown ()
      | Some { te_labels; te_var } ->
        let base =
          match te_var with
          | None -> Effect_set.pure
          | Some v ->
            (match Hashtbl.find_opt evars v with
             | Some r -> r
             | None ->
               let r = Effect_set.unknown () in
               Hashtbl.replace evars v r;
               written := free_evars_typ (TFun (TUnit, TUnit, r)) @ !written;
               r)
        in
        List.fold_left (fun acc name -> Effect_set.add (eff_of_written name) acc)
          base te_labels
    in
    let rec conv_ = function
      (* The module says which type this is; the canonical name is what the
         rest of the checker knows it by. *)
      | TEQual (m, n) -> conv_ (TEName (canonical_type_name (m ^ "." ^ n)))
      | TEVar (name, _) ->
        (match List.assoc_opt name var_table with
         | Some v -> v
         | None -> raise (TypeError (Printf.sprintf
             "type variable ''%s' is not declared as a parameter of type '%s'"
             name tname)))
      | TEName _ as te -> type_of_te te
      | TEFun (a, b, eff) -> TFun (conv_ a, conv_ b, effects_of eff)
      | TETuple ts -> TTuple (List.map conv_ ts)
      | TEApp (TEName "List", arg)   -> TList   (conv_ arg)
      (* A field may hold a decoder. Without this the field's type is a plain
         application, which prints as `Decoder Pod` and unifies with nothing:
         `type D(decoder: Decoder Pod)` could not be built from
         `Pod.decoder`. *)
      | TEApp (TEName "Decoder", arg) -> TDecoder (conv_ arg)
      | TEApp (TEApp (TEName "Result", e), a) -> TResult (conv_ e, conv_ a)
      | TEApp (TEName "Result", _) ->
        raise (TypeError "Result now takes two type arguments: Result <ErrorType> <ValueType>")
      | TEApp (TEName "Map", arg)    -> TMap    (conv_ arg)
      | TEApp (f, arg) -> TApp (head f, conv_ arg)
    (* The spine of an application is not a written type on its own, so the
       head is resolved here rather than through `type_of_te`, which checks
       arity and would find `Option` in `Option String` a whole argument
       short. The written type is checked once, below, as a whole. *)
    and head = function
      | TEApp (f, arg) -> TApp (head f, conv_ arg)
      | TEName n       -> TName (canonical_type_name n)
      | TEQual (m, n)  -> TName (canonical_type_name (m ^ "." ^ n))
      | te             -> conv_ te
    in
    (* Once per field, over the whole of what was written. `conv_` recurses
       without repeating it, since the walk covers the arguments too. *)
    let conv te = check_te_arity te; conv_ te in
    (* A field's effects are not written down -- the grammar has no place to
       put them -- so `conv` gives each arrow an effect variable and lets
       construction say what it is. Generalising that variable is what made
       the type forget: it is not reachable from the constructor's result
       (`Action`, not `Action 'e`), so quantifying it universally lets
       construction pick Shell and the match that takes the field back out
       pick nothing.

           type Action = Action (Unit -> String)
           let a = Action (fn () -> $(touch /tmp/x))
           let fire x = match x with | Action f -> f ()

       That typechecked under `uses {}` and ran the command.

       So an effect variable is quantified only when it was written or when
       the result type mentions it. A written one says how the effects of
       one part of the type relate to another --

           raises: ((Unit -> 'b ! 'e) -> TestOutcome ! 'e)

       -- and instantiating it afresh per use is what makes that
       relationship hold for each caller. A variable nobody wrote is
       inference's own: it holds what construction found out, and
       quantifying it is exactly the throwing-away above. Those stay
       monomorphic, shared by every use of the constructor.

       Type variables generalise as they always did: those the result does
       mention are what makes `Box 'a` usable at two types. *)
    let quantifiable =
      (free_evars_typ result @ !written) |> List.sort_uniq compare in
    List.map (fun ctor ->
      (* Applying a constructor performs nothing, so its own arrows are pure.
         `@->` would give each an effect variable instead, and since these
         are no longer generalised that variable would be shared by every use
         of the constructor and accumulate whatever any of them met -- one
         `Some` in a raising context made every `Some` in the program raise.
         The variables that matter are the ones `conv` puts on a field. *)
      let t =
        List.fold_right (fun (_, te) acc -> TFun (conv te, acc, Effect_set.pure))
          ctor.fields result
      in
      let tvars = free_tvars t |> List.sort_uniq compare in
      let evars =
        free_evars_typ t
        |> List.sort_uniq compare
        |> List.filter (fun id -> List.mem id quantifiable)
      in
      let scheme = if tvars = [] && evars = [] then Mono t else Poly (tvars, evars, t) in
      (ctor.name, scheme)
    ) ctors

(* ── Derived decoders ─────────────────────────────────────────────────────
   A single-constructor type whose fields are named already states what a
   decoder for it would do, so `T.decoder` is derived from the definition
   rather than written out beside it. Everything checkable about that is
   checked here: what is not derivable says why, at the point where someone
   asked for it.

   A type may mention itself, so the walk carries the types it is already
   inside. *)
let rec derivable_field_type tenv seen params (te : type_expr) : (unit, string) result =
  match te with
  | TEName ("Int" | "Float" | "String" | "Bool" | "Path" | "Glob" | "Duration"
           | "URL" | "Size" | "Version" | "Date" | "Time" | "DateTime"
           | "IPv4" | "CIDR" | "Port") -> Ok ()
  | TEQual (m, n) ->
    derivable_field_type tenv seen params
      (TEName (canonical_type_name (m ^ "." ^ n)))
  | TEName tname ->
    let key = canonical_type_name tname in
    if List.mem key seen then Ok ()   (* recursive: read when it is reached *)
    else
      (match List.assoc_opt key tenv with
       | Some tdef -> derivable_typedef tenv (key :: seen) tdef
       | None -> Error (Printf.sprintf "no decoder is known for type '%s'"
                          (short_type_name tname)))
  | TEApp (TEName "List", inner)   -> derivable_field_type tenv seen params inner
  | TEApp (TEName "Option", inner) -> derivable_field_type tenv seen params inner
  | TEApp (TEName "Map", inner)    -> derivable_field_type tenv seen params inner
  (* A type variable is read by the decoder supplied for it, so it is fine
     when the type declares it and nonsense when it does not. *)
  | TEVar (v, _) ->
    if List.mem v params then Ok ()
    else Error (Printf.sprintf "'%s is not one of the type's parameters" v)
  | TETuple _ ->
    Error "a tuple has no field names for a document to be read by"
  | TEFun _ -> Error "no document contains a function"
  | TEApp _ ->
    (* A generic type applied to arguments: derivable when it is and its
       arguments are. *)
    let rec spine te = match te with
      | TEApp (f, a) -> let (h, args) = spine f in (h, args @ [a])
      | TEName n -> (Some n, [])
      | _ -> (None, [])
    in
    (match spine te with
     | (Some tname, args) ->
       let head =
         if List.mem tname seen then Ok ()
         else
           (match List.assoc_opt tname tenv with
            | Some tdef -> derivable_typedef tenv (tname :: seen) tdef
            | None -> Error (Printf.sprintf "no decoder is known for type '%s'" tname))
       in
       List.fold_left (fun acc a ->
         match acc with
         | Error _ -> acc
         | Ok () -> derivable_field_type tenv seen params a) head args
     | _ -> Error "no decoder is known for that type")

and derivable_typedef tenv seen (tdef : type_def) : (unit, string) result =
  match tdef with
  (* Deriving reads a type's own shape. An alias has none of its own; the
     decoder belongs to whatever it names. *)
  | Alias _ -> Error "it is an alias, so its decoder is the one it names"
  | Variants (_, _, []) -> Error "it has no constructor"
  | Variants (_, _, _ :: _ :: _) -> Error "it has more than one constructor"
  | Variants (_, params, [ctor]) ->
    if ctor.fields = [] then Error "it has no fields"
    else if List.exists (fun (n, _) -> n = None) ctor.fields then
      (* "fields" in wand means named fields -- a constructor's positional
         payload is not one, which is exactly why it cannot be derived. Say
         payload, so the message does not teach the wrong word. *)
      Error "its payload has no field names, and a document is read by name"
    else
      List.fold_left (fun acc (fname, te) ->
        match acc with
        | Error _ -> acc
        | Ok () ->
          (match derivable_field_type tenv seen params te with
           | Ok () -> Ok ()
           | Error why ->
             Error (Printf.sprintf "field '%s' cannot be read: %s"
               (match fname with Some n -> n | None -> "?") why))
      ) (Ok ()) ctor.fields

(* The types a module declares, under their bare names. An import records
   them twice: as `Status` and as `Foo.Status`. Putting the second set first,
   with the prefix taken off, makes every lookup below resolve inside that
   module without knowing anything about qualification. *)
(* A built-in type the compiler names, whose constructors are a module's
   words rather than the language's. `HTTPMethod` is the `Net!http`
   payload's method field, so it is built in for the reason `HTTPRequest`
   is; `GET` and `POST` belong to HTTP, so they are written `HTTP.GET`. *)
let module_only_ctors = [ ("HTTPMethod", "HTTP") ]

(* `POST` alone names nothing, and the fix is the module it belongs to. *)
let module_only_hint tenv name =
  match
    List.find_opt (fun (t, _) ->
      match List.assoc_opt t tenv with
      | Some (Variants (_, _, ctors)) ->
        List.exists (fun c -> c.name = name) ctors
      | _ -> false) module_only_ctors
  with
  | None -> ""
  | Some (_, m) -> Printf.sprintf " -- write '%s.%s'" m name

let via_module_only_alias tenv own =
  List.concat_map (fun (_, d) ->
    match d with
    | Alias (_, _, TEName target) when List.mem_assoc target module_only_ctors ->
      (match List.assoc_opt target tenv with
       | Some td -> [ (target, td) ]
       | None -> [])
    | _ -> []) own

let module_first tenv m =
  let pre = m ^ "." in
  let n = String.length pre in
  (* The module's own types, keyed as they are everywhere else. Putting them
     first is what makes a constructor lookup find this module's. *)
  let own =
    List.concat_map (fun (written, canon) ->
      if String.length written > n && String.sub written 0 n = pre then
        match List.assoc_opt canon tenv with
        | Some d ->
          (* Under the canonical key, and under the short name the module
             wrote. `Foo.Conf(...)` reads `Conf` inside this scope, and an
             alias is resolved by the name it was written under -- without
             the short entry, `Foo.Alias(...)` looked up `Alias`, found
             nothing, and reported an unknown constructor for a type that
             is right there. *)
          let short = String.sub written n (String.length written - n) in
          [(canon, d); (short, d)]
        | None -> []
      else []) !type_name_map
  in
  (* A module that aliases one of those built-ins is how its constructors
     are reached: `HTTP.POST` reads `POST` with `HTTPMethod` visible. An
     alias forwards a single constructor by name, and a method type has
     six, so without this the qualified name finds nothing. *)
  let own = own @ via_module_only_alias tenv own in
  (own, own @ tenv)

(* `Apps.Deployment` where `Deployment` is one of `Apps`'s types, rather than
   a construction of it. The map holds the written name, so its presence is
   what says this is a type reached through a module. *)
let qualified_type_head e =
  match Ast.strip_located e with
  | Ast.Qualified (m, inner) ->
    (match Ast.strip_located inner with
     | Ast.Constr t when List.mem_assoc (m ^ "." ^ t) !type_name_map ->
       Some (m, t)
     | _ -> None)
  | _ -> None

(* A type with one constructor names that constructor too, so a name given to
   such a type -- by an alias, or by renaming it on import -- builds and
   matches one. A type with several has no single constructor to forward. *)
let rec ctor_name_for tenv name =
  (* The name a file writes; the declaration is under its canonical one. *)
  match List.assoc_opt (canonical_type_name name) tenv with
  | Some (Variants (_, _, [c])) when c.name <> name -> c.name
  | Some (Alias (_, _, te)) ->
    (* The head of what the alias names. A parameterised alias writes its
       target applied -- `type Parser 'a = CommandLine 'a` -- and the
       constructor belongs to the head, so reading only the bare forms left
       `Args.Parser(...)` naming a type and no constructor. *)
    let rec head = function
      | TEApp (f, _) -> head f
      | TEName t | TEQual (_, t) -> Some t
      | _ -> None
    in
    (match head te with
     | Some target when target <> name -> ctor_name_for tenv target
     | _ -> name)
  | _ -> name

(* Whose constructors a file may name without a qualifier: its own, the ones
   it selected in an import, and the built-ins. A module's own types travel
   under canonical names so that a value of one can be read and matched here,
   but naming their constructors bare is what stopped being allowed. The
   qualified forms lift this for the module they name. *)
let visible_canonical : string list ref = ref []

(* `nameable` is asked once per type in scope, for every constructor the
   file mentions, so a list membership test here is the whole cost of
   importing a large module: 700 types put 490,000 string comparisons behind
   one constructor. The set is built from whichever list is current and kept
   while that list is, which `with_visible` makes cheap -- it restores the
   list it saved, so entering and leaving a scope each rebuild once and
   every test between them is a lookup. *)
let visible_set : (string list * (string, unit) Hashtbl.t) option ref = ref None

let visible_mem key =
  let current = !visible_canonical in
  let tbl =
    match !visible_set with
    | Some (cached, tbl) when cached == current -> tbl
    | _ ->
      let tbl = Hashtbl.create (2 * List.length current + 1) in
      List.iter (fun k -> Hashtbl.replace tbl k ()) current;
      visible_set := Some (current, tbl);
      tbl
  in
  Hashtbl.mem tbl key

let nameable key =
  if List.mem_assoc key module_only_ctors then visible_mem key
  else not (String.contains key '#') || visible_mem key

let with_visible keys f =
  let saved = !visible_canonical in
  visible_canonical := keys @ saved;
  Fun.protect ~finally:(fun () -> visible_canonical := saved) f

let find_ctor_in_tenv tenv name =
  List.find_map (fun (tname, tdef) ->
    if not (nameable tname) then None else
    match tdef with
    | Alias _ -> None
    | Variants (_, _, ctors) ->
      (match List.find_opt (fun c -> c.name = name) ctors with
       | Some c -> Some (tname, c)
       | None -> None)
  ) tenv

(* Whether a pattern can fail to match. A parameter with a refutable pattern
   makes the function partial -- `let head! [h : _] = h` has nothing to do
   with an empty list but raise -- and that raise comes from the binding
   itself rather than from any call, so nothing else would record it. A
   `match` is different: it is checked for exhaustiveness, so its cases
   cannot all fail.

   A constructor pattern cannot mismatch when there is no other constructor
   for the value to be, which is why `let unwrap (Wrap n) = n` is a total
   function. Whether the fields are named has nothing to do with it: a named
   pattern was read as irrefutable on that reasoning alone, so
   `let area (Circle (radius = r)) = r` over `Circle | Square` was a function
   that raises and said it did not -- no Raise in its type, no Raise in the
   file's manifest, and no `!` on its name. *)
let ctor_is_alone tenv name =
  List.exists (fun (_, tdef) ->
    match tdef with
    | Alias _ -> false
    | Variants (_, _, ctors) ->
      (match ctors with [c] -> c.name = name | _ -> false)) tenv

let rec pat_is_refutable tenv (p : pat) =
  match p with
  | PVar _ | Wild  -> false
  | Unit           -> false
  | PTuple ps      -> List.exists (pat_is_refutable tenv) ps
  | PConstr (name, ps) ->
    not (ctor_is_alone tenv name) || List.exists (pat_is_refutable tenv) ps
  | PConstrNamed (name, fields) ->
    not (ctor_is_alone tenv name)
    || List.exists (fun (_, p) -> pat_is_refutable tenv p) fields
  | PConstrBare (name, _) -> not (ctor_is_alone tenv name)
  | PQualified (_, p) -> pat_is_refutable tenv p
  (* An annotation constrains the type, and a type cannot fail to match. *)
  | PAnnot (p, _)  -> pat_is_refutable tenv p
  | _              -> true

(* ── The shape of a command line ──────────────────────────────────────────
   A command line is flags and arguments. The flags are a record, since each
   has a name and a type; the arguments have no names at all. So a type that
   describes a whole command line has one field whose type is a record -- the
   flags -- and one that is not -- what was written without a flag in front
   of it. Which is which comes from the types rather than the names, so both
   are the author's to call whatever they like.

   A type with no record field is the older, flatter shape: all flags, no
   arguments. Anything else has no reading, and says so rather than guessing
   which field meant what. *)
type cmdline_shape =
  | Flags_only
  | Flags_and_arguments of string * string * type_expr
      (* flags field, argument field, the argument field's type *)

let is_record_field tenv (te : type_expr) =
  match te with
  | TEName n ->
    (match List.assoc_opt n tenv with
     | Some tdef -> derivable_typedef tenv [n] tdef = Ok ()
     | None -> false)
  | _ -> false

let cmdline_shape tenv (ctor : ctor_def) : (cmdline_shape, string) result =
  let named = List.filter_map (fun (n, te) ->
    match n with Some n -> Some (n, te) | None -> None) ctor.fields in
  match List.partition (fun (_, te) -> is_record_field tenv te) named with
  | [], _ -> Ok Flags_only
  | [(fname, _)], [(aname, ate)] -> Ok (Flags_and_arguments (fname, aname, ate))
  | [(fname, _)], [] ->
    Error (Printf.sprintf
      "'%s' holds the flags and nothing holds the arguments; a field that is \
       not a record is what the command line was given without a flag in \
       front of it" fname)
  | [(fname, _)], _ :: _ :: _ ->
    Error (Printf.sprintf
      "'%s' holds the flags, and more than one field is left for the \
       arguments; a command line has one list of those" fname)
  | (a, _) :: (b, _) :: _, _ ->
    Error (Printf.sprintf
      "'%s' and '%s' are both records, so which holds the flags is a guess; \
       a command line has one set of them" a b)

(* Built once per program rather than per lookup. A constructor's field
   effects are monomorphic (see `ctor_schemes`), so they only link the
   construction to the match that takes the field back out if both look at
   the same scheme -- rebuilding it per lookup hands each site its own
   variable and the link is gone. Keyed by the definition itself, so two
   modules declaring the same type name do not collapse into one.

   Cleared with the rest of the per-program state in `infer_program_`. *)
let ctor_scheme_cache : (string * type_def, (string * scheme) list) Hashtbl.t =
  Hashtbl.create 32

let ctor_schemes_for tname tdef =
  match Hashtbl.find_opt ctor_scheme_cache (tname, tdef) with
  | Some schemes -> schemes
  | None ->
    let schemes = ctor_schemes ~key:tname tdef in
    Hashtbl.replace ctor_scheme_cache (tname, tdef) schemes;
    schemes

(* Inference asks for this at every constructor it meets, and the answer
   only changes when the type environment does. Keyed on the environment's
   identity: a `typedef_env` is an immutable list, so the same one is the
   same answer, and a different one rebuilds.

   Without it, a file importing a module of 700 types spent its run walking
   that list once per constructor -- 52.7s where the module alone typechecks
   in 1.2s.

   Cleared with the rest of the per-program state in `infer_program_`. *)
let ctor_env_memo : (typedef_env * env) option ref = ref None

let tenv_to_ctor_env (tenv : typedef_env) : env =
  match !ctor_env_memo with
  | Some (cached, result) when cached == tenv -> result
  | _ ->
    let result =
      List.concat_map (fun (tname, tdef) ->
        if nameable tname then ctor_schemes_for tname tdef else []) tenv
    in
    ctor_env_memo := Some (tenv, result);
    result

(* ── Pattern inference ────────────────────────────────────────────────────── *)

(* Ok and Error are built in rather than declared, so their schemes are made
   here -- fresh per use, exactly as instantiation would. *)
let builtin_result_scheme = function
  | "Ok"    -> let e = fresh () in let t = fresh () in Some (Mono (t @-> TResult (e, t)))
  | "Error" -> let e = fresh () in let t = fresh () in Some (Mono (e @-> TResult (e, t)))
  | _ -> None

let rec unwrap_ctor_type t =
  match repr t with
  | TFun (arg, rest, _) ->
    let (args, result) = unwrap_ctor_type rest in
    (arg :: args, result)
  | _ -> ([], t)

let rec infer_pat tenv (p : pat) t (env : env) : env =
  match p with
  | PVar name  -> record_local name t; (name, Mono t) :: env
  | Wild       -> env
  | Int _      -> unify_expected ~expected:t ~got:TInt;      env
  | Float _    -> unify_expected ~expected:t ~got:TFloat;    env
  | String _   -> unify_expected ~expected:t ~got:TString;   env
  | Bool _     -> unify_expected ~expected:t ~got:TBool;     env
  | Unit       -> unify_expected ~expected:t ~got:TUnit;     env
  | Path _     -> unify_expected ~expected:t ~got:TPath;     env
  | DateTime _ -> unify_expected ~expected:t ~got:TDateTime; env
  | Duration _ -> unify_expected ~expected:t ~got:TDuration; env
  | URL _      -> unify_expected ~expected:t ~got:TURL;      env
  | IPv4 _     -> unify_expected ~expected:t ~got:TIPv4;     env
  | CIDR _     -> unify_expected ~expected:t ~got:TCIDR;     env
  | Port _     -> unify_expected ~expected:t ~got:TPort;     env
  | Version _  -> unify_expected ~expected:t ~got:TVersion;  env
  | Size _     -> unify_expected ~expected:t ~got:TSize;     env
  | PTuple ps  ->
    (* Tuple syntax destructures tuples only. It used to also unwrap a
       single-constructor named type, so `let (w, h) = rect` bound fields by
       position -- silently wrong on reorder, and invisible at the binding
       site. Named-field types are destructured by naming their fields.

       An unresolved type unifies with a tuple here rather than waiting for a
       call site to say what it is. Waiting sounds harmless -- the parts get
       bound either way -- but it means `fn (a, b) -> a` is inferred as
       `'a -> 'b` and accepts anything, and the mismatch arrives at run time
       as a non-exhaustive match. A pattern that binds two things is a
       statement that there are two things to bind. *)
    (match repr t with
     | TName tname ->
       (match find_ctor_in_tenv tenv tname with
        | Some (_, ctor) when List.length ctor.fields = List.length ps ->
          let named = List.filter_map (fun (fname, _) -> fname) ctor.fields in
          if named <> [] then
            raise (TypeError (Printf.sprintf
              "cannot destructure '%s' with tuple syntax; match its fields by \
               name, as in %s(%s)" tname tname
              (String.concat ", " (List.map (fun n -> n ^ " = " ^ n) named))))
          else begin
            let arg_ts = List.map (fun (_, te) -> type_of_te te) ctor.fields in
            List.fold_left2 (fun env p at -> infer_pat tenv p at env) env ps arg_ts
          end
        | _ ->
          let ts = List.map (fun _ -> fresh ()) ps in
          unify_expected ~expected:t ~got:(TTuple ts);
          List.fold_left2 (fun env p t -> infer_pat tenv p t env) env ps ts)
     | _ ->
       let ts = List.map (fun _ -> fresh ()) ps in
       unify_expected ~expected:t ~got:(TTuple ts);
       List.fold_left2 (fun env p t -> infer_pat tenv p t env) env ps ts)
  | PList [] ->
    unify_expected ~expected:t ~got:(TList (fresh ())); env
  | PList (p :: rest) ->
    let elem_t = fresh () in
    unify_expected ~expected:t ~got:(TList elem_t);
    let env' = infer_pat tenv p elem_t env in
    List.fold_left (fun env p -> infer_pat tenv p elem_t env) env' rest
  | PCons (hp, tp) ->
    let elem_t = fresh () in
    unify_expected ~expected:t ~got:(TList elem_t);
    let env' = infer_pat tenv hp elem_t env in
    infer_pat tenv tp (TList elem_t) env'
  | PConstr (name, pats) ->
    let name = ctor_name_for tenv name in
    let ctor_env = tenv_to_ctor_env tenv in
    (match (match builtin_result_scheme name with
            | Some _ as s -> s
            | None -> List.assoc_opt name ctor_env) with
     | None -> raise (TypeError (Printf.sprintf "unknown constructor '%s'%s" name
         (module_only_hint tenv name)))
     | Some s ->
       let ctor_t = instantiate s in
       let (arg_ts, result_t) = unwrap_ctor_type ctor_t in
       if List.length arg_ts <> List.length pats then
         raise (TypeError (Printf.sprintf
           "constructor '%s' expects %d argument(s), got %d"
           name (List.length arg_ts) (List.length pats)));
       unify_expected ~expected:t ~got:result_t;
       List.fold_left2 (fun env p at -> infer_pat tenv p at env) env pats arg_ts)
  (* `Pod(name, restarts)`: the declaration decides whether these are fields
     or a tuple payload, and the declaration is in hand here. Both readings
     are patterns this function already knows, so it settles on one and
     carries on. *)
  (* `Foo.Live`: read inside the module that declares it. *)
  | PQualified (m, inner) ->
    let (own, tenv') = module_first tenv m in
    if own = [] then
      raise (TypeError (Printf.sprintf
        "'%s' declares no types, so '%s' names nothing in it"
        m (Ast.show_pat inner)));
    with_visible (List.map fst own) (fun () -> infer_pat tenv' inner t env)
  | PConstrBare (name, ids) ->
    let named_fields =
      match find_ctor_in_tenv tenv (ctor_name_for tenv name) with
      | Some (_, ctor) -> List.exists (fun (dn, _) -> dn <> None) ctor.fields
      | None -> false
    in
    infer_pat tenv (Ast.constr_bare_reading ~named_fields name ids) t env
  | PConstrNamed (name, bindings) ->
    let name = ctor_name_for tenv name in
    (match find_ctor_in_tenv tenv name with
     | None -> raise (TypeError (Printf.sprintf "unknown constructor '%s'%s" name
         (module_only_hint tenv name)))
     | Some (tname, ctor) ->
       (* Field types come from the constructor's own scheme, the same way
          construction reads them. Converting the written type again here
          gave the field a brand-new effect variable, so what construction
          learned about a function-typed field -- that the function it was
          given performs Shell -- was not there when the match took it back
          out, and the effect disappeared. `Testing`'s `raises` field is the
          one that mattered: it made `t.raises (fn () -> $(cmd))` typecheck
          in a file whose manifest was `uses {}`. *)
       let arg_ts, result_t =
         match List.assoc_opt name (tenv_to_ctor_env tenv) with
         | Some sch -> unwrap_ctor_type (instantiate sch)
         | None -> ([], TName tname)
       in
       unify_expected ~expected:t ~got:result_t;
       let field_type fname =
         let rec index i = function
           | [] -> None
           | (dn, _) :: rest -> if dn = Some fname then Some i else index (i + 1) rest
         in
         match index 0 ctor.fields with
         | Some i -> List.nth_opt arg_ts i
         | None -> None
       in
       List.fold_left (fun env (fname, p) ->
         match List.find_opt (fun (dn, _) -> dn = Some fname) ctor.fields with
         | None -> raise (TypeError (Printf.sprintf
             "constructor '%s' has no field '%s'%s"
             name fname (Util.hint fname (List.filter_map Fun.id (List.map fst ctor.fields)))))
         | Some (_, te) ->
           let at = match field_type fname with Some t -> t | None -> type_of_te te in
           infer_pat tenv p at env
       ) env bindings)

  | PMap bindings ->
    let vt = fresh () in
    unify_expected ~expected:t ~got:(TMap vt);
    List.fold_left (fun env (_, p) -> infer_pat tenv p vt env) env bindings
  (* `(p : Pod)`. The annotation is a constraint on the value the pattern
     binds, so it is unified and then the pattern under it is inferred as
     usual. This is what lets a function read a field off a parameter: dot
     access needs a named type, and inference has nowhere else to get one
     before the body is read.

     A type variable is refused here. Each annotation resolves its own
     names, so `'a` in two parameters would be two variables, and the
     reader would have been promised one. The whole type says it instead:
     `let f : 'a -> 'a = ...`. *)
  | PAnnot (inner, te) ->
    let rec names_a_var = function
      | TEVar _ -> true
      | TEApp (f, a) -> names_a_var f || names_a_var a
      | TEFun (a, b, _) -> names_a_var a || names_a_var b
      | TETuple ts -> List.exists names_a_var ts
      | TEName _ | TEQual _ -> false
    in
    if names_a_var te then
      raise (TypeError
        "a type variable in a pattern is not shared with the other \
         patterns, so it cannot say what it looks like it says. Write the \
         type of the whole definition instead: let f : 'a -> 'a = ...");
    unify_expected ~expected:t ~got:(type_of_te te);
    infer_pat tenv inner t env

(* for let bindings: PVar gets the generalized scheme, rest are monomorphic *)
let infer_pat_let tenv (p : pat) t scheme (env : env) : env =
  match p with
  | PVar name -> record_local name t; (name, scheme) :: env
  | Wild      -> env
  | _         -> infer_pat tenv p t env

(* ── Match exhaustiveness ─────────────────────────────────────────────────── *)
(* Maranget-style specialization algorithm: recurse over a matrix of
   pattern rows and a parallel list of column types, checking that every
   constructor of the head column's type is either matched directly or
   covered by a wildcard, then recursing into each covered constructor's
   sub-columns. Guards are excluded by the caller (a guarded case might not
   fire, so it can't be relied on for exhaustiveness). *)

let rec is_wild_pat = function
  | PVar _ | Wild -> true
  | PAnnot (p, _) -> is_wild_pat p
  | _ -> false

(* A generic type like `Option 'a` instantiates to `TApp (TName "Option", arg)`;
   peel the TApp chain down to the underlying type-def name. *)
let rec app_head_name t =
  match repr t with
  | TName tname -> Some tname
  | TApp (f, _) -> app_head_name f
  | _ -> None

(* All constructors of t's type, each with its arg types instantiated to
   match t specifically (so `Option Int`'s `Some` reports arg type Int,
   not a generic fresh var) -- reuses the same instantiate/unify machinery
   `infer_pat`'s PConstr case already uses for the same reason. *)
let rec ctors_of_type tenv (ctor_env : env) (t : typ) : (string * typ list) list =
  let via_scheme name scheme =
    let ctor_t = instantiate scheme in
    let (arg_ts, result_t) = unwrap_ctor_type ctor_t in
    (try unify result_t t with TypeError _ -> ());
    (name, arg_ts)
  in
  match repr t with
  | TAlias (_, _, u) -> ctors_of_type tenv ctor_env u   (* `repr` strips it *)
  | TBool -> [("true", []); ("false", [])]
  | TUnit -> [("()", [])]
  | TTuple ts -> [("(tuple)", ts)]
  | TList elem_t -> [("[]", []); ("::", [elem_t; TList elem_t])]
  | TResult _ ->
    List.filter_map (fun name ->
      match builtin_result_scheme name with
      | Some s -> Some (via_scheme name s)
      | None -> None
    ) ["Ok"; "Error"]
  | TMap _ -> []  (* partial by design (README) -- never flagged as non-exhaustive *)
  | TName _ | TApp _ ->
    (match app_head_name t with
     | Some tname ->
       (match List.assoc_opt tname tenv with
        | Some (Variants (_, _, ctors)) ->
          List.map (fun c ->
            match List.assoc_opt c.name ctor_env with
            | Some s -> via_scheme c.name s
            | None -> (c.name, [])
          ) ctors
        | Some (Alias _) | None -> [])
     | None -> [])
  | TVar _ -> []  (* still unresolved -- shape unknown, can't check, never flagged *)
  | TInt | TFloat | TString | TPath | TGlob | TDateTime
  | TDuration | TURL | TIPv4 | TCIDR | TPort | TVersion | TSize
  | TRegex | TJson | TToml | TYaml | TCommand | TFun _ | TResource _ | TStream _
  | TDecoder _ | TIface _ | TModule _ ->
    []  (* infinite/opaque domains: only a wildcard row can cover these *)

let is_infinite_domain t =
  match repr t with
  | TApp _ when app_head_name t <> None -> false
  | TVar _ -> false  (* unresolved -- handled as "unchecked" via ctors_of_type = [] *)
  | TInt | TFloat | TString | TPath | TGlob | TDateTime
  | TDuration | TURL | TIPv4 | TCIDR | TPort | TVersion | TSize
  | TRegex | TJson | TToml | TYaml | TCommand | TFun _ | TApp _ | TResource _
  | TStream _ | TDecoder _ -> true
  | _ -> false

(* Does pattern p match constructor `name` (of the given arity)? Returns
   the sub-patterns to specialize with if so. PConstrNamed/PMap match
   their own constructor as fully covered without recursing into named
   fields (a deliberate, conservative simplification: this can miss a
   genuinely non-exhaustive nested pattern inside a named field, but never
   produces a false "exhaustive" claim for the outer constructor itself). *)
let rec match_against_ctor ?(tenv = []) name arity (p : pat) =
  let ctor_of n = ctor_name_for tenv n in
  match p with
  | _ when is_wild_pat p -> `Wildcard
  | PAnnot (inner, _) -> match_against_ctor ~tenv name arity inner
  | Bool b -> if (b && name = "true") || (not b && name = "false") then `Match [] else `NoMatch
  | Unit -> `Match []
  | PTuple ps -> `Match ps
  | PList [] -> if name = "[]" then `Match [] else `NoMatch
  | PList (hd :: tl) -> if name = "::" then `Match [hd; PList tl] else `NoMatch
  | PCons (hp, tp) -> if name = "::" then `Match [hp; tp] else `NoMatch
  | PConstr (n, ps) -> if ctor_of n = name then `Match ps else `NoMatch
  | PConstrNamed (n, _) ->
    if ctor_of n = name then `Match (List.init arity (fun _ -> Wild))
    else `NoMatch
  | PConstrBare (n, _) ->
    if ctor_of n = name then `Match (List.init arity (fun _ -> Wild))
    else `NoMatch
  (* Which module it was reached through does not change which constructor
     it is -- but it does say where to look the name up. The module's types
     go in front, the way `infer_pat` reads the same pattern, so an alias
     written `M.Response(...)` forwards to the constructor it names instead
     of standing for a constructor of its own and covering nothing. *)
  | PQualified (m, p) -> match_against_ctor ~tenv:(snd (module_first tenv m)) name arity p
  | _ -> `NoMatch  (* literal patterns never arise for finite-ctor types *)

type witness = Witness of string * witness list

let rec render_witness (Witness (name, args) : witness) : string =
  match name, args with
  | _, [] -> name
  | "(tuple)", args -> "(" ^ String.concat ", " (List.map render_witness_arg args) ^ ")"
  (* A list pattern is written `[h :: t]` -- cons is `::`, and the brackets
     are part of the pattern. This printed `h : t`, so the case the message
     told you to add was one the parser rejects with "cons is '::' -- a
     single ':' gives a name a type". A message that cannot be copied is
     worse than no message. *)
  | "::", [h; t] -> "[" ^ render_witness_arg h ^ " :: " ^ render_witness_arg t ^ "]"
  | name, args -> name ^ " " ^ String.concat " " (List.map render_witness_arg args)
and render_witness_arg (Witness (name, args) as w : witness) : string =
  match name, args with
  | _, [] -> name
  (* Already bracketed, so it is an atom and parentheses around it would be
     a second, wrong way of writing it. *)
  | "::", [_; _] -> render_witness w
  | _ -> "(" ^ render_witness w ^ ")"

(* A multi-equation definition desugars to a match over synthetic `_p0.._pN`
   parameters (parser.ml's collapse_multi_equation). That exact shape is what
   distinguishes it from a match the author actually wrote, and it decides
   both how failures are phrased and whether unreachable cases are rejected --
   an unreachable case in a hand-written match can be deliberate, but a dead
   equation is always a mistake, since nothing about the definition hints
   that an earlier line already answered for it. *)
let strip_located = Ast.strip_located

(* An application taken apart into the function at its head and its
   arguments in written order. `f a b` parses as two nested applications, so
   the arguments are only reachable one at a time on the way down; a checker
   that wants to look at all of them together has to put the spine back
   together first.

   Each argument carries the position of the application node that supplies
   it -- the position `infer` would have promoted an error to, had it walked
   down through that node instead. *)
let rec flatten_app e acc =
  match e with
  | Located (l, App (g, a)) -> flatten_app g ((Some l, a) :: acc)
  | App (g, a)              -> flatten_app g ((None, a) :: acc)
  | _                       -> (e, acc)

let is_fn e = match strip_located e with Fn _ -> true | _ -> false

let rec any_lambda = function
  | [] -> false
  | (_, a) :: rest ->
    (match strip_located a with Fn _ -> true | _ -> any_lambda rest)

(* Whether a lambda is written before the last argument. That is the only
   arrangement the two-pass path exists for: a lambda in last place already
   has every other argument behind it -- unless a pipe is about to put one
   after it, which is why `|>` asks `any_lambda` instead. *)
let rec lambda_before_last = function
  | [] | [_] -> false
  | (_, a) :: rest ->
    (match strip_located a with Fn _ -> true | _ -> lambda_before_last rest)

let is_equation_group (scrutinee : expr) (arity : int) =
  let synthetic i = Printf.sprintf "_p%d" i in
  match strip_located scrutinee with
  | Var v when arity = 1 -> v = synthetic 0
  | Tuple vs ->
    List.length vs = arity &&
    List.for_all2 (fun v i -> match strip_located v with
      | Var name -> name = synthetic i
      | _ -> false) vs (List.init arity (fun i -> i))
  | _ -> false

(* Returns None if exhaustive, or Some human-readable witness pattern
   naming one uncovered case. *)
let check_exhaustive tenv (scrutinee_t : typ) (pats : pat list) : string option =
  let ctor_env = tenv_to_ctor_env tenv in
  (* go types matrix: None if `matrix` covers every value of `types`,
     else Some witnesses -- one witness per column in `types`, describing
     one concrete uncovered combination. *)
  let rec go (types : typ list) (matrix : pat list list) : witness list option =
    match types with
    | [] -> if matrix = [] then Some [] else None
    | t :: rest_types ->
      if matrix <> [] && List.for_all (fun row -> is_wild_pat (List.hd row)) matrix then begin
        let default = List.map (fun row -> List.tl row) matrix in
        match go rest_types default with
        | None -> None
        | Some rest_w -> Some (Witness ("_", []) :: rest_w)
      end else if is_infinite_domain t then begin
        if List.exists (fun row -> is_wild_pat (List.hd row)) matrix then begin
          let default = List.filter_map (fun row ->
            if is_wild_pat (List.hd row) then Some (List.tl row) else None) matrix in
          match go rest_types default with
          | None -> None
          | Some rest_w -> Some (Witness ("_", []) :: rest_w)
        end else
          Some (Witness ("_", []) :: List.map (fun _ -> Witness ("_", [])) rest_types)
      end else begin
        let ctors = ctors_of_type tenv ctor_env t in
        match ctors with
        | [] -> None  (* TMap, or an unresolved type var: nothing to check *)
        | _ ->
          List.find_map (fun (cname, arg_ts) ->
            let arity = List.length arg_ts in
            let specialized = List.filter_map (fun row ->
              match row with
              | [] -> None
              | h :: tl ->
                match match_against_ctor ~tenv cname arity h with
                | `Match subs -> Some (subs @ tl)
                | `Wildcard -> Some (List.init arity (fun _ -> Wild) @ tl)
                | `NoMatch -> None
            ) matrix in
            if specialized = [] then
              Some (Witness (cname, List.init arity (fun _ -> Witness ("_", [])))
                    :: List.map (fun _ -> Witness ("_", [])) rest_types)
            else
              match go (arg_ts @ rest_types) specialized with
              | None -> None
              | Some all_w ->
                let rec split n xs =
                  if n <= 0 then ([], xs)
                  else match xs with
                    | [] -> ([], [])
                    | x :: xs' -> let (a, b) = split (n - 1) xs' in (x :: a, b)
                in
                let (this_ctor_args, rest_w) = split arity all_w in
                Some (Witness (cname, this_ctor_args) :: rest_w)
          ) ctors
      end
  in
  match go [scrutinee_t] (List.map (fun p -> [p]) pats) with
  | None -> None
  | Some [] -> None
  | Some (w :: _) -> Some (render_witness w)

(* ── Expression inference ─────────────────────────────────────────────────── *)

let is_import_expr e = match strip_located e with ImportExpr _ -> true | _ -> false

let rec infer tenv (env : env) (e : expr) : typ =
  match e with
  | Int _      -> TInt
  | Float _    -> TFloat
  | String _   -> TString
  | Bool _     -> TBool
  | Unit       -> TUnit
  | Path _     -> TPath
  | Glob _     -> TGlob
  | DateTime _ -> TDateTime
  | Duration _ -> TDuration
  | URL _      -> TURL
  | IPv4 _     -> TIPv4
  | CIDR _     -> TCIDR
  | Port _     -> TPort
  | Version _  -> TVersion
  | Size _     -> TSize
  | Var name ->
    (match List.assoc_opt name env with
     | Some s -> instantiate s
     | None ->
       if not (Hashtbl.mem unbound_seen name) then begin
         Hashtbl.add unbound_seen name ();
         unbound_names := !unbound_names @ [(!cur_loc, unbound_message name env)]
       end;
       fresh ())
  (* A module used as a value. A stdlib module's name is uppercase, so it
     arrives here rather than as a variable -- `biggest Int 3 7` passes the
     `Int` module. Only where nothing declares a constructor of the name, so
     a constructor is never shadowed by a module. *)
  (* The environment lookup first: almost every `Constr` is a constructor and
     no module, so the cheap test is what should decide. Written the other way
     round, every constructor in every program paid two `find_ctor_in_tenv`
     calls -- the guard's, then the ordinary case's. *)
  | Constr name when (match List.assoc_opt name env with
                      | Some (Namespace _) -> true | _ -> false)
                  && find_ctor_in_tenv tenv name = None ->
    (match List.assoc_opt name env with
     | Some (Namespace (_, claims)) -> TModule claims
     | _ -> assert false)
  | Constr name ->
    let ctor_env = tenv_to_ctor_env tenv in
    (* A constructor with named fields is built by naming them. Supplying its
       fields positionally is silently wrong whenever two of them share a
       type, since reordering still typechecks. *)
    (match find_ctor_in_tenv tenv name with
     | Some (_, ctor)
       when List.exists (fun (fname, _) -> Option.is_some fname) ctor.fields ->
       (* Only the fields that have to be given: one with a default is not
          part of what the construction has to say. *)
       let names =
         List.filter_map (fun (fname, _) ->
           match fname with
           | Some n when not (List.mem_assoc n ctor.defaults) -> Some n
           | _ -> None) ctor.fields
       in
       raise (TypeError (Printf.sprintf
         "constructor '%s' has named fields; construct it as %s(%s)"
         name name
         (String.concat ", " (List.map (fun n -> n ^ " = ...") names))))
     | _ -> ());
    (match List.assoc_opt name ctor_env with
     | Some s -> instantiate s
     | None   ->
       (match name with
        | "Ok"    -> let e = fresh () in let t = fresh () in t @-> TResult (e, t)
        | "Error" -> let e = fresh () in let t = fresh () in e @-> TResult (e, t)
        | _ when List.mem name stdlib_module_names ->
          (* The fix is the line the file is missing, so it travels with the
             finding: `wand t --fix` writes it, and the editor offers it. *)
          pending_fix := Some (Diag.InsertLine ("import " ^ name));
          raise (TypeError (Printf.sprintf
            "did you forget to import the standard library %s?" name))
        | _ when List.mem_assoc name moved_module_names ->
          raise (TypeError (Printf.sprintf
            "'%s' is not a module: %s" name
            (List.assoc name moved_module_names)))
        (* A type with one constructor names that constructor too. So a name
           given to such a type -- by an alias, or by renaming it on import --
           builds one, and the rename is whole rather than half. A type with
           several has no single constructor to forward. *)
        | _ when ctor_name_for tenv name <> name ->
          let cname = ctor_name_for tenv name in
          (match List.assoc_opt cname ctor_env with
           | Some sch -> instantiate sch
           | None -> raise (TypeError (Printf.sprintf
               "'%s' is a type, not a value" name)))
        (* A name that is a type rather than a constructor is not unknown,
           and calling it so sends the reader looking for a declaration that
           is right there. An alias in particular has no constructor. *)
        | _ when List.mem_assoc name tenv ->
          raise (TypeError (Printf.sprintf
            "'%s' is a type, not a value%s" name
            (match List.assoc name tenv with
             | Alias _ -> "; it is an alias, so build the type it names"
             | Variants (_, _, [c]) -> Printf.sprintf "; its constructor is '%s'" c.name
             | Variants _ -> "")))
        | _ ->
          raise (TypeError (Printf.sprintf "unknown constructor '%s'%s%s"
            name (module_only_hint tenv name)
            (Util.hint name (List.map fst ctor_env))))))
  (* $NAME reads the environment. *)
  | EnvVar _ -> performs (Effect_set.single Effect_set.Env); TString
  | Hole ->
    let t = fresh () in
    holes := t :: !holes;
    t
  | UnOp ("-", e) ->
    let n = fresh_num () in
    unify (infer tenv env e) n; n
  | UnOp ("!", e) ->
    unify_expected ~expected:TBool ~got:(infer tenv env e); TBool
  | UnOp (op, _)  -> raise (TypeError (Printf.sprintf "unknown operator '%s'" op))
  | BinOp (op, a, b) -> infer_binop tenv env op a b
  | Fn (params, body) ->
    let (param_ts, env') =
      List.fold_left (fun (ts, env) p ->
        let t = fresh () in
        (ts @ [t], infer_pat tenv p t env)
      ) ([], env) params
    in
    let (body_t, body_effects) = scoped_eff (fun () -> infer tenv env' body) in
    let body_effects =
      if List.exists (pat_is_refutable tenv) params
      then Effect_set.add Effect_set.Raise body_effects
      else body_effects
    in
    (* Only the innermost arrow carries the body's effects: supplying one
       argument of a curried function does nothing until the last one
       arrives. *)
    let rec build = function
      | []      -> body_t
      | [t]     -> TFun (t, body_t, body_effects)
      | t :: tl -> TFun (t, build tl, Effect_set.unknown ())
    in
    build param_ts
  | App (f, x) when nullary_payload tenv x <> None ->
    (* `Hash.string Sha256 (body ++ "b")`. A bracket after a constructor is
       its payload, and the parser attaches one without reading arity so
       that `Ctor (a, b)` means the same thing in every file. A constructor
       that takes no payload does not take the bracket, so it belongs to the
       call around it: this hands it back and infers what was written.

       Nothing that compiles today changes meaning, because a nullary
       constructor is not a function and applying it is always an error.
       Where there is no call around it -- `let x = Red (1)` -- there is
       nowhere to hand the argument, and `check_app_shape` still reports it. *)
    (match nullary_payload tenv x with
     | Some (ctor, arg) -> infer tenv env (App (App (f, ctor), arg))
     | None -> assert false)
  | App (f, x) ->
    check_app_shape tenv env f x;
    (* `f (fn p -> p.host) xs` reads the lambda before `xs` has said what
       `p` is. Put the spine back together and check it in two passes.

       Only an application of an application can have an argument standing
       after a lambda, so a one-argument call -- which most are -- answers
       here without taking anything apart. *)
    let two_pass =
      match strip_located f with
      (* `(fn p -> p.port) pod` -- the argument stands right there, and
         inferring the function before it is what hid it. *)
      | Fn _  -> Some (f, [(None, x)])
      | App _ ->
        let (head, spine) = flatten_app f [(None, x)] in
        if lambda_before_last spine || is_fn head then Some (head, spine)
        else None
      | _ -> None
    in
    (match two_pass with
     | Some (head, spine) -> infer_app_spine tenv env head spine
     | None ->
    let tf = infer tenv env f in
    (match strip_located x with
     | Fn (params, body) ->
       (* Propagate f's expected argument type into a literal lambda
          argument's params before inferring its body, so the body can
          see a concrete (not fresh/unresolved) param type -- needed for
          e.g. field access on the param when its type is otherwise only
          known from how this call site uses it. *)
       let param_ts = List.map (fun _ -> fresh ()) params in
       let body_result_t = fresh () in
       let arg_effects = Effect_set.unknown () in
       let fn_arg_t =
         let rec build = function
           | []      -> body_result_t
           | [t]     -> TFun (t, body_result_t, arg_effects)
           | t :: tl -> TFun (t, build tl, Effect_set.unknown ())
         in
         build param_ts
       in
       let tr = fresh () in
       let latent = Effect_set.unknown () in
       unify_expected ~expected:tf ~got:(TFun (fn_arg_t, tr, latent));
       let env' = List.fold_left2 (fun env p t -> infer_pat tenv p t env) env params param_ts in
       let (body_t, body_effects) = scoped_eff (fun () -> infer tenv env' body) in
    let body_effects =
      if List.exists (pat_is_refutable tenv) params
      then Effect_set.add Effect_set.Raise body_effects
      else body_effects
    in
       unify body_t body_result_t;
       (* The function being applied says what its argument may perform;
          the lambda written at the call site is what performs it. *)
       (try Effect_set.unify arg_effects body_effects
        with
        | Effect_set.Mismatch msg -> raise (TypeError msg)
        | Effect_set.Conflict (a, b) ->
          raise (TypeError (effects_conflict_message
                              ~expected:"the parameter" ~got:"the function given"
                              a b)));
       performs latent;
       tr
     | _ ->
       let tx = infer tenv env x in
       let tr = fresh () in
       let latent = Effect_set.unknown () in
       unify_expected ~expected:tf ~got:(TFun (tx, tr, latent));
       performs latent;
       tr))
  | Let (p, e1, e2, _) ->
    (match p, e1 with
     | PVar name, Fn _ ->
       let placeholder = fresh () in
       let env_rec = (name, Mono placeholder) :: env in
       let t1 = with_current_fn name (fun () -> infer tenv env_rec e1) in
       unify placeholder t1;
       record_local name t1;
       infer tenv ((name, generalize env t1) :: env) e2
     | _ ->
       let t1     = infer tenv env e1 in
       (match p, e1 with
        | Wild, Located (loc, _) ->
          wild_let_types := (loc, t1) :: !wild_let_types
        | _ -> ());
       (* A binding whose pattern can fail raises where it stands, the same
          way a parameter's does -- `let Ok v = r in ...` has nothing to do
          with an Error but raise. *)
       if pat_is_refutable tenv p then performs (Effect_set.single Effect_set.Raise);
       let scheme = generalize env t1 in
       infer tenv (infer_pat_let tenv p t1 scheme env) e2)
  | LetRec (bindings, e2, _) ->
    let placeholders = List.map (fun (name, _, _) -> (name, fresh ())) bindings in
    let env_rec = List.map (fun (name, t) -> (name, Mono t)) placeholders @ env in
    let inferred = List.map (fun (name, params, body) ->
      let t = with_current_fn name (fun () -> infer tenv env_rec (Fn (params, body))) in
      unify (List.assoc name placeholders) t;
      record_local name t;
      (name, t)
    ) bindings in
    let env' = List.map (fun (name, t) -> (name, generalize env t)) inferred @ env in
    infer tenv env' e2
  | If (cond, then_, else_) ->
    unify_expected ~expected:TBool ~got:(infer tenv env cond);
    let tt = infer tenv env then_ in
    (* An `if` the parser completed for us -- one written without an `else` --
       has a bare `Unit` for its second branch, where one that was written
       carries a location. Worth telling apart only to explain the `Unit`:
       "expected Unit, got Int" is true but says nothing about the `else`
       that is not there. *)
    (match else_ with
     | Unit ->
       (try unify tt TUnit with
        | TypeError _ ->
          raise (TypeError (Printf.sprintf
            "an `if` with no `else` does nothing when the condition is false, so its branch must be Unit -- this one is %s" (string_of_typ tt))))
     | _ -> unify_expected ~expected:tt ~got:(infer tenv env else_));
    tt
  | Match (scrutinee, cases) ->
    let ts       = infer tenv env scrutinee in
    let result_t = fresh () in
    List.iter (fun (p, guard, body) ->
      let env' = infer_pat tenv p ts env in
      (match guard with
       | None   -> ()
       | Some g -> unify_expected ~expected:TBool ~got:(infer tenv env' g));
      unify_expected ~expected:result_t ~got:(infer tenv env' body)
    ) cases;
    let unguarded = List.filter_map (fun (p, guard, body) ->
      match guard with None -> Some (p, body) | Some _ -> None) cases in
    let unguarded_pats = List.map fst unguarded in
    (* Phrase failures as equations when this match is a desugared
       multi-equation definition -- `_p0` means nothing to its author. *)
    let arity = match strip_located scrutinee with
      | Tuple vs -> List.length vs
      | _ -> 1
    in
    let as_equations = is_equation_group scrutinee arity in
    let fn_desc = match !current_fn with
      | Some n -> Printf.sprintf " for '%s'" n
      | None   -> ""
    in
    if as_equations then begin
      (* An equation that no value can reach is dead code the author cannot
         see: source order decides, so an earlier equation already answered
         for everything this one names. *)
      let n = List.length unguarded_pats in
      let rec find_dead i =
        if i >= n then None
        else
          let prefix = List.filteri (fun j _ -> j < i) unguarded_pats in
          if prefix <> [] && check_exhaustive tenv ts prefix = None then Some i
          else find_dead (i + 1)
      in
      (match find_dead 1 with
       | Some i ->
         (* The equation as it was written -- `f 1`, not `equation 2`, which
            leaves the reader counting lines. Its location comes from the
            `Located` the parser wrapped the body in when it folded the
            equations into this match. *)
         let (dead_pat, dead_body) = List.nth unguarded i in
         let params = match dead_pat with
           | PTuple ps when arity > 1 -> ps
           | p -> [p]
         in
         let written = String.concat " " (List.map Formatter.emit_pat_atom params) in
         let named = match !current_fn with
           | Some n -> Printf.sprintf "%s %s" n written
           | None   -> written
         in
         fail_at_opt (loc_of_expr dead_body) (Printf.sprintf
           "equation '%s' is unreachable — an earlier equation already \
            matches every remaining case" named)
       | None -> ())
    end;
    (match check_exhaustive tenv ts unguarded_pats with
     | None -> ()
     | Some witness ->
       if as_equations then
         (* No one equation is at fault -- the gap is what they leave between
            them -- so this points at the first, where the definition starts.
            A hand-written match needs no help: it sits inside a `Located` of
            its own, and the position of the `match` is already the answer. *)
         let group_loc = match cases with
           | (_, _, body) :: _ -> loc_of_expr body
           | []               -> None
         in
         fail_at_opt group_loc (Printf.sprintf
           "the equations%s do not cover every case, e.g. %s" fn_desc witness)
       else
         raise (TypeError (Printf.sprintf
           "non-exhaustive match: missing case, e.g. %s" witness)));
    result_t
  | Tuple es -> TTuple (List.map (infer tenv env) es)
  | List []        -> TList (fresh ())
  | List (e :: rest) ->
    let t = infer tenv env e in
    (* The first element sets the type of the list; a later one that differs
       is the one to report. *)
    List.iter (fun e' ->
      unify_expected ~expected:t ~got:(infer tenv env e')) rest;
    TList t
  (* Settled here, where the declaration is, exactly as the pattern side
     settles `PConstrBare`. *)
  | Qualified (m, inner) ->
    let (own, tenv') = module_first tenv m in
    if own = [] then
      raise (TypeError (Printf.sprintf
        "'%s' declares no types, so '%s' names nothing in it"
        m (Ast.show inner)));
    with_visible (List.map fst own) (fun () -> infer tenv' env inner)
  | ConstrBare (name, ids) ->
    let named_fields =
      match find_ctor_in_tenv tenv (ctor_name_for tenv name) with
      | Some (_, ctor) -> List.exists (fun (dn, _) -> dn <> None) ctor.fields
      | None -> false
    in
    infer tenv env (Ast.constr_bare_construction ~named_fields name ids)
  | ConstrApp (name, fields, _) ->
    let name = ctor_name_for tenv name in
    (match find_ctor_in_tenv tenv name with
     (* A name that is a type rather than a constructor is not unknown, and
        saying so sends the reader looking for a declaration that is right
        there. An alias in particular has no constructor of its own. *)
     | None when List.mem_assoc name tenv ->
       raise (TypeError (Printf.sprintf
         "'%s' is a type, not a value%s" name
         (match List.assoc name tenv with
          | Alias (_, _, _) ->
            Printf.sprintf "; it is an alias, so build the type it names"
          | Variants (_, _, ctors) ->
            (match ctors with
             | [c] -> Printf.sprintf "; its constructor is '%s'" c.name
             | _ -> ""))))
     | None -> raise (TypeError (Printf.sprintf "unknown constructor '%s'%s%s"
         name (module_only_hint tenv name)
         (Util.hint name (List.map fst (tenv_to_ctor_env tenv)))))
     | Some (tname, ctor) ->
       (* Types come from the constructor's own scheme rather than from each
          field expression on its own, so a generic type keeps its arguments:
          `Box(v = 3)` is a `Box Int`, exactly as `Box 3` is. Converting each
          field type separately gave every one an unrelated variable and left
          the result unapplied. *)
       let arg_ts, result_t =
         match List.assoc_opt name (tenv_to_ctor_env tenv) with
         | Some sch -> unwrap_ctor_type (instantiate sch)
         | None -> ([], TName tname)
       in
       let field_type fname =
         let rec index i = function
           | [] -> None
           | (dn, _) :: rest -> if dn = Some fname then Some i else index (i + 1) rest
         in
         match index 0 ctor.fields with
         | Some i -> List.nth_opt arg_ts i
         | None -> None
       in
       List.iter (fun (fname_opt, e) ->
         match fname_opt with
         | None -> raise (TypeError "positional field in named construction")
         | Some fname ->
           (match List.find_opt (fun (dn, _) -> dn = Some fname) ctor.fields with
            | None -> raise (TypeError (Printf.sprintf
                "constructor '%s' has no field '%s'%s"
                name fname (Util.hint fname (List.filter_map Fun.id (List.map fst ctor.fields)))))
            | Some (_, te) ->
              let expected =
                match field_type fname with Some t -> t | None -> type_of_te te
              in
              unify (infer tenv env e) expected)
       ) fields;
       (* The loop above checks that each field given is declared. The other
          direction was checked only when the value was built, so a
          construction missing a field typechecked and failed at runtime --
          a positional constructor's arity has always been static, and a
          named one's is no less known. *)
       let given = List.filter_map fst fields in
       (* A field with a default is not missing: leaving it out is what the
          default is for. *)
       let missing =
         List.filter_map (fun (dn, _) ->
           match dn with
           | Some n when not (List.mem n given)
                      && not (List.mem_assoc n ctor.defaults) -> Some n
           | _ -> None) ctor.fields
       in
       if missing <> [] then
         raise (TypeError (Printf.sprintf
           "constructor '%s' is missing field%s %s"
           name
           (if List.length missing = 1 then "" else "s")
           (String.concat ", " (List.map (fun n -> "'" ^ n ^ "'") missing))));
       result_t)
  (* `T(r, b = 3)`: `r` is a `T` already, so the fields not named keep what
     it holds. Only the named ones are checked, which is the whole
     difference from a construction -- that has to name every field. *)
  | ConstrUpdate (name, base, fields, _) ->
    (* A type with one constructor names that constructor too, the same as
       construction reads it a few lines up. Without this an update through
       an alias -- `HTTP.Request(base, redirects = 0)` -- reported an
       unknown constructor for a type that builds perfectly well. *)
    let name = ctor_name_for tenv name in
    (match find_ctor_in_tenv tenv name with
     | None -> raise (TypeError (Printf.sprintf "unknown constructor '%s'%s%s"
         name (module_only_hint tenv name)
         (Util.hint name (List.map fst (tenv_to_ctor_env tenv)))))
     | Some (tname, ctor) ->
       let arg_ts, result_t =
         match List.assoc_opt name (tenv_to_ctor_env tenv) with
         | Some sch -> unwrap_ctor_type (instantiate sch)
         | None -> ([], TName tname)
       in
       let field_type fname =
         let rec index i = function
           | [] -> None
           | (dn, _) :: rest -> if dn = Some fname then Some i else index (i + 1) rest
         in
         match index 0 ctor.fields with
         | Some i -> List.nth_opt arg_ts i
         | None -> None
       in
       (* The base decides the type arguments, so it is unified before the
          fields are: `Box(b, v = 3)` takes its element type from `b`. *)
       (* A name before a named field is the base of an update, and that
          spelling was taken before puns existed. Somebody who meant the pun
          gets told which reading they wrote and how to get the other. *)
       let base_is_field =
         match strip_located base with
         | Var x -> List.exists (fun (dn, _) -> dn = Some x) ctor.fields
         | _ -> false
       in
       (try unify_expected ~expected:result_t ~got:(infer tenv env base)
        with TypeError why when base_is_field ->
          let x = match strip_located base with Var x -> x | _ -> "" in
          let other =
            match List.filter_map (fun (fname, _) ->
              if fname = x then None else Some fname) fields with
            | f :: _ -> f
            | [] -> "field"
          in
          raise (TypeError (Printf.sprintf
            "%s -- '%s' here is the record being updated, not a field. \
             Write '%s(%s = ..., %s)' to pun it"
            why x name other x)));
       let seen = ref [] in
       List.iter (fun (fname, e) ->
         if List.mem fname !seen then
           raise (TypeError (Printf.sprintf
             "field '%s' is given twice" fname));
         seen := fname :: !seen;
         match List.find_opt (fun (dn, _) -> dn = Some fname) ctor.fields with
         | None -> raise (TypeError (Printf.sprintf
             "constructor '%s' has no field '%s'%s"
             name fname (Util.hint fname (List.filter_map Fun.id (List.map fst ctor.fields)))))
         | Some (_, te) ->
           let expected =
             match field_type fname with Some t -> t | None -> type_of_te te
           in
           unify (infer tenv env e) expected
       ) fields;
       result_t)
  (* `Apps.Deployment.decoder`: a type named through its module. The derived
     members are read off the declaration, and the declaration is the
     module's, so it is looked up with that module's types in scope and under
     the canonical name -- two modules may each declare a `Deployment`, and
     the short name does not say which. Only the derived members take this
     path; every other label reads as the field it is. *)
  | Field (e, label)
    when (match qualified_type_head e with
          | Some _ ->
            List.mem label ["decoder"; "encoder"; "usage"; "parser";
                            "spec"; "reader"]
          | None -> false) ->
    let (m, tname) = Option.get (qualified_type_head e) in
    let (own, tenv') = module_first tenv m in
    let canon = canonical_type_name (m ^ "." ^ tname) in
    with_visible (List.map fst own)
      (fun () -> infer tenv' env (Field (Constr canon, label)))
  | Field (e, label) ->
    (* Namespace access: Ns.member — check before falling into regular field inference *)
    let lookup_ns ns_name =
      match List.assoc_opt ns_name env with
      | Some (Namespace (ns_env, _)) ->
        Some (match List.assoc_opt label ns_env with
          | Some s -> instantiate s
          | None ->
            let hint = match foreign_member_hint ns_name label with
              | Some h -> " -- " ^ h
              | None ->
                (match Util.hint label (List.map fst ns_env) with
                 | "" -> Printf.sprintf
                     " -- 'wand d %s' lists its members" ns_name
                 | h -> h)
            in
            raise (TypeError (Printf.sprintf
              "namespace '%s' has no member '%s'%s" ns_name label hint)))
      | _ -> None
    in
    let ns_result = match strip_located e with
      | Constr ns_name | Var ns_name -> lookup_ns ns_name
      | _ -> None
    in
    (* `Pod.decoder`, when `Pod` is a type rather than a module. Checked after
       the namespace lookup, so a module of the same name keeps its member. *)
    let derived = match ns_result, strip_located e, label with
      (* `Opts.usage`: what a command line reading this type looks like, and
         `Opts.flags`: what its own text cannot say -- which take no value,
         and which collect. Neither takes a decoder per parameter the way
         `decoder` does: both are facts about the declaration rather than
         readers built from it. *)
      (* Both were public until 0.49.0, and `Args.read` took them side by
         side. Named here so the change says what to write rather than
         falling through to a message about constructing the type. *)
      | None, Constr tname, ("spec" | "reader")
        when List.mem_assoc (canonical_type_name tname) tenv ->
        raise (TypeError (Printf.sprintf
          "'spec' and 'reader' are one member now: write '%s.parser', which \
           holds both and the usage line" (short_type_name tname)))
      | None, Constr tname, (("usage" | "parser") as which) ->
        (match List.assoc_opt (canonical_type_name tname) tenv with
         | Some tdef ->
           let refuse why =
             raise (TypeError (Printf.sprintf
               "type '%s' has no derived %s: %s"
               (short_type_name tname) which why))
           in
           (match derivable_typedef tenv [tname] tdef with
            | Error why -> refuse why
            | Ok () ->
              (* All three read a command line, so all three need the type to
                 describe one. *)
              (match tdef with
               | Variants (_, params, [ctor]) ->
                 (match cmdline_shape tenv ctor with
                  | Error why -> refuse why
                  | Ok _ ->
                    if which = "parser" && params <> [] then
                      refuse "a command line is not generic, and this type \
                              takes a parameter";
                    Some (match which with
                          | "usage" -> TString
                          | _ ->
                            TApp (TName "CommandLine",
                                  TName (canonical_type_name tname))))
               | _ -> refuse "it has no fields"))
         | None -> None)
      | None, Constr tname, (("decoder" | "encoder") as which) ->
        (match List.assoc_opt (canonical_type_name tname) tenv with
         | Some tdef ->
           (match derivable_typedef tenv [tname] tdef with
            | Ok () ->
              (* A generic type takes one decoder per parameter, in the order
                 it declares them: `Box.decoder : Decoder 'a -> Decoder
                 (Box 'a)`. With no parameters there is nothing to take, and
                 it is a decoder outright. *)
              let params =
                match tdef with Variants (_, ps, _) | Alias (_, ps, _) -> ps in
              let vars = List.map (fun _ -> fresh ()) params in
              let applied =
                List.fold_left (fun acc v -> TApp (acc, v))
                  (TName (canonical_type_name tname)) vars
              in
              (* An encoder is an ordinary function: encoding cannot fail, so
                 there is nothing for a type of its own to carry. *)
              let result =
                if which = "decoder" then TDecoder applied
                else TFun (applied, TJson, Effect_set.pure)
              in
              Some (List.fold_right (fun v acc ->
                let arg =
                  if which = "decoder" then TDecoder v
                  else TFun (v, TJson, Effect_set.pure)
                in
                TFun (arg, acc, Effect_set.pure)) vars result)
            | Error why ->
              raise (TypeError (Printf.sprintf
                "type '%s' has no derived %s: %s"
                (short_type_name tname) which why)))
         | None -> None)
      | _ -> None
    in
    (match (match derived with Some _ -> derived | None -> ns_result) with
     | Some t -> t
     | None ->
       (* A generic type reaches here applied to its arguments, so the head
          is what names the type and the arguments say what its parameters
          stand for: the `v` of a `Box Int` is an `Int`. *)
       let rec head_and_args t = match repr t with
         | TApp (f, a) -> let (h, args) = head_and_args f in (h, args @ [a])
         | other -> (other, [])
       in
       let scrut_t = infer tenv env e in
       (match head_and_args scrut_t with
        (* A parameter annotated with an interface is a module, and its
           members are the ones the interface declares, at the types the
           annotation bound them to. Nothing is dispatched: the member's
           type is read off the contract. *)
        | (TIface (iname, iargs), _) ->
          (match Hashtbl.find_opt iface_defs iname with
           | None -> raise (TypeError (Printf.sprintf
               "'%s' is not an interface in scope" iname))
           | Some idef ->
             let members = iface_members idef iargs in
             (match List.assoc_opt label members with
              | Some t -> t
              | None -> raise (TypeError (Printf.sprintf
                  "'%s' declares no member '%s'%s" iname label
                  (Util.hint label (List.map fst members))))))
        | (TName tname, args) ->
          (match List.assoc_opt tname tenv with
           | Some (Variants (_, params, ctors)) ->
             let bound =
               try List.combine params args with Invalid_argument _ -> []
             in
             let named c =
               List.filter_map (fun (fname, te) ->
                 match fname with Some n -> Some (n, te) | None -> None)
               c.fields
             in
             (* A value of this type is one of its constructors, and which
                one is not known here -- so a field is only readable if every
                constructor carries it. Accepting a field that only some have
                let `v.x` typecheck on a `B` that has no `x` and fail at
                runtime instead. *)
             let has, lacks =
               List.partition (fun c -> List.mem_assoc label (named c)) ctors
             in
             (match has, lacks with
              | [], _ ->
                let names =
                  List.concat_map (fun c -> List.map fst (named c)) ctors in
                raise (TypeError (Printf.sprintf "type '%s' has no field '%s'%s"
                  tname label (Util.hint label names)))
              | _, (_ :: _) ->
                raise (TypeError (Printf.sprintf
                  "field '%s' is not on every constructor of '%s': %s %s it, \
                   so which constructor a value holds decides whether '%s' \
                   is there. Match on the constructor instead"
                  label tname
                  (String.concat ", " (List.map (fun c -> c.name) lacks))
                  (if List.length lacks = 1 then "does not have" else "do not have")
                  label))
              | c0 :: rest, [] ->
                (* From the constructor's own scheme, tied to this value's
                   type arguments -- not by converting the written field type
                   again. A fresh conversion gives a function-typed field a
                   brand-new effect variable, so what construction learned
                   about it is gone by the time it is read back out, and
                   `t.raises (fn () -> $(cmd))` typechecked under `uses {}`.
                   Falls back to the written type for a constructor with no
                   scheme, which is what the builtin types have. *)
                let field_t c =
                  let written () =
                    type_of_te_bound bound (List.assoc label (named c)) in
                  match List.assoc_opt c.name (tenv_to_ctor_env tenv) with
                  | None -> written ()
                  | Some sch ->
                    let (arg_ts, result_t) = unwrap_ctor_type (instantiate sch) in
                    (* Binds the scheme's parameters to this value's own. The
                       scrutinee is already known to be this type, so a
                       failure here is a bug rather than a user error. *)
                    unify result_t scrut_t;
                    let rec index i = function
                      | [] -> None
                      | (dn, _) :: rest ->
                        if dn = Some label then Some i else index (i + 1) rest
                    in
                    (match index 0 c.fields with
                     | Some i -> (match List.nth_opt arg_ts i with
                                  | Some t -> t
                                  | None -> written ())
                     | None -> written ())
                in
                let t0 = field_t c0 in
                (* Every constructor's `x` has to be one type, or the type of
                   `v.x` would depend on which constructor v holds. *)
                List.iter (fun c ->
                  let t = field_t c in
                  try unify t0 t with TypeError _ ->
                    raise (TypeError (Printf.sprintf
                      "field '%s' of '%s' is %s in %s but %s in %s, so its \
                       type depends on the constructor. Match on the \
                       constructor instead"
                      label tname (string_of_typ t0) c0.name
                      (string_of_typ t) c.name))) rest;
                t0)
           | _ -> raise (TypeError (Printf.sprintf
               "cannot access field '%s' on type '%s'" label tname)))
        (* Dot access is checked field access: `p.x` on a named type is
           verified to exist. Key presence in a Map is a runtime question, so
           the same syntax cannot carry the same guarantee -- Map.get returns
           an Option and Map.get! raises, each saying so at the call site. *)
        | (TMap vt, _) -> raise (TypeError (Printf.sprintf
            "cannot use dot access on a Map (Map %s); use Map.get for an \
             Option or Map.get! to raise on a missing key" (string_of_typ vt)))
        | (t, _) ->
          (match repr t with
           (* Nothing has said what this is yet, so the answer is to write
              the type down. The declarations are all here, so the message
              can name the ones that would answer rather than leave the
              reader to search for them. A concrete type that is simply not
              a record falls through to the plain form below: annotating a
              `Size` would not help. *)
           | TVar _ ->
             let owners =
               List.sort_uniq compare
                 (List.filter_map (fun (tname, tdef) ->
                    match tdef with
                    | Variants (_, _, ctors)
                      when List.exists (fun c ->
                             List.exists (fun (f, _) -> f = Some label)
                               c.fields) ctors ->
                      Some (short_type_name tname)
                    | _ -> None) tenv)
             in
             (* A name can carry its type where it stands. Anything else has
                to be bound to one first, so the two say different things. *)
             let subject, write, choose =
               match strip_located e with
               | Var n ->
                 Printf.sprintf "'%s'" n,
                 (fun ty -> Printf.sprintf "write '(%s: %s)'" n ty),
                 Printf.sprintf "write '(%s: T)', naming which" n
               | _ ->
                 "this value",
                 (fun ty -> Printf.sprintf
                    "bind it to a name that carries its type, \
                     'let (x: %s) = ...'" ty),
                 "bind it to a name that carries its type"
             in
             let msg =
               match owners with
               | [one] ->
                 Printf.sprintf
                   "%s needs its type before '.%s' can be read: %s"
                   subject label (write one)
               | [] ->
                 Printf.sprintf
                   "%s needs its type before '.%s' can be read, and no type \
                    declares a field '%s'%s" subject label label
                   (Util.hint label
                      (List.sort_uniq compare
                         (List.concat_map (fun (_, tdef) ->
                            match tdef with
                            | Variants (_, _, ctors) ->
                              List.concat_map (fun c ->
                                List.filter_map fst c.fields) ctors
                            | _ -> []) tenv)))
               | many ->
                 Printf.sprintf
                   "%s needs its type before '.%s' can be read: %s. '%s' is \
                    a field of %s"
                   subject label choose label (String.concat ", " many)
             in
             raise (TypeError msg)
           | _ ->
             raise (TypeError (Printf.sprintf
               "field access requires a named type, got %s"
               (string_of_typ t))))))
  | MapLit [] ->
    TMap (fresh ())
  | MapLit ((_, e0) :: rest) ->
    let t = infer tenv env e0 in
    List.iter (fun (_, e) ->
      unify_expected ~expected:t ~got:(infer tenv env e)) rest;
    TMap t
  (* A command literal runs nothing. It performs `Shell!command` all the
     same, so that the words it names and the effect it carries are read off
     one site: the manifest that has to list `git` is the manifest of the
     file that wrote `$*(git ...)`, whether or not that file ever runs it.
     `Shell` is therefore the one label that means does or describes. *)
  | MkCommand (e, _)  ->
    unify_expected ~expected:TString ~got:(infer tenv env e);
    performs (Effect_set.single Effect_set.Shell);
    TCommand
  (* Shell execution is its own form rather than a call to a builtin, so it
     records its effects here. $() raises on a non-zero exit; $?() hands back
     a ShellResult instead, so it cannot. Each is sugar over a literal and a
     function -- `$(c)` is `Shell.run! $*(c)` -- and so performs what both
     of those perform. *)
  | RunCmd    (e, _)  ->
    unify_expected ~expected:TString ~got:(infer tenv env e);
    performs (Effect_set.of_list [Effect_set.Shell; Effect_set.Raise]);
    TString
  | RunQuery  (e, _)  ->
    unify_expected ~expected:TString ~got:(infer tenv env e);
    performs (Effect_set.single Effect_set.Shell);
    TName "ShellResult"
  | RegexLit  _       -> TRegex
  | ImportExpr _      -> raise (TypeError "import can only appear in a let binding")
  | Handle (body_expr, cases) ->
    let (body_t, body_effects) = scoped_eff (fun () -> infer tenv env body_expr) in
    let handled =
      List.filter_map (function
        | Ast.EffectCase (op, _, _, _) -> Some op
        | Ast.ReturnCase _ -> None) cases
    in
    (* A case for an operation that does not exist used to be accepted in
       silence, and since nothing intercepted it the real effect ran. A
       mistyped mock is the likely way to write one -- `FS!read_fil` -- and
       the cost of getting it wrong is the thing the mock existed to
       prevent. *)
    List.iter (fun op ->
      if effect_of_operation op = None then
        raise (TypeError (Printf.sprintf
          "no effect operation named '%s'%s" op
          (Util.hint op (List.map (fun o -> o.op_name) operations))))) handled;
    (* A case intercepts one operation, but an effect covers several, and it
       is the effect that a signature and a manifest are written in. So an
       effect is discharged only when every operation carrying it is handled:
       handling `Shell!exit_code` alone leaves `Shell!run` to reach the
       default handler and run for real, and a signature that dropped Shell
       there would be describing a program that does not exist.

       Erring towards keeping an effect only over-reports. Erring the other
       way is what let a file whose manifest was `uses {IO}` run any command
       it liked. *)
    let ops_performing e =
      List.filter (fun o -> o.op_effect = e) operations in
    let fully_handled e =
      match ops_performing e with
      (* Raise carries no operations, so "every one is handled" is vacuously
         true of it. `try` is what discharges Raise, not a handler. *)
      | []  -> false
      | ops -> List.for_all (fun o -> List.mem o.op_name handled) ops
    in
    let discharged =
      Effect_set.discharge (List.filter fully_handled Effect_set.all) body_effects
    in
    performs discharged;
    let result_t = fresh () in
    (* Without a `return` case the handler returns what the body returned, so
       the two types are the same. Leaving them apart lost the body's type
       entirely. *)
    if not (List.exists (function Ast.ReturnCase _ -> true | _ -> false) cases)
    then unify result_t body_t;
    List.iter (fun case ->
      match case with
      | Ast.ReturnCase (p, b) ->
        let env' = infer_pat tenv p body_t env in
        unify_expected ~expected:result_t ~got:(infer tenv env' b)
      | Ast.EffectCase (op, arg_pat, cont_name, case_body) ->
        (* The operation says what it carries and what resuming it supplies.
           An operation with no single payload shape says neither, and those
           two stay open, as every case used to be. *)
        let (arg_t, cont_arg_t) =
          match operation_types op with
          | Some (a, r) -> (a, r)
          | None -> (fresh (), fresh ())
        in
        let env' = infer_pat tenv arg_pat arg_t env in
        (* Resuming a handler's continuation runs the rest of the handled
           expression, whose effects the handler is in the middle of
           deciding, so its effects are left to inference. *)
        let cont_t = TFun (cont_arg_t, result_t, Effect_set.unknown ()) in
        let env'' = (cont_name, Mono cont_t) :: env' in
        unify_expected ~expected:result_t ~got:(infer tenv env'' case_body)
    ) cases;
    result_t
  | RawString _ -> TString
  | Interp (parts, _) | RawInterp (parts, _) ->
    List.iter (fun (_, e) -> ignore (infer tenv env e)) parts;
    TString
  | CmdInterp (parts, _) ->
    List.iter (fun (_, e, _) -> ignore (infer tenv env e)) parts;
    TString
  | Seq (a, b) ->
    let ta = infer tenv env a in
    (match a with
     | Located (loc, _) -> seq_discard_types := (loc, ta) :: !seq_discard_types
     | _ -> ());
    infer tenv env b
  | Contract (reqs, ens, body) ->
    if reqs <> [] || ens <> [] then
      performs (Effect_set.single Effect_set.Raise);
    List.iter (fun req ->
      unify_expected ~expected:TBool ~got:(infer tenv env req)) reqs;
    let body_t = infer tenv env body in
    List.iter (fun e ->
      unify_expected ~expected:TBool
        ~got:(infer tenv (("result", Mono body_t) :: env) e)
    ) ens;
    body_t
  | Try e ->
    (* try turns a raise into a Result, so Raise does not escape it -- and
       that holds however the body's effects arrived. Taking the label out
       of the known half removed nothing when they came through a parameter,
       so `let attempt f = try f ()` answered a Result and still said it
       could raise. `discharge` splits the tail the way a handler does:
       whatever the body performs is a raise and a rest, and the rest is
       what escapes. *)
    let (t, effects) = scoped_eff (fun () -> infer tenv env e) in
    performs (Effect_set.discharge [Effect_set.Raise] effects);
    TResult (TString, t)
  | With (resource, p, body) ->
    (* The resource says what acquiring and releasing perform; the bracket
       performs all of it, plus whatever the body does. Nothing here is
       discharged -- a bracket is not a handler, it just guarantees the
       release runs -- so every one is folded into the enclosing scope and
       a file that takes a lock says so in its signature. *)
    let held = fresh () in
    let effects = Effect_set.unknown () in
    unify_expected ~expected:(TResource (effects, held))
      ~got:(infer tenv env resource);
    performs effects;
    let env' = infer_pat tenv p held env in
    infer tenv env' body
  | Annot (te, e) ->
    let (t, written) = type_of_te_bound_with_vars [] te in
    (* A written signature over a lambda says what the parameters are, so the
       parameters are bound to it before the body is read. Inferring the body
       first and unifying after leaves a parameter as a bare variable while
       the body is checked, and `fn b -> b.v` cannot read a field of one:
       `let get : Box 'a -> 'a = fn b -> b.v` said "field access requires a
       named type, got 'a".

       Only as far as the annotation reaches. A lambda with more parameters
       than the annotation has arrows falls back to inferring it whole. *)
    let rec bind_params expected params env =
      match params, repr expected with
      | [], _ -> Some (expected, env)
      | p :: rest, TFun (a, b, _) ->
        bind_params b rest (infer_pat tenv p a env)
      | _ -> None
    in
    (match strip_located e with
     | Fn (params, body) when params <> [] ->
       (match bind_params t params env with
        | None ->
          unify_expected ~expected:t ~got:(infer tenv env e);
          check_written_vars written;
          t
        | Some (result_t, env') ->
          let (body_t, body_effects) =
            scoped_eff (fun () -> infer tenv env' body) in
          let body_effects =
            if List.exists (pat_is_refutable tenv) params
            then Effect_set.add Effect_set.Raise body_effects
            else body_effects
          in
          (* The effects the annotation states are checked against the ones
             the body performs, which is what the unannotated path does when
             the arrow it built meets the annotation. *)
          let rec innermost_eff ty n =
            match repr ty, n with
            | TFun (_, b, eff), 1 -> Some (b, eff)
            | TFun (_, b, _), n -> innermost_eff b (n - 1)
            | _ -> None
          in
          (match innermost_eff t (List.length params) with
           | Some (_, eff) ->
             (try Effect_set.unify eff body_effects
              with Effect_set.Conflict (a, b) ->
                raise (TypeError (effects_conflict_message ~expected:"the type"
                                    ~got:"the body" a b)))
           | None -> ());
          unify_expected ~expected:result_t ~got:body_t;
          check_written_vars written;
          t)
     | _ ->
       (* The annotation is what the reader expects; the body is what they
          wrote. Naming both is what makes `expected 'a -> 'a, got Int ->
          Int` readable at all. *)
       unify_expected ~expected:t ~got:(infer tenv env e);
       check_written_vars written;
       t)
  | Located (loc, e) ->
    let outer = !cur_loc in
    cur_loc := Some loc;
    Fun.protect ~finally:(fun () -> cur_loc := outer)
      (fun () ->
         try infer tenv env e
         with TypeError msg -> raise (TypeErrorAt (loc, msg)))

(* An argument that is a nullary constructor holding a bracket it cannot
   own: `Sha256 (x)` as the argument of a call. Answers the constructor and
   the argument it swallowed, keeping the qualifier where there is one, so
   `Digest.Sha256 (x)` hands back `Digest.Sha256` rather than `Sha256`. *)
and nullary_ctor tenv e =
  match strip_located e with
  | Constr name ->
    (match find_ctor_in_tenv tenv name with
     | Some (_, ctor) -> ctor.fields = []
     | None -> false)
  | _ -> false

and nullary_payload tenv x =
  (* A qualified constructor's arity is declared in its own module, so the
     walk through `Qualified` moves to that module's types the way the
     `Qualified` case of `infer` does. Looking it up in the tenv here would
     answer "not a constructor" and leave `Digest.Sha256 (x)` erroring where
     the bare spelling was corrected. *)
  let rec unpack tenv wrap e =
    match e with
    | Located (_, inner) -> unpack tenv wrap inner
    | Qualified (m, inner) ->
      (match module_first tenv m with
       | (own, tenv') when own <> [] ->
         (* The module's types have to be visible for the lookup, as they
            are inside the `Qualified` case; without this the arity comes
            back unknown and the correction never fires. *)
         with_visible (List.map fst own)
           (fun () -> unpack tenv' (fun c -> wrap (Qualified (m, c))) inner)
       | _ -> None
       | exception _ -> None)
    | App (f, arg) when nullary_ctor tenv f -> Some (wrap f, arg)
    | _ -> None
  in
  unpack tenv (fun c -> c) x

(* The shapes that are a parse away from what was meant, checked before the
   application itself. Each fires only when the head of the spine is a bare
   name or constructor, so one call at the head covers a spine of any
   length. *)
and check_app_shape tenv (env : env) f x =
    (* `Rect (3, 4)` reads naturally but means "apply Rect to a tuple", and
       Rect takes two arguments. The constructor's arity is known here even
       when it was declared in another file, so say what to write. *)
    (match strip_located f, strip_located x with
     (* Parentheses after a constructor are its payload, so a nullary one
        has swallowed an argument meant for the call: `t.eq None (usage row)`
        is `t.eq (None (usage row))`, and the type error that follows is
        about an application nobody wrote. The parser cannot tell -- it
        stopped reading arity on purpose, so that `Ctor (a, b)` means the
        same thing in every file -- but the arity is known here, so say what
        to write. `wand f` already brackets a bare constructor that is not
        the last argument. *)
     | Constr name, _ when
         (match find_ctor_in_tenv tenv name with
          | Some (_, ctor) -> ctor.fields = []
          | None -> false) ->
       (* What is left after `nullary_payload` has handed the bracket back to
          the call around it: a payload with no call to give it to, as in
          `let r = Red (1)`. Bracketing the constructor does not help there
          -- `(Red) (1)` applies it just the same -- so there is no
          correction to carry, only the one thing to say. Reported at the
          constructor rather than at the head of the spine, which is where
          the reader has to look. *)
       let msg = Printf.sprintf
         "'%s' takes no arguments -- write `%s`, with nothing after it"
         name name
       in
       (match f with
        | Located (l, _) -> raise (TypeErrorAt (l, msg))
        | _ -> raise (TypeError msg))
     | Constr name, Tuple es when List.length es > 1 ->
       (match find_ctor_in_tenv tenv name with
        | Some (_, ctor)
          when List.length ctor.fields = List.length es
            && List.for_all (fun (n, _) -> n = None) ctor.fields ->
          raise (TypeError (Printf.sprintf
            "'%s' takes %d arguments, so write `%s %s` rather than `%s (%s)`"
            name (List.length ctor.fields) name
            (String.concat " " (List.init (List.length es)
               (fun i -> Printf.sprintf "a%d" (i + 1))))
            name
            (String.concat ", " (List.init (List.length es)
               (fun i -> Printf.sprintf "a%d" (i + 1))))))
        | _ -> ())
     (* `file*.txt` is not multiplication and never was: `*.txt` lexes as a
        glob literal, so this reads as applying `file` to it. Says what to
        write and stops; the rule it follows from is in the reference. Only
        fires when the name is unbound, so `FS.glob *.wand` and any other
        application of a real function to a glob are untouched. *)
     | Var name, Glob g when not (List.mem_assoc name env) ->
       raise (TypeError (Printf.sprintf
         "'%s%s' should be written as './%s%s'" name g name g))
     | _ -> ())

(* An application with a lambda written before another argument.

   Arguments are checked left to right, which leaves such a lambda with
   nothing to go on: in `List.map (fn p -> p.host) pods` the parameter is
   still a variable when the body is read, so `p.host` has no type to look
   the field up in. The signature says no more than the call does --
   `List.map` is `('a -> 'b) -> List 'a -> List 'b`, and `'a` arrives with
   `pods`.

   So the arrows come apart in written order, exactly as the one-argument
   path takes them apart, and every argument that is not a lambda is
   inferred as it is reached. Only the lambda bodies wait. They run
   afterwards, in the order they were written, by which time the arguments
   standing after them have pinned whatever they are going to pin. A lambda
   whose parameter is still open then is one nothing in the call determines,
   which is what it was before. *)
and infer_app_spine ?extra tenv (env : env) head args =
  (* Errors inside the spine are promoted to the application node that
     supplies the argument, which is where walking down would have left
     them. *)
  let at loc f =
    match loc with
    | None   -> f ()
    | Some l -> (try f () with TypeError msg -> raise (TypeErrorAt (l, msg)))
  in
  (match args with
   | (_, first) :: _ -> check_app_shape tenv env head first
   | [] -> ());
  let waiting = ref [] in
  let fn_type param_ts body_result_t arg_effects =
    let rec build = function
      | []      -> body_result_t
      | [t]     -> TFun (t, body_result_t, arg_effects)
      | t :: tl -> TFun (t, build tl, Effect_set.unknown ())
    in
    build param_ts
  in
  (* A lambda written in function position waits like one written as an
     argument. Its type is built from what it declares -- the parameters and
     an arrow -- which is all the arguments need in order to pin them; only
     the body has to wait, and it is the body that wanted them pinned. *)
  let cur = ref (
    match strip_located head with
    | Fn (params, body) ->
      let param_ts = List.map (fun _ -> fresh ()) params in
      let body_result_t = fresh () in
      let arg_effects = Effect_set.unknown () in
      waiting := [(loc_of_expr head, params, body, param_ts, body_result_t,
                   arg_effects, None)];
      fn_type param_ts body_result_t arg_effects
    | _ -> infer tenv env head)
  in
  List.iter (fun (loc, x) ->
    match strip_located x with
    | Fn (params, body) ->
      let param_ts = List.map (fun _ -> fresh ()) params in
      let body_result_t = fresh () in
      let arg_effects = Effect_set.unknown () in
      let fn_arg_t = fn_type param_ts body_result_t arg_effects in
      let tr = fresh () in
      let latent = Effect_set.unknown () in
      at loc (fun () ->
        unify_expected ~expected:!cur ~got:(TFun (fn_arg_t, tr, latent)));
      cur := tr;
      waiting := (loc, params, body, param_ts, body_result_t, arg_effects,
                  Some latent) :: !waiting
    | _ ->
      let tx = infer tenv env x in
      let tr = fresh () in
      let latent = Effect_set.unknown () in
      at loc (fun () ->
        unify_expected ~expected:!cur ~got:(TFun (tx, tr, latent)));
      performs latent;
      cur := tr) args;
  (* A piped value is an argument written to the left of the call, so it
     lands after every argument written inside it. *)
  (match extra with
   | None -> ()
   | Some tx ->
     let tr = fresh () in
     let latent = Effect_set.unknown () in
     unify_expected ~expected:!cur ~got:(TFun (tx, tr, latent));
     performs latent;
     cur := tr);
  List.iter (fun (loc, params, body, param_ts, body_result_t, arg_effects,
                  latent) ->
    at loc (fun () ->
      let env' =
        List.fold_left2 (fun env p t -> infer_pat tenv p t env)
          env params param_ts
      in
      let (body_t, body_effects) = scoped_eff (fun () -> infer tenv env' body) in
      let body_effects =
        if List.exists (pat_is_refutable tenv) params
        then Effect_set.add Effect_set.Raise body_effects
        else body_effects
      in
      unify body_t body_result_t;
      (* The function being applied says what its argument may perform; the
         lambda written at the call site is what performs it. *)
      (try Effect_set.unify arg_effects body_effects
       with
       | Effect_set.Mismatch msg -> raise (TypeError msg)
       | Effect_set.Conflict (a, b) ->
         raise (TypeError (effects_conflict_message
                             ~expected:"the parameter" ~got:"the function given"
                             a b)));
      (* A lambda in function position has no separate latent set: applying
         it is what performs its body, and the arrow already carries that. *)
      (match latent with Some l -> performs l | None -> ()))) (List.rev !waiting);
  !cur

and infer_binop tenv (env : env) op a b : typ =
  match op with
  | "+" | "-" ->
    (* One type throughout, resolved by use and dispatched by the evaluator
       on the value's tag. Nothing defaults: an unpinned `fn x -> x + x`
       stays `Add -> Add` and works at all four.

       An instant is the exception, and it is why both sides are inferred
       before either is unified. A `DateTime` is a point rather than a
       quantity: a `Duration` moves it, and two of them subtract to the
       length between them. Two of them do not add -- there is no instant
       twice as late as another -- and a `Duration` does not subtract an
       instant. *)
    let ta = infer tenv env a in
    let tb = infer tenv env b in
    (match repr ta, repr tb, op with
     | TDateTime, TDuration, _ -> TDateTime
     | TDuration, TDateTime, "+" -> TDateTime
     | TDateTime, TDateTime, "-" -> TDuration
     | TDateTime, TDateTime, _ ->
       raise (TypeError
         "two instants do not add. Subtract them for the Duration between \
          them, or add a Duration to one of them")
     | TDuration, TDateTime, _ ->
       raise (TypeError
         "an instant cannot be subtracted from a Duration. Write the \
          instant first: `deadline - 30s`")
     | _ ->
       let n = fresh_add () in
       unify ta n;
       unify tb n;
       n)
  | "*" | "/" ->
    (* One numeric type throughout: Int -> Int -> Int or the same at
       Float. A `Size` times a `Size` is not a `Size`, so the quantities
       that add do not multiply. *)
    let n = fresh_num () in
    unify (infer tenv env a) n;
    unify (infer tenv env b) n;
    n
  | "%" ->
    (* Int only: float modulo is a niche with sharp edges. *)
    unify_expected ~expected:TInt ~got:(infer tenv env a);
    unify_expected ~expected:TInt ~got:(infer tenv env b);
    TInt
  | "++" ->
    unify_expected ~expected:TString ~got:(infer tenv env a);
    unify_expected ~expected:TString ~got:(infer tenv env b);
    TString
  | "::" ->
    let elem_t = fresh () in
    unify_expected ~expected:elem_t ~got:(infer tenv env a);
    unify_expected ~expected:(TList elem_t) ~got:(infer tenv env b);
    TList elem_t
  | "==" | "!=" ->
    unify (infer tenv env a) (infer tenv env b); TBool
  (* Both operands are one ordered type. The constraint is what makes
     `r/a/ < r/b/` a type error rather than a run-time one, and what keeps
     two functions from being compared at all. *)
  | "<" | ">" | "<=" | ">=" ->
    let o = fresh_ord () in
    unify_expected ~expected:o ~got:(infer tenv env a);
    unify_expected ~expected:o ~got:(infer tenv env b);
    TBool
  | "&&" | "||" ->
    unify (infer tenv env a) TBool;
    unify (infer tenv env b) TBool;
    TBool
  | "|>" ->
    let ta = infer tenv env a in
    (match b with
     | RunCmd (e, _) ->
       unify_expected ~expected:TString ~got:(infer tenv env e);
       unify_expected ~expected:TString ~got:ta;
       TString
     | RunQuery (e, _) ->
       unify_expected ~expected:TString ~got:(infer tenv env e);
       unify_expected ~expected:TString ~got:ta;
       TName "ShellResult"
     | _ ->
       (* `xs |> List.map (fn p -> p.host)` has the same problem the spine
          path solves, one step further along: the lambda is the last
          argument written, and the value that says what `p` is arrives from
          the left. *)
       let piped =
         match strip_located b with
         | Fn _  -> Some (b, [])
         | App _ ->
           let (head, spine) = flatten_app b [] in
           if any_lambda spine || is_fn head then Some (head, spine) else None
         | _ -> None
       in
       match piped with
       | Some (head, spine) -> infer_app_spine ~extra:ta tenv env head spine
       | None ->
       let tb = infer tenv env b in
       let tr = fresh () in
       let latent = Effect_set.unknown () in
       unify tb (TFun (ta, tr, latent));
       performs latent;
       tr)
  | op -> raise (TypeError (Printf.sprintf "unknown operator '%s'" op))

(* ── Public API ───────────────────────────────────────────────────────────── *)

let infer_expr (e : expr) : (typ, string) result =
  try
    forget_unbound ();
    let t = infer [] [] e in
    raise_first_unbound ();
    Ok t
  with
  | TypeError msg -> Error msg
  | TypeErrorAt (loc, msg) ->
    Error (Printf.sprintf "%d:%d: %s" loc.Token.line loc.Token.col msg)

(* All primitives — used when typechecking stdlib modules *)
let stdlib_type_env : env = [
  ("io_print",   let a = fresh () in generalize [] (effs [Effect_set.IO] (a) (TUnit)));
  ("io_println", let a = fresh () in generalize [] (effs [Effect_set.IO] (a) (TUnit)));
  ("proc_exit",  let a = fresh () in generalize [] (effs [Effect_set.Proc] (TInt) (a)));
  (* No effect. Both are handed to the process before it runs and never
     change, so reading either reaches nothing and answers the same twice.
     A `Par` worker is a domain rather than a second process, so it shares
     the pid; a fork would not, and would make `proc_pid` an effect. *)
  ("proc_args",  generalize [] ((TUnit @-> TList TString)));
  ("proc_pid",   generalize [] ((TUnit @-> TInt)));
  ("clock_sleep", generalize [] (effs [Effect_set.Clock] (TDuration) (TUnit)));
  ("clock_now", generalize [] (effs [Effect_set.Clock] (TUnit) (TDateTime)));
  ("clock_elapsed", generalize [] (effs [Effect_set.Clock] (TUnit) (TDuration)));
  (* Drawing. `random_below` answers `0 <= x < bound`; a bound below 1 is
     clamped to 1 rather than raising, so nothing in `Random` needs Raise
     and the module's every export is total. *)
  ("random_below", generalize [] (effs [Effect_set.Random] (TInt) (TInt)));
  ("random_float", generalize [] (effs [Effect_set.Random] (TUnit) (TFloat)));
  ("random_seed", generalize [] (effs [Effect_set.Random] (TInt) (TUnit)));
  ("option_get_exn", let a = fresh () in generalize [] (effs [Effect_set.Raise] (TUnit) (a)));
  ("fail_exn", let a = fresh () in generalize [] (effs [Effect_set.Raise] (TString) (a)));
  (* A file is named by a Path, like every other filesystem operation. These
     two took a String, so a script holding a Path had to convert away from
     the domain type at the one boundary the domain type is for. *)
  ("read_file",  generalize [] (effs [Effect_set.FsRead; Effect_set.Raise] (TPath) (TString)));
  (* The algorithm arrives first and performs nothing; the read happens when
     the path does, so the labels sit on the last arrow. *)
  ("hash_file",  generalize [] ((TString @-> effs [Effect_set.FsRead; Effect_set.Raise] (TPath) (TString))));
  ("write_file", generalize [] (effs [Effect_set.FsWrite; Effect_set.Raise] (TPath) ((TString @-> TUnit))));
  (* `FS.Read` is here because the mode of an existing target is read before
     it is replaced, and because a symlink is resolved rather than
     overwritten. Both are stats. A script that publishes a file atomically
     declares the read, which is true of what it does. *)
  ("write_atomic", generalize [] (effs [Effect_set.FsRead; Effect_set.FsWrite; Effect_set.Raise] (TPath) ((TString @-> TUnit))));
  (* String primitives *)
  ("str_length",     generalize [] ((TString @-> TInt)));
  ("str_upper",      generalize [] ((TString @-> TString)));
  ("str_lower",      generalize [] ((TString @-> TString)));
  ("str_trim",       generalize [] ((TString @-> TString)));
  ("str_slice",      generalize [] ((TInt @-> (TInt @-> (TString @-> TString)))));
  ("str_split",      generalize [] ((TString @-> (TString @-> TList TString))));
  ("str_words",      generalize [] ((TString @-> TList TString)));
  ("str_lines",      generalize [] ((TString @-> TList TString)));
  ("str_word",       generalize [] ((TInt @-> (TString @-> TApp (TName "Option", TString)))));
  ("str_word_exn",   generalize [] (TInt @-> effs [Effect_set.Raise] TString TString));
  ("str_contains",   generalize [] ((TString @-> (TString @-> TBool))));
  ("str_starts_with",generalize [] ((TString @-> (TString @-> TBool))));
  ("str_ends_with",  generalize [] ((TString @-> (TString @-> TBool))));
  ("str_replace",    generalize [] ((TString @-> (TString @-> (TString @-> TString)))));
  ("str_trim_left",  generalize [] ((TString @-> TString)));
  ("str_trim_right", generalize [] ((TString @-> TString)));
  ("str_repeat",     generalize [] ((TInt @-> (TString @-> TString))));
  ("str_reverse",    generalize [] ((TString @-> TString)));
  ("str_truncate",   generalize [] ((TInt @-> (TString @-> TString))));
  ("hash_hex",       generalize [] ((TString @-> (TString @-> TString))));
  ("hmac_hex",       generalize [] ((TString @-> (TString @-> (TString @-> TString)))));
  ("hex_encode",     generalize [] ((TString @-> TString)));
  ("hex_decode",     generalize [] ((TString @-> TResult (TString, TString))));
  ("base64_encode",     generalize [] ((TString @-> TString)));
  ("base64_encode_url", generalize [] ((TString @-> TString)));
  ("base64_decode",     generalize [] ((TString @-> TResult (TString, TString))));
  ("base64_decode_url", generalize [] ((TString @-> TResult (TString, TString))));
  ("ct_equal",       generalize [] ((TString @-> (TString @-> TBool))));
  ("str_bytes",      generalize [] ((TString @-> TList TString)));
  ("int_to_str",       generalize [] ((TInt @-> TString)));
  ("float_of_int",     generalize [] ((TInt @-> TFloat)));
  ("float_round",      generalize [] ((TFloat @-> TInt)));
  ("float_floor",      generalize [] ((TFloat @-> TInt)));
  ("float_ceil",       generalize [] ((TFloat @-> TInt)));
  ("float_abs",        generalize [] ((TFloat @-> TFloat)));
  ("float_format",     generalize [] ((TInt @-> (TFloat @-> TString))));
  ("str_to_int",       generalize [] ((TString @-> TResult (TString, TInt))));
  ("str_to_float",     generalize [] ((TString @-> TResult (TString, TFloat))));
  ("str_to_bool",      generalize [] ((TString @-> TResult (TString, TBool))));
  ("str_to_path",      generalize [] ((TString @-> TPath)));
  (* Glob primitives. `matches?` is pure: the pattern and the path are both
     in hand, so nothing has to be read to answer. *)
  ("str_to_glob",   generalize [] ((TString @-> TResult (TString, TGlob))));
  ("glob_matches",  generalize [] ((TGlob @-> (TPath @-> TBool))));
  ("glob_to_str",   generalize [] ((TGlob @-> TString)));
  ("glob_base",     generalize [] ((TGlob @-> TPath)));
  ("str_to_url",       generalize [] ((TString @-> TResult (TString, TURL))));
  ("str_to_ipv4",      generalize [] ((TString @-> TResult (TString, TIPv4))));
  ("str_to_cidr",      generalize [] ((TString @-> TResult (TString, TCIDR))));
  (* IPv4 and CIDR primitives. Every accessor is total: the value is an
     address or a network already, so its parts are there. *)
  ("ipv4_octets",      generalize [] ((TIPv4 @-> TTuple [TInt; TInt; TInt; TInt])));
  ("ipv4_to_int",      generalize [] ((TIPv4 @-> TInt)));
  ("ipv4_of_int",      generalize [] ((TInt @-> TResult (TString, TIPv4))));
  ("ipv4_to_str",      generalize [] ((TIPv4 @-> TString)));
  ("ipv4_is_private",  generalize [] ((TIPv4 @-> TBool)));
  ("ipv4_is_loopback", generalize [] ((TIPv4 @-> TBool)));
  ("cidr_contains",    generalize [] ((TCIDR @-> (TIPv4 @-> TBool))));
  ("cidr_network",     generalize [] ((TCIDR @-> TIPv4)));
  ("cidr_prefix",      generalize [] ((TCIDR @-> TInt)));
  ("cidr_first",       generalize [] ((TCIDR @-> TIPv4)));
  ("cidr_last",        generalize [] ((TCIDR @-> TIPv4)));
  ("cidr_count",       generalize [] ((TCIDR @-> TInt)));
  ("cidr_to_str",      generalize [] ((TCIDR @-> TString)));
  ("cidr_of_parts",    generalize [] ((TIPv4 @-> (TInt @-> TResult (TString, TCIDR)))));
  ("port_to_int",      generalize [] ((TPort @-> TInt)));
  ("port_of_int",      generalize [] ((TInt @-> TResult (TString, TPort))));
  ("str_to_port",      generalize [] ((TString @-> TResult (TString, TPort))));
  ("str_to_version",   generalize [] ((TString @-> TResult (TString, TVersion))));
  (* Version primitives. The three numbers exist by the time a value does,
     so reading them cannot fail. *)
  ("version_major",      generalize [] ((TVersion @-> TInt)));
  ("version_minor",      generalize [] ((TVersion @-> TInt)));
  ("version_patch",      generalize [] ((TVersion @-> TInt)));
  ("version_prerelease", generalize [] ((TVersion @-> TApp (TName "Option", TString))));
  ("version_build",      generalize [] ((TVersion @-> TApp (TName "Option", TString))));
  ("version_is_stable",  generalize [] ((TVersion @-> TBool)));
  ("version_core",       generalize [] ((TVersion @-> TVersion)));
  ("version_to_str",     generalize [] ((TVersion @-> TString)));
  ("version_of_parts",
     generalize [] ((TInt @-> (TInt @-> (TInt @-> TResult (TString, TVersion))))));
  ("version_bump_major", generalize [] ((TVersion @-> TVersion)));
  ("version_bump_minor", generalize [] ((TVersion @-> TVersion)));
  ("version_bump_patch", generalize [] ((TVersion @-> TVersion)));
  ("version_with_prerelease",
     generalize [] ((TApp (TName "Option", TString) @-> (TVersion @-> TResult (TString, TVersion)))));
  ("version_with_build",
     generalize [] ((TApp (TName "Option", TString) @-> (TVersion @-> TResult (TString, TVersion)))));
  ("str_to_size",      generalize [] ((TString @-> TResult (TString, TSize))));
  ("dt_year",         generalize [] ((TDateTime @-> TInt)));
  ("dt_month",        generalize [] ((TDateTime @-> TInt)));
  ("dt_day",          generalize [] ((TDateTime @-> TInt)));
  ("dt_hour",         generalize [] ((TDateTime @-> TInt)));
  ("dt_minute",       generalize [] ((TDateTime @-> TInt)));
  ("dt_second",       generalize [] ((TDateTime @-> TInt)));
  ("dt_weekday",      generalize [] ((TDateTime @-> TInt)));
  ("dt_day_start",    generalize [] ((TDateTime @-> TDateTime)));
  ("dt_on",           generalize [] ((TTuple [TInt; TInt; TInt]
                                      @-> TResult (TString, TDateTime))));
  ("dt_on_exn",       generalize [] (effs [Effect_set.Raise]
                                      (TTuple [TInt; TInt; TInt]) (TDateTime)));
  ("dt_date_string",  generalize [] ((TDateTime @-> TString)));
  ("dt_time_string",  generalize [] ((TDateTime @-> TString)));
  ("str_to_datetime",  generalize [] ((TString @-> TResult (TString, TDateTime))));
  ("str_to_duration",  generalize [] ((TString @-> TResult (TString, TDuration))));
  (* Regex primitives *)
  ("regex_match",       generalize [] ((TRegex @-> (TString @-> TBool))));
  ("regex_capture",     generalize [] ((TRegex @-> (TString @-> TList TString))));
  ("regex_replace",     generalize [] ((TRegex @-> (TString @-> (TString @-> TString)))));
  ("regex_replace_all", generalize [] ((TRegex @-> (TString @-> (TString @-> TString)))));
  ("regex_split",       generalize [] ((TRegex @-> (TString @-> TList TString))));
  ("regex_find_all",    generalize [] ((TRegex @-> (TString @-> TList TString))));
  ("resource_make",
   (* Acquire and release share one set: a resource performs what either of
      them performs, and `with` folds that into its caller. *)
   let a = fresh () in
   let e = Effect_set.unknown () in
   generalize []
     (TFun (TFun (TUnit, a, e),
            TFun (TFun (a, TUnit, e), TResource (e, a), Effect_set.pure),
            Effect_set.pure)));
  (* Stream primitives. A source's effects are what enumerating it performs
     -- open (a tail variable), so a terminal operation's closure can join
     it; the terminals share one set with the stream and the closure, the
     same sharing resource_make uses. Construction itself is pure:
     nothing is read until a terminal operation. *)
  (* Streaming a command. Like the other two sources this is inert until a
     terminal operation runs it, so building one performs nothing and the
     `Shell` is in the stream's own set, paid at the fold. *)
  ("shell_stream",
   let r = Effect_set.Set
       (Effect_set.EffSet.of_list [Effect_set.Shell; Effect_set.Raise],
        Some (Effect_set.fresh_var ())) in
   generalize [] (TFun (TCommand, TStream (r, TString), Effect_set.pure)));
  (* Several files as one stream. The row is the single-file one: it opens
     the same way, once per file. *)
  ("fs_stream_lines_all",
   let r = Effect_set.Set
       (Effect_set.EffSet.of_list [Effect_set.FsRead; Effect_set.Raise],
        Some (Effect_set.fresh_var ())) in
   generalize [] (TFun (TList TPath, TStream (r, TString), Effect_set.pure)));
  (* The sinks. A terminal operation, so the stream's own effects join what
     writing performs: reading the source happens inside the write. *)
  ("fs_write_lines",
   let e = Effect_set.unknown () in
   let with_write = Effect_set.add Effect_set.Raise
                      (Effect_set.add Effect_set.FsWrite e) in
   generalize []
     (TFun (TPath, TFun (TStream (e, TString), TUnit, with_write),
            Effect_set.pure)));
  ("fs_append_lines",
   let e = Effect_set.unknown () in
   let with_write = Effect_set.add Effect_set.Raise
                      (Effect_set.add Effect_set.FsWrite e) in
   generalize []
     (TFun (TPath, TFun (TStream (e, TString), TUnit, with_write),
            Effect_set.pure)));
  (* `FS.Read` for the same two reasons `write_atomic` carries it: the
     target's mode is read before it is replaced, and a symlink is resolved
     rather than overwritten. Both are stats. *)
  ("fs_write_lines_atomic",
   let e = Effect_set.unknown () in
   let with_write =
     Effect_set.add Effect_set.Raise
       (Effect_set.add Effect_set.FsRead
          (Effect_set.add Effect_set.FsWrite e)) in
   generalize []
     (TFun (TPath, TFun (TStream (e, TString), TUnit, with_write),
            Effect_set.pure)));
  ("fs_stream_lines",
   let r = Effect_set.Set
       (Effect_set.EffSet.of_list [Effect_set.FsRead; Effect_set.Raise],
        Some (Effect_set.fresh_var ())) in
   generalize [] (TFun (TPath, TStream (r, TString), Effect_set.pure)));
  ("io_stdin_lines",
   let r = Effect_set.Set
       (Effect_set.EffSet.of_list [Effect_set.IO; Effect_set.Raise],
        Some (Effect_set.fresh_var ())) in
   generalize [] (TFun (TUnit, TStream (r, TString), Effect_set.pure)));
  ("stream_of_list",
   let a = fresh () in
   let r = Effect_set.unknown () in
   generalize [] (TFun (TList a, TStream (r, a), Effect_set.pure)));
  ("stream_map",
   let a = fresh () and b = fresh () in
   let e = Effect_set.unknown () in
   generalize []
     (TFun (TFun (a, b, e),
            TFun (TStream (e, a), TStream (e, b), Effect_set.pure),
            Effect_set.pure)));
  ("stream_filter",
   let a = fresh () in
   let e = Effect_set.unknown () in
   generalize []
     (TFun (TFun (a, TBool, e),
            TFun (TStream (e, a), TStream (e, a), Effect_set.pure),
            Effect_set.pure)));
  ("stream_filter_map",
   let a = fresh () and b = fresh () in
   let e = Effect_set.unknown () in
   generalize []
     (TFun (TFun (a, TApp (TName "Option", b), e),
            TFun (TStream (e, a), TStream (e, b), Effect_set.pure),
            Effect_set.pure)));
  ("stream_flat_map",
   let a = fresh () and b = fresh () in
   let e = Effect_set.unknown () in
   generalize []
     (TFun (TFun (a, TList b, e),
            TFun (TStream (e, a), TStream (e, b), Effect_set.pure),
            Effect_set.pure)));
  ("stream_take_while",
   let a = fresh () in
   let e = Effect_set.unknown () in
   generalize []
     (TFun (TFun (a, TBool, e),
            TFun (TStream (e, a), TStream (e, a), Effect_set.pure),
            Effect_set.pure)));
  ("stream_drop_while",
   let a = fresh () in
   let e = Effect_set.unknown () in
   generalize []
     (TFun (TFun (a, TBool, e),
            TFun (TStream (e, a), TStream (e, a), Effect_set.pure),
            Effect_set.pure)));
  ("stream_drop",
   let a = fresh () in
   let e = Effect_set.unknown () in
   generalize []
     (TFun (TInt,
            TFun (TStream (e, a), TStream (e, a), Effect_set.pure),
            Effect_set.pure)));
  ("stream_indexed",
   let a = fresh () in
   let e = Effect_set.unknown () in
   generalize []
     (TFun (TStream (e, a), TStream (e, TTuple [TInt; a]), Effect_set.pure)));
  ("stream_scan",
   let acc = fresh () and a = fresh () in
   let e = Effect_set.unknown () in
   generalize []
     (TFun (TFun (acc, TFun (a, acc, e), e),
            TFun (acc, TFun (TStream (e, a), TStream (e, acc), Effect_set.pure),
                  Effect_set.pure),
            Effect_set.pure)));
  ("stream_chunks",
   let a = fresh () in
   let e = Effect_set.unknown () in
   generalize []
     (TFun (TInt,
            TFun (TStream (e, a), TStream (e, TList a), Effect_set.pure),
            Effect_set.pure)));
  ("stream_unique",
   let a = fresh () in
   let e = Effect_set.unknown () in
   generalize []
     (TFun (TStream (e, a), TStream (e, a), Effect_set.pure)));
  ("stream_take",
   let a = fresh () in
   let e = Effect_set.unknown () in
   generalize []
     (TFun (TInt,
            TFun (TStream (e, a), TStream (e, a), Effect_set.pure),
            Effect_set.pure)));
  ("stream_fold",
   let acc = fresh () and b = fresh () in
   let e = Effect_set.unknown () in
   generalize []
     (TFun (TFun (acc, TFun (b, acc, e), e),
            TFun (acc, TFun (TStream (e, b), acc, e), Effect_set.pure),
            Effect_set.pure)));
  ("stream_each",
   let a = fresh () and b = fresh () in
   let e = Effect_set.unknown () in
   generalize []
     (TFun (TFun (a, b, e),
            TFun (TStream (e, a), TUnit, e),
            Effect_set.pure)));
  ("stream_to_list",
   let a = fresh () in
   let e = Effect_set.unknown () in
   generalize [] (TFun (TStream (e, a), TList a, e)));
  ("stream_tally",
   let e = Effect_set.unknown () in
   generalize [] (TFun (TStream (e, TString), TMap TInt, e)));
  ("regex_compile",     generalize [] ((TString @-> TResult (TString, TRegex))));
  (* Duration primitives *)
  ("dur_zero",    Mono TDuration);
  ("dur_seconds", generalize [] ((TInt @-> TDuration)));
  ("dur_minutes", generalize [] ((TInt @-> TDuration)));
  ("dur_hours",   generalize [] ((TInt @-> TDuration)));
  ("dur_days",    generalize [] ((TInt @-> TDuration)));
  ("dur_weeks",   generalize [] ((TInt @-> TDuration)));
  ("dur_add",     generalize [] ((TDuration @-> (TDuration @-> TDuration))));
  ("dur_sub",     generalize [] ((TDuration @-> (TDuration @-> TDuration))));
  ("dur_scale",   generalize [] ((TInt @-> (TDuration @-> TDuration))));
  ("dur_format",  generalize [] ((TDuration @-> TString)));
  ("dur_to_ms",   generalize [] ((TDuration @-> TInt)));
  (* Size primitives *)
  ("size_to_bytes", generalize [] ((TSize @-> TInt)));
  ("size_of_bytes", generalize [] ((TInt @-> TSize)));
  ("size_format",   generalize [] ((TSize @-> TString)));
  (* URL primitives. Every accessor is total: the value is a URL already, and
     a part it does not have is "" or None rather than a failure. *)
  ("url_scheme",     generalize [] ((TURL @-> TString)));
  ("url_hostname",   generalize [] ((TURL @-> TString)));
  ("url_host",       generalize [] ((TURL @-> TString)));
  ("url_username",   generalize [] ((TURL @-> TApp (TName "Option", TString))));
  ("url_password",   generalize [] ((TURL @-> TApp (TName "Option", TString))));
  ("url_origin",     generalize [] ((TURL @-> TString)));
  ("url_port",       generalize [] ((TURL @-> TApp (TName "Option", TPort))));
  ("url_path",       generalize [] ((TURL @-> TPath)));
  ("url_query",      generalize [] ((TURL @-> TMap TString)));
  ("url_query_list", generalize [] ((TURL @-> TList (TTuple [TString; TString]))));
  ("url_fragment",   generalize [] ((TURL @-> TApp (TName "Option", TString))));
  ("url_to_str",     generalize [] ((TURL @-> TString)));
  ("url_with_query", generalize [] ((TMap TString @-> (TURL @-> TURL))));
  ("url_join",       generalize [] ((TString @-> (TURL @-> TResult (TString, TURL)))));
  ("url_with_query_list",
     generalize [] ((TList (TTuple [TString; TString]) @-> (TURL @-> TURL))));
  ("url_with_scheme",   generalize [] ((TString @-> (TURL @-> TResult (TString, TURL)))));
  ("url_with_hostname", generalize [] ((TString @-> (TURL @-> TResult (TString, TURL)))));
  ("url_with_port",     generalize [] ((TApp (TName "Option", TPort) @-> (TURL @-> TURL))));
  ("url_with_path",     generalize [] ((TPath @-> (TURL @-> TURL))));
  ("url_with_fragment",
     generalize [] ((TApp (TName "Option", TString) @-> (TURL @-> TURL))));
  ("url_with_username",
     generalize [] ((TApp (TName "Option", TString) @-> (TURL @-> TURL))));
  ("url_with_password",
     generalize [] ((TApp (TName "Option", TString) @-> (TURL @-> TURL))));
  ("url_encode",     generalize [] ((TString @-> TString)));
  ("url_decode",     generalize [] ((TString @-> TResult (TString, TString))));
  (* Path primitives *)
  ("path_join",           generalize [] ((TPath @-> (TPath @-> TPath))));
  ("path_parent",         generalize [] ((TPath @-> TPath)));
  ("path_basename",       generalize [] ((TPath @-> TPath)));
  ("path_extension",      generalize [] ((TPath @-> TString)));
  ("path_with_extension", generalize [] ((TString @-> (TPath @-> TPath))));
  ("path_is_absolute",    generalize [] ((TPath @-> TBool)));
  ("path_is_relative",    generalize [] ((TPath @-> TBool)));
  ("path_normalize",      generalize [] ((TPath @-> TPath)));
  ("path_to_string",      generalize [] ((TPath @-> TString)));
  ("path_of_string",      generalize [] ((TString @-> TPath)));
  ("path_components",     generalize [] ((TPath @-> TList TString)));
  (* FS primitives *)
  ("fs_exists",  generalize [] (effs [Effect_set.FsRead] (TPath) (TBool)));
  ("fs_is_file", generalize [] (effs [Effect_set.FsRead] (TPath) (TBool)));
  ("fs_is_dir",  generalize [] (effs [Effect_set.FsRead] (TPath) (TBool)));
  ("fs_mkdir",   generalize [] (effs [Effect_set.FsWrite; Effect_set.Raise] (TPath) (TUnit)));
  ("fs_ls",      generalize [] (effs [Effect_set.FsRead; Effect_set.Raise] (TPath) (TList TPath)));
  ("fs_remove",  generalize [] (effs [Effect_set.FsWrite; Effect_set.Raise] (TPath) (TUnit)));
  ("fs_append",  generalize [] (effs [Effect_set.FsWrite; Effect_set.Raise] (TPath) ((TString @-> TUnit))));
  ("fs_create",  generalize [] (effs [Effect_set.FsWrite; Effect_set.Raise] (TPath) (TUnit)));
  ("fs_temp_file", generalize [] (effs [Effect_set.FsWrite; Effect_set.Raise] (TString) ((TString @-> TPath))));
  ("fs_temp_dir",  generalize [] (effs [Effect_set.FsWrite; Effect_set.Raise] (TString) TPath));
  ("fs_lock",   generalize [] (effs [Effect_set.FsWrite; Effect_set.Raise] (TPath) (TApp (TName "Option", TPath))));
  ("fs_lock_wait", generalize [] (effs [Effect_set.Clock; Effect_set.FsWrite; Effect_set.Raise] (TDuration) ((TPath @-> TApp (TName "Option", TPath))))); 
  ("fs_unlock", generalize [] (effs [Effect_set.FsWrite] (TPath) TUnit));
  ("fs_delete_tree", generalize [] (effs [Effect_set.FsWrite; Effect_set.Raise] (TPath) TUnit));
  ("fs_copy_tree", generalize [] (effs [Effect_set.FsWrite; Effect_set.Raise] (TPath) (TPath @-> TUnit)));
  ("fs_rename",  generalize [] (effs [Effect_set.FsWrite; Effect_set.Raise] (TPath) ((TPath @-> TUnit))));
  ("fs_copy",    generalize [] (effs [Effect_set.FsWrite; Effect_set.Raise] (TPath) ((TPath @-> TUnit))));
  ("fs_cwd",     generalize [] (effs [Effect_set.FsRead] (TUnit) (TPath)));
  ("fs_mtime",   generalize [] (effs [Effect_set.FsRead; Effect_set.Raise] (TPath) (TDateTime)));
  ("fs_size",    generalize [] (effs [Effect_set.FsRead; Effect_set.Raise] (TPath) (TSize)));
  ("fs_glob",    generalize [] (effs [Effect_set.FsRead] (TGlob) ((TPath @-> TList TPath))));
  (* IO primitives *)
  ("io_print_err",   generalize [] (effs [Effect_set.IO] (TString) (TUnit)));
  ("io_println_err", generalize [] (effs [Effect_set.IO] (TString) (TUnit)));
  ("io_read_line",   generalize [] (effs [Effect_set.IO; Effect_set.Raise] (TUnit) (TString)));
  ("io_read_all",    generalize [] (effs [Effect_set.IO; Effect_set.Raise] (TUnit) (TString)));
  ("io_flush",       generalize [] (effs [Effect_set.IO] (TUnit) (TUnit)));
  (* Process primitives. `shell_run` and `shell_query` take a `Command`
     rather than a String on purpose: the quoting a command needs is done by
     the literal that wrote it, so a function that took text would be an
     injection where `$(...)` is safe. There is no way to make a `Command`
     out of a String, which is what makes these two safe -- and why no
     builtin here takes a command as text. *)
  ("shell_run",   generalize [] (effs [Effect_set.Shell; Effect_set.Raise] (TCommand) (TString)));
  ("shell_query", generalize [] (effs [Effect_set.Shell] (TCommand) (TName "ShellResult")));
  (* A 404 is not a failure of this call: the exchange succeeded and the
     server said no. `Raise` is here for the transport failing -- DNS, a
     connect, TLS, a timeout -- which is the same line `$()` draws. *)
  ("net_http", generalize [] (effs [Effect_set.Net; Effect_set.Raise] (TName "HTTPRequest") (TName "HTTPResponse")));
  ("net_download", generalize [] (effs [Effect_set.Net; Effect_set.FsWrite; Effect_set.Raise] (TURL) ((TPath @-> TUnit))));
  (* Env primitives *)
  ("env_read_dotenv", generalize [] (effs [Effect_set.Env; Effect_set.Raise] (TString) (TList (TTuple [TString; TString]))));
  (* Reads the file and sets each variable, so it performs FS.Read as well as
     Env and has to declare both. It declared only Env for long enough that a
     file whose whole manifest was `uses {Env}` could read any path on disk
     and typecheck, with A-USES1 advising the honest manifest be trimmed back
     to the lie. The sibling `Env.read!` is written in wand over `read_file`
     and inferred, which is why it was right all along. *)
  ("env_load_file",   generalize [] (effs [Effect_set.Env; Effect_set.FsRead; Effect_set.Raise] (TPath) (TUnit)));
  (* CSV primitives *)
  ("csv_parse",         generalize [] ((TString @-> (TString @-> TList (TList TString)))));
  (* A cell is text, and every value has a text form, so a row does not have
     to be converted before it is written. Nothing here can fail. *)
  ("csv_stringify",     let a = fresh () in
                        generalize [] ((TString @-> (TList (TList a) @-> TString))));
  (* JSON primitives *)
  ("json_parse",         generalize [] ((TString @-> TResult (TString, TJson))));
  ("json_parse_exn",     generalize [] (effs [Effect_set.Raise] (TString) (TJson)));
  ("json_stringify",     generalize [] ((TJson @-> TString)));
  ("json_stringify_pretty", generalize [] ((TJson @-> TString)));
  ("json_field_exn",     generalize [] (effs [Effect_set.Raise] (TString) ((TJson @-> TJson))));
  ("json_null",         Mono TJson);
  ("json_of_bool",      generalize [] ((TBool @-> TJson)));
  ("json_of_int",       generalize [] ((TInt @-> TJson)));
  ("json_of_float",     generalize [] ((TFloat @-> TJson)));
  ("json_of_string",    generalize [] ((TString @-> TJson)));
  ("toml_of",         let a = fresh () in
                      generalize [] ((a @-> TResult (TString, TToml))));
  ("toml_of_exn",     let a = fresh () in
                      generalize [] (effs [Effect_set.Raise] (a) (TToml)));
  ("json_of",         let a = fresh () in
                      generalize [] ((a @-> TResult (TString, TJson))));
  ("json_of_exn",     let a = fresh () in
                      generalize [] (effs [Effect_set.Raise] (a) (TJson)));
  ("json_of_list",      generalize [] ((TList TJson @-> TJson)));
  ("json_of_map",       generalize [] ((TMap TJson @-> TJson)));
  ("json_is_null",      generalize [] ((TJson @-> TBool)));
  ("json_get_bool",     generalize [] ((TJson @-> TResult (TString, TBool))));
  ("json_get_int",      generalize [] ((TJson @-> TResult (TString, TInt))));
  ("json_get_float",    generalize [] ((TJson @-> TResult (TString, TFloat))));
  ("json_get_string",   generalize [] ((TJson @-> TResult (TString, TString))));
  ("json_get_array",    generalize [] ((TJson @-> TResult (TString, (TList TJson)))));
  ("json_get_object",   generalize [] ((TJson @-> TResult (TString, (TMap TJson)))));
  ("json_field",        generalize [] ((TString @-> (TJson @-> TResult (TString, TJson)))));
  (* Decoder primitives.
     Decoding is pure by construction: the functions a decoder is built from
     carry the empty set, so a decoder cannot read a file or run a command on
     the way past. Getting the data is the caller's job and already says so
     in the caller's signature. *)
  ("decode_int",      Mono (TDecoder TInt));
  ("decode_float",    Mono (TDecoder TFloat));
  ("decode_string",   Mono (TDecoder TString));
  ("decode_bool",     Mono (TDecoder TBool));
  ("decode_path",     Mono (TDecoder TPath));
  ("decode_glob",     Mono (TDecoder TGlob));
  ("decode_duration", Mono (TDecoder TDuration));
  ("decode_url",      Mono (TDecoder TURL));
  ("decode_size",     Mono (TDecoder TSize));
  ("decode_version",  Mono (TDecoder TVersion));
  ("decode_datetime", Mono (TDecoder TDateTime));
  ("decode_ipv4",     Mono (TDecoder TIPv4));
  ("decode_cidr",     Mono (TDecoder TCIDR));
  ("decode_port",     Mono (TDecoder TPort));
  ("decode_succeed",  let a = fresh () in generalize [] (a @-> TDecoder a));
  ("decode_fail",     let a = fresh () in generalize [] (TString @-> TDecoder a));
  ("decode_field",    let a = fresh () in
                      generalize [] (TString @-> (TDecoder a @-> TDecoder a)));
  ("decode_optional", let a = fresh () in
                      generalize []
                        (TString @->
                         (TDecoder a @-> TDecoder (TApp (TName "Option", a)))));
  ("decode_list",     let a = fresh () in
                      generalize [] (TDecoder a @-> TDecoder (TList a)));
  ("decode_dict",     let a = fresh () in
                      generalize [] (TDecoder a @-> TDecoder (TMap a)));
  ("decode_nullable", let a = fresh () in
                      generalize []
                        (TDecoder a @-> TDecoder (TApp (TName "Option", a))));
  ("decode_map2",     let a = fresh () in let b = fresh () in let c = fresh () in
                      generalize []
                        (TFun (a, TFun (b, c, Effect_set.pure), Effect_set.pure)
                         @-> (TDecoder a @-> (TDecoder b @-> TDecoder c))));
  ("decode_and_then", let a = fresh () in let b = fresh () in
                      generalize []
                        (TFun (a, TDecoder b, Effect_set.pure)
                         @-> (TDecoder a @-> TDecoder b)));
  ("decode_one_of",   let a = fresh () in
                      generalize [] (TList (TDecoder a) @-> TDecoder a));
  ("json_decode",     let a = fresh () in
                      generalize []
                        (TDecoder a @-> (TJson @-> TResult (TString, a))));
  ("toml_decode",     let a = fresh () in
                      generalize []
                        (TDecoder a @-> (TToml @-> TResult (TString, a))));
  ("yaml_decode",     let a = fresh () in
                      generalize []
                        (TDecoder a @-> (TYaml @-> TResult (TString, a))));
  ("shell_decode",    let a = fresh () in
                      generalize []
                        (TDecoder a @-> (TString @-> TResult (TString, a))));
  ("shell_lines",     let a = fresh () in
                      generalize []
                        (TDecoder a @-> (TString @-> TResult (TString, TList a))));
  ("csv_rows",        let a = fresh () in
                      generalize []
                        (TDecoder a @-> (TString @-> TResult (TString, TList a))));
  (* Par primitives. The effects on the last arrow are the same variable as the
     one on the supplied function, so calling par_map performs exactly what
     that function performs -- the work happens inside, where inference
     cannot otherwise see it. *)
  (* `Shell.timeout d thunk` performs what the thunk performs, and waits, so
     it adds Clock to the thunk's own effects. The same variable on both
     sides is what says the effects come from the caller's thunk. *)
  ("shell_timeout",
   let a = fresh () in
   let e = Effect_set.unknown () in
   let with_clock = Effect_set.add Effect_set.Clock e in
   generalize [] (TDuration @-> TFun (TFun (TUnit, a, e),
                                      TResult (TString, a), with_clock)));
  (* Every thunk performs the same effects, and the race performs them too:
     one of the thunks really runs. It waits on workers, not on a clock, so
     no Clock. *)
  ("par_deadline_guard", generalize [] ((TUnit @-> TUnit)));
  ("par_race", let a = fresh () in
               let e = Effect_set.unknown () in
               generalize [] (TFun (TList (TFun (TUnit, a, e)),
                                    TResult (TString, a), e)));
  ("par_map",  let a = fresh () in let b = fresh () in
               let e = Effect_set.unknown () in
               generalize [] (TInt @-> (TFun (a, b, e)
                 @-> TFun (TList a, TList (TResult (TString, b)), e))));
  (* Like `List.each`, what the worker returns is dropped: a command run for
     its effect still hands back stdout, and demanding Unit taxed every such
     call site with a discard that said nothing. *)
  ("par_each", let a = fresh () in
               let b = fresh () in
               let e = Effect_set.unknown () in
               generalize [] (TInt @-> (TFun (a, b, e)
                 @-> TFun (TList a, TUnit, e))));
  (* TOML primitives *)
  ("toml_parse",        generalize [] ((TString @-> TResult (TString, TToml))));
  ("toml_parse_exn",    generalize [] (effs [Effect_set.Raise] (TString) (TToml)));
  ("toml_stringify",    generalize [] ((TToml @-> TString)));
  ("toml_is_table",     generalize [] ((TToml @-> TBool)));
  ("toml_is_array",     generalize [] ((TToml @-> TBool)));
  ("toml_get_bool",     generalize [] ((TToml @-> TResult (TString, TBool))));
  ("toml_get_int",      generalize [] ((TToml @-> TResult (TString, TInt))));
  ("toml_get_float",    generalize [] ((TToml @-> TResult (TString, TFloat))));
  ("toml_get_string",   generalize [] ((TToml @-> TResult (TString, TString))));
  ("toml_get_array",    generalize [] ((TToml @-> TResult (TString, (TList TToml)))));
  ("toml_get_table",    generalize [] ((TToml @-> TResult (TString, (TMap TToml)))));
  ("toml_field",        generalize [] ((TString @-> (TToml @-> TResult (TString, TToml)))));
  ("toml_field_exn",    generalize [] (effs [Effect_set.Raise] (TString) ((TToml @-> TToml))));

  (* YAML is read, never written: there is no `yaml_stringify` to pair with
     the other two, because emitting means choosing among many equivalent
     spellings and the one job that would want it -- editing a workflow in
     place -- needs comments and layout kept, which this does not carry. *)
  ("yaml_parse",         generalize [] ((TString @-> TResult (TString, TYaml))));
  ("yaml_parse_exn",     generalize [] (effs [Effect_set.Raise] (TString) (TYaml)));
  ("yaml_parse_all",     generalize [] ((TString @-> TResult (TString, (TList TYaml)))));
  ("yaml_parse_all_exn", generalize [] (effs [Effect_set.Raise] (TString) ((TList TYaml))));
  ("yaml_is_mapping",    generalize [] ((TYaml @-> TBool)));
  ("yaml_is_sequence",   generalize [] ((TYaml @-> TBool)));
  ("yaml_is_null",       generalize [] ((TYaml @-> TBool)));
  ("yaml_get_bool",      generalize [] ((TYaml @-> TResult (TString, TBool))));
  ("yaml_get_int",       generalize [] ((TYaml @-> TResult (TString, TInt))));
  ("yaml_get_float",     generalize [] ((TYaml @-> TResult (TString, TFloat))));
  ("yaml_get_string",    generalize [] ((TYaml @-> TResult (TString, TString))));
  ("yaml_get_sequence",  generalize [] ((TYaml @-> TResult (TString, (TList TYaml)))));
  ("yaml_get_mapping",   generalize [] ((TYaml @-> TResult (TString, (TMap TYaml)))));
  ("yaml_field",         generalize [] ((TString @-> (TYaml @-> TResult (TString, TYaml)))));
  ("yaml_field_exn",     generalize [] (effs [Effect_set.Raise] (TString) ((TYaml @-> TYaml))));
  ("env_get_exn", generalize [] (effs [Effect_set.Env; Effect_set.Raise] (TString) (TString)));
  ("env_set",     generalize [] (effs [Effect_set.Env] (TString) ((TString @-> TUnit))));
  ("env_clear",   generalize [] (effs [Effect_set.Env] (TString) (TUnit)));
  ("env_all",     generalize [] (effs [Effect_set.Env] (TUnit) (TList (TTuple [TString; TString]))));
  ("env_home",    generalize [] (effs [Effect_set.Env] (TUnit) (TPath)));
  ("env_user",    generalize [] (effs [Effect_set.Env] (TUnit) (TString)));
  (* List primitives *)
  ("list_tally",   generalize [] ((TList TString @-> TMap TInt)));
  ("list_get",     let a = fresh () in generalize [] ((TInt @-> (TList a @-> TApp (TName "Option", a)))));
  ("list_get_exn", let a = fresh () in generalize [] (TInt @-> effs [Effect_set.Raise] (TList a) (a)));
  ("list_sort",    let a = fresh () in generalize [] ((TList a @-> TList a)));
  ("list_sort_by", let a = fresh () in let b = fresh () in
                   generalize [] (((a @-> b) @-> (TList a @-> TList a))));
  ("list_unique",  let a = fresh () in generalize [] ((TList a @-> TList a)));
  ("list_range",   generalize [] ((TInt @-> (TInt @-> TList TInt))));
  ("list_flatten", let a = fresh () in generalize [] ((TList (TList a) @-> TList a)));
  ("list_concat",  let a = fresh () in generalize [] ((TList a @-> (TList a @-> TList a))));
  (* Map builtins *)
  ("map_empty",    let a = fresh () in generalize [] (TMap a));
  ("map_get",      let a = fresh () in generalize [] ((TString @-> (TMap a @-> TApp (TName "Option", a)))));
  ("map_get_exn",  let a = fresh () in generalize [] (TString @-> effs [Effect_set.Raise] (TMap a) (a)));
  ("map_set",      let a = fresh () in generalize [] ((TString @-> (a @-> (TMap a @-> TMap a)))));
  ("map_delete",   let a = fresh () in generalize [] ((TString @-> (TMap a @-> TMap a))));
  ("map_has",      let a = fresh () in generalize [] ((TString @-> (TMap a @-> TBool))));
  ("map_keys",     let a = fresh () in generalize [] ((TMap a @-> TList TString)));
  ("map_values",   let a = fresh () in generalize [] ((TMap a @-> TList a)));
  ("map_size",     let a = fresh () in generalize [] ((TMap a @-> TInt)));
  ("map_to_list",  let a = fresh () in generalize [] ((TMap a @-> TList (TTuple [TString; a]))));
  ("map_from_list",let a = fresh () in generalize [] ((TList (TTuple [TString; a]) @-> TMap a)));
  ("map_update",   let a = fresh () in
                   generalize [] ((TString @-> (a @-> ((a @-> a) @-> (TMap a @-> TMap a))))));
  ("map_merge",    let a = fresh () in generalize [] ((TMap a @-> (TMap a @-> TMap a))));
  ("map_map",      let a = fresh () in let b = fresh () in
                   generalize [] (((a @-> b) @-> (TMap a @-> TMap b))));
  ("map_filter",   let a = fresh () in generalize [] (((a @-> TBool) @-> (TMap a @-> TMap a))));
]

(* Built-in type definitions always available *)

(* `Option` is as built in as `Result`: `Env.get`, `Map.get` and `List.get`
   answer with one, `Decode.optional` builds one, a derived decoder reads an
   absent field as one, and both serialisers write `Some` through and leave
   `None` out. It was declared in `stdlib/Option.wand` all the same, so the
   name needed an import that no other type of its standing needed -- and
   `type Opts(tag : Option String)` said "unknown type 'Option'" in a file
   that had every reason to think it knew what an Option was.

   Kept as an ordinary `Variants` rather than a type of its own, so
   exhaustiveness, matching and derivation read it the way they read any
   declaration. *)
let option_tdef : type_def =
  Variants ("Option", ["a"], [
    { name = "None"; loc = None; fields = []; defaults = [] };
    { name = "Some"; loc = None; fields = [ (None, TEVar ("a", None)) ]; defaults = [] };
  ])

let shell_result_tdef : type_def =
  Variants ("ShellResult", [], [{
    name   = "ShellResult";
    loc    = None;
    fields = [ (Some "stdout", TEName "String");
               (Some "stderr", TEName "String");
               (Some "code",   TEName "Int") ];
    defaults = [];
  }])

(* Everything needed to read one command line and to describe it: what the
   flags do that their own text cannot say, the reader for the type, and the
   usage line. Derived together as `T.parser`, because they are only ever
   used together and nothing stopped one type's account meeting another
   type's reader -- `Args.read B.spec A.reader argv` typechecked, and failed
   during the run with a message about the data.

   Built in rather than declared in `stdlib/Args.wand` because a module's
   types are keyed by its path, which moves with `WAND_STDLIB`, and the
   compiler names this one. `CommandLine` rather than `Parser`: a built-in
   name cannot be declared over, and `Parser` is a name a file has every
   right to want. *)
let command_line_tdef : type_def =
  Variants ("CommandLine", ["a"], [{
    name   = "CommandLine";
    loc    = None;
    fields = [ (Some "spec",   TEApp (TEName "Map", TEName "String"));
               (Some "reader", TEApp (TEName "Decoder", TEVar ("a", None)));
               (Some "usage",  TEName "String") ];
    defaults = [];
  }])

(* What a request is made of, and what one answers with.

   Built in for the reason `CommandLine` is: the compiler names these -- the
   `Net!http` operation's payload is one of them -- and a module's types are
   keyed by its path, which moves with `WAND_STDLIB`.

   Named `HTTPRequest` rather than `Request` for the other reason
   `CommandLine` gives: a built-in name cannot be declared over, and
   `Request`, `Response` and `Method` are three names a file has every right
   to want.

   `stdlib/HTTP.wand` aliases all three to `HTTP.Request`, `HTTP.Response`
   and `HTTP.Method`. An alias declared in a module resolves inside it and
   under the qualified name outside it, in an annotation and in a pattern
   alike, so the two spellings are one type.

   `HTTPMethod` is in `module_only_ctors`, so its constructors are the one
   part of this that a file cannot write bare: `GET` and `POST` are HTTP's
   words rather than the language's, and `HTTP.GET` reads them through the
   alias. The type stays built in because the compiler names it -- it is a
   field of the `Net!http` payload -- and a module's types are keyed by a
   path that moves with `WAND_STDLIB`.

   Every field but the URL has a default, so field defaults do the work a
   builder pattern does elsewhere and record update gives the chaining. *)
let http_method_tdef : type_def =
  Variants ("HTTPMethod", [],
    List.map (fun n -> { name = n; loc = None; fields = []; defaults = [] })
      ["GET"; "POST"; "PUT"; "PATCH"; "DELETE"; "HEAD"])

let http_request_tdef : type_def =
  Variants ("HTTPRequest", [], [{
    name   = "HTTPRequest";
    loc    = None;
    fields = [ (Some "url",       TEName "URL");
               (Some "method",    TEName "HTTPMethod");
               (Some "headers",   TEApp (TEName "Map", TEName "String"));
               (Some "body",      TEName "String");
               (Some "timeout",   TEName "Duration");
               (Some "redirects", TEName "Int") ];
    defaults = [ ("method",    Ast.Constr "GET");
                 ("headers",   Ast.MapLit []);
                 ("body",      Ast.String "");
                 ("timeout",   Ast.Duration "30s");
                 ("redirects", Ast.Int 5) ];
  }])

(* A body is a `String` because a wand `String` is a byte string, and
   because the alternative -- a sum with a `File` case -- would put
   `FS.Read` in the signature of every script that posts a little JSON.
   File work has its own functions, where the signature stays exact. *)
let http_response_tdef : type_def =
  Variants ("HTTPResponse", [], [{
    name   = "HTTPResponse";
    loc    = None;
    fields = [ (Some "status",  TEName "Int");
               (Some "headers", TEApp (TEName "Map", TEName "String"));
               (Some "body",    TEName "String") ];
    defaults = [];
  }])

let builtin_tenv : typedef_env = [
  ("HTTPMethod", http_method_tdef);
  ("HTTPRequest", http_request_tdef);
  ("HTTPResponse", http_response_tdef);
  ("Option", option_tdef);
  ("ShellResult", shell_result_tdef);
  ("CommandLine", command_line_tdef);
]

(* Every function a file calls comes from a module it imported, so nothing
   is in scope here. `Ok` and `Error` are constructors of a built-in type,
   which the typechecker knows about elsewhere. *)
let builtin_type_env : env = []

(* ── Manifests ────────────────────────────────────────────────────────────── *)

(* One spelling of every effect, in Effect_set, so a manifest and a written
   signature cannot drift apart from a printed one. *)
let eff_of_label = Effect_set.of_name

(* Every effect anywhere in a type, including the arrows nested inside it: a
   function that returns a function still performs what the inner one does
   once it is called. *)
(* The labels a type commits its file to.

   An effect on an argument's arrow is a demand on the caller, not something
   the file does. `pinned : (Unit -> 'a ! {Clock | 'e}) -> 'a ! 'e` asks for
   a thunk that may read a clock and answers one that cannot -- a handler
   doing exactly its job -- and counting that row put Clock in the manifest
   of every file that mocked one away. A file could not say it had stopped
   an effect without also declaring it.

   So argument positions do not count, and positions flip through them the
   way they do in any variance rule: a function the file is handed is one
   the file calls, and a function handed to *that* one is one the file
   supplies. Nothing the file performs is lost, because performing it puts
   the label on an arrow the file returns, and those are all followed.

   Only arrows flip. A resource or a stream carries its row wherever it
   sits: it is a value with effects attached rather than a call, and reading
   one the caller opened is still reading. *)
let rec labels_of_typ ?(demanded = false) t =
  let self ?(flip = false) t =
    labels_of_typ ~demanded:(if flip then not demanded else demanded) t
  in
  match repr t with
  | TFun (a, b, r) ->
    let own = if demanded then Effect_set.EffSet.empty else Effect_set.labels_of r in
    Effect_set.EffSet.union own
      (Effect_set.EffSet.union (self ~flip:true a) (self b))
  | TTuple ts -> List.fold_left (fun acc t ->
      Effect_set.EffSet.union acc (self t)) Effect_set.EffSet.empty ts
  | TList t | TMap t -> self t
  | TResult (e, t) -> Effect_set.EffSet.union (self e) (self t)
  | TResource (r, t) ->
    Effect_set.EffSet.union (Effect_set.labels_of r) (self t)
  | TStream (r, t) ->
    Effect_set.EffSet.union (Effect_set.labels_of r) (self t)
  | TDecoder t -> self t
  | TApp (f, a) -> Effect_set.EffSet.union (self f) (self a)
  | TIface (_, args) ->
    List.fold_left (fun acc a -> Effect_set.EffSet.union acc (self a))
      Effect_set.EffSet.empty args
  | _ -> Effect_set.EffSet.empty

(* A manifest bounds what a file can do to the machine. Raise is control
   flow, not reach: it is already visible in a `!` name and in a signature,
   and including it would put Raise in almost every manifest while saying
   nothing about blast radius. *)
let manifest_relevant labels = Effect_set.EffSet.remove Effect_set.Raise labels


(* The manifest labels using a member commits a file to -- what typing
   `FS.write_file!` obliges `uses {...}` to say. Concrete labels only: a
   a polymorphic set means the function passes its argument's effects
   through, which commits the caller to nothing by itself. `Raise` is
   excluded like everywhere manifests are concerned. Serves the editor's
   auto-import tier and the manifest check below, so the two
   cannot disagree about what a member implies. *)
let manifest_labels_of_scheme (s : scheme) : Effect_set.EffSet.t =
  match s with
  | Mono t | Poly (_, _, t) -> manifest_relevant (labels_of_typ t)
  | Namespace _ -> Effect_set.EffSet.empty

(* `?shell` narrows the Shell label to the binaries the file was seen to
   run: `uses {Shell(git, curl), FS.Write}`. Passed only when every command
   position in the file is literal, so the narrowed suggestion is never a
   lie about what an interpolated word might spawn. *)
let render_manifest ?shell labels =
  "uses {" ^ String.concat ", "
    (List.map (fun e ->
       match Effect_set.name_of e, shell with
       | "Shell", Some ws -> Shell_scan.render_label ("Shell", Some ws)
       | n, _ -> n)
     (Effect_set.EffSet.elements labels)) ^ "}"

(* ── Shell command words ─────────────────────────────────────────────────── *)

(* What the file's own $()/$?()/$*() sites say, syntactically: the literal
   command words (for suggesting a narrowed manifest), whether every
   position was literal, and the declared allowlist. The linter reads
   these; `check_shell_words` fills them. *)
let last_shell_words  : string list ref = ref []
let last_shell_static : bool ref = ref true
let last_shell_allow  : string list option ref = ref None

(* Every $()/$?()/$*() payload in the file, with the nearest enclosing location.
   Only this file's text: an imported helper's sites are that file's
   manifest's business, which is what keeps the audit story compositional. *)
let shell_sites (prog : program) : (Token.loc * Ast.expr) list =
  let sites = ref [] in
  let no_loc = Token.point 1 1 0 in
  let rec go loc (e : Ast.expr) =
    match e with
    | Located (l, inner) -> go l inner
    | RunCmd (payload, _) | RunQuery (payload, _) | MkCommand (payload, _) ->
      sites := (loc, payload) :: !sites;
      go loc payload                    (* nested $() inside interpolations *)
    | App (a, b) | BinOp (_, a, b) | Seq (a, b) -> go loc a; go loc b
    | UnOp (_, a) | Fn (_, a) | Annot (_, a) | Field (a, _) | Try a ->
      go loc a
    | Let (_, a, b, _) -> go loc a; go loc b
    | LetRec (bs, b, _) -> List.iter (fun (_, _, e) -> go loc e) bs; go loc b
    | If (c, t, e) -> go loc c; go loc t; go loc e
    | Match (s, cases) ->
      go loc s;
      List.iter (fun (_, g, b) ->
        (match g with Some g -> go loc g | None -> ()); go loc b) cases
    | Tuple es | List es -> List.iter (go loc) es
    | MapLit kvs -> List.iter (fun (_, v) -> go loc v) kvs
    | ConstrApp (_, fs, _) -> List.iter (fun (_, v) -> go loc v) fs
    | ConstrBare (_, _) -> ()
    | ConstrUpdate (_, b, fs, _) -> go loc b; List.iter (fun (_, v) -> go loc v) fs
    | Interp (parts, _) | RawInterp (parts, _) ->
      List.iter (fun (_, e) -> go loc e) parts
    | CmdInterp (parts, _) -> List.iter (fun (_, e, _) -> go loc e) parts
    | Handle (b, cases) ->
      go loc b;
      List.iter (function
        | Ast.EffectCase (_, _, _, x) -> go loc x
        | Ast.ReturnCase (_, x) -> go loc x) cases
    | With (r, _, b) -> go loc r; go loc b
    | Contract (rs, es, b) ->
      List.iter (go loc) rs; List.iter (go loc) es; go loc b
    | _ -> ()
  in
  List.iter (fun (item : Ast.top_item) ->
    match item with
    | TLLet (_, _, b) -> go no_loc b
    | TLLetPat (_, b) -> go no_loc b
    | TLLetRec bs -> List.iter (fun (_, _, b) -> go no_loc b) bs
    | TLExpr b -> go no_loc b
    | TLImplement (im, _) ->
      List.iter (fun (_, _, b, _) -> go no_loc b) im.Ast.im_binds
    | TLImport _ | TLType _ | TLInterface _ -> ()) prog.items;
  List.rev !sites

let check_shell_words (prog : program) =
  let labels = Option.map fst prog.manifest in
  let allow = match labels with
    | Some ls -> Option.join (List.assoc_opt "Shell" ls)
    | None -> None
  in
  last_shell_allow := allow;
  let words = ref [] in
  let static = ref true in
  (* Rendered in canonical form -- labels and binaries sorted -- so the
     suggested line is always already what `wand f` would emit. *)
  let corrected_with word =
    match labels with
    | Some ls ->
      "uses {" ^ String.concat ", "
        (List.map (fun (n, a) ->
           Shell_scan.render_label
             (n, if n = "Shell"
                 then Option.map (fun ws -> List.sort compare (ws @ [word])) a
                 else Option.map (List.sort compare) a))
           (List.sort (fun (a, _) (b, _) -> compare a b) ls))
      ^ "}"
    | None -> ""
  in
  List.iter (fun ((loc : Token.loc), payload) ->
    let s = Shell_scan.scan (Shell_scan.segs_of_cmd payload) in
    if s.Shell_scan.raw_tail then static := false;
    List.iter (fun w ->
      match (w : Shell_scan.word_class) with
      | Shell_scan.Literal word ->
        if not (List.mem word !words) then words := word :: !words;
        (match allow with
         | Some allow_list when not (Shell_scan.allowed ~allow:allow_list word) ->
           (match corrected_with word with
            | "" -> ()
            | line -> pending_fix := Some (Diag.ReplaceLine line));
           raise (TypeErrorAt (loc, Printf.sprintf
             "this command runs '%s', which %s does not allow.\n       \
              The manifest could be:  \"%s\""
             word
             (Shell_scan.render_label ("Shell", Some allow_list))
             (corrected_with word)))
         | _ -> ())
      | Shell_scan.Dynamic -> static := false
      | Shell_scan.Compound kw ->
        static := false;
        (match allow with
         | Some allow_list ->
           raise (TypeErrorAt (loc, Printf.sprintf
             "this $() uses shell control flow ('%s'), which %s \
              cannot bound.\n       Write the loop in wand (List.each over \
              one $() per item), or declare bare Shell."
             kw
             (Shell_scan.render_label ("Shell", Some allow_list))))
         | None -> ())
    ) s.Shell_scan.words
  ) (shell_sites prog);
  last_shell_words := List.sort compare !words;
  last_shell_static := !static

(* The narrowed form for suggestions, when it would be honest. *)
let shell_suggestion () =
  if !last_shell_static && !last_shell_words <> []
  then Some !last_shell_words
  else None

(* A manifest bounds what the file can do, so it is checked against every
   binding in it -- not only what running the file performs. A function that
   shells out still shells out when something else imports and calls it. *)
(* What the last manifest check concluded, for the linter: the declared set,
   what the file actually uses, and where the manifest sits. Reported as a
   warning rather than an error -- permitting more than you use is the safe
   direction, and failing a build over it would punish caution. *)
let last_manifest : (Effect_set.EffSet.t * Effect_set.EffSet.t * Token.loc) option ref =
  ref None

(* What the file reaches outside itself to do, whether or not it says so.
   Recorded for every file, because the linter's question about a file with
   no manifest is exactly this set: a file that does nothing outward has
   nothing to declare, and one that does should say what. *)
let last_file_effects : Effect_set.EffSet.t ref = ref Effect_set.EffSet.empty

let check_manifest (prog : program) (own_env : env) =
  last_manifest := None;
  (* Command words first: a disallowed literal word or a compound command
     under a narrowed manifest is an error in its own right, and the
     word/staticness facts feed every suggestion rendered below. *)
  check_shell_words prog;
  let per_binding =
    List.filter_map (fun (name, scheme) ->
      let ls = manifest_labels_of_scheme scheme in
      if Effect_set.EffSet.is_empty ls then None else Some (name, ls)
    ) own_env
  in
  let inferred =
    List.fold_left (fun acc (_, ls) -> Effect_set.EffSet.union acc ls)
      (manifest_relevant (Effect_set.labels_of !current_eff)) per_binding
  in
  last_file_effects := inferred;
  match prog.manifest with
  | None -> ()
  | Some (labels, loc) ->
    let declared =
      List.fold_left (fun acc (name, _) ->
        match eff_of_label name with
        | Some e -> Effect_set.EffSet.add e acc
        | None ->
          raise (TypeErrorAt (loc, Printf.sprintf
            "'%s' is not an effect. The effects are %s" name
            (String.concat ", " (List.map Effect_set.name_of Effect_set.all))))
      ) Effect_set.EffSet.empty labels
    in
    last_manifest := Some (declared, inferred, loc);
    let missing = Effect_set.EffSet.diff inferred declared in
    if not (Effect_set.EffSet.is_empty missing) then begin
      (* Name a binding that needs one of the missing effects, so the reader
         is pointed at the code rather than only told the total is wrong. *)
      (* Name the binding that accounts for most of what is missing, rather
         than the first that happens to share one effect with it. *)
      let culprit =
        List.fold_left (fun best (name, ls) ->
          let overlap =
            Effect_set.EffSet.cardinal (Effect_set.EffSet.inter ls missing) in
          match best with
          | Some (_, n) when n >= overlap -> best
          | _ when overlap = 0 -> best
          | _ -> Some (name, overlap)
        ) None (List.rev per_binding)
      in
      let where = match culprit with
        | Some (name, _) -> Printf.sprintf "'%s' " name
        | None -> ""
      in
      let corrected = render_manifest ?shell:(shell_suggestion ()) inferred in
      pending_fix := Some (Diag.ReplaceLine corrected);
      raise (TypeErrorAt (loc, Printf.sprintf
        "%sperforms %s, which the manifest does not allow.\n       The manifest should be:  \"%s\""
        where
        (String.concat ", "
          (List.map Effect_set.name_of (Effect_set.EffSet.elements missing)))
        corrected))
    end

(* Single inference pass: builds env and returns (tenv, full_env, own_env, last_expr_typ). *)
(* The type of each bare top-level expression, by item index. A named
   definition's type is in `own_env`; an expression has no name, and the
   lint that catches a discarded `Result` needs it. *)
let expr_item_types : (int * typ) list ref = ref []

(* What each bare top-level expression performed, by item index. The
   interactive session asks this to decide whether a Unit answer is worth
   printing: a call that performed IO has already said what it had to say,
   and `() : Unit` after it would be noise, whereas a pure expression that
   evaluates to Unit -- `()` itself, or an `if` whose arms are both `()` --
   is a value the user asked to see. Only the concrete labels are kept: an
   unresolved tail on a top-level expression means "performs nothing
   known", which reads the same as pure here. *)
let expr_item_effects : (int * Effect_set.EffSet.t) list ref = ref []

(* `type Point = Pair` parses as a variant with one nullary constructor,
   because whether `Pair` names a type is not known until every declaration
   has been read. Here it is, so a lone constructor whose name is some other
   type becomes the alias it was written as.

   This runs before the typechecker and before the evaluator, over the same
   program, because the two disagreeing about what a declaration means is
   the bug this whole area already had once. Every caller settles first for
   the same reason: a lint or a tenv built from an unsettled program has the
   alias declaring a constructor over the very name it aliases. *)
let settle_aliases ?(init_tenv=[]) (prog : program) : program =
  let declared =
    List.filter_map (function
      | TLType (Variants (n, _, _), _) | TLType (Alias (n, _, _), _) -> Some n
      | _ -> None) prog.items
    @ List.map fst init_tenv @ builtin_type_names
    (* The built-in records as well as the built-in primitives. Without
       them `type SR = ShellResult` was not a name that "is some other
       type", so it stayed a variant declaring a nullary constructor called
       `ShellResult` -- which shadowed the real one, and took its fields
       with it for the rest of the file. *)
    @ List.map fst builtin_tenv
  in
  let settle = function
    | TLType (Variants (n, params, [{ name = c; fields; _ }]), loc)
      (* Not its own name: `type Wrap 'a = Wrap 'a` is the wrapper form,
         where the constructor says the type's name again. Only a name that
         is some *other* type makes this an alias. *)
      when c <> n
        && List.mem c declared
        && List.for_all (fun (f, _) -> f = None) fields ->
      (* Positional fields on a name that is a type are its arguments, not a
         payload: `type Ids = List Int` is the applied type, where
         `type Shape = Circle Int` -- `Circle` naming no type -- is a
         constructor carrying one. *)
      let te =
        List.fold_left (fun acc (_, ft) -> TEApp (acc, ft)) (TEName c) fields in
      TLType (Alias (n, params, te), loc)
    | item -> item
  in
  { prog with items = List.map settle prog.items }

let infer_program_body ?(base_env=builtin_type_env) ?(init_tenv=[]) ?(init_env=[])
    ?(init_ifaces=[]) (prog : program) : typedef_env * env * env * typ =
  next_id := 0;
  (* Seeded per file rather than accumulated: an interface is in scope where
     it is declared and where it is imported, and nowhere else. *)
  Hashtbl.reset iface_defs;
  List.iter (fun (n, i) -> Hashtbl.replace iface_defs n i) builtin_ifaces;
  List.iter (fun (n, i) -> Hashtbl.replace iface_defs n i) init_ifaces;
  expr_item_types := [];
  expr_item_effects := [];
  local_binders := [];
  current_item := -1;
  seq_discard_types := [];
  wild_let_types := [];
  last_shell_words := [];
  last_shell_static := true;
  last_shell_allow := None;
  pending_fix := None;
  current_eff := Effect_set.unknown ();
  Hashtbl.reset ctor_scheme_cache;
  ctor_env_memo := None;
  visible_set := None;
  holes := [];
  let prog = settle_aliases ~init_tenv prog in
  (* Read before the items are walked, so an implementation can precede the
     interface it answers to, the way a function may call one defined below
     it. *)
  List.iter (function
    | TLInterface (i, loc) ->
      (* A name declares one thing, and a built-in's name is taken before
         any file is read. *)
      if List.mem_assoc i.Ast.if_name builtin_ifaces then
        fail_at_opt loc (Printf.sprintf
          "'%s' is a built-in interface, so a declaration cannot take its \
           name; rename this one" i.Ast.if_name);
      if List.mem_assoc i.Ast.if_name init_ifaces then
        fail_at_opt loc (Printf.sprintf
          "'%s' is already an interface this file imports: a name declares \
           one thing" i.Ast.if_name);
      List.iter (fun (mname, te) ->
        match undetermined_member_effect te with
        | None -> ()
        | Some v ->
          fail_at_opt loc (Printf.sprintf
            "'%s' answers with effects ''%s', and no argument of it names \
             ''%s', so the effects are not bounded: a module implementing \
             '%s' could perform anything and a file calling it would not be \
             told. Write what it performs, as '! {Shell}', or leave the \
             effects off where it performs none"
            mname v v i.Ast.if_name)) i.Ast.if_members;
      Hashtbl.replace iface_defs i.Ast.if_name i
    | _ -> ()) prog.items;
  let local_tenv = List.filter_map (function
    | TLType (((Variants (n, _, _) | Alias (n, _, _)) as tdef), _) -> Some (n, tdef)
    | _ -> None) prog.items
  in
  (* A module's own types are keyed by the module, so that two modules which
     each declare `Status` declare two types. The file writes the short name
     and `canonical_type_name` says which one it means. *)
  let local_tenv =
    List.map (fun (n, d) -> (canonical_type_name n, d)) local_tenv in
  let tenv = local_tenv @ init_tenv @ builtin_tenv in
  (* Every type this file can name: its own, whatever it imported, and the
     builtins. Written names, not canonical ones -- this is what a reader
     may type. The definitions are collected before any of them is read, so
     a field may still name a type declared further down. Set around the
     constructor env too -- that is where a field's type is first read, and
     an unknown name in one has to be caught there rather than wherever the
     constructor is eventually used. *)
  let known =
    List.map fst !type_name_map
    @ List.filter_map (function
        | TLType (((Variants (n, _, _) | Alias (n, _, _)), _)) -> Some n
        | _ -> None) prog.items
    @ List.map fst builtin_tenv
  in
  (* Checked here, ahead of the constructor env, so the message can say which
     declaration invented the name. Reaching it through `type_of_te_bound`
     instead would report it wherever the constructor was first used, or --
     for a constructor nobody calls -- not at all. An annotation is left to
     that path, where the expression carries its own location. *)
  let rec te_names = function
    | TEName n     -> [n]
    | TEQual (m, n) -> [m ^ "." ^ n]
    | TEVar _      -> []
    | TEApp (f, a) -> te_names f @ te_names a
    | TETuple ts   -> List.concat_map te_names ts
    | TEFun (a, b, _) -> te_names a @ te_names b
  in
  List.iter (function
    | TLType (Variants (tname, params, ctors), _) ->
      List.iter (fun c ->
        List.iter (fun (fname, te) ->
          List.iter (fun n ->
            if not (builtin_type_name n || List.mem n known || List.mem n params)
            then
              let where = match fname with
                | Some f -> Printf.sprintf "field '%s' of '%s'" f c.name
                | None   -> Printf.sprintf "a field of '%s'" c.name
              in
              (* The single-constructor shorthand names both the same, and
                 saying it twice reads like two different things. *)
              let decl =
                if c.name = tname then ""
                else Printf.sprintf ", declaring '%s'" tname
              in
              raise (TypeError (Printf.sprintf
                "unknown type '%s' in %s%s%s"
                n where decl (Util.hint n (known @ builtin_type_names)))))
            (te_names te))
          c.fields)
        ctors
    | _ -> ()) prog.items;
  (* A name declares one thing. Two `type` declarations of one name, or two
     constructors sharing one, used to be taken silently -- and which of
     them won differed between a file and the REPL, so one text had two
     meanings. Worse, the loser stayed constructible and stopped being
     matchable: `match x with | Foo -> ...` reported the other type.

     A file is one text, so a repeat there is a mistake rather than an
     intent. The REPL still replaces, which is what a REPL is for. *)
  let seen_types = Hashtbl.create 16 in
  let seen_ctors = Hashtbl.create 16 in
  List.iter (function
    (* An alias is a declaration of the name too, so it answers to both rules
       below. It used to answer to neither -- the walk read variants only --
       so `type Int = String` was taken, and every `Int` in the file after it
       meant `String`. A built-in cannot be declared over, whichever form the
       declaration takes. *)
    | TLType (Alias (tname, _, _), tdef_loc) ->
      if builtin_type_name tname then
        fail_at_opt tdef_loc (Printf.sprintf
          "'%s' is a built-in type, so it cannot be declared" tname);
      if Hashtbl.mem seen_types tname then
        fail_at_opt tdef_loc (Printf.sprintf "'%s' is declared twice" tname);
      Hashtbl.add seen_types tname ()
    | TLType (Variants (tname, _, ctors), tdef_loc) ->
      (* A builtin's name is taken too. Declaring over one used to be
         accepted, and then field access on the result answered "field
         access requires a named type, got Size" -- which reads like
         nonsense, because the name resolved to the builtin while the
         constructor came from here. *)
      if builtin_type_name tname then
        fail_at_opt tdef_loc (Printf.sprintf
          "'%s' is a built-in type, so it cannot be declared" tname);
      (* The location is the second declaration's, which is the one a repeat
         is about: pointing at the first sends a reader -- and anything that
         edits by location -- to the declaration that was already fine. *)
      if Hashtbl.mem seen_types tname then
        fail_at_opt tdef_loc (Printf.sprintf "'%s' is declared twice" tname);
      Hashtbl.add seen_types tname ();
      List.iter (fun c ->
        (* A name declares one thing here too. Two fields of one name used to
           be taken silently, and the first won: the second's type was never
           checked against anything and its default never applied, so a
           declaration could carry a value that nothing could ever read. *)
        let seen_fields = Hashtbl.create 8 in
        List.iter (fun (fname, _) ->
          match fname with
          | None -> ()
          | Some f ->
            if Hashtbl.mem seen_fields f then
              fail_at_opt c.loc (Printf.sprintf
                "constructor '%s' declares field '%s' twice" c.name f);
            Hashtbl.add seen_fields f ()) c.fields;
        (match Hashtbl.find_opt seen_ctors c.name with
         | Some owner ->
           let where =
             if owner = tname then Printf.sprintf "twice in '%s'" tname
             else Printf.sprintf "by '%s' and by '%s'" owner tname
           in
           fail_at_opt c.loc (Printf.sprintf
             "constructor '%s' is declared %s" c.name where)
         | None -> Hashtbl.add seen_ctors c.name tname)) ctors
    | _ -> ()) prog.items;
  (* A value cannot take a name a type or a constructor already has. The two
     were accepted together and read by position: `Pod.decoder` gave the
     derived decoder, `Pod(host = "a")` constructed, and bare `Pod` was an
     error suggesting the constructor. So the value was unreachable, and
     nothing said so. Types are checked above; this is the same rule, at the
     other kind of declaration. *)
  List.iter (fun item ->
    let bound = match item with
      | TLLet (name, _, body) -> [(name, loc_of_expr body)]
      | TLLetRec bs -> List.map (fun (n, _, b) -> (n, loc_of_expr b)) bs
      | TLLetPat (pat, body) ->
        let loc = loc_of_expr body in
        let rec names (p : pat) =
          match p with
          | PVar n -> [n]
          | PTuple ps | PList ps -> List.concat_map names ps
          | PCons (h, t) -> names h @ names t
          | PMap kvs | PConstrNamed (_, kvs) ->
            List.concat_map (fun (_, p) -> names p) kvs
          | PConstr (_, ps) -> List.concat_map names ps
          | PAnnot (p, _) -> names p
          | _ -> []
        in
        List.map (fun n -> (n, loc)) (names pat)
      | _ -> []
    in
    List.iter (fun (name, loc) ->
      if Hashtbl.mem seen_types name then
        fail_at_opt loc (Printf.sprintf
          "'%s' is a type, so it cannot also name a value" name)
      else if Hashtbl.mem seen_ctors name then
        fail_at_opt loc (Printf.sprintf
          "'%s' is a constructor, so it cannot also name a value" name)) bound
  ) prog.items;
  let alias_table =
    List.filter_map (function
      | (n, Alias (_, params, te)) -> Some (n, (params, te))
      | _ -> None) tenv
  in
  (* A module's types are keyed by the module, so a module's own alias is in
     the table under its canonical name while the file that declared it
     writes the short one. Both spellings answer, or `type Response =
     HTTPResponse` resolved for every file except the one it was written in,
     where `(r: Response)` stayed an opaque name and field access on it had
     nothing to read. *)
  known_aliases :=
    alias_table
    @ List.filter_map (fun (written, canon) ->
        if written = canon then None
        else Option.map (fun v -> (written, v))
               (List.assoc_opt canon alias_table)) !type_name_map;
  let arity_table =
    List.filter_map (function
      | (n, Variants (_, params, _)) -> Some (n, List.length params)
      | _ -> None) tenv
  in
  known_type_arities :=
    arity_table
    @ List.filter_map (fun (written, canon) ->
        if written = canon then None
        else Option.map (fun v -> (written, v))
               (List.assoc_opt canon arity_table)) !type_name_map;
  (* The file that declares the alias reads the target's constructors
     unqualified: `stdlib/HTTP.wand` writes `method = POST`, and outside it
     the same constructor is `HTTP.POST`. *)
  let own_module_only = List.map fst (via_module_only_alias tenv local_tenv) in
  with_visible own_module_only (fun () ->
  with_known_type_names known (fun () ->
  let base_env = tenv_to_ctor_env tenv @ base_env @ init_env in
  (* A field default is checked once, here, rather than at each construction
     that leaves the field out: the default belongs to the declaration, and a
     construction that omits the field has nothing at its own site to blame.
     It has to be a value written out, so there is no environment to read it
     in and no effect for a construction to declare. *)
  List.iter (function
    | TLType (Variants (_, _, ctors), _) ->
      List.iter (fun c ->
        List.iter (fun (fname, e) ->
          if not (Ast.is_written_value e) then
            fail_at_opt (loc_of_expr e) (Printf.sprintf
              "the default for field '%s' of '%s' has to be a value written \
               out: a literal, or a constructor applied to literals. It is \
               read with nothing in scope, so it says the same thing at \
               every construction that leaves the field out" fname c.name);
          let declared =
            match List.find_opt (fun (dn, _) -> dn = Some fname) c.fields with
            | Some (_, te) -> type_of_te te
            | None -> fresh ()
          in
          let got = infer tenv (tenv_to_ctor_env tenv) e in
          (try unify_expected ~expected:declared ~got
           with TypeError why ->
             fail_at_opt (loc_of_expr e) (Printf.sprintf
               "the default for field '%s' of '%s' does not have the field's \
                type: %s" fname c.name why))
        ) c.defaults) ctors
    | _ -> ()) prog.items;
  let item_index = ref (-1) in
  let (env, last_t) =
  List.fold_left (fun (env, last_t) item ->
    incr item_index;
    current_item := !item_index;
    match item with
    | TLLet (_, [], body) when is_import_expr body ->
      (env, last_t)  (* pre-loaded by load_imports_for *)
    | TLLet (name, [], body) ->
      let t = infer tenv env body in
      ((name, generalize env t) :: env, last_t)
    | TLLet (name, params, body) ->
      let placeholder = fresh () in
      let env_rec = (name, Mono placeholder) :: env in
      let t = with_current_fn name (fun () -> infer tenv env_rec (Fn (params, body))) in
      unify placeholder t;
      ((name, generalize env t) :: env, last_t)
    | TLLetRec bindings ->
      let placeholders = List.map (fun (name, _, _) -> (name, fresh ())) bindings in
      let env_rec = List.map (fun (name, t) -> (name, Mono t)) placeholders @ env in
      let inferred = List.map (fun (name, params, body) ->
        let t = with_current_fn name (fun () -> infer tenv env_rec (Fn (params, body))) in
        unify (List.assoc name placeholders) t;
        (name, t)
      ) bindings in
      let env' = List.map (fun (name, t) -> (name, generalize env t)) inferred @ env in
      (env', last_t)
    | TLLetPat (_, body) when is_import_expr body ->
      (env, last_t)  (* pre-loaded by load_imports_for *)
    | TLLetPat (pat, e) ->
      let t = infer tenv env e in
      (* A top-level `let _ = ...` is this item, not the `Let` expression
         below, so the type the A-BIND1 lint reads has to be recorded here
         too. Keyed by the value's position, exactly as the expression case
         records it. *)
      (match pat, e with
       | Wild, Located (loc, _) -> wild_let_types := (loc, t) :: !wild_let_types
       | _ -> ());
      if pat_is_refutable tenv pat then
        performs (Effect_set.single Effect_set.Raise);
      let env' = infer_pat tenv pat t env in
      (env', last_t)
    | TLExpr e ->
      (* Measured in its own accumulator, then handed back to the enclosing
         one: scoping it is what separates this item's effects from the
         items around it, and re-absorbing is what keeps the file's total --
         which the manifest check reads -- whole. *)
      let (t, eff) = scoped_eff (fun () -> infer tenv env e) in
      performs eff;
      expr_item_types := (!item_index, t) :: !expr_item_types;
      expr_item_effects :=
        (!item_index, Effect_set.labels_of eff) :: !expr_item_effects;
      (env, t)
    | TLImplement (im, loc) ->
      let idef =
        match Hashtbl.find_opt iface_defs im.Ast.im_iface with
        | Some i -> i
        | None ->
          (* Reached through the module that declares it, so a bare name
             that names one gets the spelling that works. *)
          fail_at_opt loc (
            match qualified_ifaces_named im.Ast.im_iface with
            | [] -> Printf.sprintf
                "'%s' is not an interface in scope. An implementation \
                 answers to one that is declared, here or in a module this \
                 file imports" im.Ast.im_iface
            | qs -> Printf.sprintf
                "'%s' is reached through the module that declares it: write \
                 %s" im.Ast.im_iface
                (String.concat " or "
                   (List.map (fun q -> "'implement " ^ q ^ "'") qs)))
      in
      let want = List.length idef.Ast.if_params
      and got = List.length im.Ast.im_args in
      if want <> got then
        fail_at_opt loc (Printf.sprintf
          "'%s' takes %d type argument%s, and this names %d: write \
           'implement %s %s'"
          im.Ast.im_iface want (if want = 1 then "" else "s") got
          im.Ast.im_iface
          (String.concat " " (List.init want (fun i ->
             "'" ^ List.nth idef.Ast.if_params i))));
      let declared = iface_members idef (List.map type_of_te im.Ast.im_args) in
      (* A member the interface does not declare is not part of the contract,
         so the block is the wrong place for it: it would read as though the
         interface asked for it. *)
      List.iter (fun (n, _, _, _) ->
        if not (List.mem_assoc n declared) then
          fail_at_opt loc (Printf.sprintf
            "'%s' declares no member '%s', so this belongs outside the \
             block, as a binding of its own. Its members are %s"
            im.Ast.im_iface n
            (String.concat ", " (List.map fst declared)))) im.Ast.im_binds;
      List.iter (fun (n, _) ->
        if not (List.exists (fun (b, _, _, _) -> b = n) im.Ast.im_binds) then
          fail_at_opt loc (Printf.sprintf
            "'%s' declares '%s', which this implementation does not provide"
            im.Ast.im_iface n)) declared;
      (* Each binding is the module's own, and lands where writing it as a
         top-level `let` would put it. What the block adds is the check
         against the type the interface declared. *)
      let env' =
        List.fold_left (fun env (name, params, body, _) ->
          let want = List.assoc name declared in
          let placeholder = fresh () in
          let env_rec = (name, Mono placeholder) :: env in
          let t =
            with_current_fn name (fun () ->
              match params with
              | [] -> infer tenv env_rec body
              | _  -> infer tenv env_rec (Fn (params, body)))
          in
          unify placeholder t;
          (* Located at the binding rather than at the block: the member that
             does not fit is the line to look at. *)
          (try unify_expected ~expected:want ~got:t
           with TypeError msg ->
             fail_at_opt (loc_of_expr body) (Printf.sprintf
               "'%s' does not match what '%s' declares for it: %s"
               name im.Ast.im_iface msg));
          ((name, generalize env want) :: env)) env im.Ast.im_binds
      in
      (env', last_t)
    | TLType _ | TLImport _ | TLInterface _ -> (env, last_t)
  ) (base_env, TUnit) prog.items
  in
  let n_own = List.length env - List.length base_env in
  let own_env = List.filteri (fun i _ -> i < n_own) env in
  (* What this module claims, under a name no file can write, so it rides in
     the module's own environment: an import brings it along and a cached
     module keeps it, with no table to hold consistent. *)
  let claims =
    List.filter_map (function
      | TLImplement (im, _) ->
        Some (im.Ast.im_iface, List.map type_of_te im.Ast.im_args)
      | _ -> None) prog.items
  in
  let own_env =
    if claims = [] then own_env
    else (implements_key, Mono (TModule claims)) :: own_env
  in
  check_manifest prog own_env;
  (tenv, env, own_env, last_t)))

(* The string-returning entry points render the position back into the
   message; `infer_program_full_with_own` below hands it over as data. *)
let error_message = function
  | TypeError msg -> msg
  | TypeErrorAt (loc, msg) ->
    Printf.sprintf "%d:%d: %s" loc.Token.line loc.Token.col msg
  | _ -> assert false

let infer_program_ ?base_env ?init_tenv ?init_env ?init_ifaces
    ?(type_names = []) prog =
  (* A name with no dot is one this file may write: its own declarations, and
     what it selected in an import. `Foo.Status` is written with the module,
     and its constructors are reached the same way. *)
  let visible =
    List.filter_map (fun (written, canon) ->
      if String.contains written '.' then None else Some canon) type_names
  in
  forget_unbound ();
  let result =
    with_type_name_map type_names (fun () ->
      with_visible visible (fun () ->
        infer_program_body ?base_env ?init_tenv ?init_env ?init_ifaces prog))
  in
  raise_first_unbound ();
  result

let infer_program_full ?(init_tenv=[]) ?(init_env=[]) ?init_ifaces
    ?(type_names=[]) (prog : program)
    : (env * typ, string) result =
  try
    let (_, env, _, last_t) =
      infer_program_ ~init_tenv ~init_env ?init_ifaces ~type_names prog in
    Ok (env, last_t)
  with (TypeError _ | TypeErrorAt _) as e -> Error (error_message e)

(* Returns (full_env, own_env); uses stdlib_type_env as base (for module loading). *)
let infer_program_env_with_own ?(init_tenv=[]) ?(init_env=[]) ?init_ifaces
    ?(type_names=[])
    (prog : program) : (env * env, string) result =
  try
    let (_, env, own, _) =
      infer_program_ ~base_env:stdlib_type_env ~init_tenv ~init_env ?init_ifaces
        ~type_names prog in
    Ok (env, own)
  with (TypeError _ | TypeErrorAt _) as e -> Error (error_message e)

let infer_program (prog : program) : (typ, string) result =
  Result.map snd (infer_program_full prog)

let infer_program_env ?(init_tenv=[]) ?(init_env=[]) ?init_ifaces
    ?(type_names=[])
    (prog : program) : (env, string) result =
  Result.map fst
    (infer_program_full ~init_tenv ~init_env ?init_ifaces ~type_names prog)

let string_of_scheme = function
  | Mono t | Poly (_, _, t) -> string_of_typ t
  | Namespace _           -> "<namespace>"

(* The Error side is (position, message, correction): everything the raise
   site knew, as data. *)
let infer_program_full_with_own ?(base_env=builtin_type_env) ?(init_tenv=[])
    ?(init_env=[]) ?init_ifaces ?(type_names=[]) (prog : program)
    : (env * env * typ * typ list,
       Token.loc option * string * Diag.fix option) result =
  (* An unbound name is recorded rather than raised, so the check reaches the
     end and every one of them is known. Where any were found, they are the
     answer: an error raised after the first miss may be a consequence of it,
     and reporting a consequence as a mistake sends the reader to the wrong
     line. *)
  let answer_with_unbound () =
    match !unbound_names with
    | [] -> None
    | (loc, msg) :: _ -> Some (loc, msg)
  in
  try
    let (_, full_env, own_env, last_t) =
      infer_program_ ~base_env ~init_tenv ~init_env ?init_ifaces ~type_names prog in
    match answer_with_unbound () with
    | Some (loc, msg) -> Error (loc, msg, None)
    | None ->
      let hole_types = List.rev_map repr !holes in
      Ok (full_env, own_env, last_t, hole_types)
  with
  | TypeError msg ->
    (match answer_with_unbound () with
     | Some (loc, m) -> Error (loc, m, None)
     | None -> Error (None, msg, take_pending_fix ()))
  | TypeErrorAt (loc, msg) ->
    (match answer_with_unbound () with
     | Some (l, m) -> Error (l, m, None)
     | None -> Error (Some loc, msg, take_pending_fix ()))
