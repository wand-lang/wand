(* Effect sets.

   An effect set describes what evaluating something does to the outside
   world. A set is either closed -- exactly these effects and no others --
   or open, meaning these effects plus whatever an effect variable stands
   for. The open case is what lets a function written in wand stay honest
   without being over-committed: `List.map` performs whatever the function
   it is given performs, so what it performs is a variable, not a fixed set.

   The effects are fixed and few on purpose. A script cannot define new
   ones, so an effect set is always a subset of these twelve, and a reader of
   a signature has a finite vocabulary to learn. One is added when something
   can actually perform it: network access reaches the outside world through
   a command today, and so reports as Shell. Waiting is not like that --
   `Clock.sleep` is performed by the evaluator and reduces to nothing else,
   so it has a primitive that only Clock explains.

   This module knows nothing about types; `Typechecker` puts an effect set
   on the arrow and unifies it alongside them.

   (The representation is an open record of labels, which type theory calls
   a row. The word is not used here or anywhere else: it names the encoding
   rather than the idea, and a reader of this compiler does not need it.) *)

type eff =
  (* An effect set says what a caller must know that the type otherwise
     hides. Four of these leave the world alone and still have to be
     declared, because each is something a caller cannot see in the type:
     Raise is "can raise instead of returning", Proc is "ends the process",
     Clock is "may take unbounded wall-clock time", and Random is "will not
     answer the same twice". *)
  | Clock     (* waits; its behavior depends on wall-clock time *)
  | Shell     (* runs a subprocess *)
  | FsRead    (* reads from the filesystem *)
  | FsWrite   (* creates, changes or removes something on disk *)
  | Env       (* reads or changes environment variables *)
  | Net       (* sends bytes to a host outside this machine *)
  | NetListen (* accepts connections on a port of this machine *)
  | IO        (* reads or writes the program's own streams *)
  | Proc      (* ends the process; nothing catches this *)
  | Random    (* answers differently between runs; draws from entropy *)
  | Shared    (* keeps state that changes, in a `Shared` *)
  | Raise     (* can raise instead of returning *)

(* Alphabetical by rendered name. This list is the one definition of
   display order: it governs rendered effect sets, manifests, and the suggestion
   path alike (through the `EffSet` compare below), so a suggested
   manifest is always already in canonical form and a reader can predict
   where a label sits without knowing any convention beyond the
   alphabet. *)
let all = [Clock; Env; FsRead; FsWrite; IO; Net; NetListen; Proc; Raise; Random;
           Shared; Shell]

let name_of = function
  | Clock   -> "Clock"
  | Shell   -> "Shell"
  | FsRead  -> "FS.Read"
  | FsWrite -> "FS.Write"
  | Env     -> "Env"
  | IO      -> "IO"
  | Net     -> "Net"
  | NetListen -> "Net.Listen"
  | Proc    -> "Proc"
  | Random  -> "Random"
  | Shared  -> "Shared"
  | Raise   -> "Raise"

(* What a label admits, one sentence each, for a reader who hovers a
   manifest rather than reading the reference. Written in the manifest's
   terms -- what the file may do -- because that is the question the label
   is answering where it is read. *)
let description = function
  | Clock   -> "Waits: sleeps, or takes a wall-clock time it does not bound."
  | Shell   -> "Runs subprocesses. `Shell(git, curl)` narrows it to the \
                binaries named; bare `Shell` admits any."
  | FsRead  -> "Reads from the filesystem."
  | FsWrite -> "Creates, changes or removes something on disk."
  | Env     -> "Reads or changes environment variables."
  | IO      -> "Reads or writes the program's own streams."
  | Net     -> "Sends bytes to a host outside this machine. \
                `Net(api.github.com)` narrows it to the hosts named; bare \
                `Net` admits any."
  | NetListen -> "Accepts connections on a port of this machine. \
                  `Net.Listen(:8080)` narrows it to the ports named; bare \
                  `Net.Listen` admits any."
  | Proc    -> "Ends the process. Nothing catches this."
  | Random  -> "Draws from entropy: answers differently on two runs unless \
                the seed is pinned."
  | Shared  -> "Keeps state that changes: makes, reads or updates a \
                `Shared`."
  | Raise   -> "Can raise instead of returning."

(* The inverse of `name_of`, derived from it rather than written out again:
   a manifest, a written signature and a printed one all spell an effect the
   same way, and a second list would be a second thing to keep in step. *)
let of_name name = List.find_opt (fun e -> name_of e = name) all

let display_order e =
  let rec index i = function
    | []      -> i
    | x :: tl -> if x = e then i else index (i + 1) tl
  in
  index 0 all

module EffSet = Set.Make (struct
  type t = eff
  let compare a b = compare (display_order a) (display_order b)
end)

type evar = {
  id          : int;
  mutable def : t option;
}

(* Labels known to be present, plus an optional tail standing for "and
   possibly more". `Set (s, None)` is closed at exactly `s`. *)
and t = Set of EffSet.t * evar option

exception Mismatch of string

(* Two effect sets that do not fit. The caller words the message, because
   only the caller knows which of the two the reader wrote. *)
exception Conflict of t * t

let next_id = ref 0

let fresh_var () =
  let v = { id = !next_id; def = None } in
  incr next_id;
  v

(* An open set with no known effects: what something whose effects are
   not yet determined. *)
let unknown () = Set (EffSet.empty, Some (fresh_var ()))

let pure = Set (EffSet.empty, None)

let of_list es = Set (EffSet.of_list es, None)

let single e = Set (EffSet.singleton e, None)

(* Follow bound effect variables, and flatten: a tail that has been bound
   to another set contributes its effects here. *)
let rec repr (Set (labels, tail) as r) =
  match tail with
  | None -> r
  | Some v ->
    (match v.def with
     | None -> r
     | Some inner ->
       let (Set (inner_labels, inner_tail)) = repr inner in
       Set (EffSet.union labels inner_labels, inner_tail))

let labels_of r = let (Set (l, _)) = repr r in l
let tail_of   r = let (Set (_, t)) = repr r in t

let is_closed r = tail_of r = None

let mem e r = EffSet.mem e (labels_of r)

(* The set with `e` added. Used when seeding a builtin, and when a raise
   escapes an expression. *)
let add e r =
  let (Set (l, t)) = repr r in
  Set (EffSet.add e l, t)

(* The set with `e` removed, for `try` discharging Raise and a handler
   discharging what it intercepts. Only meaningful on the labels actually
   known here; an open tail may still supply it, which is why discharge
   closes the set it is applied to (see `close`). *)
let remove e r =
  let (Set (l, t)) = repr r in
  Set (EffSet.remove e l, t)

let union a b =
  let (Set (la, ta)) = repr a in
  let (Set (lb, tb)) = repr b in
  let labels = EffSet.union la lb in
  match ta, tb with
  | None, None -> Set (labels, None)
  | Some v, None | None, Some v -> Set (labels, Some v)
  | Some _, Some _ ->
    (* Two open tails: the union is open, standing for either. A fresh
       variable would over-generalize, so reuse the first. *)
    Set (labels, ta)

(* Whether `v` appears in `r`'s tail, so unification cannot build a set that
   contains itself. `tail_of` goes through `repr`, which flattens every
   bound tail away, so the variable it answers with is always unbound --
   one comparison decides. *)
let occurs v r =
  match tail_of r with
  | None -> false
  | Some v' -> v'.id = v.id

let bind v r =
  if occurs v r then
    raise (Mismatch "an effect set cannot contain itself");
  v.def <- Some r

(* What escapes a handler that answers every operation of `es`.

   Taking the labels out of the known half is enough when the body's effects
   are known: `handle Clock.now () with` is a closed set with Clock in it,
   and removing it leaves nothing. It is not enough when the body's effects
   arrive through a parameter, which is how a handler worth reusing is
   written:

     let pinned thunk =
       handle thunk () with
       | Clock!now _ k -> ..      (and every other Clock operation)

   `thunk ()` performs whatever `thunk` performs, which is an open set with
   nothing known in it. There was nothing to remove, so `pinned` came out as
   `(Unit -> 'a ! 'e) -> 'a ! 'e`: one variable standing for both what went
   in and what came out, and a caller's Clock passing straight through the
   handler that existed to stop it.

   So the tail is split rather than searched. Whatever the body turns out to
   perform is `es` and a rest, and the rest is what escapes. `pinned` then
   reads `(Unit -> 'a ! {Clock | 'e}) -> 'a ! 'e`, which is the shape
   `Par.timeout` has been written by hand with all along.

   Asking `es` of the argument does not turn a pure thunk away. A
   generalized row instantiates to a fresh open variable, so
   `pinned (fn () -> 42)` binds that variable to `es` and performs none of
   it -- the same reason `Par.timeout 1s (fn () -> 42)` typechecks. *)
let discharge es r =
  let (Set (l, t)) = repr r in
  let kept = List.fold_left (fun acc e -> EffSet.remove e acc) l es in
  match t with
  | None -> Set (kept, None)
  | Some _ when es = [] -> Set (kept, t)
  | Some v ->
    let rest = fresh_var () in
    bind v (Set (EffSet.of_list es, Some rest));
    Set (kept, Some rest)

let to_string r =
  let (Set (labels, tail)) = repr r in
  let names = EffSet.elements labels |> List.map name_of in
  match names, tail with
  | [], None    -> "{}"
  | [], Some _  -> "{..}"
  | ns, None    -> "{" ^ String.concat ", " ns ^ "}"
  | ns, Some _  -> "{" ^ String.concat ", " ns ^ " | ..}"

(* Unify two effect sets, in the three cases that arise:

   - closed against closed: the label sets must already agree, since neither
     side can grow;
   - open against closed: the variable takes on whatever the closed side has
     that the open side does not, and the open side must not claim anything
     the closed side lacks;
   - open against open: both tails are bound to a shared fresh tail carrying
     the labels each side is missing, so later information reaches both. *)
let unify a b =
  let (Set (la, ta) as ra) = repr a in
  let (Set (lb, tb) as rb) = repr b in
  match ta, tb with
  | None, None ->
    if not (EffSet.equal la lb) then
      raise (Conflict (ra, rb))
  | Some v, None ->
    if not (EffSet.subset la lb) then
      raise (Conflict (ra, rb));
    bind v (Set (EffSet.diff lb la, None))
  | None, Some v ->
    if not (EffSet.subset lb la) then
      raise (Conflict (ra, rb));
    bind v (Set (EffSet.diff la lb, None))
  | Some va, Some vb ->
    if va.id = vb.id then begin
      (* One variable on both sides, carrying different labels. This is not
         a conflict but a recursive equation -- `p = {IO} + p` -- and it is
         how a function that calls itself arrives here: performing an effect
         puts it in the ambient set, the recursive call contributes the same
         tail without it, and the two meet.

         Solved rather than rejected. Binding the variable to the labels the
         two sides disagree on leaves both reading `la + lb + p'`, which is
         the least set satisfying the equation. Without this a recursive
         function could perform no effect of its own: it typechecked only
         while every effect it had came from a function it was passed, which
         is why `List.each` is fine and a loop that prints is not. *)
      if not (EffSet.equal la lb) then
        bind va (Set (EffSet.union (EffSet.diff la lb) (EffSet.diff lb la),
                      Some (fresh_var ())))
    end else begin
      let shared = fresh_var () in
      bind va (Set (EffSet.diff lb la, Some shared));
      bind vb (Set (EffSet.diff la lb, Some shared))
    end

(* Record that `l` is performed inside a scope whose effects so far are
   `ambient`, returning the extended ambient.

   Two things happen. The labels `l` is known to carry are added, which is
   the easy half. The harder half is `l`'s tail: if what the callee does is
   still undetermined, whatever it turns out to do must also reach the
   enclosing signature, so its tail becomes the ambient's tail. Joining the
   two sets by union instead would keep only one tail and silently drop
   the other call's effects.

   Tying the tails together over-approximates -- two different callees in
   one body end up sharing the scope's unknown effects, so an effect proved
   for one is attributed to both. That is the safe direction: a signature
   may name an effect the function does not always perform, but it can never
   omit one it does. *)
let absorb ~ambient l =
  let (Set (la, ta)) = repr ambient in
  let (Set (ll, tl)) = repr l in
  (match tl, ta with
   | Some vl, Some va when vl.id <> va.id ->
     bind vl (Set (EffSet.empty, Some va))
   | Some vl, None ->
     (* The scope's effects are already fixed, so the callee adds nothing
        beyond what it is known to carry. *)
     bind vl (Set (EffSet.empty, None))
   | _ -> ());
  Set (EffSet.union la ll, ta)

(* Rebuild `r` with its effect variables replaced according to `subst`. A
   polymorphic function's effect variable must be freshened at each use, or
   every caller shares one and the first caller to perform an effect
   attributes it to all of them. *)
let subst subst r =
  let (Set (labels, tail)) = repr r in
  match tail with
  | None -> Set (labels, None)
  | Some v ->
    (match List.assoc_opt v.id subst with
     | Some v' -> Set (labels, Some v')
     | None    -> Set (labels, Some v))

(* Every effect variable reachable from `r`, so schemes can quantify them. The
   tail `tail_of` answers with is always unbound (see `occurs`), so it is
   the one free variable a set can have. *)
let free_vars r =
  match tail_of r with
  | None -> []
  | Some v -> [v.id]

(* The labels that `found` performs and `allowed` does not permit. Empty
   when the two conflict for another reason -- one side closed where the
   other is open, and neither holding a label the other lacks. *)
let extra ~allowed ~found =
  let (Set (la, _)) = repr allowed and (Set (lb, _)) = repr found in
  List.map name_of (EffSet.elements (EffSet.diff lb la))

(* `absorb`, for a call whose callee shares its caller's row: an open callee
   is tied to all of the ambient, labels as well as tail, rather than to the
   tail alone. Used once the caller's scope is done, so the ambient is all
   the caller performs. *)
let absorb_whole ~ambient l =
  let (Set (la, ta)) = repr ambient in
  let (Set (ll, tl)) = repr l in
  (match tl, ta with
   | Some vl, Some va when vl.id = va.id -> ()
   | Some vl, _ -> bind vl (Set (EffSet.diff la ll, ta))
   | None, _ -> ());
  Set (EffSet.union la ll, ta)
