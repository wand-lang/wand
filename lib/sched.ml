(* Fibers: many tasks on one domain, switched at waits and at yields.

   A scheduler is an effect handler installed where `run` is called, so a
   fiber's other effects pass out through it to whatever handlers enclose
   that call. Nothing outside this module sees a fiber. *)

external elapsed_ms : unit -> int = "wand_elapsed_ms"
external poll_fds : Unix.file_descr array -> int array -> int -> bool array
  = "wand_poll"

type wait = {
  reads : Unix.file_descr list;
  writes : Unix.file_descr list;
  until : int option;  (* elapsed_ms *)
}

type _ Effect.t += Yield : unit Effect.t
type _ Effect.t += Suspend : wait -> unit Effect.t

type t = {
  runq : (unit -> unit) Queue.t;
  mutable waiting : (wait * (unit -> unit)) list;
  mutable live : int;
  mutable failure : exn option;
  mutable interrupt_seen : bool;
  parent : t option;
}

let current : t option Domain.DLS.key = Domain.DLS.new_key (fun () -> None)

let active () = Option.is_some (Domain.DLS.get current)

(* Checked on every idle turn; set by the evaluator. *)
let interrupt_pending : (unit -> bool) ref = ref (fun () -> false)

(* The longest one idle turn blocks, so an interrupt is seen in time. *)
let slice_ms = 50

let events_of w =
  let tbl = Hashtbl.create 8 in
  let add fd bit =
    let prev = Option.value (Hashtbl.find_opt tbl fd) ~default:0 in
    Hashtbl.replace tbl fd (prev lor bit)
  in
  List.iter (fun fd -> add fd 1) w.reads;
  List.iter (fun fd -> add fd 2) w.writes;
  let pairs = Hashtbl.fold (fun fd ev acc -> (fd, ev) :: acc) tbl [] in
  (Array.of_list (List.map fst pairs), Array.of_list (List.map snd pairs))

let timeout_of until =
  match until with
  | None -> slice_ms
  | Some t -> max 0 (min slice_ms (t - elapsed_ms ()))

(* The descriptors of [w] that are ready now, waiting at most [timeout]. *)
let ready_fds w timeout =
  let fds, evs = events_of w in
  if Array.length fds = 0 then begin
    if timeout > 0 then
      (try Unix.sleepf (float_of_int timeout /. 1000.)
       with Unix.Unix_error (Unix.EINTR, _, _) -> ());
    []
  end else begin
    let ready = poll_fds fds evs timeout in
    let acc = ref [] in
    Array.iteri (fun i r -> if r then acc := fds.(i) :: !acc) ready;
    !acc
  end

let due w now = match w.until with Some t -> t <= now | None -> false

let touches w fds =
  List.exists (fun fd -> List.mem fd fds) w.reads
  || List.exists (fun fd -> List.mem fd fds) w.writes

(* Block the calling code until [w] is met or a slice has passed. Returns
   early on a signal; callers look again. *)
let block_directly w = ignore (ready_fds w (timeout_of w.until))

let suspend w =
  if active () then
    (try Effect.perform (Suspend w) with Effect.Unhandled _ -> block_directly w)
  else block_directly w

let yield () =
  if active () then
    (try Effect.perform Yield with Effect.Unhandled _ -> ())

let wait_readable fd = suspend { reads = [fd]; writes = []; until = None }
let wait_writable fd = suspend { reads = []; writes = [fd]; until = None }
let sleep_until t = suspend { reads = []; writes = []; until = Some t }

(* Make every suspended fiber of the current scheduler runnable. Each one
   looks again at what it was waiting for. *)
let wake_all_in s =
  List.iter (fun (_, resume) -> Queue.push resume s.runq) (List.rev s.waiting);
  s.waiting <- []

let wake_all () =
  match Domain.DLS.get current with Some s -> wake_all_in s | None -> ()

let union ws =
  { reads = List.concat_map (fun w -> w.reads) ws;
    writes = List.concat_map (fun w -> w.writes) ws;
    until = List.fold_left (fun acc w ->
      match acc, w.until with
      | None, u | u, None -> u
      | Some a, Some b -> Some (min a b)) None ws }

(* An interrupt wakes every suspended fiber once, so each takes it at its
   next checkpoint. Waits after that, in releases, are real waits. *)
let take_interrupt s =
  if (not s.interrupt_seen) && !interrupt_pending () then begin
    s.interrupt_seen <- true;
    wake_all_in s;
    true
  end else false

(* Move the waiters whose wait is over to the run queue. [block] says
   whether to wait for one when none is. *)
let check_waiting s ~block =
  if not (take_interrupt s) then begin
    let all = union (List.map fst s.waiting) in
    (* A scheduler inside a fiber waits by suspending that fiber. *)
    let ready =
      if not block then
        (if all.reads = [] && all.writes = [] then [] else ready_fds all 0)
      else match s.parent with
        | Some _ -> suspend all; ready_fds all 0
        | None -> ready_fds all (timeout_of all.until)
    in
    if not (take_interrupt s) then begin
      let now = elapsed_ms () in
      let still, woken =
        List.partition (fun (w, _) -> not (due w now || touches w ready))
          s.waiting
      in
      s.waiting <- still;
      List.iter (fun (_, resume) -> Queue.push resume s.runq) (List.rev woken)
    end
  end

(* Run fibers until every one has finished. [save] reads the running
   code's fiber-local state and [restore] installs one. [start] is given
   [spawn st body], which adds a fiber starting from state [st]; a fiber may
   call it too. The first exception a body raises is raised again once all
   have finished. *)
let run_with ~(save : unit -> 's) ~(restore : 's -> unit)
    (start : ('s -> (unit -> unit) -> unit) -> unit) =
  let parent = Domain.DLS.get current in
  let s = { runq = Queue.create (); waiting = []; live = 0; failure = None;
            interrupt_seen = false; parent } in
  let owner = save () in
  let enter st = restore st; Domain.DLS.set current (Some s) in
  let leave () =
    let st = save () in
    restore owner;
    Domain.DLS.set current parent;
    st
  in
  let finish () =
    ignore (leave ());
    s.live <- s.live - 1
  in
  let handler =
    { Effect.Deep.
      retc = (fun () -> finish ());
      exnc = (fun e ->
        finish ();
        if s.failure = None then s.failure <- Some e);
      effc = fun (type a) (eff : a Effect.t) ->
        match eff with
        | Yield ->
          Some (fun (k : (a, unit) Effect.Deep.continuation) ->
            let st = leave () in
            Queue.push (fun () -> enter st; Effect.Deep.continue k ()) s.runq;
            if parent <> None then yield ())
        | Suspend w ->
          Some (fun (k : (a, unit) Effect.Deep.continuation) ->
            let st = leave () in
            s.waiting <-
              (w, fun () -> enter st; Effect.Deep.continue k ()) :: s.waiting)
        | _ -> None }
  in
  let spawn st body =
    s.live <- s.live + 1;
    Queue.push (fun () ->
      enter st;
      Effect.Deep.match_with body () handler) s.runq
  in
  start spawn;
  (* While fibers are runnable, waiters are looked at once a millisecond,
     so a fiber that computes does not starve one whose wait is over. *)
  let next_look = ref 0 in
  let rec loop () =
    match Queue.take_opt s.runq with
    | Some f ->
      if s.waiting <> [] then begin
        let now = elapsed_ms () in
        if now >= !next_look then begin
          next_look := now + 1;
          check_waiting s ~block:false
        end
      end;
      f (); loop ()
    | None -> if s.live > 0 then (check_waiting s ~block:true; loop ())
  in
  Fun.protect ~finally:(fun () ->
    restore owner; Domain.DLS.set current parent) loop;
  match s.failure with Some e -> raise e | None -> ()

(* [bodies] as fibers, [states.(i)] fiber [i]'s state at its start. *)
let run ~save ~restore states bodies =
  run_with ~save ~restore (fun spawn ->
    Array.iteri (fun i body -> spawn states.(i) body) bodies)

(* Wait about [ms]: a fiber suspends, anything else sleeps. May return
   early; callers look again. *)
let pause ms =
  if active () then sleep_until (elapsed_ms () + ms)
  else
    try Unix.sleepf (float_of_int ms /. 1000.)
    with Unix.Unix_error (Unix.EINTR, _, _) -> ()

(* The descriptors of [reads] and [writes] that are ready, waiting at most
   [timeout_ms] (-1: until one is). A fiber suspends for the wait. *)
let select reads writes timeout_ms =
  let until = if timeout_ms < 0 then None
              else Some (elapsed_ms () + timeout_ms) in
  let w = { reads; writes; until } in
  let fds, evs = events_of w in
  let rec go () =
    let wait_ms =
      if active () then (suspend w; 0)
      else match until with
        | None -> -1
        | Some t -> max 0 (t - elapsed_ms ())
    in
    let ready = poll_fds fds evs wait_ms in
    if Array.exists Fun.id ready || due w (elapsed_ms ())
       || !interrupt_pending () then ready
    else go ()
  in
  let ready = go () in
  let r = ref [] and wr = ref [] in
  Array.iteri (fun i ok ->
    if ok then begin
      if List.mem fds.(i) reads then r := fds.(i) :: !r;
      if List.mem fds.(i) writes then wr := fds.(i) :: !wr
    end) ready;
  (!r, !wr)

(* Which scheduler the running code is in, for code that runs on a
   handler's stack on behalf of a fiber. *)
type place = t option
let place () : place = Domain.DLS.get current
let enter_place (p : place) = Domain.DLS.set current p
let same_place (a : place) (b : place) =
  match a, b with
  | None, None -> true
  | Some x, Some y -> x == y
  | _ -> false
