(* Domains kept while there is work, which `Par` hands work to.

   A job is a ticket: whoever takes it first runs it, a pool domain or the
   caller that submitted it. A caller takes back a job no domain has picked
   up, so work never waits on a pool that is busy.

   Every minor collection stops every domain there is, idle or not, so a
   pool that has had nothing to do for [idle_ms] is let go. That is decided
   at the end of a major collection -- the collections that idle domains
   make dearer are what drive it -- and the pool starts domains again as
   work arrives. *)

type ticket = { taken : bool Atomic.t; job : unit -> unit; at : int }

let queue : ticket Queue.t = Queue.create ()
let m = Mutex.create ()
let c = Condition.create ()

let size = max 1 (Domain.recommended_domain_count () - 1)

let idle_ms = 500

(* Domains running, how many wait for work, when work last arrived, and
   whether the waiting ones are to end. *)
let live = ref 0
let idle = ref 0
let last_work = ref 0
let retire = ref false

let locked f = Mutex.lock m; Fun.protect ~finally:(fun () -> Mutex.unlock m) f

let worker () =
  let rec loop () =
    let next = locked (fun () ->
      incr idle;
      while Queue.is_empty queue && not !retire do Condition.wait c m done;
      decr idle;
      if Queue.is_empty queue then begin
        decr live;
        if !live = 0 then retire := false;
        None
      end else Some (Queue.pop queue)) in
    match next with
    | None -> ()
    | Some t ->
      if Atomic.compare_and_set t.taken false true then
        (try t.job () with _ -> ());
      loop ()
  in
  loop ()

let alarm = ref None

(* Run at the end of a major collection, which can end inside [locked]
   on this same domain, so it never waits for the lock: a turn it cannot
   take is left to the next collection. *)
let let_go () =
  if Mutex.try_lock m then
    Fun.protect ~finally:(fun () -> Mutex.unlock m) (fun () ->
      if !idle > 0 && !idle = !live && Queue.is_empty queue
         && Sched.elapsed_ms () - !last_work >= idle_ms then begin
        retire := true;
        Condition.broadcast c
      end)

let submit job =
  let t = { taken = Atomic.make false; job; at = Sched.elapsed_ms () } in
  let start = locked (fun () ->
    Queue.push t queue;
    last_work := t.at;
    retire := false;
    Condition.signal c;
    if !idle = 0 && !live < size then (incr live; true) else false) in
  if start then begin
    ignore (Domain.spawn worker);
    if !alarm = None then alarm := Some (Gc.create_alarm let_go)
  end;
  t

(* Take [t] back, to run it here; false if a pool domain has it. *)
let take_back t = Atomic.compare_and_set t.taken false true

let waiting t = not (Atomic.get t.taken)
