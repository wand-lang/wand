(* The driver: mutate the corpus, check the property, shrink what fails,
   and file one reproducer per distinct finding.

   Reproducibility is the point. Every iteration draws from a state derived
   from `(seed, iteration)` alone, so `--seed S --only I` replays iteration
   I of run S exactly, on any machine, without carrying a corpus of inputs
   between runs. A finding names the seed and the iteration that found it. *)

open Wand_fuzz

(* ── Where the corpus lives ───────────────────────────────────────────────── *)

let rec find_root dir =
  if Sys.file_exists (Filename.concat dir "dune-project") then Some dir
  else
    let up = Filename.dirname dir in
    if up = dir then None else find_root up

(* Every `.wand` below a directory, not just the ones directly in it.
   `examples/ports/` is twenty files of valid wand -- the largest single
   body of it in the repository after the stdlib -- and a corpus that stops
   at the top level never sees one of them. *)
let rec wand_files dir =
  if not (Sys.file_exists dir && Sys.is_directory dir) then []
  else
    Sys.readdir dir
    |> Array.to_list
    |> List.sort compare
    |> List.concat_map (fun f ->
         let full = Filename.concat dir f in
         if Sys.is_directory full then wand_files full
         else if Filename.check_suffix f ".wand" then [full]
         else [])

let read_file path = In_channel.with_open_bin path In_channel.input_all

let write_file path contents =
  Out_channel.with_open_bin path (fun oc -> Out_channel.output_string oc contents)

(* ── Findings already accounted for ───────────────────────────────────────── *)

(* One signature per line; `#` comments. A bug that is known and not yet
   fixed belongs here with the issue number beside it, so a daily run
   stays red only for what is new. Nothing is suppressed silently: the
   summary counts every suppressed hit. *)
let load_known path =
  if not (Sys.file_exists path) then []
  else
    read_file path
    |> String.split_on_char '\n'
    |> List.map String.trim
    |> List.filter (fun l -> l <> "" && l.[0] <> '#')
    |> List.map (fun l ->
         match String.index_opt l '#' with
         | Some i -> String.trim (String.sub l 0 i)
         | None -> l)

(* ── Shrinking ────────────────────────────────────────────────────────────── *)

(* Delete as much as can be deleted while the same signature comes back.
   Lines first, in runs that halve, then bytes -- the same shape as ddmin,
   without the parts of it that only pay off on inputs far larger than a
   wand script.

   Two budgets, and either one ends it. The count of oracle calls bounds the
   ordinary case. The wall clock bounds the case the count cannot: shrinking
   a finding that is *itself* a timeout costs the full timeout on every probe
   that still hangs, so 400 calls at ten seconds is an hour on one input.
   That is not a hypothetical -- it is what a 20,000-input run did before
   this line existed. *)
let shrink ~timeout ~eval ~width ~path ~target ~budget ~seconds src =
  let calls = ref 0 in
  let until = Unix.gettimeofday () +. seconds in
  let still s =
    if !calls >= budget || Unix.gettimeofday () > until then false
    else begin
      incr calls;
      Oracle.signature (Oracle.check_all ~timeout ~eval ~width ~path s) = Some target
    end
  in
  let by_lines s =
    let current = ref s in
    let n = List.length (String.split_on_char '\n' !current) in
    let width = ref (max 1 (n / 2)) in
    let continue_ = ref true in
    while !continue_ && !calls < budget && Unix.gettimeofday () <= until do
      continue_ := false;
      let ls = String.split_on_char '\n' !current in
      let total = List.length ls in
      let i = ref 0 in
      while !i < total && !calls < budget && Unix.gettimeofday () <= until do
        let lo = !i and hi = min total (!i + !width) in
        let kept =
          List.filteri (fun j _ -> j < lo || j >= hi) ls |> String.concat "\n"
        in
        if kept <> !current && still kept then begin
          current := kept; continue_ := true; i := total
        end else i := !i + !width
      done;
      if not !continue_ && !width > 1 then begin
        width := !width / 2; continue_ := true
      end
    done;
    !current
  in
  let by_bytes s =
    let current = ref s in
    List.iter (fun span ->
      let progressed = ref true in
      while !progressed && !calls < budget && Unix.gettimeofday () <= until do
        progressed := false;
        let len = String.length !current in
        let off = ref 0 in
        while !off < len && !off + span <= String.length !current
              && !calls < budget && Unix.gettimeofday () <= until do
          let c = !current in
          let kept =
            String.sub c 0 !off
            ^ String.sub c (!off + span) (String.length c - !off - span)
          in
          if still kept then begin current := kept; progressed := true end
          else off := !off + span
        done
      done) [64; 16; 4; 1];
    !current
  in
  by_bytes (by_lines src)

(* ── Reporting ────────────────────────────────────────────────────────────── *)

(* A finding is two files: the input, byte for byte, and everything else as
   JSON beside it.

   JSON because the daily workflow reads this to decide what to file, and
   a format invented here would mean a shell script picking it apart with
   `sed` -- which works until a message contains whatever the pattern
   assumed it would not. The input stays a separate file rather than a
   string in the JSON so it can be run without being unpacked first. *)
let json_escape s =
  let b = Buffer.create (String.length s + 16) in
  String.iter (fun c ->
    match c with
    | '"'  -> Buffer.add_string b "\\\""
    | '\\' -> Buffer.add_string b "\\\\"
    | '\n' -> Buffer.add_string b "\\n"
    | '\r' -> Buffer.add_string b "\\r"
    | '\t' -> Buffer.add_string b "\\t"
    | c when Char.code c < 0x20 || Char.code c = 0x7f ->
      Buffer.add_string b (Printf.sprintf "\\u%04x" (Char.code c))
    | c -> Buffer.add_char b c) s;
  Buffer.contents b

(* Quoted here rather than with `%S`, which escapes for OCaml: it writes
   `\\NNN` and `\\xNN`, and neither is a JSON escape. *)
let json_string s = "\"" ^ json_escape s ^ "\""

let json_field name value =
  Printf.sprintf "  %s: %s" (json_string name) (json_string value)

let report ~out ~seed ~iteration ~origin ~edits ~width ~verdict src =
  let sg = Option.get (Oracle.signature verdict) in
  let base = Filename.concat out (Oracle.slug sg) in
  write_file (base ^ ".wand") src;
  let fields =
    [ ("signature", sg);
      ("verdict",   Oracle.describe verdict);
      ("origin",    origin);
      ("edits",     String.concat ", " edits);
      ("width",     string_of_int width);
      ("input",     base ^ ".wand");
      ("replay",    Printf.sprintf "fuzz --seed %d --only %d" seed iteration);
      ("recheck",   Printf.sprintf "fuzz --input %s.wand --path %s --width %d"
                      base origin width);
      ("backtrace", Oracle.backtrace_of verdict) ]
  in
  write_file (base ^ ".json")
    ("{\n"
     ^ String.concat ",\n"
         (Printf.sprintf "  \"seed\": %d" seed
          :: Printf.sprintf "  \"iteration\": %d" iteration
          :: List.map (fun (k, v) -> json_field k v) fields)
     ^ "\n}\n");
  base

(* ── Main ─────────────────────────────────────────────────────────────────── *)

let usage = {|usage: fuzz [options]

  --seed N          seed for the run (default 0)
  --iterations N    how many mutants to try (default 1000)
  --seconds N       stop after N seconds, whichever comes first
  --only N          run just iteration N of this seed, and report it
  --timeout SECS    per-input budget before a hang is a finding (default 10)
  --width N         format at this margin rather than one drawn per input
  --eval            also run each side of a format and compare the answers
  --shrink N        oracle calls a single shrink may spend (default 400)
  --shrink-seconds N  wall clock a single shrink may spend (default 45)
  --edits N         at most N mutations per input (default 6)
  --corpus DIR      a directory of .wand files (repeatable; defaults to
                    stdlib/, test/wand/, examples/, demos/ and tools/)
  --out DIR         where reproducers are written (default _fuzz-findings/)
  --known FILE      signatures not to fail on (default test/fuzz/known.txt)
  --input FILE      check one file and exit, no mutation
  --show            with --input, print what the formatter did to it
  --path P          the name --input is checked under
  --quiet           only print the summary and the findings

exits 1 if a finding is not already in --known.
|}

let () =
  Printexc.record_backtrace true;
  let seed = ref 0 and iterations = ref 1000 and seconds = ref infinity in
  let only = ref (-1) and timeout = ref 10.0 and shrink_budget = ref 400 in
  let shrink_seconds = ref 45.0 in
  let edits = ref 6 and corpus = ref [] and out = ref "" and known_path = ref "" in
  let width = ref 0 in
  (* Off by default. It runs the program on both sides of a format, which
     answers a question nothing else asks -- and costs a little over twice
     the throughput, which is a little under half the ground a shard covers
     in its 45 minutes. Asked for, not assumed. *)
  let eval = ref false in
  let input = ref "" and input_path = ref "" and quiet = ref false in
  let show = ref false in
  let args = Array.to_list Sys.argv in
  let rec parse = function
    | [] -> ()
    | "--seed" :: v :: r -> seed := int_of_string v; parse r
    | "--iterations" :: v :: r -> iterations := int_of_string v; parse r
    | "--seconds" :: v :: r -> seconds := float_of_string v; parse r
    | "--only" :: v :: r -> only := int_of_string v; parse r
    | "--timeout" :: v :: r -> timeout := float_of_string v; parse r
    | "--width" :: v :: r -> width := int_of_string v; parse r
    | "--eval" :: r -> eval := true; parse r
    | "--shrink" :: v :: r -> shrink_budget := int_of_string v; parse r
    | "--shrink-seconds" :: v :: r -> shrink_seconds := float_of_string v; parse r
    | "--edits" :: v :: r -> edits := int_of_string v; parse r
    | "--corpus" :: v :: r -> corpus := !corpus @ [v]; parse r
    | "--out" :: v :: r -> out := v; parse r
    | "--known" :: v :: r -> known_path := v; parse r
    | "--input" :: v :: r -> input := v; parse r
    | "--path" :: v :: r -> input_path := v; parse r
    | "--quiet" :: r -> quiet := true; parse r
    | "--show" :: r -> show := true; parse r
    | ("-h" | "--help") :: _ -> print_string usage; Stdlib.exit 0
    | a :: _ -> prerr_endline ("fuzz: unknown argument " ^ a);
                prerr_string usage; Stdlib.exit 2
  in
  parse (List.tl args);

  let root =
    match find_root (Sys.getcwd ()) with
    | Some d -> d
    | None -> prerr_endline "fuzz: no dune-project above the current directory";
              Stdlib.exit 2
  in
  let at p = Filename.concat root p in
  if !out = "" then out := at "_fuzz-findings";
  if !known_path = "" then known_path := at "test/fuzz/known.txt";
  if !corpus = [] then
    corpus :=
      [at "stdlib"; at "test/wand"; at "examples"; at "demos"; at "tools"];

  (* One file checked and nothing mutated: how a reproducer is re-run, and
     how the regression fixtures are checked. *)
  if !input <> "" then begin
    let path = if !input_path = "" then !input else !input_path in
    let w = if !width > 0 then !width else 92 in
    if !show then Oracle.explain ~eval:!eval ~width:w ~path (read_file !input);
    let v = Oracle.check_in_child ~timeout:!timeout ~eval:!eval ~width:w ~path (read_file !input) in
    print_endline (!input ^ ": " ^ Oracle.describe v);
    Stdlib.exit (if Oracle.is_finding v then 1 else 0)
  end;

  let files =
    List.concat_map wand_files !corpus
    |> List.filter (fun f -> read_file f <> "")
  in
  if files = [] then begin
    prerr_endline ("fuzz: no .wand files under " ^ String.concat ", " !corpus);
    Stdlib.exit 2
  end;
  let files = Array.of_list files in
  let sources = Array.map read_file files in

  if not (Sys.file_exists !out) then Unix.mkdir !out 0o755;
  let known = load_known !known_path in

  let started = Unix.gettimeofday () in
  let counts = Hashtbl.create 8 in
  let bump k = Hashtbl.replace counts k (1 + Option.value ~default:0 (Hashtbl.find_opt counts k)) in
  let filed = Hashtbl.create 8 in
  let suppressed = Hashtbl.create 8 in
  let unverified = ref 0 in
  let ran = ref 0 in

  let iteration i =
    incr ran;
    let st = Random.State.make [| !seed; i |] in
    let n = Array.length files in
    let k = Random.State.int st n in
    let donor = sources.(Random.State.int st n) in
    let origin = files.(k) in
    let (mutant, applied) = Mutate.mutate ~max_edits:!edits st ~donor sources.(k) in
    (* A margin per input unless one was asked for. Narrow enough to force
       wrapping everywhere, wide enough that nothing wraps at all, and the
       default in between. *)
    let w = if !width > 0 then !width else 24 + Random.State.int st 96 in
    let v = Oracle.check_all ~timeout:!timeout ~eval:!eval ~width:w ~path:origin mutant in
    (match v with
     | Oracle.Skipped -> bump "checked"
     | Oracle.Typed _ -> bump "typed"
     | Oracle.Rejected code -> bump ("rejected " ^ code)
     | _ -> bump "finding");
    if Oracle.is_finding v then begin
      let sg = Option.get (Oracle.signature v) in
      if List.mem sg known then
        Hashtbl.replace suppressed sg
          (1 + Option.value ~default:0 (Hashtbl.find_opt suppressed sg))
      else if not (Hashtbl.mem filed sg) then begin
        (* Confirm it in a process that has typechecked nothing before
           spending a shrink on it -- see Oracle.check_in_child. *)
        match Oracle.check_in_child ~timeout:!timeout ~eval:!eval ~width:w ~path:origin mutant with
        | v' when Oracle.signature v' <> Some sg ->
          incr unverified;
          if not !quiet then
            Printf.printf "  [%d] %s did not reproduce alone (%s)\n%!"
              i sg (Oracle.describe v')
        | v' ->
          Hashtbl.replace filed sg ();
          let small =
            shrink ~timeout:!timeout ~eval:!eval ~width:w ~path:origin ~target:sg
              ~budget:!shrink_budget ~seconds:!shrink_seconds mutant
          in
          let base =
            report ~out:!out ~seed:!seed ~iteration:i ~origin ~edits:applied
              ~width:w ~verdict:v' small
          in
          Printf.printf "  [%d] %s\n      %s\n      %d bytes -> %s.wand\n%!"
            i (Oracle.describe v') origin (String.length small) base
      end
    end
  in

  if !only >= 0 then iteration !only
  else begin
    if not !quiet then
      Printf.printf "fuzz: seed %d, %d files, %d iterations\n%!"
        !seed (Array.length files) !iterations;
    (try
       for i = 0 to !iterations - 1 do
         if Unix.gettimeofday () -. started > !seconds then raise Exit;
         iteration i
       done
     with Exit -> ())
  end;

  let elapsed = Unix.gettimeofday () -. started in
  Printf.printf "\n%d inputs in %.1fs (%.0f/s)\n" !ran elapsed
    (if elapsed > 0. then float_of_int !ran /. elapsed else 0.);
  Hashtbl.fold (fun k v acc -> (k, v) :: acc) counts []
  |> List.sort compare
  |> List.iter (fun (k, v) -> Printf.printf "  %-28s %d\n" k v);
  if Hashtbl.length suppressed > 0 then begin
    print_endline "\nknown, not failing:";
    Hashtbl.iter (fun sg n -> Printf.printf "  %-40s %d hits\n" sg n) suppressed
  end;
  if !unverified > 0 then
    Printf.printf "\n%d finding(s) did not reproduce in a fresh process\n" !unverified;
  if Hashtbl.length filed = 0 then begin
    print_endline "\nno new findings";
    Stdlib.exit 0
  end else begin
    Printf.printf "\n%d new finding(s) in %s\n" (Hashtbl.length filed) !out;
    Stdlib.exit 1
  end
