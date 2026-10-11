(* `wand t --fix`: apply the corrections the checker already computes,
   re-check, and repeat to a fixed point -- a fix can unlock a further
   finding. One fix representation, two consumers: this engine for the
   CLI's generate/typecheck loop, `codeAction` for the human in the
   editor. A fix that behaves differently in the two paths is a bug.

   A `Diag.Replace` is applied only over an extent that holds exactly the
   text it replaces -- the test `codeAction` makes, so a fix cannot behave
   one way here and another in the editor. The textual drift corrections
   are `Replace`s that name their substitution in prose rather than
   spanning it, so they go on declining, which is what keeps a file that
   did not parse from being rewritten on a guess. An error carrying no
   applicable fix refuses the whole run -- nothing is written. *)

type applied = {
  code : string;    (* "V-USES2", "V-IMP1", "E-TYPE", ... *)
  line : int;       (* the 1-based line it changed, in the file as it was *)
  note : string;    (* what changed, for the per-fix report *)
  diag : Diag.t;    (* the diagnostic it answered, for --json *)
}

(* Lines are 1-based. Splitting on '\n' keeps a final empty piece for a
   trailing newline, so joining restores the file byte for byte. *)
let split_lines = String.split_on_char '\n'
let join_lines = String.concat "\n"

let is_shebang = function
  | first :: _ -> String.length first >= 2 && first.[0] = '#' && first.[1] = '!'
  | [] -> false

let is_manifest_text s =
  let t = String.trim s in
  String.length t > 4 && String.sub t 0 4 = "uses"
  && String.trim (String.sub t 4 (String.length t - 4)) <> ""
  && (String.trim (String.sub t 4 (String.length t - 4))).[0] = '{'

(* Replace the 1-based line [n]. *)
let replace_line lines n text =
  List.mapi (fun i l -> if i = n - 1 then text else l) lines

let delete_line lines n =
  List.filteri (fun i _ -> i <> n - 1) lines

(* The line a manifest correction lands on. The diagnostic usually points
   there already (A-USES1, the manifest type error); a shell-word error
   points at the $() that tripped it, so the manifest is found by looking. *)
let manifest_line lines =
  let rec go i = function
    | [] -> None
    | l :: rest -> if is_manifest_text l then Some (i + 1) else go (i + 1) rest
  in
  go 0 lines

let quote s = "\"" ^ String.trim s ^ "\""

let is_import_text s =
  let t = String.trim s in
  String.length t > 7 && String.sub t 0 7 = "import "

(* Where a missing `import` goes. A file reads manifest, then imports, and
   the plain `import X` forms come before the destructured `let {a} = import
   X` ones -- so the line joins the run of plain imports, in the order that
   run is already kept. With no imports to join it goes under the manifest,
   with a blank line between, which is where the first one would have been
   written by hand. Returns a 1-based line to insert before. *)
let import_line_for lines text =
  let numbered = List.mapi (fun i l -> (i + 1, String.trim l)) lines in
  let plain =
    List.filter (fun (_, l) ->
      String.length l > 7 && String.sub l 0 7 = "import ") numbered
  in
  let want = String.trim text in
  match plain with
  | [] ->
    (* After the manifest and the blank line that follows it, or after a
       shebang, or at the top. *)
    let start =
      match manifest_line lines with
      | Some n -> n + 1
      | None -> if is_shebang lines then 2 else 1
    in
    let rec skip_blank n =
      match List.nth_opt lines (n - 1) with
      | Some l when String.trim l = "" -> skip_blank (n + 1)
      | _ -> n
    in
    if manifest_line lines = None then start else skip_blank start
  | _ ->
    (match List.find_opt (fun (_, l) -> l > want) plain with
     | Some (n, _) -> n
     | None -> fst (List.nth plain (List.length plain - 1)) + 1)

(* Whether `text` parses. A fix must not write a file that does not parse,
   so a replacement that gives one is declined. The text is lexed under a
   name of its own: the lexer keeps each whole source by its file name, and
   the file being checked is kept under "". *)
let parses text =
  let file = "<wand fix check>" in
  let ok =
    match Parser.parse_program (Lexer.tokenize ~file text) with
    | _ -> true
    | exception _ -> false
  in
  Hashtbl.remove Lexer.sources file;
  ok

(* The byte offset of a 1-based line and column in `text`, when that
   position is in the text. *)
let offset_of text line col =
  let n = String.length text in
  let rec line_start i l =
    if l = line then Some i
    else match String.index_from_opt text i '\n' with
      | Some j when j + 1 <= n -> line_start (j + 1) (l + 1)
      | _ -> None
  in
  if line < 1 || col < 1 then None
  else
    match line_start 0 1 with
    | None -> None
    | Some s ->
      let stop = match String.index_from_opt text s '\n' with
        | Some j -> j | None -> n in
      if s + col - 1 <= stop then Some (s + col - 1) else None

(* `text` with the extent `l` replaced by `to_`, when the extent holds
   exactly `from_` and the result parses. The extent is read by its lines
   and columns in `text` as it is now, so a fix made after an earlier fix
   in the same pass reads the text that fix left. Only an extent in the
   file being checked counts: an extent from an imported file has a file
   name, and its positions say nothing about this text. This is the one
   test for a substitution. `wand t --fix` and the editor's code action
   both make it. *)
let replace_extent text (l : Token.loc) from_ to_ =
  if l.Token.file <> "" then None
  else
    match offset_of text l.Token.line l.Token.col,
          offset_of text l.Token.end_line l.Token.end_col with
    | Some start, Some stop
      when stop > start && String.sub text start (stop - start) = from_ ->
      let replaced =
        String.sub text 0 start ^ to_
        ^ String.sub text stop (String.length text - stop)
      in
      if parses replaced then Some replaced else None
    | _ -> None


(* Apply one fix, answering the new lines and what happened -- or None when
   the fix is not one this engine applies. *)
let apply_fix lines (d : Diag.t) : (string list * applied) option =
  let loc_line = match d.Diag.loc with Some l -> l.Token.line | None -> 1 in
  let at line note lines' =
    Some (lines', { code = d.Diag.code; line; note; diag = d })
  in
  match d.Diag.fix with
  | Some (Diag.InsertLine text) ->
    let n =
      if is_import_text text then import_line_for lines text
      (* Manifest creation: the manifest is the first thing in the file,
         after a shebang if one is there. *)
      else if is_shebang lines then 2 else 1
    in
    let before = List.filteri (fun i _ -> i < n - 1) lines in
    let after  = List.filteri (fun i _ -> i >= n - 1) lines in
    (* An import that lands against the code below it takes the blank line
       the formatter would put there, so a fixed file is a fixed point. *)
    let inserted =
      match is_import_text text, after with
      | true, next :: _ when String.trim next <> "" && not (is_import_text next) ->
        [text; ""]
      (* A manifest stands off from the file below it. Every file in the
         tree writes it that way and the formatter leaves it that way, so a
         manifest inserted against the first import made a file that was
         correct and looked hand-patched. *)
      | false, next :: _ when String.trim next <> "" -> [text; ""]
      | _ -> [text]
    in
    at n ("inserted " ^ quote text) (before @ inserted @ after)
  | Some (Diag.ReplaceLine text) ->
    let n =
      if is_manifest_text text then
        match manifest_line lines with Some n -> n | None -> loc_line
      else loc_line
    in
    (match List.nth_opt lines (n - 1) with
     | Some old when old <> text ->
       at n ("replaced " ^ quote old ^ " with " ^ quote text)
         (replace_line lines n text)
     | _ -> None)
  | Some Diag.DeleteLine ->
    (match List.nth_opt lines (loc_line - 1) with
     | Some old -> at loc_line ("deleted " ^ quote old) (delete_line lines loc_line)
     | None -> None)
  (* Applied only when the flagged extent holds exactly the text being
     replaced -- the same test `codeAction` makes, so the two consumers
     agree. Anything looser would be a guess about which occurrence on the
     line was meant, and a line may hold the name twice. A drift correction
     declines under the same test: it names its substitution in prose and
     the extent it is flagged over is not the span of that text, so nothing
     is rewritten on its behalf. *)
  | Some (Diag.AppendToLine text) ->
    (match List.nth_opt lines (d.Diag.loc |> Option.map (fun (l : Token.loc) ->
             l.Token.line) |> Option.value ~default:0 |> fun n -> n - 1) with
     | Some old when not (String.ends_with ~suffix:text old) ->
       let n = match d.Diag.loc with Some l -> l.Token.line | None -> 0 in
       at n (Printf.sprintf "appended %s" (quote text))
         (replace_line lines n (old ^ text))
     | _ -> None)
  | Some (Diag.Replace { from_; to_ }) ->
    (match d.Diag.loc with
     | Some l when l.Token.line = l.Token.end_line ->
       (match replace_extent (join_lines lines) l from_ to_ with
        | Some text ->
          at l.Token.line
            (Printf.sprintf "replaced %s with %s" (quote from_) (quote to_))
            (split_lines text)
        | None -> None)
     | _ -> None)
  (* The same test over an extent the fix carries. It can run over more
     than one line, as the brackets put around two statements do. *)
  | Some (Diag.ReplaceAt { at = l; from_; to_ }) ->
    (match replace_extent (join_lines lines) l from_ to_ with
     | Some text ->
       let what =
         if l.Token.end_line > l.Token.line
         then Printf.sprintf "lines %d to %d" l.Token.line l.Token.end_line
         else quote from_
       in
       at l.Token.line
         (Printf.sprintf "replaced %s with %s" what (quote to_))
         (split_lines text)
     | None -> None)
  | None -> None

(* One pass over a clean check's findings, bottom-up so earlier line
   numbers stay valid, at most one fix per line -- overlapping fixes wait
   for the next pass, which re-checks first. *)
let apply_findings lines (diags : Diag.t list) =
  let fixable = List.filter (fun d -> d.Diag.fix <> None) diags in
  let line_of d = match d.Diag.loc with Some l -> l.Token.line | None -> 1 in
  let sorted = List.stable_sort (fun a b -> compare (line_of b) (line_of a)) fixable in
  let (lines, applied, _) =
    List.fold_left (fun (lines, acc, seen) d ->
      let n = line_of d in
      if List.mem n seen then (lines, acc, seen)
      else
        match apply_fix lines d with
        | Some (lines', a) -> (lines', a :: acc, n :: seen)
        | None -> (lines, acc, seen)
    ) (lines, [], []) sorted
  in
  (lines, List.rev applied)

(* A fix can unlock a further finding, so the loop re-checks after every
   pass; the cap only guards against a fix that fails to converge, which
   would be a bug in the fix itself. A check stops at its first error, so
   each error takes a pass of its own, and a file can have more errors with
   a fix than five. *)
let max_passes = 50

let fix_source ~path (src : string) : (string * applied list, Diag.t) result =
  let rec go lines applied passes =
    if passes = 0 then Ok (join_lines lines, List.rev applied)
    else
      match Runner.typecheck_source ~path (join_lines lines) with
      | Error d ->
        (match apply_fix lines d with
         | Some (lines', a) -> go lines' (a :: applied) (passes - 1)
         | None ->
           (* An error this engine cannot correct: refuse, keeping the
              file exactly as it was -- even fixes from earlier passes
              are abandoned rather than written around a broken state. *)
           Error d)
      | Ok sc ->
        let diags =
          List.map (Lint.to_diag ~strict:false) sc.Runner.sc_findings in
        (match apply_findings lines diags with
         | (_, []) -> Ok (join_lines lines, List.rev applied)
         | (lines', pass) -> go lines' (List.rev_append pass applied) (passes - 1))
  in
  go (split_lines src) [] max_passes

(* In place, like `wand f`. Nothing is written when nothing changed or
   when the engine refuses. *)
let fix_file path : (applied list, Diag.t) result =
  let full =
    if Filename.is_relative path
    then Filename.concat (Sys.getcwd ()) path
    else path
  in
  match In_channel.with_open_text full In_channel.input_all with
  | exception Sys_error msg ->
    Error (Diag.error ~code:"E-FAIL" ("cannot open file: " ^ msg))
  | src ->
    match fix_source ~path src with
    | Error d -> Error d
    | Ok (fixed, applied) ->
      (* Beside and renamed into place, like every other write wand makes.
         Truncating the source the reader is editing and then filling it
         means a crash or a full disk part way through leaves half a file
         and no copy of the other half. *)
      if fixed <> src then
        match Runner.write_atomic full fixed with
        | () -> Ok applied
        | exception Unix.Unix_error (e, f, _) ->
          Error (Diag.error ~code:"E-FAIL"
                   (Printf.sprintf "cannot write file: %s: %s" f
                      (Unix.error_message e)))
        | exception Failure m ->
          Error (Diag.error ~code:"E-FAIL" ("cannot write file: " ^ m))
      else Ok applied
