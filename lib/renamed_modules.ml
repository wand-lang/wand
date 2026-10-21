(* Standard library modules that were folded into another, and the fix for
   each place a file still names one (#215). `Digest` held the type that
   every `Hash` function answered with, so a task needed both modules; its
   members are `Hash`'s now, and its type is `Hash`.

   Read from the tokens alone, as Missing_imports reads the parse: an
   import of a module that is not there stops the typechecker before it
   reaches the names, so a file with `import Digest` and three `Digest.hex`
   was told about one thing at a time. This pass finds them all, and
   `wand t --fix` rewrites them all in one run. A file whose own name is
   `Digest` -- `let Digest = import ./digest` -- is left alone. *)

(* The module, the one it joined, and its types under their new names. *)
let renamed = [ ("Digest", "Hash", [ ("Digest", "Hash") ]) ]

let message old into =
  Printf.sprintf "the module %s is now part of %s (#215): write %s" old into into

(* The extent of a run of tokens, from the first to the last. *)
let span (a : Token.loc) (b : Token.loc) =
  { a with Token.end_line = b.Token.end_line; end_col = b.Token.end_col;
           end_offset = b.Token.end_offset }

let find ?(file = "") src : Diag.t list =
  (* The tokens, with each `%{...}` of a string read in place, so that
   `"%{Digest.hex h}"` is found as well. *)
  let rec expand toks =
    List.concat_map (fun ((t, (l : Token.loc)) as tok) ->
      match t with
      | Token.LineComment _ -> []
      | Token.InterpStr (parts, _) | Token.RawInterpStr (parts, _) ->
        tok :: List.concat_map (fun (_, part, (at : Token.pos)) ->
          match Lexer.tokenize ~file:l.Token.file ~line:at.Token.p_line
                  ~col:at.Token.p_col ~base:at.Token.p_offset part with
          | inner -> expand inner
          | exception _ -> []) parts
      | _ -> [tok]) toks
  in
  let toks = try expand (Lexer.tokenize ~file src) with _ -> [] in
  let binds_itself old =
    (* `let Digest = import ./digest`, `let {Digest} = import ...`, or a
       type of the file's own named `Digest`. *)
    let rec go = function
      | (Token.Let, _) :: (Token.Upper n, _) :: (Token.Eq, _) :: _ when n = old -> true
      | (Token.Type, _) :: (Token.Upper n, _) :: _ when n = old -> true
      | (Token.Let, _) :: (Token.LBrace, _) :: rest ->
        let rec inside = function
          | (Token.RBrace, _) :: rest -> go rest
          | (Token.Upper n, _) :: _ when n = old -> true
          | _ :: rest -> inside rest
          | [] -> false
        in
        inside rest
      | _ :: rest -> go rest
      | [] -> false
    in
    go toks
  in
  let imports m =
    let rec go = function
      | (Token.Import, _) :: (Token.Upper n, _) :: _ when n = m -> true
      | _ :: rest -> go rest
      | [] -> false
    in
    go toks
  in
  List.concat_map (fun (old, into, types) ->
    if binds_itself old then []
    else
      let fix at from_ to_ =
        Diag.error ~code:"E-TYPE" ~loc:at
          ~fix:(Diag.ReplaceAt { at; from_; to_ }) (message old into)
      in
      let rec go acc = function
        (* `Digest.Digest`, the type: `Hash.Hash`. *)
        | (Token.Upper m, a) :: (Token.Dot, _) :: (Token.Upper t, b) :: rest
          when m = old && List.mem_assoc t types ->
          let to_ = into ^ "." ^ List.assoc t types in
          go (fix (span a b) (m ^ "." ^ t) to_ :: acc) rest
        (* `Digest.hex`, `Digest.Sha256`: the module's name. *)
        | (Token.Upper m, a) :: ((Token.Dot, _) :: _ as rest) when m = old ->
          go (fix a old into :: acc) rest
        (* `let {Sha256} = import Digest`: the module's name. *)
        | (Token.Eq, _) :: (Token.Import, _) :: (Token.Upper m, a) :: rest when m = old ->
          go (fix a old into :: acc) rest
        (* `import Digest`. Where the file imports `Hash` already, a second
           `import Hash` would bind it twice, so the line goes. *)
        | (Token.Import, _) :: (Token.Upper m, a) :: rest when m = old ->
          let d =
            if imports into then
              Diag.error ~code:"E-TYPE" ~loc:a ~fix:Diag.DeleteLine (message old into)
            else fix a old into
          in
          go (d :: acc) rest
        | _ :: rest -> go acc rest
        | [] -> List.rev acc
      in
      go [] toks) renamed

(* The findings of `extra` that none of `ds` already gives at the same
   place. *)
let not_in ds extra =
  List.filter (fun x ->
    not (List.exists (fun d -> d.Diag.loc = x.Diag.loc && d.Diag.message = x.Diag.message) ds))
    extra

(* Whether `d` is one of this pass's findings. *)
let is_finding (d : Diag.t) =
  List.exists (fun (old, into, _) -> d.Diag.message = message old into) renamed
