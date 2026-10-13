open Wand
open Wand.Ast

(* `Resolve` writes each name a definition binds itself as its place in
   the environment, and the evaluator reads it there with one pointer
   compare. What is pinned here: the places the pass gives, that a wrong
   place costs a lookup by name and never a wrong value, and that the
   programs which most stretch the evaluator's environment -- handlers,
   fibers, brackets, the REPL -- give the same answers under a run that
   checks every resolved lookup against a lookup by name. *)

let items src = (Parser.parse_program (Lexer.tokenize src)).items

(* Every resolved name in an expression, in the order it is written. *)
let rec slots (e : expr) : (string * string) list =
  let s = slots in
  match e with
  | Slot (x, d) -> [(x, string_of_int d)]
  | Global g -> [(g.g_name, "global")]
  | GlobalCtor g -> [(g.g_name, "ctor")]
  | ModField (g, f, _, _) -> [(g.g_name ^ "." ^ f, "field")]
  | App (a, b) | BinOp (_, a, b) | Seq (a, b) -> s a @ s b
  | Fn (_, b) | Located (_, b) | Annot (_, b) | UnOp (_, b) | Try b -> s b
  | Let (_, a, b, _) -> s a @ s b
  | If (a, b, c) | Unless (a, b, c) -> s a @ s b @ s c
  | Match (a, cases) ->
    s a @ List.concat_map (fun (_, g, b) ->
      (match g with Some g -> s g | None -> []) @ s b) cases
  | Tuple es | List es -> List.concat_map s es
  | Handle (b, cases) ->
    s b @ List.concat_map (function
      | EffectCase (_, _, b) | ReturnCase (_, b) -> s b) cases
  | With (r, _, b) -> s r @ s b
  | LetRec (bs, b, _) -> List.concat_map (fun (_, _, x) -> s x) bs @ s b
  | Interp (parts, _) -> List.concat_map (fun (_, e) -> s e) parts
  | _ -> []

let body_of = function
  | TLLet (_, _, b) | TLLetPat (_, b) | TLExpr b -> b
  | _ -> Alcotest.fail "expected a binding or an expression"

let resolved src =
  match List.rev (items src) with
  | last :: _ -> slots (body_of (Resolve.item last))
  | [] -> Alcotest.fail "no items"

let pairs = Alcotest.(list (pair string string))

let test_parameters () =
  (* `b` was pushed last, so it is first; `f` itself comes after the
     parameters, and `g` is not bound by the definition. *)
  Alcotest.check pairs "parameters, the function itself, and a global"
    [("b", "0"); ("a", "1"); ("f", "2"); ("g", "global")]
    (resolved "let g = 1\nlet f a b = (b, a, f, g)")

let test_lets_and_lambdas () =
  Alcotest.check pairs "a let, then a lambda over it"
    [("a", "1"); ("x", "0"); ("c", "1"); ("b", "2")]
    (resolved "let f a b =\n  let c = a;\n  fn x -> (x, c, b)")

let test_recursive_let () =
  (* `let go = fn ...` pushes `go` before the parameter, and `go` again
     where the body after it sees it. *)
  Alcotest.check pairs "a local recursive function"
    [("go", "1"); ("n", "0"); ("go", "0")]
    (resolved "let f u =\n  let go = fn n -> go n;\n  go")

let test_patterns () =
  Alcotest.check pairs "a match arm's names, in the order they are bound"
    [("xs", "0"); ("xs", "0"); ("t", "0"); ("h", "1"); ("xs", "2")]
    (resolved "let f xs =\n  match xs with\n  | [] -> xs\n  | [h :: t] -> (t, h, xs)")

let test_handler () =
  (* The continuation is pushed after the operation's pattern. *)
  Alcotest.check pairs "a handler's case"
    [("k", "0"); ("v", "1"); ("x", "2")]
    (resolved "let f x =\n  handle 1 with\n  | Op!run v k -> (k, v, x)")

let test_module_field () =
  Alcotest.check pairs "a module's member"
    [("List.length", "field"); ("xs", "0")]
    (resolved "import List\nlet f xs = List.length xs")

(* Each constructor a pattern names, and how many bindings are in front of
   it where it is checked: the scope of the arm, and what the pattern has
   bound before it. *)
let rec pat_ctors (p : pat) =
  match p with
  | PCtor (g, inner, _) -> (g.g_name, string_of_int g.g_depth) :: pat_ctors inner
  | PConstr (_, ps) | PTuple ps | PList ps -> List.concat_map pat_ctors ps
  | PConstrNamed (_, kvs) | PMap kvs -> List.concat_map (fun (_, p) -> pat_ctors p) kvs
  | PCons (h, t) -> pat_ctors h @ pat_ctors t
  | PLocated (_, p) | PAnnot (p, _) -> pat_ctors p
  | _ -> []

let test_pattern_constructors () =
  let arms src =
    match List.rev (items src) with
    | last :: _ ->
      (match Resolve.item last with
       | TLLet (_, ps, b) ->
         List.concat_map pat_ctors ps
         @ (match strip_located b with
            | Match (_, cases) -> List.concat_map (fun (p, _, _) -> pat_ctors p) cases
            | _ -> [])
       | _ -> Alcotest.fail "expected a definition")
    | [] -> Alcotest.fail "no items"
  in
  (* `f` and `x` are in front of each arm; `a` is bound before `Some`, and
     `a` and `b` before `Ok`. *)
  Alcotest.check pairs "constructors in arms"
    [("Some", "3"); ("Ok", "4"); ("None", "3")]
    (arms "let f x =\n  match x with\n  | (a, Some (b, Ok c)) -> c\n  | (a, None) -> a");
  Alcotest.check pairs "a parameter's constructor"
    [("Some", "1")]
    (arms "let f (Some x) = x")

(* A place the environment does not hold -- the shape changed after the
   pass ran -- answers with the lookup by name. *)
let eval_with env e = Evaluator.eval env e

let test_wrong_place_falls_back () =
  let x = "x" in
  let env = [ (x, Evaluator.VInt 1); ("y", Evaluator.VInt 2) ] in
  let show v = Evaluator.show_value v in
  Alcotest.(check string) "the right place" "1" (show (eval_with env (Slot (x, 0))));
  Alcotest.(check string) "a place past the end" "1" (show (eval_with env (Slot (x, 5))));
  Alcotest.(check string) "a place holding another name" "2"
    (show (eval_with env (Slot ("y", 0))));
  (* Same text, another string: the pointer compare fails and the name
     decides. *)
  let x' = String.init 1 (fun _ -> 'x') in
  Alcotest.(check string) "a binder that is not this one" "1"
    (show (eval_with ((x', Evaluator.VInt 1) :: env) (Slot (x, 1))))

let test_wrong_edge_falls_back () =
  let edge = "a" in
  let g = { g_name = "z"; g_depth = 1; g_edge = edge; g_cache = Resolve.new_cache () } in
  let env = [ ("b", Evaluator.VInt 0); ("z", Evaluator.VInt 7) ] in
  Alcotest.(check string) "the edge is not the resolver's binder" "7"
    (Evaluator.show_value (eval_with env (Global g)));
  let env' = [ (edge, Evaluator.VInt 0); ("z", Evaluator.VInt 8) ] in
  Alcotest.(check string) "the edge is" "8"
    (Evaluator.show_value (eval_with env' (Global g)));
  (* The same edge with another list past it: the cached answer belongs to
     the other list. *)
  let env'' = [ (edge, Evaluator.VInt 0); ("z", Evaluator.VInt 9) ] in
  Alcotest.(check string) "another list past the edge" "9"
    (Evaluator.show_value (eval_with env'' (Global g)))

(* `m.f` keeps the list past the edge, the module and the member. Another
   list, another module, or an edge that is not the resolver's gives the
   member of the module that is there. *)
let test_module_field_cache () =
  let edge = "a" in
  let g = { g_name = "M"; g_depth = 1; g_edge = edge; g_cache = Resolve.new_cache () } in
  let e = ModField (g, "f", Resolve.new_cache (), 0) in
  let modul n = Evaluator.VRecord (Evaluator.vrecord_make [("f", Evaluator.VInt n)]) in
  let one = modul 1 and two = modul 2 in
  let show env = Evaluator.show_value (eval_with env e) in
  let past = [ ("M", one) ] in
  Alcotest.(check string) "the first read" "1" (show ((edge, Evaluator.VInt 0) :: past));
  Alcotest.(check string) "the same list past the edge" "1"
    (show ((edge, Evaluator.VInt 5) :: past));
  Alcotest.(check string) "another module" "2"
    (show [ (edge, Evaluator.VInt 0); ("M", two) ]);
  Alcotest.(check string) "the edge is not the resolver's binder" "1"
    (show [ ("b", Evaluator.VInt 0); ("M", one) ]);
  Alcotest.(check string) "the first module again" "1"
    (show ((edge, Evaluator.VInt 0) :: past))

(* A function value bound by `let` is not made recursive unless a `fn` is
   written there. `add 1` was made where `inc` meant the outer one, and
   binding the new `inc` inside it took that from it. *)
let test_function_value_keeps_its_scope () =
  Alcotest.(check (result string string)) "the outer inc" (Ok "8")
    (Runner.run_string
       "let main () =\n  let inc = 5;\n  let add a b = a + b + inc;\n  let inc = add 1;\n  inc 2\nmain ()")

let session_lines lines =
  let sess = Runner.make_session () in
  let show = function
    | Runner.RVal (v, _) -> v
    | _ -> ""
  in
  let (_, out) =
    List.fold_left (fun (sess, _) line ->
      match Runner.run_session sess line with
      | Ok (sess, r) -> (sess, show r)
      | Error m -> Alcotest.failf "%s: %s" line m) (sess, "") lines
  in
  out

let test_session () =
  Alcotest.(check string) "a later line sees an earlier one" "3"
    (session_lines ["let x = 2"; "let f y = x + y"; "f 1"]);
  (* A pattern keeps where a record's fields are. A type declared again
     with its fields in another order moves them, and the pattern reads
     them from the new places. *)
  Alcotest.(check string) "a type declared again" "4"
    (session_lines ["type T(a: Int, b: Int)"; "let f (T(a, b)) = a - b";
                    "f (T(a = 5, b = 1))"; "type T(b: Int, a: Int)";
                    "f (T(a = 5, b = 1))"]);
  Alcotest.(check string) "equations given one at a time" "120"
    (session_lines ["let fact 0 = 1"; "let fact n = n * fact (n - 1)"; "fact 5"])

(* Programs under the real binary with `WAND_CHECK_SLOTS` set, which makes
   every resolved lookup also look the name up by its text and stop the
   run if the two differ. *)
let wand_binary =
  let dir = Filename.dirname (Filename.dirname Sys.executable_name) in
  Filename.concat (Filename.concat dir "bin") "wand.exe"

let checked_files = [
  "test_block_binding.wand"; "test_contracts.wand"; "test_derive.wand";
  "test_effects.wand"; "test_eval.wand"; "test_generics.wand";
  "test_group.wand"; "test_imports.wand"; "test_interfaces.wand";
  "test_lambda_params.wand"; "test_list.wand"; "test_list_callbacks.wand";
  "test_list_patterns.wand"; "test_match_places.wand";
  "test_operations.wand"; "test_ordering.wand";
  "test_par.wand"; "test_par_race.wand"; "test_resource.wand";
  "test_result.wand"; "test_shared.wand"; "test_stream.wand";
  "test_tuple_index.wand"; "test_typedef.wand"; "test_wand_limit.wand";
  "test_wand_module.wand";
]

let test_checked_runs () =
  let env = Array.append [| "WAND_CHECK_SLOTS=1" |] (Unix.environment ()) in
  List.iter (fun f ->
    let path = Filename.concat "wand" f in
    let devnull = Unix.openfile "/dev/null" [Unix.O_WRONLY] 0o644 in
    let pid = Unix.create_process_env wand_binary [| wand_binary; "s"; path |]
                env Unix.stdin devnull devnull in
    let (_, status) = Unix.waitpid [] pid in
    Unix.close devnull;
    if status <> Unix.WEXITED 0 then
      Alcotest.failf "%s does not pass when every resolved lookup is checked" f)
    checked_files

let () =
  Alcotest.run "Resolve" [
    "places", [
      Alcotest.test_case "parameters" `Quick test_parameters;
      Alcotest.test_case "lets and lambdas" `Quick test_lets_and_lambdas;
      Alcotest.test_case "a recursive let" `Quick test_recursive_let;
      Alcotest.test_case "patterns" `Quick test_patterns;
      Alcotest.test_case "a handler" `Quick test_handler;
      Alcotest.test_case "a module's member" `Quick test_module_field;
      Alcotest.test_case "constructors in patterns" `Quick test_pattern_constructors;
    ];
    "a wrong place", [
      Alcotest.test_case "a slot" `Quick test_wrong_place_falls_back;
      Alcotest.test_case "a global" `Quick test_wrong_edge_falls_back;
      Alcotest.test_case "a module's member" `Quick test_module_field_cache;
    ];
    "programs", [
      Alcotest.test_case "a function value keeps its scope" `Quick
        test_function_value_keeps_its_scope;
      Alcotest.test_case "the REPL" `Quick test_session;
      Alcotest.test_case "checked against lookups by name" `Slow test_checked_runs;
    ];
  ]
