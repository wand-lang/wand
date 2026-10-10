(* The lint catalog: every rule's identity, classification, and wording.
   Nothing here inspects a program -- `Lint` does that and asks this module
   what to say -- so the full set of rules and the exact text a user sees can
   be reviewed on one screen.

   Rule IDs are a variant, not strings: a finding can only name a rule that
   exists, and adding a rule to the catalog without handling it is a
   compile error rather than a silent gap. The reference cites these same
   IDs, so prose and enforcement can be checked against each other. *)

type id =
  | V_PRED1    (* `?` names a predicate, so it must return Bool *)
  | V_OR1      (* an error that carries no information is a misfiled Option *)
  | V_NAME1    (* keyword-collision escapes should not reach a caller *)
  | V_PRED2    (* `?` already says predicate; `is_` says it twice *)
  | V_PRED3    (* it returns Bool, and the name does not say so *)
  | V_BANG1    (* it can raise, and the name does not say so *)
  | V_BANG2    (* the name says it raises, and it cannot *)
  | A_SHELL1   (* a shell blob hides work the type system could see *)
  | A_USES1    (* the manifest permits more than the file needs *)
  | V_USES2    (* the file reaches outside itself and does not say so *)
  | V_DROP1    (* a Result is thrown away, so nobody reads the failure *)
  | V_DROP2    (* an assertion's outcome is thrown away, so the test cannot fail *)
  | V_DROP3    (* a function is thrown away, so the call it needed never happened *)
  | V_SHELL2   (* a command literal runs on to a second line without a `\` *)
  | V_SHELL1   (* Shell is narrowed, but this command word is only known at run time *)
  | V_SHELL3   (* Shell.inspect runs a command known to change things *)
  | V_CTOR1    (* a match arm names bare a constructor another type shares *)
  | A_SHELL2   (* Shell.inspect runs a command whose words only the run decides *)
  | V_NET1     (* Net is narrowed, but this host is only known at run time *)
  | V_IMP2     (* an import binds a name the file never mentions *)
  | V_CLOCK1   (* two readings of the civil clock subtracted: a step spoils it *)
  | V_SHADOW1  (* a top-level name is bound twice, so its meaning depends on the line *)
  | A_BIND1    (* `let _ =` over a Unit value dismisses a failure that is not there *)
  | V_MOD1     (* a module is bound to a name in lower case *)
  | A_IF1      (* a `match` over a Bool says what an `if` or `unless` says *)
  | V_BIDI1    (* a bidi control in a string or comment hides how the source reads *)

(* The prefix says what a finding will do to you, so a rule ID printed in a
   terminal answers that on its own -- the same reason a raising function is
   spelled with a `!`.

   V- rules report a violation: something is wrong, and --strict promotes it
   to an error. A- rules are advisory and stay warnings however wand is run.

   Being decidable is what qualifies a rule to report a violation, but it
   does not oblige it: a rule can be perfectly decidable and still be
   advisory, because failing a build over it would punish the safer choice.
   Reclassifying a rule therefore renames it. *)
type kind =
  | Violation   (* --strict makes it an error *)
  | Advisory    (* always a warning *)

type rule = {
  id      : id;
  code    : string;
  summary : string;
  kind    : kind;
}

let all = [
  { id = V_PRED1;  code = "V-PRED1";
    summary = "a `?`-named function returns Bool";
    kind = Violation };
  { id = V_PRED2;  code = "V-PRED2";
    summary = "a `?`-named function also carries a redundant `is_` prefix";
    kind = Violation };
  { id = V_PRED3;  code = "V-PRED3";
    summary = "a function that returns Bool is not named with `?`";
    kind = Violation };
  { id = V_BANG1;  code = "V-BANG1";
    summary = "a function that can raise is not named with `!`";
    kind = Violation };
  { id = V_BANG2;  code = "V-BANG2";
    summary = "a `!`-named function cannot raise";
    kind = Violation };
  { id = V_OR1;    code = "V-OR1";
    summary = "an informationless error (`Result Unit _`) is a misfiled Option";
    kind = Violation };
  { id = V_NAME1;  code = "V-NAME1";
    summary = "a public signature exposes a trailing-underscore parameter";
    kind = Violation };
  (* The two halves of the manifest, and they are not the same kind of
     wrong. A manifest wider than the file is imprecise: everything the file
     does is declared, and what is left over grants a permission nothing
     asks for. That is worth saying and not worth failing a build over.

     A file with no manifest at all is the other way round -- it reaches
     outside itself and says nothing, so there is no line to check what it
     does against, and reading the first line tells you nothing about what
     running it will touch. That is the thing the manifest exists to stop,
     which makes it a violation: a repo running --strict can insist that a
     file which touches the world says so. `wand t --fix` writes the line,
     so the remedy is one command. *)
  { id = A_USES1;  code = "A-USES1";
    summary = "the manifest permits effects the file does not use";
    kind = Advisory };
  { id = V_USES2;  code = "V-USES2";
    summary = "the file performs effects and declares no manifest";
    kind = Violation };
  { id = V_DROP1;  code = "V-DROP1";
    summary = "a statement discards a Result, so a failure goes unread";
    kind = Violation };
  (* A test block answers with one outcome, so an assertion before the last
     one is discarded and the test reports a pass however it went. The same
     shape as V-DROP1 and the same remedy shape, but its own rule: what is
     lost is the whole verdict, not the failure inside a value. *)
  { id = V_DROP2;  code = "V-DROP2";
    summary = "a statement discards an assertion, so the test cannot fail";
    kind = Violation };
  (* A missing argument makes a function, not an error: `log! "found"` where
     `log!` takes two is a value waiting for the second, and as a statement
     nothing gives it one. It does nothing, and nothing said so. *)
  { id = V_DROP3;  code = "V-DROP3";
    summary = "a statement's value is a function nothing calls, so it does nothing";
    kind = Violation };
  { id = V_IMP2;   code = "V-IMP2";
    summary = "an import binds nothing the file uses";
    kind = Violation };
  (* The civil clock steps: NTP corrects it, an operator sets it. So the
     second reading can be earlier than the first, and the length between
     them is wrong or zero. `Clock.timed` reads a clock that no correction
     moves. *)
  { id = V_CLOCK1; code = "V-CLOCK1";
    summary = "a length of time measured by subtracting two clock readings";
    kind = Violation };
  { id = A_SHELL1; code = "A-SHELL1";
    summary = "a large shell pipeline inside $() could be wand-level stages";
    kind = Advisory };
  (* A violation for its --strict semantics: a repo that narrows Shell can
     also insist every command word be readable from the text. *)
  { id = V_SHELL1; code = "V-SHELL1";
    summary = "the manifest narrows Shell, but this command word is decided at run time";
    kind = Violation };
  (* The same rule with the nouns changed. A narrowed `Net` bounds the hosts
     a file may reach, and a URL the run decides is checked when the request
     is made rather than here -- which is legal, and worth saying out loud
     for a repository that wants every host readable from the text. *)
  { id = V_NET1; code = "V-NET1";
    summary = "the manifest narrows Net, but this host is decided at run time";
    kind = Violation };
  (* A newline inside `$()` is a command separator, exactly as it is in a
     shell script, so what follows it runs as a command of its own. That is
     rarely what a line broken for width means, and the first half can have
     done its work before the second half fails. `\` is the continuation,
     and wand passes it through. *)
  { id = V_SHELL2; code = "V-SHELL2";
    summary = "a command runs on to a second line, which starts a second command";
    kind = Violation };
  (* `Shell.inspect` is the script's promise that a command changes nothing,
     and a rehearsal runs it on that promise. A command word known to change
     things breaks the promise where a rehearsal is trusted most, so this is
     a violation, and --strict refuses the file. *)
  { id = V_SHELL3; code = "V-SHELL3";
    summary = "Shell.inspect runs a command that is known to change things";
    kind = Violation };
  (* A bare constructor name that another type in scope shares is read as
     the matched type's in a `match` arm. That is allowed; this is for a
     repository that wants every such name to say its type, since a bare
     one breaks when a module it comes from adds a second type with the
     name. A violation, so --strict holds a file to it, and --fix writes
     the type. *)
  { id = V_CTOR1; code = "V-CTOR1";
    summary = "a match arm names bare a constructor that another type shares";
    kind = Violation };
  (* The same promise, over words the text does not show. Nothing is wrong
     that can be seen, so this is advice: the reader is told that nothing
     checked it. *)
  { id = A_SHELL2; code = "A-SHELL2";
    summary = "Shell.inspect runs a command whose words are decided at run time";
    kind = Advisory };
  (* `let _ =` says the value is being dropped on purpose, which is what
     V-DROP1 asks for over a Result. Over a Unit there is no failure to
     dismiss, so the binder says nothing and the statement below it reads
     the same without it. Advisory: the file is correct either way. *)
  { id = A_BIND1;  code = "A-BIND1";
    summary = "a `let _ =` binds a Unit value, so the binder says nothing";
    kind = Advisory };
  (* A module is named as a type is, in upper case, so `Engine.step` reads
     the same whether the module is the standard library's or the
     program's. A file named in lower case is imported with `let`. *)
  { id = V_MOD1;   code = "V-MOD1";
    summary = "a module is bound to a name in lower case";
    kind = Violation };
  { id = V_SHADOW1; code = "V-SHADOW1";
    summary = "a top-level name is bound twice in one file";
    kind = Violation };
  (* A `match` over `true` and `false` is an `if` written longer, and an
     arm of `()` is the branch a one-armed `if` or `unless` leaves out. A
     reader from an Algol-style language reads the `if` at once. Advisory:
     the match is correct, and exhaustive. *)
  { id = A_IF1;    code = "A-IF1";
    summary = "a `match` over a Bool says what an `if` or `unless` says";
    kind = Advisory };
  (* A left-to-right or right-to-left override reorders the glyphs around it,
     so a reviewer reads one thing and the compiler another -- the "Trojan
     Source" trick. In code it is a lex error; in a string or a comment it
     survives, and the reader is told it is there. A warning: a string may
     hold real bidirectional text on purpose. *)
  { id = V_BIDI1;  code = "V-BIDI1";
    summary = "a bidirectional control character hides how the source reads";
    kind = Advisory };
]

let rule id = List.find (fun r -> r.id = id) all
let code id = (rule id).code
let kind id = (rule id).kind

let of_code c =
  match List.find_opt (fun r -> r.code = c) all with
  | Some r -> Some r.id
  | None   -> None

(* ── Messages ────────────────────────────────────────────────────────────── *)

(* Each message says what is wrong and what the author probably meant, in
   that order, without restating the rule ID the caller already prints. *)

let pred1 ~name ~actual =
  Printf.sprintf
    "'%s' is named as a predicate but returns %s; a `?` name promises Bool"
    name actual

(* The other half of the `?` convention. V-PRED1 has held one direction
   since the beginning -- a `?` name must return Bool -- and nothing held
   the other, so a predicate could go unmarked. `!` has had both directions
   since a signature could say whether a function raises, and this is the
   same rule for the same reason: a caller reading `List.all xs` cannot see
   that it answers a question, and a convention that only fires when you
   opt into it is a style note rather than a convention. *)
let pred3 ~name =
  Printf.sprintf
    "'%s' returns Bool but is not named as a predicate; a Bool answer is a \
     question answered, and the `?` is how a caller sees that at the call site"
    name

let or1 ~name =
  Printf.sprintf
    "'%s' returns a Result whose error side is Unit, so a failure says only \
     that it happened; if there is no reason to report, this is an Option"
    name

(* `json_parser` as a module is named: `JsonParser`. A leading `_`, which
   makes a module private, stays. *)
let module_name n =
  let lead = ref 0 in
  while !lead < String.length n && n.[!lead] = '_' do incr lead done;
  let rest = String.sub n !lead (String.length n - !lead) in
  String.make !lead '_'
  ^ String.concat ""
      (List.map String.capitalize_ascii
         (List.filter (fun s -> s <> "") (String.split_on_char '_' rest)))

let mod1 ~name ~what ~bare =
  let upper = module_name name in
  if bare then
    Printf.sprintf
      "`import %s` binds the module as '%s'; a module is named in upper case, \
       as the standard library's are: write `let %s = import %s`, or name the \
       file %s.wand"
      what name upper what upper
  else
    Printf.sprintf
      "'%s' names a module; a module is named in upper case, as the standard \
       library's are: write `let %s = import %s`"
      name upper what

let name1 ~name ~params =
  Printf.sprintf
    "'%s' exposes parameter%s named %s; a trailing underscore escapes a \
     keyword collision and is not part of the interface"
    name
    (if List.length params = 1 then "" else "s")
    (String.concat ", " (List.map (fun p -> "'" ^ p ^ "'") params))

(* The second binding is what the finding names, not the first. A value's
   earlier binding is not dead. A function defined between the two closes over it and goes on reading it
   after the second binding exists. That is the whole reason this is worth
   saying, and the reason no fix travels with it: the correction is a name,
   and only the author has one. *)
let shadow1 ~name ~line =
  Printf.sprintf
    "'%s' is already bound on line %d. Every mention between the two lines \
     reads that one and every mention below reads this one, so the name no \
     longer says which value it means -- rename one of them"
    name line

let imp2 ~what ~names =
  Printf.sprintf
    "nothing in this file uses %s, so the import does nothing; %s"
    what
    (match names with
     | [n] -> Printf.sprintf "'%s' is never mentioned below" n
     | ns -> Printf.sprintf "none of %s is mentioned below"
               (String.concat ", " (List.map (fun n -> "'" ^ n ^ "'") ns)))

let clock1 =
  "this measures a length of time by subtracting two readings of the civil \
   clock, which steps: the second reading can be earlier than the first. \
   Wrap the work in `Clock.timed`, which answers how long it took beside \
   what it returned"

let drop1 ~typ =
  Printf.sprintf
    "this statement's value is a %s and nothing reads it, so a failure here \
     is lost; match it, call the `!` sibling, or bind it to `_` to say the \
     failure does not matter"
    typ

let drop3 ~typ =
  Printf.sprintf
    "this statement's value is a function, %s, so nothing calls it and it \
     does nothing; give it the arguments it is missing"
    typ

let drop2 =
  "this statement is an assertion and nothing reads its outcome, so the test \
   passes however this assertion went; a test block answers with one outcome \
   -- return this one, or give each assertion its own `test` inside a `group` \
   so every one of them is reported"

let pred2 ~name =
  let bare = String.sub name 3 (String.length name - 3) in
  Printf.sprintf
    "'%s' says it is a predicate twice; `?` already carries that, so this is \
     '%s'" name bare

let bang1 ~name =
  (* A name takes one ending. `ok?!` and `ok!?` are both parse errors, so a
     predicate that raises cannot be told to add the `!`, which is what this
     used to say. It ends in `!`. *)
  if String.length name > 0 && name.[String.length name - 1] = '?' then
    let bare = String.sub name 0 (String.length name - 1) in
    Printf.sprintf
      "'%s' can raise, so `?` is not the ending it takes; it is '%s!'"
      name bare
  else
    Printf.sprintf
      "'%s' can raise, but its name does not say so; call it '%s!' and give \
       the plain name to a version that returns a Result" name name

let bang2 ~name =
  let bare = String.sub name 0 (String.length name - 1) in
  Printf.sprintf
    "'%s' cannot raise, so the `!` promises a risk that is not there; it is \
     '%s'" name bare

(* `corrected` is None when the file reaches outside itself for nothing:
   `uses {}` is not the advice there, since a file that does nothing outward
   has nothing to declare and the line should go. *)
let uses1 ~unused ~corrected =
  match corrected with
  | Some c ->
    Printf.sprintf
      "the manifest permits %s, which this file does not use; it could be \"%s\""
      unused c
  | None ->
    Printf.sprintf
      "the manifest permits %s, and this file reaches outside itself for \
       nothing; it could be removed"
      unused

(* Advisory rather than a violation, and deliberately so: a file without a
   manifest is legal, and a rule that failed a build over one would make
   every casual script pay for a feature it did not ask for. But a manifest
   is only worth having if it makes code better, so the linter is where the
   file is told what better looks like -- and it can hand over the exact
   line, since the effects are already inferred. *)
let uses2 ~performs ~corrected =
  Printf.sprintf
    "this file performs %s and does not say so; it could declare \"%s\""
    performs corrected

(* The same shape as uses1, for binaries instead of effect labels: the
   Shell(...) list admits a program no command position names. Only
   reported when every command position is literal -- an interpolated one
   may be exactly where the unused-looking binary is spawned. *)
let uses1_shell ~unused ~corrected =
  Printf.sprintf
    "the manifest allows %s, which no command here runs; it could be \"%s\""
    unused corrected

let net1_dynamic =
  "this request's host is decided at run time, so the Net(...) list is \
   checked when the request is made rather than here"

let shell1_dynamic =
  "this command's first word is decided at run time, so the Shell(...) \
   list is checked when it spawns rather than here"

let ctor1 ~name ~type_name =
  Printf.sprintf
    "'%s' is a constructor of more than one type here, and this arm names \
     it bare; write '%s.%s'" name type_name name

let shell3 ~what ~fn =
  Printf.sprintf
    "'%s' changes things, and Shell.%s runs it in a rehearsal as well as in \
     a real run; run it with $(...) so that --dry-run withholds it" what fn

let inspect_dynamic ~fn =
  Printf.sprintf
    "Shell.%s runs this command in a rehearsal, and its words are only \
     known at run time, so wand cannot check that it only reads" fn

let shell2 =
  "this command runs on to the next line, and a newline inside $() starts a \
   second command -- end the line with \\ to continue it"

let shell1 ~stages =
  Printf.sprintf
    "this $() is a %d-operator shell pipeline; stages moved into wand are \
     typed, and appear individually under --trace" stages

let bind1 ~standalone =
  "`let _ =` says a dropped failure does not matter, and this value is Unit, \
   so there is no failure to drop -- "
  ^ (if standalone then "write the statement on its own"
     else "write the statement on its own, sequenced with `;`")

(* Which arm of the match does nothing, if one does: that arm is the
   branch the suggested form leaves out. *)
let if1 ~empty =
  match empty with
  | `True ->
    "this `match` over a Bool does nothing when the value is true -- \
     write `unless <value> then` with the `false` arm as its branch"
  | `False ->
    "this `match` over a Bool does nothing when the value is false -- \
     write `if <value> then` with the `true` arm as its branch"
  | `Neither ->
    "a `match` over a Bool is an `if` -- write `if <value> then` with the \
     `true` arm, and `else` with the `false` arm"

(* `name` is the character's Unicode label, so the message says which one it
   found without printing the character itself -- printing it would move the
   message's own glyphs around. *)
let bidi1 ~name =
  Printf.sprintf
    "a bidirectional control character (%s) is here; it reorders the \
     characters around it, so the source reads one way and runs another -- \
     take it out, or write it as an escape in a string if the text needs it"
    name
