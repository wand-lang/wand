(* Narrowing: a manifest label that carries a word list, bounding what it
   admits.

   `Shell(git, curl)` says which binaries the file may run. `Net(api.x.com)`
   will say which hosts its bytes may reach. One mechanism with two users,
   which is why it lives here rather than in `shell_scan.ml`: the same list
   is checked against the words written in the file, checked again at
   dispatch over what the run actually resolved, and rendered back into a
   suggested manifest line by `--fix`.

   What differs between the two is not the checking. It is the separator a
   pattern may not cross, and whether an entry matches a trailing component
   of the word. Both are properties of what the word names, so they live in
   a rule beside the label that owns it. *)

type rule = {
  (* The character `*` will not cross. `/` in a binary, because that is what
     a shell glob does; `.` in a host, because that is what a TLS
     certificate does. Neither reader learns a third convention. *)
  sep : char;
  (* Whether an entry that names no path may match the word's final
     component. `git` admits `/usr/bin/git`, because a binary is the same
     binary wherever it was found on PATH. A host has no such reading:
     `example.com` is not `api.example.com`, and treating it as one would
     silently widen the claim. *)
  basename : bool;
}

let binary = { sep = '/'; basename = true }

let host = { sep = '.'; basename = false }

(* A port is written `:8080`, and has no parts for `*` to stop at. *)
let port = { sep = ':'; basename = false }

(* Which labels take a word list. The manifest parser asks this, so a label
   that is not here rejects `(...)` with a message rather than parsing
   something that means nothing. *)
let rule_of_label = function
  | "Shell" -> Some binary
  | "Net" -> Some host
  | "Net.Listen" -> Some port
  | _ -> None

let narrowable name = rule_of_label name <> None

(* `*` matches any run of characters that holds no separator.

   Backtracking rather than a regex, because a manifest word is a handful of
   characters and this keeps the rule readable as the one sentence the
   reference states. The worst case is exponential in the number of `*`s,
   which for a hostname or a binary name is not a case that exists. *)
let matches ~sep pattern word =
  let np = String.length pattern and nw = String.length word in
  let rec go i j =
    if i = np then j = nw
    else if pattern.[i] = '*' then
      (* Zero or more, stopping at the separator: `*` in `*.example.com`
         cannot swallow the dot that makes `a.b.example.com` a level
         deeper. *)
      let rec grow k =
        if go (i + 1) k then true
        else if k < nw && word.[k] <> sep then grow (k + 1)
        else false
      in
      grow j
    else j < nw && pattern.[i] = word.[j] && go (i + 1) (j + 1)
  in
  go 0 0

(* Does one manifest entry admit one word?

   An entry that names a path matches the whole word. An entry that does not
   may also match the word's final component, where the rule allows it, so
   `git` covers `/usr/bin/git` and `docker-*` covers
   `/usr/local/bin/docker-compose`. *)
let admits ~rule entry word =
  matches ~sep:rule.sep entry word
  || (rule.basename
      && not (String.contains entry rule.sep)
      && matches ~sep:rule.sep entry (Filename.basename word))

let allowed ~rule ~allow word =
  List.exists (fun entry -> admits ~rule entry word) allow

(* A pattern that admits everything is a bare label spelled at greater
   length. A manifest should not have two spellings for one claim, and the
   shorter one is the one a reader already knows. *)
let admits_everything word = word = "*"
