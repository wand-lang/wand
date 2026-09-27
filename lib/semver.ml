(* Semver precedence. Numbers compare as numbers, so `1.10.0` is above
   `1.9.0`. A version with a prerelease is below the same version without
   one, and two prereleases compare identifier by identifier: a number
   against a number numerically, a number below a word, two words by their
   text, and if all of them match, the longer list wins.

   `Lexer.version_error` holds the grammar, so both spellings that used to
   reach here and are not semver -- a leading zero, and an empty prerelease
   identifier -- no longer exist by the time anything is compared. *)
(* Numbers, prerelease, build. Build metadata is taken off first: semver
   says it is ignored when determining precedence, so nothing below this
   point ever sees it -- and leaving it on would put a `+` in front of
   `int_of_string`, which is how `1.2.3+b` used to raise rather than
   compare. *)
let version_build s =
  match String.index_opt s '+' with
  | None -> (s, None)
  | Some i -> (String.sub s 0 i,
               Some (String.sub s (i + 1) (String.length s - i - 1)))

let version_parts s =
  let s, _build = version_build s in
  match String.index_opt s '-' with
  | None -> (s, None)
  | Some i -> (String.sub s 0 i,
               Some (String.sub s (i + 1) (String.length s - i - 1)))

(* One of the three numbers. Total: a value of this type has three, so the
   segment is there and is digits. *)
let version_number v i =
  match String.split_on_char '.' (fst (version_parts v)) with
  | segs -> (match List.nth_opt segs i with
             | Some seg -> (match int_of_string_opt seg with Some n -> n | None -> 0)
             | None -> 0)

let compare_prerelease a b =
  let ids t = String.split_on_char '.' t in
  let numeric t = t <> "" && String.for_all (fun c -> c >= '0' && c <= '9') t in
  let rec go xs ys =
    match xs, ys with
    | [], [] -> 0
    | [], _  -> -1          (* fewer identifiers is lower *)
    | _, []  -> 1
    | x :: xs, y :: ys ->
      let c =
        match numeric x, numeric y with
        | true, true   -> compare (int_of_string x) (int_of_string y)
        | true, false  -> -1
        | false, true  -> 1
        | false, false -> compare x y
      in
      if c <> 0 then c else go xs ys
  in
  go (ids a) (ids b)

let compare_versions a b =
  let (na, pa) = version_parts a and (nb, pb) = version_parts b in
  let nums t = List.map int_of_string (String.split_on_char '.' t) in
  let c = compare (nums na) (nums nb) in
  if c <> 0 then c
  else
    match pa, pb with
    | None,   None   -> 0
    | Some _, None   -> -1      (* a prerelease is below the release *)
    | None,   Some _ -> 1
    | Some x, Some y -> compare_prerelease x y
