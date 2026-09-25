(* What a module costs to load, kept between runs.

   Loading a module is read, lex, parse, infer, evaluate. Measured on a
   200-definition module: 0.04ms, 0.31ms, 0.99ms, 5.72ms, 0.17ms. Inference
   is almost all of it and evaluation is almost none, so what is worth
   keeping is the front end's answer -- the parsed program and the types it
   inferred -- and evaluation runs again every time from the parsed form.

   Nothing here holds a closure. `Marshal` cannot write one, and an
   evaluation environment is made of them; the types and the syntax tree are
   ordinary data.

   ── Staleness ────────────────────────────────────────────────────────────
   A module's types are inferred against its imports, so its entry is only
   good while they are unchanged too. The key is the hash of the module's own
   source *and* of everything it imports, transitively. That makes a stale
   entry unreachable rather than merely unused: change anything in the
   closure and the key changes, so nothing has to notice the change or check
   for it. The alternative -- key on the file alone and record what its
   imports hashed to -- keeps fewer entries and makes validity something
   checked rather than guaranteed. Reading a dependency's bytes is the cheap
   part of loading it, so the guarantee is nearly free. *)

(* `WAND_CACHE=0` turns the cache off, and so do `false`, `no` and `off`.
   Anything else, or nothing at all, leaves it on.

   Named for what it controls rather than against it. The obvious first choice
   is a negative switch tested for presence -- `NO_COLOR` and its relatives
   work that way -- but a name with a negative in it invites a value, and then
   every value a reader would pick to mean *off* (`WAND_NO_CACHE=0`, `=false`,
   or the empty string a shell leaves behind when an unset variable is
   interpolated) turns the cache off instead. A switch that reads backwards
   under exactly the values people reach for is not a switch worth having. *)
let disabled =
  match Sys.getenv_opt "WAND_CACHE" with
  | Some v ->
    (match String.lowercase_ascii (String.trim v) with
     | "0" | "false" | "no" | "off" -> true
     | _ -> false)
  | None -> false

(* Bumped when the shape of what is written changes. An old entry then has a
   different key rather than being read back as the wrong shape -- Marshal
   returns the bytes typed as whatever the reader expects, whatever they
   hold. "5" is the digest an entry carries in front of its bytes. *)
let format_version = "6"

(* Where the entries live, most specific first.

   `WAND_CACHE_HOME` is wand's own, and is the directory itself rather than a
   parent to append to -- point it somewhere and that is where entries go.
   It exists so wand's cache can be moved (a fast disk, a container volume, a
   sandbox) without relocating every other tool's.

   `XDG_CACHE_HOME` is the shared convention beneath it, and is a parent: the
   spec says a program takes a subdirectory of it. Honouring it matters where
   `$HOME` is read-only and the environment has already been told where
   writable scratch lives.

   Then the platform's own answer. `~/.cache` is a Unix convention and not a
   Windows one, where the place for this is `%LOCALAPPDATA%`; falling through
   to `$HOME/.cache` there would land somewhere Windows does not keep, and
   falling through to the temp directory -- which is what happened before --
   means an entry written by one run is not there for the next. *)
let dir () =
  let non_empty name =
    match Sys.getenv_opt name with
    | Some d when String.trim d <> "" -> Some d
    | _ -> None
  in
  match non_empty "WAND_CACHE_HOME" with
  | Some d -> d
  | None ->
    (match non_empty "XDG_CACHE_HOME" with
     | Some d -> Filename.concat d "wand"
     | None ->
       if Sys.win32 then
         (match non_empty "LOCALAPPDATA" with
          | Some d -> Filename.concat (Filename.concat d "wand") "cache"
          | None -> Filename.concat (Filename.get_temp_dir_name ()) "wand")
       else
         (match non_empty "HOME" with
          | Some h -> Filename.concat (Filename.concat h ".cache") "wand"
          | None -> Filename.concat (Filename.get_temp_dir_name ()) "wand"))

(* A cache entry is a marshalled value handed straight back typed as whatever
   the reader expects, and `Marshal` checks nothing: a file whose bytes were
   chosen by someone else becomes a type-confused value that can crash or
   corrupt. So an entry is only ever read from a directory the current user
   owns and no one else can write to -- otherwise a local attacker could plant
   a `.wandc` under the key a run will look for. This is the check ssh and gpg
   make of their own directories, and it is the load-bearing one: a private
   directory cannot hold a file this process did not put there.

   On Windows the POSIX ownership and mode bits do not carry this meaning, so
   the check is skipped there; `%LOCALAPPDATA%` is already per-user. *)
let dir_is_trustworthy d =
  if Sys.win32 then true
  else
    match Unix.stat d with
    | st ->
      st.Unix.st_kind = Unix.S_DIR
      && st.Unix.st_uid = Unix.getuid ()
      (* Not writable by group or other: 0o022 are those two write bits. *)
      && st.Unix.st_perm land 0o022 = 0
    | exception Unix.Unix_error _ -> false

(* Best-effort: a cache that cannot be created is a cache that is not used,
   never an error a script has to hear about. Created private (0700) so it is
   trustworthy from the start; a directory that already exists but is not is
   left alone and simply not used, rather than being loosened underneath
   whoever made it. *)
let ensure_dir () =
  let d = dir () in
  if not (Sys.file_exists d) then
    (try
       let parent = Filename.dirname d in
       if not (Sys.file_exists parent) then Unix.mkdir parent 0o755;
       Unix.mkdir d 0o700
     with Unix.Unix_error _ -> ());
  d

(* A module's types come from its source *and* from the binary that inferred
   them: change a builtin's signature and every cached entry is wrong while
   every hash still matches. That is not hypothetical -- loosening
   `par_each` left `Par.each` reporting its old type until the cache was
   turned off, which reads exactly like the change not having worked.

   The binary's identity is its version, plus its size and mtime, which move
   on every rebuild during development. `Sys.executable_name` can be wrong
   about where it is; that costs a cache miss, never a stale hit. *)
let binary_identity =
  lazy
    (match Unix.stat Sys.executable_name with
     | st ->
       Printf.sprintf "%s:%d:%.0f" Version.value st.Unix.st_size
         st.Unix.st_mtime
     | exception _ -> Version.value)

(* `path` is part of the key because a type's name now carries the module
   that declares it. Two files with the same bytes at two paths declare two
   types, and an entry keyed on the bytes alone would hand one file the
   other's names. Moving a file therefore misses its entry, which costs one
   re-check. *)
let key ~path ~source ~deps =
  Digest.to_hex
    (Digest.string
       (String.concat "\000"
          (format_version :: Lazy.force binary_identity :: path :: source
          :: List.sort compare deps)))

let path_for key = Filename.concat (dir ()) (key ^ ".wandc")

(* An entry is its own digest and then its bytes. Marshal reads whatever it
   is given: an entry with a valid header and a corrupt body is not an
   exception it can catch but a segmentation fault, and a fault cannot be
   caught either -- so the entry stayed, and every later run of that script
   died the same way until someone deleted the cache by hand. A power loss
   between the write and the rename produced that shape with nobody at
   fault, which is why the write below now reaches the disk before the
   rename.

   This answers corruption, not a chosen file. An entry whose bytes someone
   picked can carry a matching digest as easily as a matching header. What
   stops that is `dir_is_trustworthy`: the bytes are only ever read from a
   directory this user owns and no one else can write to. *)
let digest_len = 16

let find (key : string) : 'a option =
  if disabled then None
  (* A value is deserialised only out of a directory this user owns and no one
     else can write to -- see `dir_is_trustworthy`. Anywhere else, every entry
     is a cache miss, so a planted file is never read. *)
  else if not (dir_is_trustworthy (dir ())) then None
  else
    let p = path_for key in
    if not (Sys.file_exists p) then None
    else
      (* A truncated, unreadable or damaged entry is a cache miss, not a
         failure: two runs writing at once, a half-written file, a format
         that moved on. Drop it and let the caller do the work. *)
      let drop () = (try Sys.remove p with _ -> ()); None in
      (* The descriptor is asked what it is, rather than the name being
         asked again. The check above is of the directory; between it and
         this open a name inside it can still be replaced, and a `stat` of
         the path afterwards would be answering about whatever the name
         points at now. `fstat` answers about the file in hand: a regular
         file, owned by this user, that no one else can write. A symlink
         planted under the key resolves to whatever it names, and that file
         has to pass the same three questions.

         `O_NOFOLLOW` would say it in one word; OCaml's Unix does not have
         it. *)
      let read_entry () =
        let fd = Unix.openfile p [Unix.O_RDONLY; Unix.O_CLOEXEC] 0 in
        Fun.protect ~finally:(fun () -> try Unix.close fd with Unix.Unix_error _ -> ())
          (fun () ->
            let st = Unix.fstat fd in
            if st.Unix.st_kind <> Unix.S_REG
               || st.Unix.st_uid <> Unix.getuid ()
               || st.Unix.st_perm land 0o022 <> 0
            then None
            else Some (In_channel.input_all (Unix.in_channel_of_descr fd)))
      in
      try
        match read_entry () with
        | None -> drop ()
        | Some bytes ->
        if String.length bytes <= digest_len then drop ()
        else
          let recorded = String.sub bytes 0 digest_len in
          let body = String.sub bytes digest_len (String.length bytes - digest_len) in
          if not (String.equal recorded (Digest.string body)) then drop ()
          else Some (Marshal.from_string body 0)
      with _ -> drop ()

let store (key : string) (value : 'a) : unit =
  if not disabled then begin
    let d = ensure_dir () in
    (* Written only where it can later be trusted to read back. A directory
       that is not private is not written to either, rather than seeding it
       with entries no run will read. *)
    if Sys.file_exists d && dir_is_trustworthy d then
      try
        (* Written beside and renamed into place, so a reader never sees a
           half-written entry: two runs of the same script race often. The
           temp file is opened 0600, so an entry is never briefly readable by
           anyone but its owner even inside a private directory. *)
        let tmp = Filename.concat d (key ^ "." ^ string_of_int (Unix.getpid ()) ^ ".tmp") in
        let body = Marshal.to_string value [] in
        let oc = open_out_gen [Open_wronly; Open_creat; Open_trunc; Open_binary] 0o600 tmp in
        Fun.protect ~finally:(fun () -> close_out_noerr oc)
          (fun () ->
            output_string oc (Digest.string body);
            output_string oc body;
            (* On the disk before the rename, or a power loss leaves the
               name pointing at bytes that were never all written. *)
            flush oc;
            Unix.fsync (Unix.descr_of_out_channel oc));
        Sys.rename tmp (path_for key)
      with _ -> ()
  end
