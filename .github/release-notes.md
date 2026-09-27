## 0.88.0 - 2026-09-27

Documents with sums, constructors that share a name, and commands a rehearsal may run.

### Added

- **Sums, aliases and `JSON` fields derive decoders and encoders.** A sum
  derives when the document can say by itself which constructor it holds:

  ```
  type PullPolicy = Always | Never | IfNotPresent   -- "Never"
  type IntOrString = I Int | S String               -- 1 or "25%"
  ```

  Every constructor a bare word is written as the word. Every constructor
  holding one value, each of a different JSON kind, is written as the
  value. Any other sum is refused, and the error says why. A field of an
  alias type is read as the type the alias names, so `type Quantity =
  String` is a string in the document; before, it cost the record its
  decoder. A field of type `JSON` holds whatever the document holds.

- **Two types may share a constructor name.** Write the type to say which:

  ```
  type PullPolicy    = Always | Never | IfNotPresent
  type RestartPolicy = Always | OnFailure | Never
  let p = PullPolicy.Always
  let c = apps.Container(pull = apps.PullPolicy.Always, ...)
  ```

  A bare name that two types in scope share is an error that gives both
  spellings. In a `match` arm over a value whose type is already known, a
  bare name means that type's constructor. A name only one type has stays
  bare. A derived encoder still writes the bare word.

- **`Shell.inspect!` and `Shell.inspect` run a command that only reads.**
  They are `$(...)` and `Shell.run`, except that `--dry-run` runs them too
  and reports `ran (inspect): ...`. Use them only for a command that changes
  nothing, such as a query or a status. Before, a rehearsal withheld every
  command, so a script that asked a question first got `""` and took a path
  a real run never would.

- **Three lint rules.**
  - `V-SHELL3`: `Shell.inspect` runs a command known to change things, such
    as `kubectl apply`, `git push` or `rm`.
  - `A-SHELL2`: `Shell.inspect` runs a command whose words only the run
    decides, so nothing could check it.
  - `V-CTOR1`: a `match` arm names bare a constructor that another type in
    scope shares. `wand t --fix` writes the type.

- **Completion offers constructors after a dot:** `Color.`,
  `apps.PullPolicy.`, and `apps.` for a name only one type in the module has.

### Changed

- **A constructor cannot take a built-in constructor's name.** `type A =
  None | Other` was accepted, and after it every `None` in the file was an
  `A`. `Some`, `None`, `Ok`, `Error` and the built-in records' constructors
  are refused now, as a built-in type's name already was.

### Fixed

- A mismatch between a top-level binding's written type and its value, as in
  `let x : Int = "s"`, is reported with its line and column.
- `wand f` no longer leaves a space at the end of the first line when it
  wraps a long sum at its alternatives.
- `[1] ++ [2]` says that `++` joins strings and `List.concat` joins lists.
  `List.map2` and `zip_with` name `List.zip` with `List.map`, not `map`.
