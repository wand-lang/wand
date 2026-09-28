## 0.91.0 - 2026-09-28

A constructor can have its own spelling in JSON, TOML, YAML and CSV.

### Added

- **A constructor can have its own spelling in JSON, TOML, YAML and CSV.**
  Some values cannot be wand constructor names, such as `None`, `*` or
  `client auth`. Give the constructor a name that wand accepts, and put the
  real spelling after it in quotes:

  ```
  type DnsPolicy = ClusterFirst | Default | None_ "None"
  type Operation = All "*" | CREATE
  ```

  wand reads `"None"` as `DnsPolicy.None_`, and writes `DnsPolicy.None_` as
  `"None"`. This applies to JSON, TOML, YAML, CSV and the values of
  command-line flags. In your code you still write `DnsPolicy.None_`.

  Only a constructor with no payload can have a spelling. Two constructors
  of one type cannot have the same spelling:

  ```
  type P = A | B "A"
  -- constructors 'A' and 'B' of 'P' have the same spelling "A"; give one
  -- of them another spelling
  ```

### Fixed

- **Text after a type declaration is now an error.** Before, wand ignored
  the problem: `type P = A | B 42` declared the type, and then ran `42` as a
  separate statement. Now wand stops with "a type declaration ends at the
  end of its line".
