## 0.90.0 - 2026-09-27

`Shell.inspect_with` sends input to a command that only reads.

### Added

- **`Shell.inspect_with!` and `Shell.inspect_with` send input to a command
  that only reads.** They are `inspect!` and `inspect` with text for the
  command's stdin, in the way that `input |> $(cmd)` writes it. A rehearsal
  runs them, and the line says `ran (inspect): ...`:

  ```
  let stored =
    json |> Shell.inspect_with! $*(kubectl apply --server-side --dry-run=server -o json -f -)
  ```

  Before, a command that reads its input from stdin could not be run with
  `inspect`. A server-side dry run was run with `$(...)`, so a rehearsal
  withheld it and gave `""`, and the script stopped at the first step that
  read the result.

### Changed

- **V-SHELL3 accepts a kubectl dry run.** A kubectl command with
  `--dry-run=server` or `--dry-run=client` stores nothing, whatever its
  verb, so `Shell.inspect!` can run `kubectl apply --dry-run=server`.
  `--dry-run=none` is a real run and is still a violation.
