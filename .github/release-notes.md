## 0.94.1 - 2026-09-30

A signal or an `exit` inside `Par.all!` stops the program.

### Fixed

- **A signal or an `exit` inside `Par.all!`, `Par.race` or `Par.timeout`
  stops the program.** A SIGTERM or Ctrl-C that reached every branch of
  `Par.all!` failed with `race: no thunk finished` and exit 1, where a
  stopped script exits 143 or 130. An `exit` in one branch was lost: the
  race waited for another branch, and the script went on. Now the other
  branches are stopped, every `with` releases, and the program exits with
  the signal's code or the `exit` code.
