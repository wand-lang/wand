## 0.111.0 - 2026-10-20

This release makes destructuring work the same everywhere: a `let` at the top of a file takes a constructor pattern, and a list pattern means the same in a `let` as in a `match`. One change can make an existing file fail: read Upgrading.

### Upgrading

- A list pattern in a `let` now matches only a list of its own length, as in a `match`. `let [a, b] = [1, 2, 3]` raises; before, it bound the first two and dropped the 3. Write `let [a, b :: _] = xs` where the rest of the list does not matter (#209).

### Changed

- A list pattern means the same in a `let` as in a `match`: `[a, b]` is a list of exactly two (#209).
- `wand f` writes a field-list pattern as a parameter without brackets: `fn Pod(name) -> name`, `let f Pod(a = x) = x`. A positional payload keeps them: `fn (Some x) -> x`.
- `wand f` writes a constructor pattern at the top of a file without brackets: `let Some x = o`, `let Pod(name) = p` (#208).

### Fixed

- At the top of a file, `let` with a constructor pattern binds as it does in a body. Before, `let Pod(name) = p` and `let Wrap x = w` were refused, and `let Some x = o` left `x` unbound (#208).

### Documentation

- The reference has a Destructuring section in Let bindings, and a Destructuring arguments section in Functions (#208).
