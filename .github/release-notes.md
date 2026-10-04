## 0.101.3 - 2026-10-04

### Fixed

- A top-level `let` with a literal binder, as `let 0 = 0`, now ends at the end of its line, as `let _ = e` does, and the lines below it are statements of their own. Before, the `let` took all the lines below it as its body. `wand f` removes the brackets from `let (0) = 0`, so a second `wand f` gave a different file (#83).
