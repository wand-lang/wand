## 0.115.2 - 2026-11-11

This release fixes two formatter bugs that wrote files that do not parse, and a typechecker bug that used all the memory of the machine.

### Fixed

- `wand f` no longer breaks `type P = U; of 1`. It wrote `of 1` on the next line, and the parser read that as `U of Int` (#223).
- `wand f` keeps the `;` in `let f s = 1; let f s = ... in --`. Without it, the second `let f` became a second equation of `f` (#224).
- `wand t` no longer uses all the memory of the machine on a read such as `p.1686018427387903` from a value with no known type. Past `.25`, the message names the elements and gives no example tuple type.
- The fuzzer reports an input that holds more than 2GB in one check as a finding, `memory:budget`, and goes on to the next input. Before, the runner ran out of memory and shut down, and no issue was filed.
