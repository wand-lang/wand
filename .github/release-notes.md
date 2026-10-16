## 0.107.3 - 2026-10-16

This release fixes four bugs that the fuzzer found. Three of them are in
`wand f`, and one is in the parser.

### Fixed

- `Wand.cost` gives the same steps for statements in brackets as for the same statements written as a block. Before, `wand f` could change the count (#181).
- A call below a local function, such as `log (String.upper x)` below `let log x = ...`, parses. Before, it was read as a second equation and was a parse error (#182).
- `wand f` keeps the brackets on a `then` branch that ends in an `if` with no `else`. Before, the `else` that follows went to the inner `if` (#184).
- `wand f` keeps a `\` line continuation that stands on a line of its own in a command. Before, a second `wand f` removed it.
