## 0.107.3 - 2026-10-15

This release fixes three bugs that the fuzzer found. Two of them are in
`wand f`, and one is in the parser.

### Fixed

- `Wand.cost` gives the same steps for statements in brackets as for the same statements written as a block. Before, `wand f` could change the count (#181).
- A call below a local function, such as `log (String.upper x)` below `let log x = ...`, parses. Before, it was read as a second equation and was a parse error (#182).
- `wand f` keeps the brackets on a `then` branch that ends in an `if` with no `else`. Before, the `else` that follows went to the inner `if` (#184).
