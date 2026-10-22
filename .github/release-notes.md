## 0.114.0 - 2026-10-21

This release refuses a text decoder that is given JSON, and fixes the call to parse the JSON first. A file that gives `Decode.run` a decoder of an object or a list fails until `wand t --fix` runs.

### Changed

- `Decode.run` or `Decode.lines` with a decoder that reads an object or a list is an error, because they read text and do not parse JSON. For `Decode.run`, `wand t --fix` writes `JSON.parse` and `JSON.decode` (#216).

### Fixed

- A text decoder given JSON says what to use: `JSON.parse`, then `JSON.decode` (#216).

### Documentation

- The reference's Decoders section says that `Decode.run` and `Decode.lines` do not parse JSON, and shows the form to use (#216).
