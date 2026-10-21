## 0.112.0 - 2026-10-21

This release renames `Map.from_list` to `Map.of_list`, so that every conversion into a type is named `of_X`. A file that uses the old name fails until `wand t --fix` runs.

### Changed

- `Map.from_list` is `Map.of_list`, as `Stream.of_list`, `JSON.of_list` and the other conversions are named. The old name is an error, and `wand t --fix` writes the new one (#210).
