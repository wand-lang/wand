## 0.100.0 - 2026-10-01

### Upgrading

One change can make a file fail that passed with 0.99.0. `wand t` names each place, and `wand t --fix` corrects it:

- `f (None (x))` is now one argument, and an error. An older `wand f` wrote `f None (x)` that way. Run `wand t --fix`, then `wand f`, to get `f None (x)` back.

### Changed

- `wand p upgrade <url>@<version>` moves a 0.x requirement to another minor in place, and a bare `wand p upgrade` says when a newer minor is released (#81).

### Fixed

- `wand p upgrade` finds a required package by the URL as `wand.pkg` writes it, with no scheme (#80).
- `wand f` keeps `f None (x)` as written, and `f (None (x))` is one argument, an error that `wand t --fix` corrects (#78).
- An update that leaves a function field alone is charged nothing for it, so an unused function field no longer breaks a check in other code (#79).
