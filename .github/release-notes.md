## 0.95.2 - 2026-09-30

### Fixed

- `State.decoder` and `State.encoder` use their own module's `State` (#64).
- A field read in a qualified constructor's arguments resolves in the caller's scope, and `m.T(base, f = v)` updates the module's `T` (#63).
