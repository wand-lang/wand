## 0.104.1 - 2026-10-10

### Fixed

- A renamed import is no longer a breaking change in `wand p release` (#112).
  For example, a field type in the interface section changes from
  `MetaV1.LabelSelector` to `k8s/meta/v1.LabelSelector`.
