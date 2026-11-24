## 0.115.5 - 2026-11-24

This release fixes a formatter bug that changed what a file means.

### Fixed

- `wand f` no longer changes a payload against a constructor's name into a field list. `Boxed(tag;)` gives `Boxed` the payload `tag`. `wand f` wrote it as `Boxed(tag)`, which is the field `tag`, and the next `wand f` wrote it again in a different layout. Now it writes `Boxed((tag))` (#228):

  ```
  ytytytytytytytytytytytytytytytytytytytytytytytytytytytytytytytytytytytytytytype Boxed(tag;)
  ```
