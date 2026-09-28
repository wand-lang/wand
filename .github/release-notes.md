## 0.92.0 - 2026-09-28

wand writes YAML.

### Added

- **wand writes YAML.** `YAML.of` turns a value into YAML, the same way
  `JSON.of` does, and `YAML.stringify` writes it as text.
  `YAML.stringify_all` writes several documents with `---` between them,
  which is how a manifest file with more than one object looks.
  `YAML.of_json` turns a JSON value, such as what an encoder gives, into
  YAML, and cannot fail:

  ```
  FS.write_file! ./deploy.yaml (YAML.stringify_all [YAML.of_json (Service.encoder s), YAML.of_json (Deployment.encoder d)])
  ```

  Every value has one spelling: block style, two spaces for each level.
  A string gets quotes whenever a YAML reader could read it as something
  else. That includes `yes`, `no`, `on` and `off`, which kubectl reads as
  booleans. A whole `Float` keeps its `.0`, so `1.0` reads back as a
  `Float`.

  Writing makes a new document. It does not edit a file in place, so
  comments and layout in a file that is read and written again are lost.
