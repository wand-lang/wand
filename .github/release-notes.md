## 0.89.0 - 2026-09-27

A field may name its key in a document.

### Added

- **A field may name its key in a document.** Write the key between the
  field's name and its type when the document spells it in a way a field
  name cannot be:

  ```
  type DaemonEndpoint(port "Port": Int)
  type Props(ref "$ref": Option String = None, list_type "x-kubernetes-list-type": Option String = None)
  ```

  The derived decoder reads the field from the key, and the derived
  encoder, `JSON.of` and `TOML.of` write it there. `TOML.decode` and
  `YAML.decode` read it too; wand writes no YAML. In wand code the field is
  its name: `DaemonEndpoint(port = 10250)`, `e.port`. A command line reads a
  flag by the field's name (`--port`). An error names the key: `.Port: no
  such field`.

  Before, one key that a field name cannot be cost the type its derived
  decoder and encoder, and every type that held it.

  Two fields of one constructor cannot read one key, written or implied by
  a field's name:

  ```
  type A(port "x": Int, x: Int)
  -- fields 'port' and 'x' of 'A' both read the key "x" in a document;
  -- give one of them another key
  ```

### Fixed

- **`TOML.of` writes a sum** as `JSON.of` does: the word for a bare
  constructor (`pull = "Never"`), the value for one that holds one
  (`maxSurge = "25%"`). Before, it refused one with "it has no named
  fields".
