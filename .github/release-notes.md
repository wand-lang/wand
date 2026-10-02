## 0.101.1 - 2026-10-02

### Fixed

- The code lens names a type alias from an import by its short name, as `ObjId`. It showed the path of the imported file and the expansion of the alias.
- Go to definition on a member of an imported file, as `Driver.Init`, goes to that member in the file. It went to the line of the import.
