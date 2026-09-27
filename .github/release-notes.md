## 0.87.1 - 2026-09-27

A socket read buffer for each domain.

### Fixed

- **Each domain has its own socket read buffer.** Before, every socket
  read in the process went through one shared buffer. That was safe only
  while all reads ran on one domain. If two domains read at the same time,
  two connections could get each other's bytes, with no error. No release
  ran socket reads on two domains, so no known program was affected.

### Changed

- CI uses `ocaml/setup-ocaml` 3.9.0.
