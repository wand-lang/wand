# Replace text in files

Write a program that replaces one piece of text with another in every text
file of a directory tree. It is run as `prog DIR OLD NEW`.

Work on every regular file whose name ends in `.txt`, in `DIR` and in all
directories below it. In each file, replace every occurrence of `OLD` with
`NEW`. `OLD` and `NEW` are plain text, not patterns: `.`, `*`, `/`, `&`
and `\` mean only themselves. Scan each file from the start; after a
replacement, continue after the replaced text, so occurrences do not overlap
and the text that `NEW` puts in is not scanned again. `NEW` can be empty.
Write a file back only if it changed.

Print `changed PATH (N)` for each file that changed, where `PATH` is the
path relative to `DIR` with `/` between the parts and `N` is the number of
replacements in that file, in ascending byte order of `PATH`. Then print
`changed F files, R replacements` with the totals (also when they are 0).

Errors:

- If there are fewer than three arguments, or `OLD` is empty, print a message
  to stderr and exit with code 2.
- If `DIR` is not a directory, print a message to stderr and exit with
  code 1.

Example. If `DIR` holds `a.txt` with `v1.0 and v1.0` and `docs/b.txt` with
`v1.0`, and `c.md` with `v1.0`, then `prog DIR v1.0 v2.0` prints:

```
changed a.txt (2)
changed docs/b.txt (1)
changed 2 files, 3 replacements
```
