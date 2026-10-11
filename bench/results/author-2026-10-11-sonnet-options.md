# Authoring benchmark: 2026-10-11

Model `sonnet`, 3 runs per task and language, wand 0.104.1.

| language | runs | pass | turns (passing) | turns (all) | escaped | cost |
|---|---|---|---|---|---|---|
| wand | 45 | 100% | 6.1 | 6.1 | 0 | $4.20 |

| task | wand |
|---|---|
| 01-log-levels | 3/3, 4.0 turns |
| 02-rotate-backups | 3/3, 6.7 turns |
| 03-crashlooping-pods | 3/3, 5.7 turns |
| 04-csv-column-sum | 3/3, 7.0 turns |
| 05-env-file-merge | 3/3, 4.3 turns |
| 06-disk-threshold | 3/3, 5.0 turns |
| 07-commits-by-author | 3/3, 4.3 turns |
| 08-duplicate-files | 3/3, 8.7 turns |
| 09-version-bump | 3/3, 4.3 turns |
| 10-url-hosts | 3/3, 5.3 turns |
| 11-duration-total | 3/3, 6.7 turns |
| 12-safe-clean | 3/3, 6.0 turns |
| 13-retry-command | 3/3, 8.7 turns |
| 14-replace-in-files | 3/3, 5.0 turns |
| 15-latency-percentiles | 3/3, 10.0 turns |

## Where the turns go

Commands per run, by what each one was for. One tool call can hold several commands. The JSON lines beside this file hold every call.

| language | read the guide | look up a name | try an expression | typecheck | format | run the program | write or edit | other |
|---|---|---|---|---|---|---|---|---|
| wand | 0.5 | 5.2 | 0.4 | 0.2 | 0.1 | 1.0 | 1.4 | 3.9 |

## Errors the agent saw

Tool results that failed or reported an error or a warning, by their first such line, with line and column taken out. The most frequent come first.

### wand: 48 results

| times | first error line |
|---|---|
| 8 | `Exit code 1` |
| 2 | `#!/bin/sh` |
| 2 | `Error: parse error: N:N: expected =, got }` |
| 2 | `Error: parse error: N:N: expected }, got =` |
| 2 | `Error: type error: N:N: did you forget to import the standard library Path?` |
| 2 | `Regex.capture : Regex -> String -> List String` |
| 2 | `warning: N:N: V-BANG1: 'main' can raise, but its name does not say so; call it 'main!' and give the plain name to a version that returns a Result` |
| 1 | `E-TYPE: 3 — inserted "import Path"` |
| 1 | `Error("cannot parse \"z\" as Int") : Result String Int` |
| 1 | `Error("expected an object, got \"{\\\"ms\\\": 3}\"") : Result String Int` |
| 1 | `Error: parse error: N:N: expected ), got +` |
| 1 | `Error: parse error: N:N: the manifest must be the first thing in the file, before everything but a shebang and comments` |
| 1 | `Error: type error: N:N: 'Digest' declares no types, so '(@ Sha256 (@ (@ (. Path join) d) r))' names nothing in it. '\|>' takes the whole call on its left as its ` |
| 1 | `Error: type error: N:N: 'acc' needs its type before '.0' can be read: write its tuple type, such as '(acc: (A, B))'` |
| 1 | `Error: type error: N:N: 'acc' needs its type before '.0' can be read: write its tuple type, such as '(acc: (A, B))'. '\|>' takes the whole call on its left as it` |

## Failures

