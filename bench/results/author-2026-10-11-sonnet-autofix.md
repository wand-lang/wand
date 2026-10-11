# Authoring benchmark: 2026-10-11

Model `sonnet`, 3 runs per task and language, wand 0.104.1.

| language | runs | pass | turns (passing) | turns (all) | escaped | cost |
|---|---|---|---|---|---|---|
| wand | 45 | 97% | 6.1 | 6.1 | 1 | $4.23 |

| task | wand |
|---|---|
| 01-log-levels | 3/3, 4.0 turns |
| 02-rotate-backups | 2/3, 7.3 turns |
| 03-crashlooping-pods | 3/3, 10.3 turns |
| 04-csv-column-sum | 3/3, 4.7 turns |
| 05-env-file-merge | 3/3, 5.0 turns |
| 06-disk-threshold | 3/3, 5.3 turns |
| 07-commits-by-author | 3/3, 5.3 turns |
| 08-duplicate-files | 3/3, 7.0 turns |
| 09-version-bump | 3/3, 4.0 turns |
| 10-url-hosts | 3/3, 4.3 turns |
| 11-duration-total | 3/3, 8.3 turns |
| 12-safe-clean | 3/3, 6.0 turns |
| 13-retry-command | 3/3, 4.3 turns |
| 14-replace-in-files | 3/3, 7.3 turns |
| 15-latency-percentiles | 3/3, 8.7 turns |

## Where the turns go

Commands per run, by what each one was for. One tool call can hold several commands. The JSON lines beside this file hold every call.

| language | read the guide | look up a name | try an expression | typecheck | format | run the program | write or edit | other |
|---|---|---|---|---|---|---|---|---|
| wand | 0.3 | 5.5 | 0.3 | 0.4 | 0.1 | 1.0 | 1.8 | 3.2 |

## Errors the agent saw

Tool results that failed or reported an error or a warning, by their first such line, with line and column taken out. The most frequent come first.

### wand: 65 results

| times | first error line |
|---|---|
| 7 | `Exit code 1` |
| 6 | `Error: type error: N:N: expected Int, got Option Int` |
| 4 | `Error: type error: N:N: Option Int is not ordered, so it cannot be compared with < > <= >=` |
| 3 | `Error: type error: N:N: expected String, got Path` |
| 3 | `Regex.capture : Regex -> String -> List String` |
| 3 | `warning: N:N: V-BANG1: 'main' can raise, but its name does not say so; call it 'main!' and give the plain name to a version that returns a Result` |
| 3 | `warning: N:N: V-BANG2: 'fail!' cannot raise, so the '!' promises a risk that is not there; it is 'fail'` |
| 2 | `Error("expected an object, got \"{\\\"ms\\\": 3}\"") : Result String Int` |
| 2 | `Error: parse error: N:N: expected ->, got if` |
| 2 | `Proc.args : Unit -> List String` |
| 2 | `V-USES2: 1 — inserted "uses {FS.Read, IO, Proc}"` |
| 1 | `#!/bin/sh` |
| 1 | `93%` |
| 1 | `<fn> : Algorithm -> String` |
| 1 | `E-TYPE: 1 — inserted "import Option"` |

## Failures

- 02-rotate-backups / wand / run 2: only real backups count: wanted exit 0 and deleted backup-2025-12-31.tar\ndeleted backup-2026-01-02.tar\ndeleted backup-2026-01-10.tar\nkept 1, got exit 0 and deleted backup-2025-12-31.tar\ndeleted backup-2026-01-02.tar\ndeleted backup-2026-01-10.tar\ndeleted backup-2026-05-05.tar\nkept 1 
