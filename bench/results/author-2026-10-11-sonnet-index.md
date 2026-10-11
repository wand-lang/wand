# Authoring benchmark: 2026-10-11

Model `sonnet`, 3 runs per task and language, wand 0.104.1.

| language | runs | pass | turns (passing) | turns (all) | escaped | cost |
|---|---|---|---|---|---|---|
| wand | 45 | 93% | 7.2 | 7.4 | 3 | $4.71 |

| task | wand |
|---|---|
| 01-log-levels | 3/3, 7.7 turns |
| 02-rotate-backups | 2/3, 9.7 turns |
| 03-crashlooping-pods | 3/3, 13.7 turns |
| 04-csv-column-sum | 3/3, 3.3 turns |
| 05-env-file-merge | 3/3, 6.3 turns |
| 06-disk-threshold | 3/3, 5.0 turns |
| 07-commits-by-author | 3/3, 5.7 turns |
| 08-duplicate-files | 3/3, 8.7 turns |
| 09-version-bump | 3/3, 3.7 turns |
| 10-url-hosts | 3/3, 5.0 turns |
| 11-duration-total | 3/3, 9.0 turns |
| 12-safe-clean | 3/3, 5.3 turns |
| 13-retry-command | 3/3, 8.7 turns |
| 14-replace-in-files | 1/3, 11.0 turns |
| 15-latency-percentiles | 3/3, 8.0 turns |

## Where the turns go

Commands per run, by what each one was for. One tool call can hold several commands. The JSON lines beside this file hold every call.

| language | read the guide | look up a name | try an expression | typecheck | format | run the program | write or edit | other |
|---|---|---|---|---|---|---|---|---|
| wand | 0.4 | 6.0 | 0.2 | 0.7 | 0.3 | 1.5 | 2.2 | 3.6 |

## Errors the agent saw

Tool results that failed or reported an error or a warning, by their first such line, with line and column taken out. The most frequent come first.

### wand: 106 results

| times | first error line |
|---|---|
| 7 | `Error: type error: N:N: expected Int, got Option Int` |
| 7 | `Exit code 1` |
| 5 | `Error: type error: N:N: expected Unit, got (Int -> 'a ! {Proc}) -> 'b` |
| 4 | `Error: parse error: N:N: unexpected token: else` |
| 4 | `Error: type error: N:N: expected Option Int, got Int` |
| 4 | `Error: type error: N:N: expected String, got Path` |
| 4 | `Proc.args : Unit -> List String` |
| 4 | `Regex.capture : Regex -> String -> List String` |
| 3 | `sed: 1: "solution.wand` |
| 2 | `#!/bin/sh` |
| 2 | `Error("expected an object, got \"{\\\"ms\\\": 3}\"") : Result String Int` |
| 2 | `Error: type error: N:N: Option Int and Int are not the same type` |
| 2 | `Error: type error: N:N: constructor 'ShellResult' expects 3 argument(s), got 1` |
| 2 | `Error: type error: N:N: lines 12 to 13 are read as one expression, so line 12 is called with line 13 as its argument:` |
| 2 | `Error: type error: N:N: lines 14 to 15 are read as one expression, so line 14 is called with line 15 as its argument:` |

## Failures

- 02-rotate-backups / wand / run 3: only real backups count: wanted exit 0 and deleted backup-2025-12-31.tar\ndeleted backup-2026-01-02.tar\ndeleted backup-2026-01-10.tar\nkept 1, got exit 0 and deleted backup-2025-12-31.tar\ndeleted backup-2026-01-02.tar\ndeleted backup-2026-01-10.tar\ndeleted backup-2026-05-05.tar\nkept 1 
- 14-replace-in-files / wand / run 1: a dot is a dot: wanted exit 0 and changed .hidden.txt (1)\nchanged n.txt (1)\nchanged 2 files, 2 replacements, got exit 0 and changed n.txt (1)\nchanged 1 files, 1 replacements 
- 14-replace-in-files / wand / run 2: a dot is a dot: wanted exit 0 and changed .hidden.txt (1)\nchanged n.txt (1)\nchanged 2 files, 2 replacements, got exit 0 and changed n.txt (1)\nchanged 1 files, 1 replacements 
