# Authoring benchmark: 2026-10-21

Model `sonnet`, 3 runs per task and language, wand 0.113.1.

| language | runs | pass | turns (passing) | turns (all) | escaped | cost |
|---|---|---|---|---|---|---|
| wand | 45 | 100% | 5.5 | 5.5 | 0 | $3.81 |
| python | 45 | 100% | 4.0 | 4.0 | 0 | $1.79 |
| bash | 45 | 100% | 3.7 | 3.7 | 0 | $1.81 |

| task | wand | python | bash |
|---|---|---|---|
| 01-log-levels | 3/3, 6.0 turns | 3/3, 4.3 turns | 3/3, 3.0 turns |
| 02-rotate-backups | 3/3, 6.7 turns | 3/3, 3.7 turns | 3/3, 4.7 turns |
| 03-crashlooping-pods | 3/3, 7.7 turns | 3/3, 4.0 turns | 3/3, 3.7 turns |
| 04-csv-column-sum | 3/3, 4.3 turns | 3/3, 3.3 turns | 3/3, 4.0 turns |
| 05-env-file-merge | 3/3, 4.7 turns | 3/3, 4.0 turns | 3/3, 3.3 turns |
| 06-disk-threshold | 3/3, 4.3 turns | 3/3, 3.3 turns | 3/3, 4.3 turns |
| 07-commits-by-author | 3/3, 4.7 turns | 3/3, 3.7 turns | 3/3, 3.3 turns |
| 08-duplicate-files | 3/3, 6.0 turns | 3/3, 4.7 turns | 3/3, 3.7 turns |
| 09-version-bump | 3/3, 6.0 turns | 3/3, 4.0 turns | 3/3, 3.3 turns |
| 10-url-hosts | 3/3, 4.0 turns | 3/3, 5.3 turns | 3/3, 3.3 turns |
| 11-duration-total | 3/3, 4.7 turns | 3/3, 3.3 turns | 3/3, 4.0 turns |
| 12-safe-clean | 3/3, 5.3 turns | 3/3, 4.7 turns | 3/3, 3.7 turns |
| 13-retry-command | 3/3, 5.0 turns | 3/3, 4.7 turns | 3/3, 4.0 turns |
| 14-replace-in-files | 3/3, 4.0 turns | 3/3, 3.0 turns | 3/3, 3.3 turns |
| 15-latency-percentiles | 3/3, 9.0 turns | 3/3, 4.0 turns | 3/3, 3.3 turns |

## Where the turns go

Commands per run, by what each one was for. One tool call can hold several commands. The JSON lines beside this file hold every call.

| language | read the guide | look up a name | try an expression | typecheck | format | run the program | write or edit | other |
|---|---|---|---|---|---|---|---|---|
| wand | 0.2 | 5.8 | 0.1 | 0.2 | 0.0 | 1.0 | 1.5 | 3.2 |
| python | 0.7 | 0.0 | 0.0 | 0.0 | 0.0 | 0.2 | 1.1 | 1.8 |
| bash | 0.4 | 0.0 | 0.0 | 0.0 | 0.0 | 0.1 | 1.0 | 2.3 |

## Errors the agent saw

Tool results that failed or reported an error or a warning, by their first such line, with line and column taken out. The most frequent come first.

### wand: 36 results

| times | first error line |
|---|---|
| 9 | `Exit code 1` |
| 2 | `#!/bin/sh` |
| 2 | `Error("expected an object, got \"{\\\"ms\\\": 3}\"") : Result String Int` |
| 2 | `Error: type error: N:N: did you forget to import the standard library Option?` |
| 2 | `Error: type error: N:N: expected List (List String), got String` |
| 2 | `Proc.args : Unit -> List String` |
| 2 | `Regex.capture : Regex -> String -> Option (List String)` |
| 1 | `E-TYPE: 5 — inserted "import Result"` |
| 1 | `E-TYPE: 7 — inserted "import Proc"` |
| 1 | `Error: parse error: N:N: expected ), got ,` |
| 1 | `Error: parse error: N:N: expected ), got \|` |
| 1 | `Error: parse error: N:N: expected =, got !` |
| 1 | `Error: too many arguments` |
| 1 | `Error: type error: N:N: expected 'a -> (Result String (Map Int), (Int, String)), got Result String (Map Int)` |
| 1 | `Error: type error: N:N: expected ('a -> 'b ! 'e) -> 'a -> 'b ! 'e, got Map 'a` |

### python: 8 results

| times | first error line |
|---|---|
| 3 | `INFO 2` |
| 2 | `Exit code 1` |
| 1 | `B=1` |
| 1 | `p/a 7` |
| 1 | `removed .x` |

### bash: 8 results

| times | first error line |
|---|---|
| 2 | `prod/api-1 7` |
| 2 | `removed -x` |
| 1 | `2 Ana Silva` |
| 1 | `A=2` |
| 1 | `INFO 2` |
| 1 | `Permission for this tool use was denied. It requires approval, and this session has no approval surface — nobody can answer a permission prompt here — so it` |

## Failures

