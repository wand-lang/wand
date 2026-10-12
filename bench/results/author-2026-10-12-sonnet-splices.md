# Authoring benchmark: 2026-10-12

Model `sonnet`, 3 runs per task and language, wand 0.105.0.

| language | runs | pass | turns (passing) | turns (all) | escaped | cost |
|---|---|---|---|---|---|---|
| wand | 45 | 100% | 5.5 | 5.5 | 0 | $4.03 |

| task | wand |
|---|---|
| 01-log-levels | 3/3, 4.7 turns |
| 02-rotate-backups | 3/3, 5.0 turns |
| 03-crashlooping-pods | 3/3, 6.0 turns |
| 04-csv-column-sum | 3/3, 4.3 turns |
| 05-env-file-merge | 3/3, 4.7 turns |
| 06-disk-threshold | 3/3, 5.3 turns |
| 07-commits-by-author | 3/3, 5.3 turns |
| 08-duplicate-files | 3/3, 8.0 turns |
| 09-version-bump | 3/3, 4.7 turns |
| 10-url-hosts | 3/3, 4.3 turns |
| 11-duration-total | 3/3, 5.3 turns |
| 12-safe-clean | 3/3, 5.3 turns |
| 13-retry-command | 3/3, 6.0 turns |
| 14-replace-in-files | 3/3, 4.3 turns |
| 15-latency-percentiles | 3/3, 9.3 turns |

## Where the turns go

Commands per run, by what each one was for. One tool call can hold several commands. The JSON lines beside this file hold every call.

| language | read the guide | look up a name | try an expression | typecheck | format | run the program | write or edit | other |
|---|---|---|---|---|---|---|---|---|
| wand | 0.5 | 5.3 | 0.2 | 0.2 | 0.1 | 0.6 | 1.3 | 2.3 |

## Errors the agent saw

Tool results that failed or reported an error or a warning, by their first such line, with line and column taken out. The most frequent come first.

### wand: 37 results

| times | first error line |
|---|---|
| 7 | `Exit code 1` |
| 2 | `#!/bin/sh` |
| 2 | `Decode.run : Decoder 'a -> String -> Result String 'a` |
| 2 | `Error: type error: N:N: did you forget to import the standard library IO?` |
| 2 | `Error: type error: N:N: unknown constructor 'Sha256'` |
| 2 | `Proc.args : Unit -> List String` |
| 2 | `warning: N:N: V-BANG1: 'main' can raise, but its name does not say so; call it 'main!' and give the plain name to a version that returns a Result` |
| 2 | `warning: N:N: V-BANG1: 'num' can raise, but its name does not say so; call it 'num!' and give the plain name to a version that returns a Result` |
| 1 | `E-TYPE: 1 — inserted "import Path"` |
| 1 | `E-TYPE: 31 — replaced "s" with "(Path.to_string s)"` |
| 1 | `E-TYPE: 4 — inserted "import Proc"` |
| 1 | `E-TYPE: 7 — inserted "import Path"` |
| 1 | `Error("expected an object, got \"{\\\"ms\\\": 3}\"") : Result String Int` |
| 1 | `Error: type error: N:N: 'e' needs its type before '.1' can be read: write its tuple type, such as '(e: (A, B))'` |
| 1 | `Error: type error: N:N: Map (Option (List Int) -> Option (List Int)) and Option (List Int) -> Option (List Int) are not the same type` |

## Failures

