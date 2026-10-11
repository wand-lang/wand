# Authoring benchmark: 2026-10-11

Model `sonnet`, 3 runs per task and language, wand 0.104.1.

| language | runs | pass | turns (passing) | turns (all) | escaped | cost |
|---|---|---|---|---|---|---|
| wand | 45 | 100% | 6.2 | 6.2 | 0 | $4.21 |

| task | wand |
|---|---|
| 01-log-levels | 3/3, 4.3 turns |
| 02-rotate-backups | 3/3, 6.0 turns |
| 03-crashlooping-pods | 3/3, 13.0 turns |
| 04-csv-column-sum | 3/3, 4.0 turns |
| 05-env-file-merge | 3/3, 5.0 turns |
| 06-disk-threshold | 3/3, 4.0 turns |
| 07-commits-by-author | 3/3, 6.0 turns |
| 08-duplicate-files | 3/3, 6.3 turns |
| 09-version-bump | 3/3, 4.0 turns |
| 10-url-hosts | 3/3, 4.3 turns |
| 11-duration-total | 3/3, 9.3 turns |
| 12-safe-clean | 3/3, 5.3 turns |
| 13-retry-command | 3/3, 6.7 turns |
| 14-replace-in-files | 3/3, 6.0 turns |
| 15-latency-percentiles | 3/3, 8.3 turns |

## Where the turns go

Commands per run, by what each one was for. One tool call can hold several commands. The JSON lines beside this file hold every call.

| language | read the guide | look up a name | try an expression | typecheck | format | run the program | write or edit | other |
|---|---|---|---|---|---|---|---|---|
| wand | 0.3 | 5.3 | 0.2 | 0.5 | 0.1 | 1.3 | 1.8 | 3.3 |

## Errors the agent saw

Tool results that failed or reported an error or a warning, by their first such line, with line and column taken out. The most frequent come first.

### wand: 66 results

| times | first error line |
|---|---|
| 10 | `Error: type error: N:N: expected Int, got Option Int` |
| 6 | `Error: type error: N:N: expected String, got Path` |
| 6 | `Exit code 1` |
| 2 | `#!/bin/sh` |
| 2 | `Error("expected an object, got \"{\\\"ms\\\": 3}\"") : Result String Int` |
| 2 | `Error: type error: N:N: '++' joins two strings; two lists are joined with 'List.concat xs ys'` |
| 2 | `Error: type error: N:N: Option Int and Int are not the same type` |
| 2 | `Error: type error: N:N: expected List String, got Stream {IO, Raise \| ..} String` |
| 2 | `Error: type error: N:N: expected Map (Option (List Int) -> Option (List Int)), got Option (List Int) -> Option (List Int)` |
| 2 | `Proc.args : Unit -> List String` |
| 2 | `Regex.capture : Regex -> String -> List String` |
| 2 | `sed: 1: "solution.wand` |
| 1 | `Decode.and_map  : Decoder 'a -> Decoder ('a -> 'b) -> Decoder 'b` |
| 1 | `Decode.run : Decoder 'a -> String -> Result String 'a` |
| 1 | `Error: parse error: N:N: expected a field name, got Int(1)` |

## Failures

