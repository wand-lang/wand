# Authoring benchmark: 2026-10-21

Model `sonnet`, 3 runs per task and language, wand 0.112.0.

| language | runs | pass | turns (passing) | turns (all) | escaped | cost |
|---|---|---|---|---|---|---|
| wand | 45 | 100% | 5.7 | 5.7 | 0 | $3.85 |
| python | 45 | 100% | 3.7 | 3.7 | 0 | $1.74 |
| bash | 45 | 100% | 3.4 | 3.4 | 0 | $1.75 |

| task | wand | python | bash |
|---|---|---|---|
| 01-log-levels | 3/3, 5.0 turns | 3/3, 3.3 turns | 3/3, 3.3 turns |
| 02-rotate-backups | 3/3, 6.0 turns | 3/3, 3.7 turns | 3/3, 3.3 turns |
| 03-crashlooping-pods | 3/3, 7.3 turns | 3/3, 4.0 turns | 3/3, 3.0 turns |
| 04-csv-column-sum | 3/3, 3.3 turns | 3/3, 3.3 turns | 3/3, 4.7 turns |
| 05-env-file-merge | 3/3, 4.3 turns | 3/3, 3.7 turns | 3/3, 3.3 turns |
| 06-disk-threshold | 3/3, 5.0 turns | 3/3, 4.0 turns | 3/3, 3.0 turns |
| 07-commits-by-author | 3/3, 4.3 turns | 3/3, 3.3 turns | 3/3, 3.3 turns |
| 08-duplicate-files | 3/3, 7.7 turns | 3/3, 3.3 turns | 3/3, 3.3 turns |
| 09-version-bump | 3/3, 4.3 turns | 3/3, 3.3 turns | 3/3, 3.0 turns |
| 10-url-hosts | 3/3, 5.7 turns | 3/3, 3.3 turns | 3/3, 3.3 turns |
| 11-duration-total | 3/3, 4.7 turns | 3/3, 3.7 turns | 3/3, 3.0 turns |
| 12-safe-clean | 3/3, 6.3 turns | 3/3, 3.7 turns | 3/3, 4.3 turns |
| 13-retry-command | 3/3, 7.7 turns | 3/3, 4.7 turns | 3/3, 4.0 turns |
| 14-replace-in-files | 3/3, 5.0 turns | 3/3, 4.0 turns | 3/3, 3.3 turns |
| 15-latency-percentiles | 3/3, 9.3 turns | 3/3, 4.0 turns | 3/3, 3.0 turns |

## Where the turns go

Commands per run, by what each one was for. One tool call can hold several commands. The JSON lines beside this file hold every call.

| language | read the guide | look up a name | try an expression | typecheck | format | run the program | write or edit | other |
|---|---|---|---|---|---|---|---|---|
| wand | 0.2 | 5.3 | 0.2 | 0.2 | 0.1 | 0.7 | 1.8 | 3.9 |
| python | 0.6 | 0.0 | 0.0 | 0.0 | 0.0 | 0.0 | 1.0 | 1.5 |
| bash | 0.2 | 0.0 | 0.0 | 0.0 | 0.0 | 0.0 | 1.0 | 2.3 |

## Errors the agent saw

Tool results that failed or reported an error or a warning, by their first such line, with line and column taken out. The most frequent come first.

### wand: 44 results

| times | first error line |
|---|---|
| 6 | `Exit code 1` |
| 4 | `Error: type error: N:N: constructor 'ShellResult' expects 3 argument(s), got 1` |
| 2 | `Error: type error: N:N: Map (Option (List Int) -> Option (List Int)) and Option (List Int) -> Option (List Int) are not the same type` |
| 2 | `Error: type error: N:N: String and Path are not the same type` |
| 2 | `Error: type error: N:N: expected Unit, got Unit -> 'a` |
| 2 | `FS.list_dir : Path -> Result String (List Path) ! {FS.Read}` |
| 2 | `Regex.capture : Regex -> String -> Option (List String)` |
| 2 | `warning: N:N: V-BANG1: 'main' can raise, but its name does not say so; call it 'main!' and give the plain name to a version that returns a Result` |
| 1 | `#!/bin/sh` |
| 1 | `<tool_use_error>Error: No such tool available: bash. Tool names are case-sensitive: call Bash instead.</tool_use_error>` |
| 1 | `Error("expected an object, got \"{\\\"endpoint\\\":\\\"a\\\",\\\"ms\\\":3}\"")` |
| 1 | `Error("expected an object, got \"{\\\"ms\\\": 3}\"") : Result String Int` |
| 1 | `Error: type error: N:N: List String and Stream {IO, Raise \| ..} String are not the same type. '\|>' takes the whole call on its left as its input, not the call's` |
| 1 | `Error: type error: N:N: did you forget to import the standard library Decode?` |
| 1 | `Error: type error: N:N: did you forget to import the standard library JSON?` |

### python: 10 results

| times | first error line |
|---|---|
| 3 | `removed .h` |
| 2 | `2 Ana Silva` |
| 2 | `INFO 2` |
| 1 | `DSN=p://x?a=b` |
| 1 | `error: cannot read nope: No such file or directory` |
| 1 | `p/a 7` |

### bash: 5 results

| times | first error line |
|---|---|
| 2 | `removed -x` |
| 1 | `2 Ana Silva` |
| 1 | `A=4` |
| 1 | `prod/api-1 7` |

## Failures

