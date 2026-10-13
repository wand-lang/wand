# Authoring benchmark: 2026-10-13

Model `sonnet`, 3 runs per task and language, wand 0.107.0.

| language | runs | pass | turns (passing) | turns (all) | escaped | cost |
|---|---|---|---|---|---|---|
| wand | 45 | 100% | 5.8 | 5.8 | 0 | $4.17 |
| python | 45 | 100% | 3.6 | 3.6 | 0 | $2.12 |
| bash | 45 | 100% | 3.4 | 3.4 | 0 | $2.18 |

| task | wand | python | bash |
|---|---|---|---|
| 01-log-levels | 3/3, 5.0 turns | 3/3, 3.7 turns | 3/3, 3.0 turns |
| 02-rotate-backups | 3/3, 7.3 turns | 3/3, 4.0 turns | 3/3, 3.3 turns |
| 03-crashlooping-pods | 3/3, 8.3 turns | 3/3, 3.0 turns | 3/3, 3.0 turns |
| 04-csv-column-sum | 3/3, 4.0 turns | 3/3, 3.7 turns | 3/3, 3.0 turns |
| 05-env-file-merge | 3/3, 4.7 turns | 3/3, 4.3 turns | 3/3, 3.3 turns |
| 06-disk-threshold | 3/3, 4.7 turns | 3/3, 3.3 turns | 3/3, 3.0 turns |
| 07-commits-by-author | 3/3, 6.0 turns | 3/3, 3.7 turns | 3/3, 3.0 turns |
| 08-duplicate-files | 3/3, 6.3 turns | 3/3, 3.7 turns | 3/3, 3.7 turns |
| 09-version-bump | 3/3, 5.0 turns | 3/3, 4.7 turns | 3/3, 3.3 turns |
| 10-url-hosts | 3/3, 4.7 turns | 3/3, 3.3 turns | 3/3, 3.3 turns |
| 11-duration-total | 3/3, 5.0 turns | 3/3, 3.3 turns | 3/3, 3.7 turns |
| 12-safe-clean | 3/3, 5.7 turns | 3/3, 3.0 turns | 3/3, 4.7 turns |
| 13-retry-command | 3/3, 6.7 turns | 3/3, 4.3 turns | 3/3, 4.0 turns |
| 14-replace-in-files | 3/3, 6.0 turns | 3/3, 3.0 turns | 3/3, 3.0 turns |
| 15-latency-percentiles | 3/3, 8.0 turns | 3/3, 3.0 turns | 3/3, 3.0 turns |

## Where the turns go

Commands per run, by what each one was for. One tool call can hold several commands. The JSON lines beside this file hold every call.

| language | read the guide | look up a name | try an expression | typecheck | format | run the program | write or edit | other |
|---|---|---|---|---|---|---|---|---|
| wand | 0.4 | 5.1 | 0.4 | 0.3 | 0.1 | 1.3 | 1.3 | 3.4 |
| python | 0.3 | 0.0 | 0.0 | 0.0 | 0.0 | 0.1 | 1.0 | 2.3 |
| bash | 0.0 | 0.0 | 0.0 | 0.0 | 0.0 | 0.2 | 0.9 | 2.8 |

## Errors the agent saw

Tool results that failed or reported an error or a warning, by their first such line, with line and column taken out. The most frequent come first.

### wand: 37 results

| times | first error line |
|---|---|
| 8 | `Exit code 1` |
| 3 | `Error("expected an object, got \"{\\\"ms\\\": 3}\"") : Result String Int` |
| 2 | `Error: type error: N:N: constructor 'ShellResult' expects 3 argument(s), got 1` |
| 2 | `Error: type error: N:N: expected ('a -> 'b ! 'e) -> 'a -> 'b ! 'e, got Map 'a` |
| 2 | `Error: type error: N:N: expected List String, got Stream {IO, Raise \| ..} String` |
| 2 | `FS.list_dir : Path -> Result String (List Path) ! {FS.Read}` |
| 2 | `warning: N:N: V-BANG2: 'fail!' cannot raise, so the '!' promises a risk that is not there; it is 'fail'` |
| 1 | `E-TYPE: 1 — inserted "import Regex"` |
| 1 | `E-TYPE: 4 — inserted "import Result"` |
| 1 | `Error: type error: N:N: Result 'b 'a does not add -- + and - work on Int, Float, Size and Duration` |
| 1 | `Error: type error: N:N: did you forget to import the standard library Path?` |
| 1 | `Error: type error: N:N: did you forget to import the standard library Proc?` |
| 1 | `Error: type error: N:N: expected Unit -> List String, got List 'a` |
| 1 | `Error: type error: N:N: field access requires a named type, got Regex` |
| 1 | `Error: type error: N:N: namespace 'String' has no member 'all_digits' -- 'wand d String' lists its members` |

### python: 7 results

| times | first error line |
|---|---|
| 3 | `INFO 2` |
| 2 | `error: [Errno 21] Is a directory: '/tmp/o'` |
| 1 | `2 Ana Silva` |
| 1 | `p/a 7` |

### bash: 6 results

| times | first error line |
|---|---|
| 3 | `INFO 2` |
| 1 | `A=1` |
| 1 | `END { for (k in v) printf "%s\001%s\n", k, v[k] }` |
| 1 | `prod/api-1 7` |

## Failures

