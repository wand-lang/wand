# Authoring benchmark: 2026-10-11

Model `sonnet`, 3 runs per task and language, wand 0.104.1.

| language | runs | pass | turns (passing) | turns (all) | escaped | cost |
|---|---|---|---|---|---|---|
| wand | 45 | 100% | 8.0 | 8.0 | 0 | $4.82 |

| task | wand |
|---|---|
| 01-log-levels | 3/3, 6.7 turns |
| 02-rotate-backups | 3/3, 7.0 turns |
| 03-crashlooping-pods | 3/3, 10.3 turns |
| 04-csv-column-sum | 3/3, 8.3 turns |
| 05-env-file-merge | 3/3, 7.0 turns |
| 06-disk-threshold | 3/3, 8.7 turns |
| 07-commits-by-author | 3/3, 6.7 turns |
| 08-duplicate-files | 3/3, 9.3 turns |
| 09-version-bump | 3/3, 7.7 turns |
| 10-url-hosts | 3/3, 6.0 turns |
| 11-duration-total | 3/3, 7.3 turns |
| 12-safe-clean | 3/3, 6.3 turns |
| 13-retry-command | 3/3, 11.0 turns |
| 14-replace-in-files | 3/3, 8.7 turns |
| 15-latency-percentiles | 3/3, 9.3 turns |

## Where the turns go

Commands per run, by what each one was for. One tool call can hold several commands. The JSON lines beside this file hold every call.

| language | read the guide | look up a name | try an expression | typecheck | format | run the program | write or edit | other |
|---|---|---|---|---|---|---|---|---|
| wand | 0.4 | 7.4 | 0.5 | 0.7 | 0.1 | 1.5 | 2.2 | 4.0 |

## Errors the agent saw

Tool results that failed or reported an error or a warning, by their first such line, with line and column taken out. The most frequent come first.

### wand: 90 results

| times | first error line |
|---|---|
| 6 | `Exit code 1` |
| 3 | `Args.help? : List String -> Bool` |
| 3 | `Error: type error: N:N: expected Unit, got (Int -> 'a ! {Proc}) -> 'b` |
| 3 | `Error: type error: N:N: lines 23 to 24 are read as one expression, so line 23 is called with line 24 as its argument:` |
| 2 | `Error: type error: N:N: 'r' needs its type before '.code' can be read: write '(r: ShellResult)'` |
| 2 | `Error: type error: N:N: constructor 'ShellResult' expects 3 argument(s), got 1` |
| 2 | `Error: type error: N:N: did you forget to import the standard library Int?` |
| 2 | `Error: type error: N:N: did you forget to import the standard library Option?` |
| 2 | `Error: type error: N:N: lines 20 to 21 are read as one expression, so line 20 is called with line 21 as its argument:` |
| 2 | `Error: type error: N:N: namespace 'List' has no member 'contains?' -- 'wand d List' lists its members` |
| 2 | `Error: type error: N:N: namespace 'List' has no member 'enumerate' -- 'wand d List' lists its members` |
| 2 | `Error: type error: N:N: namespace 'List' has no member 'mapi' (did you mean 'map'?)` |
| 2 | `Error: type error: N:N: namespace 'Option' has no member 'unwrap_or' -- 'wand d Option' lists its members` |
| 2 | `Error: type error: N:N: namespace 'String' has no member 'drop' -- 'wand d String' lists its members` |
| 2 | `Regex.capture     : Regex -> String -> List String` |

## Failures

