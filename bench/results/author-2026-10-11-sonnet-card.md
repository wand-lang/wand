# Authoring benchmark: 2026-10-11

Model `sonnet`, 3 runs per task and language, wand 0.104.1.

| language | runs | pass | turns (passing) | turns (all) | escaped | cost |
|---|---|---|---|---|---|---|
| wand | 45 | 91% | 8.0 | 8.2 | 4 | $4.88 |

| task | wand |
|---|---|
| 01-log-levels | 3/3, 8.0 turns |
| 02-rotate-backups | 3/3, 8.0 turns |
| 03-crashlooping-pods | 3/3, 10.7 turns |
| 04-csv-column-sum | 3/3, 7.7 turns |
| 05-env-file-merge | 3/3, 6.0 turns |
| 06-disk-threshold | 3/3, 7.7 turns |
| 07-commits-by-author | 3/3, 8.7 turns |
| 08-duplicate-files | 2/3, 10.0 turns |
| 09-version-bump | 3/3, 7.0 turns |
| 10-url-hosts | 3/3, 6.7 turns |
| 11-duration-total | 3/3, 9.3 turns |
| 12-safe-clean | 3/3, 7.7 turns |
| 13-retry-command | 3/3, 8.0 turns |
| 14-replace-in-files | 2/3, 8.0 turns |
| 15-latency-percentiles | 1/3, 10.0 turns |

## Where the turns go

Commands per run, by what each one was for. One tool call can hold several commands. The JSON lines beside this file hold every call.

| language | read the guide | look up a name | try an expression | typecheck | format | run the program | write or edit | other |
|---|---|---|---|---|---|---|---|---|
| wand | 0.5 | 7.9 | 0.8 | 0.5 | 0.1 | 1.4 | 2.2 | 3.5 |

## Errors the agent saw

Tool results that failed or reported an error or a warning, by their first such line, with line and column taken out. The most frequent come first.

### wand: 95 results

| times | first error line |
|---|---|
| 11 | `Error: parse error: N:N: the ';' above ended the definition, so this line is a statement of its own rather than part of it -- put the body in parentheses to seq` |
| 8 | `Error: type error: N:N: non-exhaustive match: missing case, e.g. []` |
| 8 | `Exit code 1` |
| 4 | `Error: type error: N:N: namespace 'List' has no member 'flat_map' -- 'wand d List' lists its members` |
| 3 | `Args.help? : List String -> Bool` |
| 3 | `Error: parse error: N:N: unexpected token in pattern: result` |
| 2 | `Error: parse error: N:N: expected ), got \|` |
| 2 | `Error: type error: N:N: did you forget to import the standard library Option?` |
| 2 | `Error: type error: N:N: did you forget to import the standard library Path?` |
| 2 | `Error: type error: N:N: non-exhaustive match: missing case, e.g. [_ :: [_ :: _]]` |
| 2 | `Error: type error: N:N: strings concatenate with '++', not '+'` |
| 2 | `List.fold_left  : ('a -> 'b -> 'a ! 'e) -> 'a -> List 'b -> 'a ! 'e` |
| 2 | `warning: N:N: V-BANG2: 'fail!' cannot raise, so the '!' promises a risk that is not there; it is 'fail'` |
| 1 | `2 example.com` |
| 1 | `Dangerous rm operation detected: '/private/var/folders/kx/dq9cxjh14xsdsrdfwfv9ly340000gn/T/bench_author_RWow64/*'` |

## Failures

- 08-duplicate-files / wand / run 1: example: wanted exit 0 and a.txt\nlogs/b.txt\n\nc.txt\nd.txt, got exit 0 and d/a.txt\nd/logs/b.txt\n\nd/c.txt\nd/d.txt 
- 14-replace-in-files / wand / run 2: empty new: wanted exit 0 and changed deep/er/x.txt (1)\nchanged log.txt (2)\nchanged 2 files, 3 replacements, got exit 2 and  usage: prog DIR OLD NEW\n
- 15-latency-percentiles / wand / run 1: wrong types are skipped: wanted exit 0 and /B count=1 p50=1 p95=1\n/a count=2 p50=0 p95=4\nskipped 10, got exit 0 and /B count=1 p50=1 p95=1\n/a count=3 p50=4 p95=12\nskipped 9 
- 15-latency-percentiles / wand / run 3: wrong types are skipped: wanted exit 0 and /B count=1 p50=1 p95=1\n/a count=2 p50=0 p95=4\nskipped 10, got exit 0 and /B count=1 p50=1 p95=1\n/a count=3 p50=4 p95=12\nskipped 9 
