# Authoring benchmark: 2026-10-11

Model `sonnet`, 3 runs per task and language, wand 0.104.1.

| language | runs | pass | turns (passing) | turns (all) | escaped | cost |
|---|---|---|---|---|---|---|
| wand | 45 | 97% | 7.9 | 7.8 | 1 | $4.84 |

| task | wand |
|---|---|
| 01-log-levels | 3/3, 4.7 turns |
| 02-rotate-backups | 2/3, 8.3 turns |
| 03-crashlooping-pods | 3/3, 11.0 turns |
| 04-csv-column-sum | 3/3, 8.3 turns |
| 05-env-file-merge | 3/3, 6.3 turns |
| 06-disk-threshold | 3/3, 8.7 turns |
| 07-commits-by-author | 3/3, 6.3 turns |
| 08-duplicate-files | 3/3, 10.7 turns |
| 09-version-bump | 3/3, 5.7 turns |
| 10-url-hosts | 3/3, 5.0 turns |
| 11-duration-total | 3/3, 7.3 turns |
| 12-safe-clean | 3/3, 6.3 turns |
| 13-retry-command | 3/3, 11.3 turns |
| 14-replace-in-files | 3/3, 11.3 turns |
| 15-latency-percentiles | 3/3, 6.3 turns |

## Where the turns go

Commands per run, by what each one was for. One tool call can hold several commands. The JSON lines beside this file hold every call.

| language | read the guide | look up a name | try an expression | typecheck | format | run the program | write or edit | other |
|---|---|---|---|---|---|---|---|---|
| wand | 0.4 | 7.3 | 0.8 | 0.6 | 0.1 | 2.0 | 2.2 | 5.6 |

## Errors the agent saw

Tool results that failed or reported an error or a warning, by their first such line, with line and column taken out. The most frequent come first.

### wand: 83 results

| times | first error line |
|---|---|
| 4 | `Error: parse error: N:N: unexpected token: else` |
| 4 | `Exit code 1` |
| 3 | `Error: type error: N:N: did you forget to import the standard library Proc?` |
| 3 | `Error: type error: N:N: expected Unit, got (Int -> 'a ! {Proc}) -> 'b` |
| 2 | `Args.help? : List String -> Bool` |
| 2 | `Error: type error: N:N: 'r' needs its type before '.stdout' can be read: write '(r: ShellResult)'` |
| 2 | `Error: type error: N:N: did you forget to import the standard library Option?` |
| 2 | `Error: type error: N:N: expected Int, got Unit` |
| 2 | `Error: type error: N:N: lines 16 to 17 are read as one expression, so line 16 is called with line 17 as its argument:` |
| 2 | `Error: type error: N:N: lines 29 to 30 are read as one expression, so line 29 is called with line 30 as its argument:` |
| 2 | `Error: type error: N:N: namespace 'List' has no member 'contains' (did you mean 'contains?'?)` |
| 2 | `Error: type error: N:N: namespace 'Option' has no member 'unwrap_or' -- use Option.default` |
| 2 | `Error: type error: N:N: namespace 'String' has no member 'all?' -- 'wand d String' lists its members` |
| 2 | `warning: N:N: V-BANG1: 'main' can raise, but its name does not say so; call it 'main!' and give the plain name to a version that returns a Result` |
| 2 | `warning: N:N: V-BANG1: 'num' can raise, but its name does not say so; call it 'num!' and give the plain name to a version that returns a Result` |

## Failures

- 02-rotate-backups / wand / run 2: only real backups count: wanted exit 0 and deleted backup-2025-12-31.tar\ndeleted backup-2026-01-02.tar\ndeleted backup-2026-01-10.tar\nkept 1, got exit 0 and deleted backup-2025-12-31.tar\ndeleted backup-2026-01-02.tar\ndeleted backup-2026-01-10.tar\ndeleted backup-2026-05-05.tar\nkept 1 
