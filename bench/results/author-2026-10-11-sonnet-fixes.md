# Authoring benchmark: 2026-10-11

Model `sonnet`, 3 runs per task and language, wand 0.104.1.

| language | runs | pass | turns (passing) | turns (all) | escaped | cost |
|---|---|---|---|---|---|---|
| wand | 45 | 95% | 9.5 | 9.5 | 2 | $5.37 |

| task | wand |
|---|---|
| 01-log-levels | 3/3, 8.3 turns |
| 02-rotate-backups | 2/3, 10.0 turns |
| 03-crashlooping-pods | 3/3, 12.0 turns |
| 04-csv-column-sum | 3/3, 10.0 turns |
| 05-env-file-merge | 3/3, 5.0 turns |
| 06-disk-threshold | 3/3, 10.0 turns |
| 07-commits-by-author | 3/3, 11.7 turns |
| 08-duplicate-files | 3/3, 12.7 turns |
| 09-version-bump | 3/3, 7.0 turns |
| 10-url-hosts | 3/3, 4.7 turns |
| 11-duration-total | 3/3, 11.3 turns |
| 12-safe-clean | 2/3, 8.3 turns |
| 13-retry-command | 3/3, 13.0 turns |
| 14-replace-in-files | 3/3, 9.3 turns |
| 15-latency-percentiles | 3/3, 8.7 turns |

## Where the turns go

Commands per run, by what each one was for. One tool call can hold several commands. The JSON lines beside this file hold every call.

| language | read the guide | look up a name | try an expression | typecheck | format | run the program | write or edit | other |
|---|---|---|---|---|---|---|---|---|
| wand | 0.4 | 7.7 | 0.8 | 1.2 | 0.6 | 3.0 | 3.2 | 4.6 |

## Errors the agent saw

Tool results that failed or reported an error or a warning, by their first such line, with line and column taken out. The most frequent come first.

### wand: 139 results

| times | first error line |
|---|---|
| 20 | `Error: parse error: N:N: expected with, got \|` |
| 14 | `Exit code 1` |
| 11 | `Error: type error: N:N: non-exhaustive match: no arm matches the empty list -- add an arm for it: \| [] -> ..., or \| _ -> ... for every list the arms leave out` |
| 4 | `Error: type error: N:N: did you forget to import the standard library Path?` |
| 4 | `sed: 1: "solution.wand` |
| 4 | `warning: N:N: V-USES2: this file performs FS.Read, IO, Proc and does not say so; it could declare "uses {FS.Read, IO, Proc}"` |
| 3 | `Error: parse error: N:N: expected ), got else` |
| 3 | `Error: type error: N:N: 'r' needs its type before '.stdout' can be read: write '(r: ShellResult)'` |
| 3 | `Error: type error: N:N: non-exhaustive match: no arm matches a list of two or more elements -- add an arm for it: \| [_ :: _ :: _] -> ..., or \| _ -> ... for ever` |
| 2 | `Error("expected an object, got \"{\\\"ms\\\": 3}\"") : Result String Int` |
| 2 | `Error: parse error: N:N: expected =, got ,` |
| 2 | `Error: parse error: N:N: unexpected token: else` |
| 2 | `Error: type error: N:N: constructor 'ShellResult' expects 3 argument(s), got 1` |
| 2 | `Error: type error: N:N: expected Unit, got (Int -> 'a ! {Proc}) -> 'b` |
| 2 | `Error: type error: N:N: lines 23 to 24 are read as one expression, so line 23 is called with line 24 as its argument:` |

## Failures

- 02-rotate-backups / wand / run 3: only real backups count: wanted exit 0 and deleted backup-2025-12-31.tar\ndeleted backup-2026-01-02.tar\ndeleted backup-2026-01-10.tar\nkept 1, got exit 0 and deleted backup-2025-12-31.tar\ndeleted backup-2026-01-02.tar\ndeleted backup-2026-01-10.tar\ndeleted backup-2026-05-05.tar\nkept 1 
- 12-safe-clean / wand / run 1: no cache: wanted exit 0 and nothing to clean\nkept 1, got exit 0 and nothing to clean 
