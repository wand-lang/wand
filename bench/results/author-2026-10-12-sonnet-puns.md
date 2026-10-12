# Authoring benchmark: 2026-10-12

Model `sonnet`, 3 runs per task and language, wand 0.104.1.

| language | runs | pass | turns (passing) | turns (all) | escaped | cost |
|---|---|---|---|---|---|---|
| wand | 45 | 100% | 6.0 | 6.0 | 0 | $4.13 |

| task | wand |
|---|---|
| 01-log-levels | 3/3, 4.7 turns |
| 02-rotate-backups | 3/3, 7.7 turns |
| 03-crashlooping-pods | 3/3, 6.7 turns |
| 04-csv-column-sum | 3/3, 4.3 turns |
| 05-env-file-merge | 3/3, 6.7 turns |
| 06-disk-threshold | 3/3, 4.0 turns |
| 07-commits-by-author | 3/3, 5.3 turns |
| 08-duplicate-files | 3/3, 6.7 turns |
| 09-version-bump | 3/3, 5.0 turns |
| 10-url-hosts | 3/3, 6.3 turns |
| 11-duration-total | 3/3, 5.3 turns |
| 12-safe-clean | 3/3, 7.0 turns |
| 13-retry-command | 3/3, 5.7 turns |
| 14-replace-in-files | 3/3, 6.7 turns |
| 15-latency-percentiles | 3/3, 8.0 turns |

## Where the turns go

Commands per run, by what each one was for. One tool call can hold several commands. The JSON lines beside this file hold every call.

| language | read the guide | look up a name | try an expression | typecheck | format | run the program | write or edit | other |
|---|---|---|---|---|---|---|---|---|
| wand | 0.7 | 5.4 | 0.2 | 0.5 | 0.1 | 0.8 | 1.2 | 2.8 |

## Errors the agent saw

Tool results that failed or reported an error or a warning, by their first such line, with line and column taken out. The most frequent come first.

### wand: 44 results

| times | first error line |
|---|---|
| 4 | `Regex.capture : Regex -> String -> List String` |
| 3 | `Exit code 1` |
| 2 | `Decode.run : Decoder 'a -> String -> Result String 'a` |
| 2 | `Error: type error: N:N: did you forget to import the standard library Path?` |
| 2 | `Error: type error: N:N: expected Int, got Size` |
| 2 | `V-USES2: 1 — inserted "uses {FS.Read, IO, Proc}"` |
| 2 | `warning: N:N: V-BANG1: 'main' can raise, but its name does not say so; call it 'main!' and give the plain name to a version that returns a Result` |
| 2 | `warning: N:N: V-USES2: this file performs IO and does not say so; it could declare "uses {IO}"` |
| 1 | `#!/bin/sh` |
| 1 | `Dangerous rm operation detected: '/private/var/folders/kx/dq9cxjh14xsdsrdfwfv9ly340000gn/T/bench_author_nZh4Vc/t'` |
| 1 | `E-TYPE: 3 — inserted "import List"` |
| 1 | `E-TYPE: 3 — inserted "import Option"` |
| 1 | `E-TYPE: 3 — inserted "import Path"` |
| 1 | `Error: parse error: N:N: the ';' above ended the definition, so this line is a statement of its own rather than part of it -- put the body in parentheses to seq` |
| 1 | `Error: type error: N:N: '++' joins two strings; two lists are joined with 'List.concat xs ys'` |

## Failures

