# Authoring benchmark: 2026-10-11

Model `sonnet`, 3 runs per task and language, wand 0.104.1.

| language | runs | pass | turns (passing) | turns (all) | escaped | cost |
|---|---|---|---|---|---|---|
| wand | 45 | 93% | 9.8 | 10.0 | 3 | $5.55 |
| python | 45 | 100% | 3.6 | 3.6 | 0 | $2.08 |
| bash | 45 | 100% | 3.2 | 3.2 | 0 | $2.12 |

| task | wand | python | bash |
|---|---|---|---|
| 01-log-levels | 3/3, 9.7 turns | 3/3, 3.7 turns | 3/3, 3.0 turns |
| 02-rotate-backups | 3/3, 11.7 turns | 3/3, 3.3 turns | 3/3, 3.3 turns |
| 03-crashlooping-pods | 3/3, 16.7 turns | 3/3, 4.3 turns | 3/3, 3.0 turns |
| 04-csv-column-sum | 3/3, 9.3 turns | 3/3, 3.3 turns | 3/3, 3.3 turns |
| 05-env-file-merge | 3/3, 8.3 turns | 3/3, 4.0 turns | 3/3, 3.3 turns |
| 06-disk-threshold | 3/3, 8.3 turns | 3/3, 3.7 turns | 3/3, 3.3 turns |
| 07-commits-by-author | 3/3, 10.3 turns | 3/3, 4.0 turns | 3/3, 3.3 turns |
| 08-duplicate-files | 2/3, 9.3 turns | 3/3, 3.7 turns | 3/3, 3.0 turns |
| 09-version-bump | 3/3, 11.7 turns | 3/3, 3.3 turns | 3/3, 3.0 turns |
| 10-url-hosts | 2/3, 10.0 turns | 3/3, 3.0 turns | 3/3, 3.0 turns |
| 11-duration-total | 3/3, 9.3 turns | 3/3, 3.0 turns | 3/3, 3.3 turns |
| 12-safe-clean | 3/3, 6.7 turns | 3/3, 3.3 turns | 3/3, 3.0 turns |
| 13-retry-command | 3/3, 8.7 turns | 3/3, 4.3 turns | 3/3, 4.0 turns |
| 14-replace-in-files | 3/3, 11.0 turns | 3/3, 3.7 turns | 3/3, 3.3 turns |
| 15-latency-percentiles | 2/3, 9.3 turns | 3/3, 3.7 turns | 3/3, 3.3 turns |

## Where the turns go

Commands per run, by what each one was for. One tool call can hold several commands. The JSON lines beside this file hold every call.

| language | read the guide | look up a name | try an expression | typecheck | format | run the program | write or edit | other |
|---|---|---|---|---|---|---|---|---|
| wand | 0.4 | 8.7 | 0.8 | 1.2 | 0.4 | 2.3 | 3.2 | 4.7 |
| python | 0.5 | 0.0 | 0.0 | 0.0 | 0.0 | 0.1 | 0.9 | 1.8 |
| bash | 0.0 | 0.0 | 0.0 | 0.0 | 0.0 | 0.1 | 0.9 | 2.2 |

## Errors the agent saw

Tool results that failed or reported an error or a warning, by their first such line, with line and column taken out. The most frequent come first.

### wand: 176 results

| times | first error line |
|---|---|
| 24 | `Error: type error: N:N: unknown constructor 'Err'` |
| 13 | `Error: parse error: N:N: the ';' above ended the definition, so this line is a statement of its own rather than part of it -- put the body in parentheses to seq` |
| 12 | `Args.help? : List String -> Bool` |
| 10 | `Error: type error: N:N: namespace 'List' has no member 'iter' -- use List.each` |
| 10 | `Error: type error: N:N: non-exhaustive match: missing case, e.g. []` |
| 9 | `Result.error?    : Result 'b 'a -> Bool` |
| 8 | `Error: type error: N:N: namespace 'IO' has no member 'eprintln' (did you mean 'println'?)` |
| 7 | `FS.append              : Path -> String -> Result String Unit ! {FS.Write}` |
| 6 | `Error: parse error: N:N: expected ], got ::` |
| 6 | `Exit code 1` |
| 3 | `CLAUDE.md` |
| 3 | `Error: parse error: N:N: expected with, got \|` |
| 3 | `Error: type error: N:N: namespace 'List' has no member 'nth!' -- 'wand d List' lists its members` |
| 2 | `# Bump the version` |
| 2 | `# Count log lines by level` |

### python: 19 results

| times | first error line |
|---|---|
| 6 | `total 8` |
| 3 | `ERROR 1` |
| 2 | `1	# Replace text in files` |
| 2 | `removed .h` |
| 1 | `# Sum a CSV column` |
| 1 | `1	# Bump the version` |
| 1 | `1	# Count log lines by level` |
| 1 | `1	# Sum a CSV column` |
| 1 | `2 Ana Silva` |
| 1 | `DSN=p://x?a=b` |

### bash: 22 results

| times | first error line |
|---|---|
| 7 | `total 8` |
| 3 | `ERROR 1` |
| 2 | `# Bump the version` |
| 2 | `# Count log lines by level` |
| 1 | `# Sum a CSV column` |
| 1 | `A=3` |
| 1 | `B=` |
| 1 | `mkdir: /tmp/t/x/config: File exists` |
| 1 | `no column: amount` |
| 1 | `prod/api-1 7` |
| 1 | `removed -x` |
| 1 | `removed .h` |

## Failures

- 08-duplicate-files / wand / run 2: example: wanted exit 0 and a.txt\nlogs/b.txt\n\nc.txt\nd.txt, got exit 0 and d/a.txt\nd/logs/b.txt\n\nd/c.txt\nd/d.txt 
- 10-url-hosts / wand / run 2: ports, case, fragments and invalid lines: wanted exit 0 and 2 a.example\n2 b.example\n1 10.0.0.1\n1 c.example\n6 invalid, got exit 0 and 2 a.example\n2 b.example\n1 10.0.0.1\n1 c.example\n1 nohost\n5 invalid 
- 15-latency-percentiles / wand / run 1: wrong types are skipped: wanted exit 0 and /B count=1 p50=1 p95=1\n/a count=2 p50=0 p95=4\nskipped 10, got exit 0 and /B count=1 p50=1 p95=1\n/a count=3 p50=4 p95=12\nskipped 9 
