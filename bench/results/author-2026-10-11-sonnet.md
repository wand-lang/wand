# Authoring benchmark: 2026-10-11

Model `sonnet`, 3 runs per task and language, wand 0.104.1.

| language | runs | pass | turns (passing) | turns (all) | escaped | cost |
|---|---|---|---|---|---|---|
| wand | 45 | 97% | 9.7 | 9.8 | 1 | $5.35 |
| python | 45 | 100% | 3.6 | 3.6 | 0 | $2.08 |
| bash | 45 | 100% | 3.3 | 3.3 | 0 | $2.11 |

| task | wand | python | bash |
|---|---|---|---|
| 01-log-levels | 3/3, 8.3 turns | 3/3, 3.7 turns | 3/3, 3.0 turns |
| 02-rotate-backups | 3/3, 12.0 turns | 3/3, 3.7 turns | 3/3, 3.0 turns |
| 03-crashlooping-pods | 3/3, 13.7 turns | 3/3, 4.3 turns | 3/3, 3.0 turns |
| 04-csv-column-sum | 3/3, 9.0 turns | 3/3, 3.7 turns | 3/3, 3.3 turns |
| 05-env-file-merge | 3/3, 8.0 turns | 3/3, 3.7 turns | 3/3, 4.3 turns |
| 06-disk-threshold | 3/3, 9.3 turns | 3/3, 3.7 turns | 3/3, 3.3 turns |
| 07-commits-by-author | 3/3, 9.7 turns | 3/3, 3.3 turns | 3/3, 3.0 turns |
| 08-duplicate-files | 3/3, 9.3 turns | 3/3, 3.0 turns | 3/3, 3.3 turns |
| 09-version-bump | 3/3, 9.7 turns | 3/3, 3.7 turns | 3/3, 3.0 turns |
| 10-url-hosts | 3/3, 6.3 turns | 3/3, 3.3 turns | 3/3, 3.0 turns |
| 11-duration-total | 3/3, 9.3 turns | 3/3, 3.3 turns | 3/3, 3.3 turns |
| 12-safe-clean | 3/3, 8.3 turns | 3/3, 3.7 turns | 3/3, 4.0 turns |
| 13-retry-command | 3/3, 10.0 turns | 3/3, 4.3 turns | 3/3, 4.0 turns |
| 14-replace-in-files | 3/3, 13.7 turns | 3/3, 3.0 turns | 3/3, 3.0 turns |
| 15-latency-percentiles | 2/3, 10.0 turns | 3/3, 3.7 turns | 3/3, 3.0 turns |

## Failures

- 15-latency-percentiles / wand / run 2: wrong types are skipped: wanted exit 0 and /B count=1 p50=1 p95=1\n/a count=2 p50=0 p95=4\nskipped 10, got exit 0 and /B count=1 p50=1 p95=1\n/a count=3 p50=4 p95=12\nskipped 9 
