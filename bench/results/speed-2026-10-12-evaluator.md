# Speed benchmark: 2026-10-12, after the evaluator work

wand 0.106.0 and #162–#166 (main at 2a6ed21), 200000 lines per input, the minimum of 10 rounds in a shuffled order, in milliseconds.

| task | wand | python | bash | wand / python | wand / bash |
|---|---|---|---|---|---|
| 01-log-levels | 147 | 217 | 572 | 0.68 | 0.26 |
| 02-top-ips | 413 | 417 | 398 | 0.99 | 1.04 |
| 03-slowest | 346 | 199 | 1194 | 1.74 | 0.29 |
| 04-errors-per-hour | 128 | 187 | 571 | 0.68 | 0.22 |
| 05-regex-sum | 128 | 238 | 371 | 0.54 | 0.35 |
| 06-jsonl-p95 | 601 | 608 | 1294 | 0.99 | 0.46 |
| 07-jsonl-failures | 529 | 602 | 641 | 0.88 | 0.83 |
| 08-csv-group | 685 | 593 | 374 | 1.16 | 1.83 |
| 09-distinct | 727 | 611 | 1497 | 1.19 | 0.49 |
| 10-sort-top | 935 | 741 | 586 | 1.26 | 1.60 |
| 11-tree-ext | 139 | 126 | 261 | 1.10 | 0.53 |
| 12-tree-dupes | 742 | 466 | 533 | 1.59 | 1.39 |
| 13-hello | 15 | 45 | 8 | 0.33 | 1.88 |
| 14-fanout-wc | 1441 | 2410 | 1461 | 0.60 | 0.99 |
| 15-early-stop | 21 | 45 | 12 | 0.47 | 1.75 |
