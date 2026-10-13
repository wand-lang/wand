# Serve benchmark: 2026-10-13

wand 0.107.0, 5000 requests in each run of `ab`, one connection for each request, the best of 5 rounds in a shuffled order.

Requests per second:

| task | connections | wand | python threading | python asyncio | wand / threading | wand / asyncio |
|---|---|---|---|---|---|---|
| 01-health | 1 | 3016 | 2776 | 2497 | 1.09 | 1.21 |
| 01-health | 16 | 4965 | 3242 | 6381 | 1.53 | 0.78 |
| 01-health | 64 | 5048 | 3262 | 6758 | 1.55 | 0.75 |
| 02-sum | 1 | 2815 | 2397 | 2289 | 1.17 | 1.23 |
| 02-sum | 16 | 4717 | 2779 | 5204 | 1.70 | 0.91 |
| 02-sum | 64 | 4652 | 2803 | 5683 | 1.66 | 0.82 |
| 03-lines | 1 | 1099 | 1676 | 1556 | 0.66 | 0.71 |
| 03-lines | 16 | 2945 | 1898 | 2659 | 1.55 | 1.11 |
| 03-lines | 64 | 2644 | 1902 | 2753 | 1.39 | 0.96 |

Latency of the best round, p50 / p99 in milliseconds, and the failed requests of all rounds (wand / threading / asyncio). A stopped run is one that `ab` did not finish.

| task | connections | wand | python threading | python asyncio | failed |
|---|---|---|---|---|---|
| 01-health | 1 | 0 / 1 | 0 / 1 | 0 / 1 | 0 / 0 / 0 |
| 01-health | 16 | 3 / 5 | 2 / 32 | 2 / 3 | 0 / 0 / 0 |
| 01-health | 64 | 13 / 17 | 2 / 62 | 9 / 12 | 0 / 0 / 0 |
| 02-sum | 1 | 0 / 1 | 0 / 1 | 0 / 1 | 0 / 0 / 0 |
| 02-sum | 16 | 3 / 5 | 2 / 32 | 3 / 4 | 0 / 0 / 0 |
| 02-sum | 64 | 13 / 30 | 2 / 33 | 11 / 14 | 0 / 0 / 0 |
| 03-lines | 1 | 1 / 2 | 1 / 1 | 1 / 1 | 0 / 0 / 0 |
| 03-lines | 16 | 5 / 9 | 3 / 64 | 6 / 9 | 0 / 0 / 0 |
| 03-lines | 64 | 24 / 43 | 3 / 123 | 23 / 34 | 0 / 0 / 0 |
