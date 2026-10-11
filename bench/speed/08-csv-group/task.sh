#!/usr/bin/env bash
# For each region and product in sales.csv: how many sales, their total
# amount, and their mean amount rounded down.
set -euo pipefail
export LC_ALL=C
awk -F, 'NR > 1 { k = $2 " " $3; n[k]++; t[k] += $4 }
         END { for (k in n) print k, n[k], t[k], int(t[k] / n[k]) }' "$1/sales.csv" | sort
