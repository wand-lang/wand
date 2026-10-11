#!/usr/bin/env bash
# How many ERROR lines each component logged in each hour, by component
# and then by hour.
set -euo pipefail
export LC_ALL=C
awk '$2 == "ERROR" { n[substr($3, 1, length($3) - 1) " " substr($1, 12, 2)]++ }
     END { for (k in n) print k, n[k] }' "$1/app.log" | sort
