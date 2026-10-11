#!/usr/bin/env bash
# How many lines of app.log carry each level, most first.
set -euo pipefail
awk 'NF >= 2 { n[$2]++ } END { for (l in n) print l, n[l] }' "$1/app.log" | sort -k2,2nr -k1,1
