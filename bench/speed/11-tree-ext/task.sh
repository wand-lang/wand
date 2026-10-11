#!/usr/bin/env bash
# For each file extension under tree/: how many files, and their total
# size in bytes.
set -euo pipefail
export LC_ALL=C
find "$1/tree" -type f -exec wc -c {} + \
  | awk '$2 != "total" { n = split($2, part, "."); ext = "." part[n]; c[ext]++; b[ext] += $1 }
         END { for (e in c) print e, c[e], b[e] }' \
  | sort
