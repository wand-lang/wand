#!/usr/bin/env bash
# Run wc -l once for each file under tree/d0/, eight at a time, and add up
# the lines: the cost of starting many small commands.
set -euo pipefail
export LC_ALL=C
find "$1/tree/d0" -type f -print0 \
  | xargs -0 -P 8 -n 1 sh -c 'wc -l < "$0"' \
  | awk '{ n++; s += $1 } END { print "files", n + 0, "lines", s + 0 }'
