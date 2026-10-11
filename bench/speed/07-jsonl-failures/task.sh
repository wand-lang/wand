#!/usr/bin/env bash
# For each endpoint in events.jsonl: how many requests, how many failed,
# and the failures per thousand, rounded down.
set -euo pipefail
export LC_ALL=C
jq -r '[.endpoint, .ok] | @tsv' "$1/events.jsonl" \
  | awk -F '\t' '
      { total[$1]++; if ($2 == "false") failed[$1]++ }
      END { for (e in total) print e, total[e], failed[e] + 0, int(failed[e] * 1000 / total[e]) }' \
  | sort
