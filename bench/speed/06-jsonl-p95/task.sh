#!/usr/bin/env bash
# For each endpoint in events.jsonl: how many requests, and the 95th
# percentile of their ms, by nearest rank.
set -euo pipefail
export LC_ALL=C
jq -r '[.endpoint, .ms] | @tsv' "$1/events.jsonl" \
  | sort -t "$(printf '\t')" -k1,1 -k2,2n \
  | awk -F '\t' '
      function flush() { if (n) print ep, n, xs[int((95 * n + 99) / 100)] }
      $1 != ep { flush(); ep = $1; n = 0 }
      { xs[++n] = $2 }
      END { flush() }'
