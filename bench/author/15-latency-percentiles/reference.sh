#!/usr/bin/env bash
set -euo pipefail
tab=$(printf '\t')
# One "endpoint<TAB>ms" line per valid record, and "SKIP" for each other line.
jq -R -r '
  if test("^\\s*$") then empty
  else
    (try fromjson catch null) as $r
    | if ($r | type) == "object"
        and ($r.endpoint | type) == "string"
        and ($r.ms | type) == "number" and $r.ms == ($r.ms | floor) and $r.ms >= 0
      then "\($r.endpoint)\t\($r.ms)"
      else "SKIP"
      end
  end' \
  | LC_ALL=C sort -t "$tab" -k1,1 -k2,2n \
  | awk -F "$tab" '
      function rank(p, n) { r = int((p * n + 99) / 100); return r < 1 ? 1 : r }
      function flush() {
        if (n) print name " count=" n " p50=" v[rank(50, n)] " p95=" v[rank(95, n)]
        n = 0
      }
      $0 == "SKIP" { skipped++; next }
      $1 != name { flush(); name = $1 }
      { v[++n] = $2 }
      END { flush(); if (skipped) print "skipped " skipped }'
