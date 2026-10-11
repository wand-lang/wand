#!/usr/bin/env bash
# How many lines report a refused connection or a lock timeout, and the
# sum of their ms=.
set -euo pipefail
export LC_ALL=C
grep -E '(connection refused|timeout waiting for lock).*ms=[0-9]+$' "$1/app.log" \
  | sed -E 's/.*ms=([0-9]+)$/\1/' \
  | awk '{ n++; s += $1 } END { print n + 0, s + 0 }'
