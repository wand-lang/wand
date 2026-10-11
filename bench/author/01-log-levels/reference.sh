#!/usr/bin/env bash
set -euo pipefail
if [ $# -lt 1 ]; then
  echo "usage: prog LOGFILE" >&2
  exit 2
fi
if [ ! -f "$1" ]; then
  echo "no such file: $1" >&2
  exit 1
fi
awk '
  BEGIN { rank["ERROR"] = 1; rank["WARN"] = 2; rank["INFO"] = 3; rank["DEBUG"] = 4 }
  ($2 in rank) { n[$2]++ }
  END { for (l in n) print n[l], rank[l], l }
' "$1" | sort -k1,1nr -k2,2n | awk '{ print $3, $1 }'
