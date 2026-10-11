#!/usr/bin/env bash
set -euo pipefail
if [ $# -lt 2 ]; then
  echo "usage: prog BASE OVERRIDE" >&2
  exit 2
fi
for f in "$1" "$2"; do
  if [ ! -f "$f" ]; then
    echo "no such file: $f" >&2
    exit 1
  fi
done
awk '
  function trim(s) { sub(/^[ \t]+/, "", s); sub(/[ \t]+$/, "", s); return s }
  trim($0) == "" || trim($0) ~ /^#/ { next }
  {
    eq = index($0, "=")
    if (eq == 0) next
    key = trim(substr($0, 1, eq - 1))
    if (key == "") next
    value[key] = substr($0, eq + 1)
  }
  END { for (k in value) print k "=" value[k] }
' "$1" "$2" | LC_ALL=C sort -t= -k1,1
