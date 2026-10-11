#!/usr/bin/env bash
set -euo pipefail
if [ $# -lt 1 ] || ! [[ "$1" =~ ^[0-9]+$ ]] || [ "$((10#$1))" -gt 100 ]; then
  echo "usage: prog PERCENT, from 0 to 100" >&2
  exit 2
fi
awk -v limit="$((10#$1))" '
  NR == 1 { next }
  {
    line = $0
    sub(/^[ \t]+/, "", line)
    sub(/[ \t]+$/, "", line)
    if (line !~ /^[^ ]+ +[0-9]+ +[0-9]+ +[0-9]+ +[0-9]+% +[^ ]/) next
    mount = line
    sub(/^[^ ]+ +[0-9]+ +[0-9]+ +[0-9]+ +[0-9]+% +/, "", mount)
    capacity = $5
    sub(/%$/, "", capacity)
    if (capacity + 0 >= limit) print capacity "\t" mount
  }
' | LC_ALL=C sort -t "$(printf '\t')" -k1,1nr -k2,2 | awk -F '\t' '{ print $1 "% " $2 }'
