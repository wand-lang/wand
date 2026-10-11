#!/usr/bin/env bash
set -euo pipefail
if [ $# -gt 1 ] || { [ $# -eq 1 ] && ! [[ "$1" =~ ^0*[1-9][0-9]*$ ]]; }; then
  echo "usage: prog [N], where N is 1 or more" >&2
  exit 2
fi
top=${1:-0}
awk -F'|' 'index($0, "|") { n[$1]++ } END { for (a in n) print n[a] "\t" a }' \
  | LC_ALL=C sort -t "$(printf '\t')" -k1,1nr -k2,2 \
  | awk -F '\t' -v top="$((10#$top))" 'top == 0 || NR <= top { print $1 " " $2 }'
