#!/usr/bin/env bash
set -euo pipefail
if [ $# -lt 1 ]; then
  echo "usage: prog DIR" >&2
  exit 2
fi
if [ ! -d "$1" ]; then
  echo "not a directory: $1" >&2
  exit 1
fi
cd "$1"
# One "hash<TAB>path" line per file, in path order, then each group of two or
# more in the order its first path appears.
find . -type f -exec shasum -a 256 {} + \
  | awk '{ hash = $1; path = substr($0, length($1) + 3); sub(/^\.\//, "", path); print hash "\t" path }' \
  | LC_ALL=C sort -t "$(printf '\t')" -k2 \
  | awk -F '\t' '
      { if (!($1 in n)) order[++groups] = $1; n[$1]++; paths[$1] = paths[$1] (n[$1] > 1 ? "\n" : "") $2 }
      END {
        first = 1
        for (i = 1; i <= groups; i++) {
          h = order[i]
          if (n[h] < 2) continue
          if (!first) print ""
          print paths[h]
          first = 0
        }
      }'
