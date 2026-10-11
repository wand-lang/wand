#!/usr/bin/env bash
# How many groups of files under tree/ hold the same bytes, and how many
# files are in those groups.
set -euo pipefail
export LC_ALL=C
find "$1/tree" -type f -exec shasum -a 256 {} + \
  | awk '{ print $1 }' | sort | uniq -c \
  | awk '$1 > 1 { g++; f += $1 } END { print "groups", g + 0, "files", f + 0 }'
