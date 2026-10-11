#!/usr/bin/env bash
set -uo pipefail
if [ $# -lt 2 ]; then
  echo "usage: prog FILE COLUMN" >&2
  exit 2
fi
if [ ! -f "$1" ]; then
  echo "no such file: $1" >&2
  exit 1
fi
COLUMN=$2 awk -F, '
  $0 == "" { next }
  !header {
    header = 1
    for (i = 1; i <= NF; i++) if ($i == ENVIRON["COLUMN"]) col = i
    if (!col) { print "no column: " ENVIRON["COLUMN"] > "/dev/stderr"; failed = 2; exit }
    next
  }
  $col == "" { next }
  $col !~ /^-?[0-9]+$/ {
    print "line " NR ": not a whole number: " $col > "/dev/stderr"; failed = 1; exit
  }
  { total += $col }
  END {
    if (failed) exit failed
    if (!header) { print "no column: " ENVIRON["COLUMN"] > "/dev/stderr"; exit 2 }
    printf "%d\n", total
  }
' "$1"
