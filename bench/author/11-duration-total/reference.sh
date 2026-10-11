#!/usr/bin/env bash
set -euo pipefail
awk '
  function show(t,    text) {
    text = ""
    if (int(t / 3600)) text = text int(t / 3600) "h"
    if (int(t / 60) % 60) text = text (int(t / 60) % 60) "m"
    if (t % 60) text = text (t % 60) "s"
    return text == "" ? "0s" : text
  }
  NF == 0 { next }
  NF != 2 || $2 !~ /^([0-9]+h)?([0-9]+m)?([0-9]+s)?$/ {
    print "line " NR ": expected NAME DURATION, got " $0 > "/dev/stderr"
    failed = 1
    exit 1
  }
  {
    d = $2
    seconds = 0
    if (match(d, /^[0-9]+h/)) { seconds += substr(d, 1, RLENGTH - 1) * 3600; d = substr(d, RLENGTH + 1) }
    if (match(d, /^[0-9]+m/)) { seconds += substr(d, 1, RLENGTH - 1) * 60; d = substr(d, RLENGTH + 1) }
    if (match(d, /^[0-9]+s/)) { seconds += substr(d, 1, RLENGTH - 1) }
    total[$1] += seconds
    all += seconds
  }
  END {
    if (failed) exit 1
    for (name in total) print name " " show(total[name]) | "LC_ALL=C sort -k1,1"
    close("LC_ALL=C sort -k1,1")
    print "total " show(all)
  }
'
