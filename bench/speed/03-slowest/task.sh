#!/usr/bin/env bash
# The twenty slowest requests in app.log: the lines with the highest ms=,
# and lines with the same time in byte order.
# No pipefail: head stops reading early, and the writer it leaves is not a failure.
set -eu
export LC_ALL=C
awk '{ split($NF, kv, "="); print kv[2] "\t" $0 }' "$1/app.log" \
  | sort -t "$(printf '\t')" -k1,1nr -k2 | head -20 | cut -f2-
