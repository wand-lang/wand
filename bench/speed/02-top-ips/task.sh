#!/usr/bin/env bash
# The ten addresses that appear most often in app.log, most first.
# No pipefail: head stops reading early, and the writer it leaves is not a failure.
set -eu
export LC_ALL=C
grep -o 'ip=[0-9.]*' "$1/app.log" | cut -c4- | sort | uniq -c \
  | sort -k1,1nr -k2,2 | head -10 | awk '{ print $2, $1 }'
