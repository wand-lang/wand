#!/usr/bin/env bash
# The hundred largest sales in sales.csv, largest first, and sales of the
# same amount by id.
# No pipefail: head stops reading early, and the writer it leaves is not a failure.
set -eu
export LC_ALL=C
tail -n +2 "$1/sales.csv" | sort -t, -k4,4nr -k1,1n | head -100 | awk -F, '{ print $1, $4 }'
