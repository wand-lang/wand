#!/usr/bin/env bash
# The first five ERROR lines from the db component. The read stops there,
# so the rest of app.log is never read.
set -euo pipefail
export LC_ALL=C
awk '$2 == "ERROR" && $3 == "db:" { print; if (++n == 5) exit }' "$1/app.log"
