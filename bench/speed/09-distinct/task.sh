#!/usr/bin/env bash
# How many distinct addresses, distinct address and component pairs, and
# distinct lines app.log holds.
set -euo pipefail
export LC_ALL=C
log="$1/app.log"
echo "ips $(grep -o 'ip=[0-9.]*' "$log" | sort -u | wc -l | tr -d ' ')"
echo "pairs $(awk '{ for (i = 1; i <= NF; i++) if ($i ~ /^ip=/) { print $i, $3; break } }' "$log" | sort -u | wc -l | tr -d ' ')"
echo "lines $(sort -u "$log" | wc -l | tr -d ' ')"
