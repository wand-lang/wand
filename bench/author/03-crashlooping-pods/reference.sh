#!/usr/bin/env bash
set -euo pipefail
if [ $# -lt 2 ] || ! [[ "$2" =~ ^[0-9]+$ ]]; then
  echo "usage: prog FILE THRESHOLD" >&2
  exit 2
fi
if ! rows=$(jq -r --argjson t "$((10#$2))" '
    .items[]
    | { name: "\(.metadata.namespace)/\(.metadata.name)",
        restarts: ([.status.containerStatuses[]?.restartCount] | add // 0) }
    | select(.restarts > $t)
    | "\(.restarts) \(.name)"' "$1"); then
  echo "cannot read pods from $1" >&2
  exit 1
fi
[ -z "$rows" ] && exit 0
printf '%s\n' "$rows" | LC_ALL=C sort -k1,1nr -k2,2 | awk '{ print $2, $1 }'
