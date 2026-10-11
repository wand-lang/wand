#!/usr/bin/env bash
set -uo pipefail
if [ $# -ne 1 ] || ! [[ "$1" =~ ^0*[1-9][0-9]*$ ]]; then
  echo "usage: prog N, where N is 1 or more" >&2
  exit 2
fi
attempts=$((10#$1))
for ((k = 1; k <= attempts; k++)); do
  out=$(sh flaky.sh)
  code=$?
  if [ "$code" -eq 0 ]; then
    echo "attempt $k: ok"
    echo "output: $out"
    exit 0
  fi
  echo "attempt $k: exit $code"
done
echo "gave up after $attempts attempts"
exit 1
