#!/usr/bin/env bash
set -euo pipefail
if [ $# -lt 2 ] || ! [[ "$2" =~ ^[0-9]+$ ]]; then
  echo "usage: prog DIR N, where N is a whole number" >&2
  exit 2
fi
dir=$1
keep=$((10#$2))
if [ ! -d "$dir" ]; then
  echo "not a directory: $dir" >&2
  exit 1
fi
backups=()
for path in "$dir"/backup-*.tar; do
  name=${path##*/}
  if [ -f "$path" ] && [[ "$name" =~ ^backup-[0-9]{4}-[0-9]{2}-[0-9]{2}\.tar$ ]]; then
    backups+=("$name")
  fi
done
total=${#backups[@]}
drop=$((total > keep ? total - keep : 0))
# The glob expands in sorted order, so the oldest come first.
i=0
while [ "$i" -lt "$drop" ]; do
  rm -- "$dir/${backups[$i]}"
  echo "deleted ${backups[$i]}"
  i=$((i + 1))
done
echo "kept $((total - drop))"
