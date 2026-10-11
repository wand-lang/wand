#!/usr/bin/env bash
set -euo pipefail
if [ $# -lt 1 ] || [ -z "$1" ] || [ ! -d "$1" ]; then
  echo "usage: prog TARGET, where TARGET is a directory" >&2
  exit 2
fi
target=$1
cache="$target/cache"
if [ -d "$cache" ] && [ ! -L "$cache" ]; then
  find "$cache" -mindepth 1 -maxdepth 1 -print | sed 's|.*/||' | LC_ALL=C sort | while IFS= read -r name; do
    rm -rf -- "${cache:?}/$name"
    echo "removed $name"
  done
else
  echo "nothing to clean"
fi
echo "kept $(find "$target" -mindepth 1 -maxdepth 1 | wc -l | tr -d ' ')"
