#!/usr/bin/env bash
set -euo pipefail
if [ $# -ne 3 ] || [ -z "$2" ]; then
  echo "usage: prog DIR OLD NEW, where OLD is not empty" >&2
  exit 2
fi
if [ ! -d "$1" ]; then
  echo "not a directory: $1" >&2
  exit 1
fi
cd "$1"
export OLD=$2 NEW=$3
files=0
total=0
# perl reads OLD and NEW from the environment, so neither is read as a pattern
# or as shell.
while IFS= read -r path; do
  n=$(perl -0777 -i -pe '$n = s/\Q$ENV{OLD}\E/$ENV{NEW}/g; END { print STDERR $n + 0 }' "$path" 2>&1 >/dev/null)
  if [ "$n" -gt 0 ]; then
    echo "changed ${path#./} ($n)"
    files=$((files + 1))
    total=$((total + n))
  fi
done < <(find . -type f -name '*.txt' | LC_ALL=C sort)
echo "changed $files files, $total replacements"
