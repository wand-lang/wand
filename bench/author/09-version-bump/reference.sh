#!/usr/bin/env bash
set -euo pipefail
case "${1:-}" in
  major | minor | patch) part=$1 ;;
  *)
    echo "usage: prog major|minor|patch" >&2
    exit 2
    ;;
esac
if [ $# -ne 1 ]; then
  echo "usage: prog major|minor|patch" >&2
  exit 2
fi
if [ ! -f VERSION ]; then
  echo "no VERSION file" >&2
  exit 1
fi
text=$(tr -d ' \t\n\r' < VERSION)
n='(0|[1-9][0-9]*)'
if ! [[ "$text" =~ ^$n\.$n\.$n$ ]]; then
  echo "not a version: '$text'" >&2
  exit 1
fi
major=${BASH_REMATCH[1]} minor=${BASH_REMATCH[2]} patch=${BASH_REMATCH[3]}
case $part in
  major) major=$((major + 1)) minor=0 patch=0 ;;
  minor) minor=$((minor + 1)) patch=0 ;;
  patch) patch=$((patch + 1)) ;;
esac
new="$major.$minor.$patch"
echo "$new" > VERSION
echo "$new"
