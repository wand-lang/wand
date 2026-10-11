#!/bin/sh
# Fails until it has run as many times as the file `need` says (1 if there
# is no such file). It keeps its count in `.attempts`.
n=$(( $(cat .attempts 2>/dev/null || echo 0) + 1 ))
echo "$n" > .attempts
need=$(cat need 2>/dev/null || echo 1)
if [ "$n" -ge "$need" ]; then
  echo "ok on try $n"
  exit 0
fi
echo "not ready (try $n of $need)" >&2
exit 3
