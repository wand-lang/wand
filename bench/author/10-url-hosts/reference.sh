#!/usr/bin/env bash
set -euo pipefail
awk '
  {
    line = $0
    sub(/^[ \t]+/, "", line)
    sub(/[ \t]+$/, "", line)
    if (line == "") next
    host = ""
    if (line ~ /^https?:\/\//) {
      host = substr(line, index(line, "://") + 3)
      sub(/[\/?#].*/, "", host)
      sub(/:.*/, "", host)
      host = tolower(host)
    }
    if (host == "") invalid++
    else n[host]++
  }
  END {
    for (h in n) print n[h] "\t" h | "LC_ALL=C sort -t \"\t\" -k1,1nr -k2,2 | tr \"\t\" \" \""
    close("LC_ALL=C sort -t \"\t\" -k1,1nr -k2,2 | tr \"\t\" \" \"")
    if (invalid) print invalid " invalid"
  }
'
