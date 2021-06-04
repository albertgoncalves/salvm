#!/usr/bin/env bash

set -euo pipefail

if [ ! -d "$WD/out" ]; then
    mkdir "$WD/out"
fi

asm="$1"
handle=$(echo "$asm" | sed 's/^.\+\/\(.\+\)\..\+$/\1/')
bytes="$WD/out/$handle.salb"

"$WD/bin/asm" "$asm" "$bytes"
"$WD/bin/vm" "$bytes"
