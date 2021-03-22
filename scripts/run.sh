#!/usr/bin/env bash

set -euo pipefail

asm="$1"
handle=$(echo "$asm" | sed 's/^.\+\/\(.\+\)\..\+$/\1/')
bytes="$WD/out/$handle.sal"

"$WD/bin/vm_asm" "$asm" "$bytes"
"$WD/bin/vm_run" "$bytes"
