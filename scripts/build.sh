#!/usr/bin/env bash

set -eu

if [ ! -d "$WD/bin" ]; then
    mkdir "$WD/bin"
fi

now () {
    date +%s.%N
}

flags=(
    "-fno-strict-aliasing"
    "-fshort-enums"
    "-fsingle-precision-constant"
    "-g"
    "-march=native"
    "-O1"
    "-static"
    "-std=c11"
    "-Wall"
    "-Wcast-align"
    "-Wcast-qual"
    "-Wconversion"
    "-Wdate-time"
    "-Wdouble-promotion"
    "-Wduplicated-branches"
    "-Wduplicated-cond"
    "-Werror"
    "-Wextra"
    "-Wfatal-errors"
    "-Wfloat-equal"
    "-Wformat-signedness"
    "-Wformat=2"
    "-Winline"
    "-Wlogical-op"
    "-Wmissing-declarations"
    "-Wmissing-include-dirs"
    "-Wmissing-prototypes"
    "-Wnull-dereference"
    "-Wpacked"
    "-Wpedantic"
    "-Wpointer-arith"
    "-Wredundant-decls"
    "-Wshadow"
    "-Wstack-protector"
    "-Wstrict-prototypes"
    "-Wswitch-enum"
    "-Wtrampolines"
    "-Wundef"
    "-Wunused"
    "-Wunused-macros"
    "-Wwrite-strings"
)
bins=(
    test_vm_asm
    test_vm_inst
    vm_asm
    vm
)

(
    start=$(now)
    for x in "${bins[@]}"; do
        gcc "${flags[@]}" -o "$WD/bin/$x" "$WD/src/$x.c"
    done
    gcc "${flags[@]}" "-DDEBUG_PRINT_VM" -o "$WD/bin/vm_debug" "$WD/src/vm.c"
    end=$(now)
    python3 -c "print(\"Compiled! ({:.3f}s)\".format(${end} - ${start}))"
)

"$WD/bin/test_vm_asm"
"$WD/bin/test_vm_inst"
