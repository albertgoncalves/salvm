#!/usr/bin/env bash

set -eu

paths=(
    "-I$WD/src/lib"
)
flags=(
    "-fshort-enums"
    "-fsingle-precision-constant"
    "-g"
    "-march=native"
    "-O1"
    "-static"
    "-std=c11"
    "-Wall"
    "-Wcast-align"
    "-Wcast-align=strict"
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
    vm_run
)

now () {
    date +%s.%N
}

(
    if [ ! -d "$WD/bin" ]; then
        mkdir "$WD/bin"
    fi
    start=$(now)
    for x in "${bins[@]}"; do
        gcc "${paths[@]}" "${flags[@]}" -o "$WD/bin/$x" "$WD/src/app/$x.c"
    done
    gcc \
        "${paths[@]}" \
        "${flags[@]}" \
        "-DDEBUG_PRINT_VM" \
        -o "$WD/bin/vm_run_debug" \
        "$WD/src/app/vm_run.c"
    end=$(now)
    python3 -c "print(\"Compiled! ({:.3f}s)\".format(${end} - ${start}))"
)

"$WD/bin/test_vm_asm"
"$WD/bin/test_vm_inst"
