#!/usr/bin/env bash

set -eu

if [ ! -d "$WD/bin" ]; then
    mkdir "$WD/bin"
fi

now () {
    date +%s.%N
}

flags=(
    "-ferror-limit=1"
    "-fshort-enums"
    "-g"
    "-march=native"
    "-O0"
    "-std=c11"
    "-Werror"
    "-Weverything"
    "-Wno-c++98-compat"
    "-Wno-cast-align"
    "-Wno-covered-switch-default"
    "-Wno-disabled-macro-expansion"
    "-Wno-error=#warnings"
    "-Wno-extra-semi-stmt"
    "-Wno-padded"
    "-Wno-reserved-id-macro"
)
bins=(
    test_vm_asm
    test_vm_inst
    vm_asm
    vm
)

(
    clang-format -i -verbose "$WD/src/"*
    start=$(now)
    for x in "${bins[@]}"; do
        clang "${flags[@]}" -o "$WD/bin/$x" "$WD/src/$x.c"
    done
    clang "${flags[@]}" "-DDEBUG_PRINT_VM" -o "$WD/bin/vm_debug" "$WD/src/vm.c"
    end=$(now)
    python3 -c "print(\"Compiled! ({:.3f}s)\".format(${end} - ${start}))"
)

"$WD/bin/test_vm_asm"
"$WD/bin/test_vm_inst"
