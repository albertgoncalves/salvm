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
    "-ffast-math"
    "-fno-exceptions"
    "-fno-math-errno"
    "-fno-rtti"
    "-fno-unwind-tables"
    "-fshort-enums"
    "-g"
    "-march=native"
    "-std=c++11"
    "-Werror"
    "-Weverything"
    "-Wno-c++98-compat-pedantic"
    "-Wno-c99-extensions"
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
)

(
    clang-format -i -verbose "$WD/src/"*
    start=$(now)
    for x in "${bins[@]}"; do
        clang++ "${flags[@]}" -O0 -o "$WD/bin/$x" "$WD/src/$x.cpp"
    done
    clang++ \
        "${flags[@]}" \
        -O0 \
        "-DDEBUG_PRINT_VM" \
        -o "$WD/bin/vm_debug" \
        "$WD/src/vm.cpp"
    clang++ "${flags[@]}" -O3 -o "$WD/bin/vm" "$WD/src/vm.cpp"
    end=$(now)
    python3 -c "print(\"Compiled! ({:.3f}s)\".format(${end} - ${start}))"
)

"$WD/bin/test_vm_asm"
"$WD/bin/test_vm_inst"
