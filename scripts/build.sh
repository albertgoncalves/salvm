#!/usr/bin/env bash

set -eu

for x in bin build; do
    if [ ! -d "$WD/$x" ]; then
        mkdir "$WD/$x"
    fi
done

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
    "-march=native"
    "-std=c++11"
    "-Werror"
    "-Weverything"
    "-Wno-c++98-compat-pedantic"
    "-Wno-c99-extensions"
    "-Wno-covered-switch-default"
    "-Wno-disabled-macro-expansion"
    "-Wno-extra-semi-stmt"
    "-Wno-padded"
    "-Wno-reserved-id-macro"
)

obj () {
    clang++ "${flags[@]}" -c "$@"
}

exe () {
    clang++ "${flags[@]}" "-I$WD/src/lib" "$@"
}

(
    cd "$WD" || exit 1
    clang-format -i -verbose src/*/*
    start=$(now)
    for x in asm io str; do
        obj -O0 -o "build/$x.o" "src/lib/$x.cpp" &
    done
    obj -O3 -o build/inst.o src/lib/inst.cpp &
    obj -O0 -DDEBUG -o build/io_debug.o src/lib/io.cpp &
    for _ in $(jobs -p); do
        wait -n
    done
    for x in test_asm asm; do
        exe -O0 -o "bin/$x" build/str.o build/asm.o src/app/$x.cpp &
    done
    exe -O0 -o bin/test_inst build/inst.o src/app/test_inst.cpp &
    exe -O0 -o bin/vm_debug build/str.o build/inst.o build/io_debug.o \
        src/app/vm.cpp &
    exe -O3 -flto -g -DRELEASE -o bin/vm src/app/vm.cpp &
    for _ in $(jobs -p); do
        wait -n
    done
    end=$(now)
    python3 -c "print(\"Compiled! ({:.3f}s)\".format($end - $start))"
)

"$WD/bin/test_asm"
"$WD/bin/test_inst"
