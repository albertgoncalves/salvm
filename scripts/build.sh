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

shared=(
    "-ferror-limit=1"
    "-ffast-math"
    "-fno-autolink"
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
debug=(
    "-fno-omit-frame-pointer"
    "-fsanitize=address"
    "-fsanitize=undefined"
    "-O0"
)
release=(
    "-DRELEASE"
    "-g"
    "-nostdlib++"
    "-O3"
)

obj () {
    clang++ "${shared[@]}" -O0 -c "$@"
}

exe () {
    mold -run clang++ "${shared[@]}" "-I$WD/src/lib" "$@"
}

(
    cd "$WD" || exit 1
    clang-format -i -verbose src/*/*
    start=$(now)
    for x in alloc asm io str; do
        obj -o "build/$x.o" "src/lib/$x.cpp" &
    done
    obj -o build/inst.o src/lib/inst.cpp &
    obj -DDEBUG -o build/io_debug.o src/lib/io.cpp &
    for _ in $(jobs -p); do
        wait -n
    done
    for x in test_asm asm; do
        exe "${debug[@]}" -o "bin/$x" build/alloc.o build/asm.o build/str.o \
            src/app/$x.cpp &
    done
    exe "${debug[@]}" -o bin/test_inst build/alloc.o build/inst.o \
        src/app/test_inst.cpp &
    exe "${debug[@]}" -o bin/vm_debug build/alloc.o build/inst.o \
        build/io_debug.o build/str.o src/app/vm.cpp &
    exe "${release[@]}" -o bin/vm src/app/vm.cpp &
    for _ in $(jobs -p); do
        wait -n
    done
    end=$(now)
    python3 -c "print(\"Compiled! ({:.3f}s)\".format($end - $start))"
)

"$WD/bin/test_asm"
"$WD/bin/test_inst"
