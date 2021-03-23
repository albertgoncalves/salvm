#!/usr/bin/env bash

set -eu

paths=(
    "-I$WD/src/vm/lib"
)

cppcheck \
    "${paths[@]}" \
    --enable=all \
    --suppress=missingIncludeSystem \
    "$WD/src/vm"
clang-format -i -verbose "$WD/src/vm/"*/*
