#!/usr/bin/env bash

set -eu

paths=(
    "-I$WD/src/lib"
)

cppcheck \
    "${paths[@]}" \
    --enable=all \
    --suppress=missingIncludeSystem \
    "$WD/src"
clang-format -i -verbose "$WD/src/"*/*
