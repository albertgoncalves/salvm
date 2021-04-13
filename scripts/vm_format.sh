#!/usr/bin/env bash

set -eu

cppcheck --enable=all --suppress=missingIncludeSystem "$WD/src/vm"
clang-format -i -verbose "$WD/src/vm/"*
