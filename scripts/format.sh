#!/usr/bin/env bash

set -eu

cppcheck --enable=all "$WD/src"
clang-format -i -verbose "$WD/src/"*
