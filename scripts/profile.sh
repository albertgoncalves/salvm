#!/usr/bin/env bash

set -euo pipefail

sudo sh -c "echo 1 > /proc/sys/kernel/perf_event_paranoid"
sudo sh -c "echo 0 > /proc/sys/kernel/kptr_restrict"
perf record --call-graph fp "$WD/bin/vm" "$1"
perf report
rm perf.data*