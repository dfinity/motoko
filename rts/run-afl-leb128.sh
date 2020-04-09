#!/bin/bash

if test -e afl_leb128_out
then
# continue
AFL_SKIP_CPUFREQ=1 afl-fuzz -i - -o afl_leb128_out/ -- ./test_leb128 @@
else
# start from scratch
AFL_SKIP_CPUFREQ=1 afl-fuzz -i afl_leb128_in/ -o afl_leb128_out/ -- ./test_leb128 @@
fi
