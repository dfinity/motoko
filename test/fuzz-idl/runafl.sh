#!/bin/bash

if test -e afl_out
then
# continue
AFL_SKIP_CPUFREQ=1 afl-fuzz -i - -o afl_out/ -- ./main @@
else
# start from scratch
AFL_SKIP_CPUFREQ=1 afl-fuzz -i afl_in/ -o afl_out/ -- ./main @@
fi

