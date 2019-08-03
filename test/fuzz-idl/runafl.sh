#!/bin/bash

AFL_SKIP_CPUFREQ=1 afl-fuzz -i afl_in/ -o afl_out/ -- ./main @@
