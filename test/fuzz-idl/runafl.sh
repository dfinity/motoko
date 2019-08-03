#!/bin/bash

AFL_PATH=/nix/store/7d8qaslcwbpkhfl7cdy98hk4s37qmccd-afl-2.52b/bin AFL_SKIP_CPUFREQ=1 afl-fuzz -Q -i afl_in/ -o afl_out/ -- env /home/jojo/dfinity/actorscript/test/fuzz-idl/wrapper.sh @@
