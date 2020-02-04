#!/usr/bin/env bash

DRUN=${DRUN:-drun}
CONFIG=$(realpath $(dirname $0)/drun.toml)

#
# This script wraps drun to
#
# * extract the methods calls from comments in the second argument
#   (typically the test source files)
# * adds "ic:2A012B" as the destination to these calls
# * writes prometheus metrics to file descriptor 222
#   (for run.sh -p; post-processing happening in run.sh)
#


if [ -z "$1" ]
then
  echo "Usage: $0 <name>.wasm [call-script]"
  exit 1
fi

export LANG=C.UTF-8

# this number is determined empirically: how many extra batches are needed
# until all call-trees have finished (even those that return early).
# Usually darwin needs more! (Should go away with DFN-1269)
EXTRA_BATCHES=100

(echo "install ic:2A012B $1 0x";
 if [ -n "$2" ]; then LANG=C perl -ne 'print "$1 ic:2A012B $2\n" if m,^//CALL (ingress|query) (.*),' $2; fi;
) | $DRUN -c "$CONFIG" --extra-batches $EXTRA_BATCHES /dev/stdin
