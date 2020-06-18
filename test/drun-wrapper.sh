#!/usr/bin/env bash

DRUN=${DRUN:-drun}
CONFIG=$(realpath $(dirname $0)/drun.json5)

#
# This script wraps drun to
#
# * extract the methods calls from comments in the second argument
#   (typically the test source files)
# * adds the right canister ids as the destination to these calls
# * writes prometheus metrics to file descriptor 222
#   (for run.sh -p; post-processing happening in run.sh)
#


if [ -z "$1" ]
then
  echo "Usage: $0 <name>.wasm [call-script]"
  echo "or"
  echo "Usage: $0 <name>.drun"
  exit 1
fi

export LANG=C.UTF-8

export RUST_BACKTRACE=1

# this could be used to delay drun to make it more deterministic, but
# it doesn't work reliably and slows down the test significantly.
# so until DFN-1269 fixes this properly, let's just not run
# affected tests on drun (only ic-ref-run).
EXTRA_BATCHES=1

# drun creates canisters with this ID:
ID=ic:0100000000000000000000000000000000012D

if [ "${1: -5}" = ".drun" ]
then
  # work around different IDs in ic-ref-run and drun
  ( echo "create"
    LANG=C perl -npe 's,ic:2A012B,'$ID',g' $1
  ) | $DRUN -c "$CONFIG" --extra-batches $EXTRA_BATCHES /dev/stdin
else
  ( echo "create"
    echo "install $ID $1 0x"
    if [ -n "$2" ]
    then
      LANG=C perl -ne 'print "$1 '$ID' $2\n" if m,^//CALL (ingress|query) (.*),;print "upgrade '$ID' '"$1"' 0x\n" if m,^//CALL upgrade,; ' $2
    fi
  ) | $DRUN -c "$CONFIG" --extra-batches $EXTRA_BATCHES /dev/stdin
fi
