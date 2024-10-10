#!/usr/bin/env bash

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

# on darwin, I have seen
#   thread 'MR Batch Processor' has overflowed its stack
# and this helps (default is 2MB)
export RUST_MIN_STACK=$((10*1024*1024))

# drun creates canisters with this ID:
ID=lxzze-o7777-77777-aaaaa-cai

if [ "${1: -5}" = ".drun" ]
then
  # work around different IDs in ic-ref-run and drun
  ( echo "create"
    LANG=C perl -npe 's,\$ID,'$ID',g' $1
  ) | drun --cycles-used-file /dev/fd/222 /dev/stdin
else
  ( echo "create"
    echo "install $ID $1 0x"
    if [ -n "$2" ]
    then
      LANG=C perl -ne 'print "$1 '$ID' $2\n" if m,^//CALL (ingress|query) (.*),;print "upgrade '$ID' '"$1"' 0x\n" if m,^//CALL upgrade,; ' $2
    fi
  ) | drun --cycles-used-file /dev/fd/222 /dev/stdin
fi
