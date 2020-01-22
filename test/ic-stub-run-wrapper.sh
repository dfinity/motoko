#!/usr/bin/env bash

IC_STUB_RUN=${IC_STUB_RUN:-ic-stub-run}
CONFIG=$(realpath $(dirname $0)/drun.toml)



if [ -z "$1" ]
then
  echo "Usage: $0 <name>.wasm [call-script]"
  exit 1
fi

export LANG=C.UTF-8

# canister id "ic:000000000000040054", in little-endian decimal, is:
# 1125899906842624
# (drun currently expects a decimal number for the canister id)

(echo "install ic:2A012B $1 0x";
 if [ -n "$2" ]; then LANG=C perl -ne 'print "$1 ic:2A012B $2\n" if m,^//CALL (ingress|query) (.*),' $2; fi;
) | $IC_STUB_RUN -c "$CONFIG" /dev/stdin
