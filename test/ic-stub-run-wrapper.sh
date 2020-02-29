#!/usr/bin/env bash

IC_STUB_RUN=${IC_STUB_RUN:-ic-stub-run}
CONFIG=$(realpath $(dirname $0)/drun.toml)



if [ -z "$1" ]
then
  echo "Usage: $0 <name>.wasm [call-script]"
  exit 1
fi

export LANG=C.UTF-8

(echo "install ic:2A012B $1 0x";
 if [ -n "$2" ]; then LANG=C perl -ne 'print "$1 ic:2A012B $2\n" if m,^//CALL (ingress|query) (.*),;print "upgrade ic:2A012B '"$1"' 0x\n" if m,^//CALL upgrade,; ' $2; fi;
) | $IC_STUB_RUN -c "$CONFIG" /dev/stdin
