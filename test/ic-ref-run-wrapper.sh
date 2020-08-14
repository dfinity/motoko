#!/usr/bin/env bash

if [ -z "$1" ]
then
  echo "Usage: $0 <name>.wasm [call-script]"
  exit 1
fi

export LANG=C.UTF-8

# drun creates canisters with this ID:
ID=cvccv-qqaaq-aaaaa-aaaaa-c
# ID=ic:0004000000000000014B

if [ "${1: -5}" = ".drun" ]
then
  ( echo "create"
    LANG=C perl -npe 's,\$ID,'$ID',g' $1 |
    grep -v '^$' | grep -v '^\w*#' # ic-ref-run doesn’t like empty or comment lines
  ) | ic-ref-run /dev/stdin
else
  ( echo "create"
    echo "install "$ID" $1 0x"
    if [ -n "$2" ]
    then
      LANG=C perl -ne 'print "$1 '$ID' $2\n" if m,^//CALL (ingress|query) (.*),;print "upgrade '$ID' '"$1"' 0x\n" if m,^//CALL upgrade,; ' $2
    fi
  ) | ic-ref-run /dev/stdin
fi
