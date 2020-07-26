#!/usr/bin/env bash

IC_REF_RUN=${IC_REF_RUN:-ic-ref-run}

if [ -z "$1" ]
then
  echo "Usage: $0 <name>.wasm [call-script]"
  exit 1
fi

export LANG=C.UTF-8

if [ "${1: -5}" = ".drun" ]
then
  # ic-ref-run doesn’t like empty or comment lines
  grep -v '^$' $1 | grep -v '^\w*#' | $IC_REF_RUN /dev/stdin
else
  ( echo "install ic:2A012B $1 0x"
    if [ -n "$2" ]
    then
      LANG=C perl -ne 'print "$1 ic:2A012B $2\n" if m,^//CALL (ingress|query) (.*),;print "upgrade ic:2A012B '"$1"' 0x\n" if m,^//CALL upgrade,; ' $2
    fi
  ) | $IC_REF_RUN /dev/stdin
fi
