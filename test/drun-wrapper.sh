#!/usr/bin/env bash

DRUN=${DRUN:-drun}


if [ -z "$1" ]
then
  echo "Usage: $0 <name>.wasm [call-script]"
  exit 1
fi

export LANG=C

( if [ -n "$2" ]; then grep '^//CALL ' $2 | cut -c8-; fi;) | $DRUN $1 /dev/stdin
