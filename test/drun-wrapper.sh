#!/usr/bin/env bash

if [ -z "$1" ]
then
  echo "Usage: $0 <name>.wasm [call-script]"
  exit 1
fi

export LANG=C

( if [ -n "$2" ]; then grep '^//CALL ' $2 | cut -c8-; fi;) | drun $1 /dev/stdin
