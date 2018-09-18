#!/bin/bash

if [ -z "$1" ]
then
  echo "Usage: $0 foo.wasm"
  exit 1
fi

name="$(basename $1 .wasm)"

dsh reset
dsh new -a $1
dsh run $name start
