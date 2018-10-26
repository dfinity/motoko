#!/usr/bin/env bash

if [ -z "$1" ]
then
  echo "Usage: $0 foo.wasm"
  exit 1
fi

name="$(basename $1 .wasm)"



export LANG=C
function dsh_ () {
  echo "\$ dsh $@"

  # hide the segmentation fault message
  { dsh $@; } 2>&1 \
    | sed -e 's,.*egmentation.*,Segmentation Fault,' \
    | grep -v '^Leaked'
}

dsh_ reset
dsh_ new -a $1
dsh_ run -q $name start
# not stable across v8 vs. hero!
# dsh_ root
