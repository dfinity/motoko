#!/bin/bash

if [ -z "$1" ]
then
  echo "Usage: $0 foo.wasm"
  exit 1
fi

name="$(basename $1 .wasm)"

export LANG=C
function dsh_ () {
  echo "\$ dsh $@"
  dsh $@
}

dsh_ reset
dsh_ new -a $1
dsh_ run $name start
