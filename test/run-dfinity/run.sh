#!/bin/bash

if [ -z "$1" ]
then
  echo "Usage: $0 foo.wasm"
  exit 1
fi

name="$(basename $1 .wasm)"


ulimit -c 0
trap 'if [[ $? -eq 139 ]]; then echo "segfault !"; fi' CHLD
trap 'echo Segmentation fault!' SEGV

export LANG=C
function dsh_ () {
  echo "\$ dsh $@"
  dsh $@ || echo "Return value: $?"
}

dsh_ reset
dsh_ new -a $1
dsh_ run $name start
