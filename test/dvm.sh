#!/usr/bin/env bash

if [ -z "$1" ]
then
  echo "Usage: $0 foo.wasm"
  exit 1
fi

name="$(basename $1 .wasm)"

export LANG=C
function dvm_ () {
  echo "\$ dvm $@"

  # hide the segmentation fault message
  # hide leaked debug log
  # make sure there is a trailing newline (dvm with hero does not produce it).
  { dvm $@; } 2>&1 \
    | sed -e 's,.*egmentation.*,Segmentation Fault,' \
    | sed -e 's,Leaked.*!,,' \
    | sed -e '$a\'

}

dvm_ -q reset
dvm_ -q new -a $1
dvm_ -q run $name start
