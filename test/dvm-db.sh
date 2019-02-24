#!/usr/bin/env bash

if [ -z "$1" -o -z "$2" ]
then
  echo "Usage: $0 <tmpdir> <name>.wasm"
  exit 1
fi

name="$(basename $2 .wasm)_0"

export LANG=C
function dvm_ () {
  # echo "\$ dvm $@"

  # hide the segmentation fault message
  # hide leaked debug log
  { dvm $@; } 2>&1 \
    | perl -pe 's,.*egmentation.*,Segmentation Fault,' \
    | perl -pe 's/Leaked.*!\n//m' \

}

dvm_ -q --db $1 reset
dvm_ -q --db $1 new $2
dvm_ -q --db $1 run $name start
