#!/usr/bin/env bash

if [ -z "$1" ]
then
  echo "Usage: $0 <name>.wasm"
  exit 1
fi

name="$(basename $1 .wasm)_0"
DVM_TMP=$(mktemp -d)
trap 'rm -rf $DVM_TMP' EXIT

export LANG=C
function dvm_ () {
  # echo "\$ dvm $@"

  # hide the segmentation fault message
  # hide leaked debug log
  { dvm $@; } 2>&1 \
    | perl -pe 's,.*egmentation.*,Segmentation Fault,' \
    | perl -pe 's/Leaked.*!\n//m' \

}

dvm_ -q --db $DVM_TMP reset
dvm_ -q --db $DVM_TMP new $1
dvm_ -q --db $DVM_TMP run $name start
