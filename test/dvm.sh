#!/usr/bin/env bash

if [ -z "$1" ]
then
  echo "Usage: $0 <name>.wasm [call-script]"
  exit 1
fi

name="$(basename $1 .wasm)_0"
DVM_TMP=$(mktemp --directory --tmpdir dvm-XXXXXX)
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

if [ -n "$2" ]
then
  grep '^//CALL ' $2 | cut -c7- |
  while read call
  do
    echo "DVM: Calling method $call"
    dvm_ -q --db $DVM_TMP run $name $call
  done
fi
