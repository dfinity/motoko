#!/bin/bash

DVM_TMP=$(mktemp --directory --tmpdir dvm-XXXXXX)
trap 'rm -rf $DVM_TMP' EXIT

dvm -q --disable-colors --db $DVM_TMP reset
dvm -q --disable-colors --db $DVM_TMP new test.wasm -a test
dvm -q --disable-colors --db $DVM_TMP run test start
out="$(dvm -q --disable-colors --db $DVM_TMP run test foo "0x$(xxd -l 16 -p $1)")"

if [[ "$out" =~ (all izz well)|(unexpected IDL typ) ]]
then
  exit 0
else
  kill -TERM $$
fi
