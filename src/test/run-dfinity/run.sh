#!/bin/bash

if [ -z "$1" ]
then
  echo "Usage: $0 foo.wasm"
  exit 1
fi

name="$(basename $1 .wasm)"

dsh <<__END__
reset
new -a $1
run $name start
__END__
