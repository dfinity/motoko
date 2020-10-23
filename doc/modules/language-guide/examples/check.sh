#!/usr/bin/env bash

#
# Typechecks all examples in this directory
#

set -e

cd "$(dirname "${BASH_SOURCE[0]}")"

if [ -z "$MOTOKO_BASE" ]
then
  echo "\$MOTOKO_BASE not set. Are you running this in a nix-shell?"
  exit 1
fi

for file in *.mo
do
  echo "$file" ...
  moc --check --package base "$MOTOKO_BASE" "$file"
done
