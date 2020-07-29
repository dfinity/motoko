#!/usr/bin/env bash

set -e

file="$(dirname "${BASH_SOURCE[0]}")/rts.ml"

if [ -z "$1" ]; then
  echo "Usage: $0 path/to/mo-rts.wasm"
  echo
  echo "Note: This script is typically only invoked within a nix build."
  exit 1
fi

perl -0777 -ne 'print "let wasm = lazy \""; printf "\\x%02x", ord($_) for (split //,$_); print "\"";' "$1" > "$file"
