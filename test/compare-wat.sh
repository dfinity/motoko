#!/bin/bash

# This script compares the compiler output (in wat format)
# between the currently built version and the previous version

old="$(git rev-parse HEAD)"

echo "Building old asc (rev $old).."
nix-build --argstr rev "$old" -E '
  {rev}:
  let nixpkg = import (../nix/nixpkgs.nix) {}; in
  let checkout = (builtins.fetchGit {url = ".."; ref = "HEAD"; rev = rev; name = "old-asc";}).outPath; in
  ((import checkout) {}).native' \
  --option binary-caches '' \
  -o old-asc/

echo "Building current asc.."
nix-build -E '((import ./..) {}).native' \
  --option binary-caches '' \
  -o new-asc/

mkdir -p compare-out/

for file in */*.as
do
  base=$(basename $file .as)

  rm -rf compare-out/$base.old
  mkdir compare-out/$base.old
  old-asc/bin/asc --dfinity $file -o compare-out/$base.old/$base.wasm 2> compare-out/$base.old/$base.stderr
  test ! -e compare-out/$base.old/$base.wasm ||
  wasm2wat compare-out/$base.old/$base.wasm -o compare-out/$base.old/$base.wat 2>/dev/null

  rm -rf compare-out/$base.new
  mkdir compare-out/$base.new
  new-asc/bin/asc --dfinity $file -o compare-out/$base.new/$base.wasm 2> compare-out/$base.new/$base.stderr
  test ! -e compare-out/$base.new/$base.wasm ||
  wasm2wat compare-out/$base.new/$base.wasm -o compare-out/$base.new/$base.wat 2>/dev/null

  diff -r -N -u compare-out/$base.old compare-out/$base.new

  rm -rf compare-out/$base.old
  rm -rf compare-out/$base.new
done
rmdir compare-out/
