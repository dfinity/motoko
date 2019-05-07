#!/usr/bin/env bash

# This script compares the compiler output (in wat format)
# between the currently built version and the previous version
#
# With -f from -t to other branches can be selected (anything that git
# rev-parse understands)

old="$(git rev-parse HEAD)"
new=""

WASM2WAT="wasm2wat --no-check --enable-multi-value"

while getopts "f:t:" o; do
    case "${o}" in
        f)
            old="$(git rev-parse ${OPTARG})"
            ;;
        t)
            new="$(git rev-parse ${OPTARG})"
            ;;
    esac
done
shift $((OPTIND-1))

function build_ref_to {
  rm -f $2-asc
  if [ -z "$1" ]
  then
    echo "Building $2 asc from working copy.."
    chronic nix-build -E '((import ./..) {}).native' \
      --option binary-caches '' \
      -o $2-asc/
  else
    echo "Building $2 asc (rev $1).."
    chronic nix-build \
      --argstr ref "$(git for-each-ref --count 1 --contains "$1" --format '%(refname)')" \
      --argstr rev "$1" \
      --argstr path "$(realpath "$(dirname $0)/..")" \
      -E '
      {rev, ref, path}:
      let nixpkg = import (../nix/nixpkgs.nix).nixpkgs {}; in
      let checkout = (builtins.fetchGit {url = path; ref = ref; rev = rev; name = "old-asc";}).outPath; in
      builtins.trace checkout (
      ((import checkout) {}).native)' \
      --option binary-caches '' \
      -o $2-asc/
  fi
  test -x $2-asc/bin/asc || exit 1
}
build_ref_to "$old" old
build_ref_to "$new" new

mkdir -p compare-out/

if [[ $# -eq 0 ]] ; then
    args="*/*.as"
else
    args="$@"
fi

for file in $args
do
  if [ ! -e $file ]
  then
    echo "ActorScript file $file does not exist."
    exit 1
  fi

  base=$(basename $file .as)

  rm -rf compare-out/$base.old
  mkdir compare-out/$base.old
  old-asc/bin/asc --dfinity $file -o compare-out/$base.old/$base.wasm 2> compare-out/$base.old/$base.stderr
  test ! -e compare-out/$base.old/$base.wasm ||
  $WASM2WAT compare-out/$base.old/$base.wasm >& compare-out/$base.old/$base.wat
  #wasm-objdump -s -h -d compare-out/$base.old/$base.wasm > compare-out/$base.old/$base.dump

  rm -rf compare-out/$base.new
  mkdir compare-out/$base.new
  new-asc/bin/asc --dfinity $file -o compare-out/$base.new/$base.wasm 2> compare-out/$base.new/$base.stderr
  test ! -e compare-out/$base.new/$base.wasm ||
  $WASM2WAT compare-out/$base.new/$base.wasm >& compare-out/$base.new/$base.wat
  #wasm-objdump -s -h -d compare-out/$base.new/$base.wasm > compare-out/$base.new/$base.dump

  diff -r -N -u10 compare-out/$base.old compare-out/$base.new

  rm -rf compare-out/$base.old
  rm -rf compare-out/$base.new
done
rmdir compare-out/
