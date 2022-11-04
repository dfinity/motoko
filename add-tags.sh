#!/usr/bin/env bash

if ! test -d "$1" -o -d "$2" -o -d "$3"
then
  echo "Adds tags to the motoko and motoko-base repository based on sdk repository tags"
  echo "and tracing the nix pins. Runs git push --tags!"
  echo
  echo "Usage: $0 ../sdk ./ ../motoko-base"
  echo
  echo "(i.e. paths to sdk, motoko and motoko-base repos)"
  exit 1
fi

set -e

git -C "$1" fetch --tags
# 0.5.10 was the first where we have motoko-base via motoko
for tag in $(git -C "$1" tag --contains 0.5.10 | sort -V)
do
  mo_rev="$(git -C "$1" show "$tag:nix/sources.json" | jq -r .motoko.rev)"
  base_rev="$(git -C "$2" show "$mo_rev:nix/sources.json" | jq -r .'["motoko-base"]'.rev)"
  echo "$tag: motoko $mo_rev, motoko-base $base_rev"
  git -C "$2" tag -f "sdk-$tag" "$mo_rev"
  git -C "$3" tag -f "dfx-$tag" "$base_rev"
done

git -C "$2" push --tags
git -C "$3" push --tags
