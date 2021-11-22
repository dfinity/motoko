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
# 0.8.4 is the first where motoko is removed from sources.json (and we need to ddo something else)
for tag in $(git -C "$1" tag --contains 0.5.10 --no-contains 0.8.4 | sort -V)
do
  mo_rev="$(git -C "$1" show "$tag:nix/sources.json" | jq -r .motoko.rev)"
  base_rev="$(git -C "$2" show "$mo_rev:nix/sources.json" | jq -r .'["motoko-base"]'.rev)"
  echo "$tag: motoko $mo_rev, motoko-base $base_rev"
  git -C "$2" tag -f "sdk-$tag" "$mo_rev"
  git -C "$3" tag -f "dfx-$tag" "$base_rev"
done

for tag in $(git -C "$1" tag --contains 0.8.4 | sort -V)
do
  mo_rev="$(git -C "$1" show "$tag:nix/sources.json" | jq -r .'[
"motoko-x86_64-linux"]'.version)"
  base_rev="$(git -C "$2" show "$mo_rev:nix/sources.json" | jq -r .'["motoko-base"]'.rev)"
  echo "$tag: motoko $mo_rev, motoko-base $base_rev"
  git -C "$2" tag -f "sdk-$tag" "$mo_rev"
  git -C "$3" tag -f "dfx-$tag" "$base_rev"
done

read -p "Are you sure you want to push these tags? " -n 1 -r
echo    # (optional) move to a new line
if [[ ! $REPLY =~ ^[Yy]$ ]]
then
    exit 1
fi

git -C "$2" push --tags
git -C "$3" push --tags
