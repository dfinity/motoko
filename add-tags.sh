#!/usr/bin/env bash

if ! test -d "$1"
then
  echo "Adds tags to this (motoko) repository based on sdk repository tags"
  echo "and corresponding pins to Motoko."
  echo
  echo "Usage: $0 ../sdk"
  echo
  echo "(where ../sdk is a working copy of the sdk repository)"
  exit 1
fi

set -e

git -C "$1" fetch --tags
# commit 868aa05b9a6fd7cc33b693aede29d6eb36a77fc0 added motoko to nix/sources.json
for tag in $(git -C "$1" tag --contains 868aa05b9a6fd7cc33b693aede29d6eb36a77fc0)
do
  rev=$(git -C "$1" show $tag:nix/sources.json| jq -r .motoko.rev)
  echo "$tag: $rev"
  git tag -f sdk-$tag $rev
done

git push --tags
