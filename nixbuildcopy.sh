#!/usr/bin/env bash
out="$($(dirname "${BASH_SOURCE[0]}")/nixbuild $@ --json | jq '.[0].outputs.out' -r)"

# On Linux we run builds on nixbuild.net so we have to copy the output from there:
if [[ "$(uname)" == "Linux" ]]; then
    nix copy --from ssh-ng://eu.nixbuild.net "$out"
fi

echo "$out"
