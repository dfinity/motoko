#!/usr/bin/env bash
set -euo pipefail

out="$($(dirname "${BASH_SOURCE[0]}")/nixbuild.sh $@ --json | jq '.[0].outputs.out' -r)"

# On Linux we run builds on nixbuild.net so we have to copy the output from there:
if [[ "$(uname)" == "Linux" ]]; then
    nix copy --from ssh-ng://eu.nixbuild.net "$out"
fi

echo "$out"
