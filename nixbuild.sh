#!/usr/bin/env bash
set -euo pipefail

NIX_ARGS=()

# On Linux we run builds on nixbuild.net:
if [[ "$(uname)" == "Linux" ]]; then
    NIX_ARGS+=(
        --print-build-logs
        --builders ""
        --max-jobs 2
        --eval-store auto
        --store ssh-ng://eu.nixbuild.net
    )
fi

nix build "${NIX_ARGS[@]}" $@
