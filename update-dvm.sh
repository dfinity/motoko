#!/bin/bash

# This is a small convenience script that ensures that `nix/dev` is up-to-date

set -e

if [ ! -e nix/dev ]
then git clone --recursive git@github.com:dfinity-lab/dev nix/dev
else git -C nix/dev fetch
fi
$(grep checkout Jenkinsfile |cut -d\' -f2)
git -C nix/dev submodule update --init --recursive
nix-env -i -f . -A dvm
