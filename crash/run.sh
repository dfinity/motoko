#!/usr/bin/env bash

ulimit -c unlimited
cargo run
find -name core.*
mkdir -p /dumps
mv core.* /dumps
