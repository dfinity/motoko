#!/usr/bin/env bash

ROOT_DIR=$(dirname "$(realpath $0)")
cd $ROOT_DIR || exit
echo --- Running target/motoko-composed.wasm on wasmtime ...
wasmtime run -C cache=n target/motoko-composed.wasm
echo --- Run done.
