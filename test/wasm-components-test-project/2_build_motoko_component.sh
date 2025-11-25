#!/usr/bin/env bash
source "$(dirname "$0")/lib/set_env.sh"

ROOT_DIR=$(dirname "$(realpath $0)")
cd $ROOT_DIR || exit

echo --- Running embed... &&
wasm-tools component embed target/motoko.wit target/motoko.wasm -o target/motoko-embed.wasm &&
echo --- Creating Motoko component... &&
wasm-tools component new target/motoko-embed.wasm -v -o target/motoko-component.wasm --adapt wasi_snapshot_preview1=lib/wasi-adapter.wasm &&
echo --- Composing components... &&
wac compose target/motoko.wac -d motoko:component=target/motoko-component.wasm --deps-dir $MOPS_DIR -o target/motoko-composed.wasm &&
echo --- Composing done, output written to target/motoko-composed.wasm

