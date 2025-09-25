#!/usr/bin/env bash
source "$(dirname "$0")/lib/set_env.sh"

ROOT_DIR=$(dirname "$(realpath $0)")
MO_SRC_DIR="src/motoko"

cd $ROOT_DIR || exit
# Create target directory if it doesn't exist
mkdir -p target
echo --- Compiling Motoko that does not import components... &&
moc $MO_SRC_DIR/WithoutComponents.mo -wasi-system-api --legacy-persistence -o target/without_components.wasm &&
echo --- Compiling done, output written to target/without_components.wasm
