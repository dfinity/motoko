#!/usr/bin/env bash
source "$(dirname "$0")/lib/set_env.sh"

ROOT_DIR=$(dirname "$(realpath $0)")
MO_SRC_DIR="src/motoko"
COMPONENT_DEPS=""
for folder in $ROOT_DIR/$MOPS_DIR/component/*@*; do
  package_with_version=$(basename -- "$folder")
  package="${package_with_version%@*}"
  component_name=$(echo "$package" | tr '_' '-')
  COMPONENT_DEPS="$COMPONENT_DEPS -d component:$component_name=$ROOT_DIR/$MOPS_DIR/component/$package_with_version/$package.wasm "
done

cd $ROOT_DIR || exit
echo --- Running mo2wc... &&
MO2WC=./src/rust/mo2wc/target/release/mo2wc
RUST_LOG=info $MO2WC compose --wac-file target/motoko.wac --wasm-module-file target/motoko.wasm --wit-file target/motoko.wit \
    $COMPONENT_DEPS \
    -o target/out.wasm
wasmtime run target/out.wasm
