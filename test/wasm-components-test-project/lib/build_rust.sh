#!/bin/bash
source "$(dirname "$0")/set_env.sh"

ROOT_DIR=$(dirname "$(realpath $0)")/..
export RUST_DIR=$ROOT_DIR/src/rust
export MOPS_COMPONENTS_DIR=$ROOT_DIR/$MOPS_DIR/component
echo "NOTE: This script may fail when run in Motoko's development nix-shell, which uses an old version of cargo/rustc."
echo "      If this happens, run it in a separate shell with your regular Rust development setup."

build_rust_component() {
  component_name=$1

  echo --- Building Rust component $component_name to target/${component_name}.wasm
  cd $RUST_DIR/$component_name || exit
  cargo build --$BUILD_MODE --target $TARGET &&
  wasm-tools component new target/$TARGET/$BUILD_MODE/${component_name}.wasm -o target/${component_name}.wasm &&
  COMPONENT_DIR="$MOPS_COMPONENTS_DIR/$component_name@0.0.1"
  echo ... Copying to mops-dir $COMPONENT_DIR
  mkdir -pv $COMPONENT_DIR
  cp target/${component_name}.wasm $COMPONENT_DIR/
  cp ${component_name}.mo $COMPONENT_DIR/lib.mo
  echo ... DONE building $component_name
}

(
  echo ___ Building Rust components...
  build_rust_component ic_sig_verifier
  build_rust_component meet_and_greet
  build_rust_component zstd
  echo ___ DONE building Rust components.
)
