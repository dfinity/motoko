#!/bin/bash
source "$(dirname "$0")/lib/set_env.sh"

ROOT_DIR=$(dirname "$(realpath $0)")
MO_SRC_DIR="src/motoko"
MOC_PACKAGES="--package core $MOTOKO_CORE --package hex $MOTOKO_HEX"
MOC_COMPONENT_PACKAGES=""
for folder in $ROOT_DIR/$MOPS_DIR/component/*@*; do
  package_with_version=$(basename -- "$folder")
  package="${package_with_version%@*}"
  MOC_COMPONENT_PACKAGES="$MOC_COMPONENT_PACKAGES --package $package $ROOT_DIR/$MOPS_DIR/component/$package_with_version"
done

cd $ROOT_DIR || exit
# Create target directory if it doesn't exist
mkdir -p target
echo --- Compiling Motoko that imports components... &&
echo "    moc packages: $MOC_PACKAGES" &&
echo "    component packages: $MOC_COMPONENT_PACKAGES" &&
MOC_UNLOCK_PRIM=true moc $MO_SRC_DIR/Main.mo -wasi-system-api -wasm-components --legacy-persistence $MOC_PACKAGES $MOC_COMPONENT_PACKAGES -o target/motoko.wasm &&
echo --- Compiling done, output written to target/motoko.wasm
