#!/usr/bin/env bash
set -euo pipefail

# Run in the repository's Nix development shell. The compiler extensions and
# the runtime libraries must come from the same checkout/toolchain build.
plugin_root=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/.." && pwd)
cd "$plugin_root"

runtime_only=false
case "${1:-}" in
  "") ;;
  --runtime-only) runtime_only=true ;;
  *) printf 'Usage: %s [--runtime-only]\n' "$0" >&2; exit 2 ;;
esac
if [[ $# -gt 1 ]]; then
  printf 'Usage: %s [--runtime-only]\n' "$0" >&2
  exit 2
fi

if [[ "$runtime_only" = false ]]; then
  # OCaml 5.4 moved Bigarray fully into the standard library. The pinned Nix
  # findlib metadata still names the removed standalone archives; override
  # only that metadata until the Nix package catches up.
  plugin_ocaml_version=$(ocamlc -version)
  IFS=. read -r plugin_ocaml_major plugin_ocaml_minor _ <<< "$plugin_ocaml_version"
  if (( plugin_ocaml_major > 5 || (plugin_ocaml_major == 5 && plugin_ocaml_minor >= 4) )) &&
      [[ ! -f "$(ocamlc -where)/bigarray.cmxa" ]]; then
    plugin_compat="$plugin_root/_build/ocaml-compat"
    mkdir -p "$plugin_compat/bigarray"
    printf 'version = "%s"\ndescription = "Bigarray is in the standard library"\nrequires = ""\n' \
      "$plugin_ocaml_version" > "$plugin_compat/bigarray/META"
    export OCAMLPATH="$plugin_compat${OCAMLPATH:+:$OCAMLPATH}"
  fi
  src/source_id/gen.sh
  dune build --root src --profile=release exes/moc.exe \
    compute_plugins/candidx/compute_plugin_candidx.cmxs \
    compute_plugins/sha256/compute_plugin_sha256.cmxs
  ln -fs _build/default/exes/moc.exe src/moc
fi

plugin_target="$plugin_root/_build/compute-plugins/cargo"
RUSTFLAGS="--remap-path-prefix=$plugin_root=. -C link-arg=--import-memory ${RUSTFLAGS:-}" \
  cargo build --manifest-path plugins/runtime/Cargo.toml --locked \
    --release --target rts/motoko-rts/wasm64-none-shared.json \
    --target-dir "$plugin_target" \
    -Zjson-target-spec -Zbuild-std=core,alloc

for plugin_name in candidx sha256; do
  plugin_dest="$plugin_root/_build/compute-plugins/$plugin_name"
  mkdir -p "$plugin_dest"
  install -m644 "$plugin_target/wasm64-none-shared/release/motoko_compute_$plugin_name.wasm" \
    "$plugin_dest/runtime.wasm"
  wasm-validate --enable-memory64 "$plugin_dest/runtime.wasm"
  plugin_compiler="src/_build/default/compute_plugins/$plugin_name/compute_plugin_$plugin_name.cmxs"
  if [[ -f "$plugin_compiler" ]]; then
    install -m644 "$plugin_compiler" "$plugin_dest/plugin.cmxs"
  fi
done
