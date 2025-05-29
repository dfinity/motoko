pkgs: pkgs.rustPlatform-stable.buildRustPackage rec {
  pname = "ic-wasm";
  version = builtins.substring 0 7 src.rev;
  src = pkgs.sources.ic-wasm-src;
  cargoLock = {
    lockFile = "${src}/Cargo.lock";
  };
  doCheck = false;
}
