pkgs: pkgs.rustPlatform-stable.buildRustPackage rec {
  pname = "ic-wasm";
  version = builtins.substring 0 7 src.rev;
  src = pkgs.sources.ic-wasm-src;
  cargoLock = {
    lockFile = "${src}/Cargo.lock";
  };
  doCheck = false;
  CXX_aarch64-apple-darwin = "${pkgs.llvmPackages_19.clang-unwrapped}/bin/clang++";
  CXXFLAGS_aarch64-apple-darwin = "-isystem ${pkgs.llvmPackages_19.libcxx.dev}/include/c++/v1 -isystem ${pkgs.llvmPackages_19.clang}/resource-root/include";
}
