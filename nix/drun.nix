pkgs:
{ drun =
    pkgs.rustPlatform.buildRustPackage {
      name = "drun";

      src = pkgs.sources.ic + "/rs";

      # update this after dependency changes
      cargoSha256 = "0xg4wf6m103kn0kqgc4bfdz343lwhi5d3lx14xhq3b5jrnzk30a4";

      nativeBuildInputs = with pkgs; [
        pkg-config
        cmake
      ];

      buildInputs = with pkgs; [
        openssl
        llvm_10
        llvmPackages_10.libclang
        lmdb
      ] ++ pkgs.lib.optionals pkgs.stdenv.isDarwin [
        pkgs.darwin.apple_sdk.frameworks.Security
      ];

      # needed for bindgen
      LIBCLANG_PATH = "${pkgs.llvmPackages_10.libclang.lib}/lib";
      CLANG_PATH = "${pkgs.llvmPackages_10.clang}/bin/clang";

      # needed for ic-protobuf
      PROTOC="${pkgs.protobuf}/bin/protoc";

      doCheck = false;

      buildAndTestSubdir = "drun";
    };
}
