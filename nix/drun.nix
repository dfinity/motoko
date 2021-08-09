pkgs:
{ drun =
    pkgs.rustPlatform.buildRustPackage {
      name = "drun";

      src = pkgs.sources.ic + "/rs";

      # update this after dependency changes
      cargoSha256 = "0fnqgvqiv8lb8d0280kyhrml9fpmgw7mvvlhgkp4mxl3y77r0r0p";

      buildInputs = with pkgs; [
        openssl
        openssl.dev
        pkg-config
        cmake
        llvm_10
        llvmPackages_10.libclang
        lmdb
      ];

      PKG_CONFIG = "${pkgs.pkg-config}/bin/pkg-config";

      #OPENSSL_LIB_DIR = "${pkgs.openssl.out}/lib";
      # OPENSSL_INCLUDE_DIR = "${pkgs.openssl.dev}/include";

      # needed for bindgen
      LIBCLANG_PATH = "${pkgs.llvmPackages_10.libclang.lib}/lib";
      CLANG_PATH = "${pkgs.llvmPackages_10.clang}/bin/clang";

      # needed for protobuf
      PROTOC="${pkgs.protobuf}/bin/protoc";

      doCheck = false;

      buildAndTestSubdir = "drun";
    };
}
