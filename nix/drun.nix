pkgs:
{ drun =
    pkgs.rustPlatform.buildRustPackage {
      name = "drun";

      src = pkgs.sources.ic + "/rs";

      # update this after bumping the dfinity/ic pin.
      # 1. change the hash to something arbitrary (e.g. flip one digit to 0)
      # 2. run nix-build -A drun nix/
      # 3. copy the “expected” hash from the output into this file
      # 4. commit and push

      # To automate this, try running the following in the nix/ directory
      # nix run -f https://github.com/Mic92/nix-update/archive/master.tar.gz -c nix-update --version=skip drun

      cargoSha256 = "0656lxdlr05cjkla1blvpqlxywk7shasiwmycz10nqykdrs4gfgf";

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
