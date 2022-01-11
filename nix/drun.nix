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
      #
      # To automate this, .github/workflows/update-hash.yml has been
      # installed. You will normally not be bothered to perform
      # the command therein manually.

      cargoSha256 = "sha256-VmZIo60PAPgHrs95B1dqzTylTa1+yu+LvuyELFcejqI=";

      nativeBuildInputs = with pkgs; [
        pkg-config
        cmake
      ];

      buildInputs = with pkgs; [
        openssl
        llvm_13
        llvmPackages_13.libclang
        lmdb
      ] ++ pkgs.lib.optionals pkgs.stdenv.isDarwin [
        pkgs.darwin.apple_sdk.frameworks.Security
      ];

      # needed for bindgen
      LIBCLANG_PATH = "${pkgs.llvmPackages_13.libclang.lib}/lib";
      CLANG_PATH = "${pkgs.llvmPackages_13.clang}/bin/clang";

      # needed for ic-protobuf
      PROTOC="${pkgs.protobuf}/bin/protoc";

      doCheck = false;

      buildAndTestSubdir = "drun";
    };
}
