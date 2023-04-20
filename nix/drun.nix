pkgs:
{ drun =
    pkgs.rustPlatform_moz_stable.buildRustPackage {
      name = "drun";

      src = pkgs.sources.ic;

      # update this after bumping the dfinity/ic pin.
      # 1. change the hash to something arbitrary (e.g. flip one digit to 0 or use `pkgs.lib.fakeSha256`)
      # 2. run nix-build -A drun nix/
      # 3. copy the “expected” hash from the output into this file
      # 4. commit and push
      #
      # To automate this, .github/workflows/update-hash.yml has been
      # installed. You will normally not be bothered to perform
      # the command therein manually.

      cargoSha256 = "sha256-WkIe1mFxPWkxgzG94FJ9XIyh8J9WSZ5kK4yaVw39lUA=";

      patchPhase = ''
        # for some reason ic-btc-validation tries to reach out
        # into the web, so simply remove it
        cargo remove --package ic-btc-adapter ic-btc-validation

        substituteInPlace .cargo/config.toml \
          --replace "linker = \"clang\"" "linker = \"$CLANG_PATH\"" \
          --replace ", \"link-arg=-fuse-ld=/usr/bin/mold\"" ""

        cd ../drun-vendor.tar.gz
        patch librocksdb-sys/build.rs << EOF
@@ -118,6 +118,10 @@
         config.define("OS_MACOSX", Some("1"));
         config.define("ROCKSDB_PLATFORM_POSIX", Some("1"));
         config.define("ROCKSDB_LIB_IO_POSIX", Some("1"));
+        if target.contains("aarch64") {
+            config.define("isSSE42()", Some("0"));
+            config.define("isPCLMULQDQ()", Some("0"));
+        }
     } else if target.contains("android") {
         config.define("OS_ANDROID", Some("1"));
         config.define("ROCKSDB_PLATFORM_POSIX", Some("1"));
EOF

        sed -i -e s/08d86b53188dc6f15c8dc09d8aadece72e39f145e3ae497bb8711936a916335a/536e44802de57cc7d3690c90c80f154f770f48e82b82756c36443b8b47c9b5e7/g librocksdb-sys/.cargo-checksum.json
        cd -
      '';

      nativeBuildInputs = with pkgs; [
        pkg-config
        cmake
      ];

      buildInputs = with pkgs; [
        openssl
        llvm_13
        llvmPackages_13.libclang
        lmdb
        libunwind
        libiconv
      ] ++ pkgs.lib.optional pkgs.stdenv.isDarwin
        pkgs.darwin.apple_sdk.frameworks.Security;

      # needed for bindgen
      LIBCLANG_PATH = "${pkgs.llvmPackages_13.libclang.lib}/lib";
      CLANG_PATH = "${pkgs.llvmPackages_13.clang}/bin/clang";

      # needed for ic-protobuf
      PROTOC="${pkgs.protobuf}/bin/protoc";

      doCheck = false;

      buildAndTestSubdir = "rs/drun";
    };
}
