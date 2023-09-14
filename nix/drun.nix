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

      cargoLock = {
        lockFile = "${pkgs.sources.ic}/Cargo.lock";
        outputHashes = {
          "derive_more-0.99.8-alpha.0" = "sha256-tEsfYC9oCAsDjinCsUDgRg3q6ruvayuA1lRmsEP9cys=";
          "ic-btc-interface-0.1.0" = "sha256-7X82NzllKEpXU+fOtc4GiGwnKNCB6DZZf7MFftnr5cI=";
          "ic-btc-test-utils-0.1.0" = "sha256-VecEMFjoeiRi0VgJ9CeDoOzdyJbJNiZ5MBmiV1+b7As=";
          "ic-btc-validation-0.1.0" = "sha256-DRrZ0MX6Qykh+IbEpmWPYvT5XxsiED0jPp6l0Ci+Leo=";
          "icrc1-test-env-0.1.0" = "sha256-+R9mClFb7I7dSYbKHnDVwyTN4QrMRDxiDNx/smDJe3g=";
          "jsonrpc-0.12.1" = "sha256-3FtdZlt2PqVDkE5iKWYIp1eiIELsaYlUPRSP2Xp8ejM=";
          "libssh2-sys-0.2.23" = "sha256-9Hb7CnPF+lxrVO1NAhS7EXcPVWZutJXr6UWxpptzk4U=";
          "lmdb-rkv-0.14.99" = "sha256-dqxWgtNZ/qRukQLiDawjHRwdPgfzm/sVcWwudwX9rD8=";
          "pprof-0.10.1" = "sha256-ioZ8AyFTUUev8MDZapto0yXC6G+dZzg+7ZNtTR87Rg4=";
        };
      };

      patchPhase = ''
pwd
        cd ../cargo-vendor-dir
        patch librocksdb-sys*/build.rs << EOF
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
