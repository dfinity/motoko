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
          "build-info-0.0.27" = "sha256-SkwWwDNrTsntkNiCv6rsyTFGazhpRDnKtVzPpYLKF9U=";
          "cloudflare-0.11.0" = "sha256-bJYiypmDI4KEy/VWt/7UcOv+g2CZLb9qUA9c1xlLxhM=";
          "ic-agent-0.36.0" = "sha256-vDONIVjz0cwVgiszVRIjTKcqRUMHdVwTURflAMqmzHM=";
          "icrc1-test-env-0.1.1" = "sha256-2PB7e64Owin/Eji3k8UoeWs+pfDfOOTaAyXjvjOZ/4g=";
          "jsonrpc-0.12.1" = "sha256-3FtdZlt2PqVDkE5iKWYIp1eiIELsaYlUPRSP2Xp8ejM=";
          "lmdb-rkv-0.14.99" = "sha256-5WcUzapkrc/s3wCBNCuUDhtbp17n67rTbm2rx0qtITg=";
        };
      };

      patchPhase = ''
        cd ../cargo-vendor-dir
        patch librocksdb-sys*/build.rs << EOF
@@ -249,6 +249,9 @@ fn build_rocksdb() {
         config.flag("-Wno-missing-field-initializers");
         config.flag("-Wno-strict-aliasing");
         config.flag("-Wno-invalid-offsetof");
+        if target.contains("darwin") {
+            config.flag("-faligned-allocation");
+        }    
     }

     for file in lib_sources {
EOF
        cd -

        # Disable DTS for `drun`
        patch rs/config/src/subnet_config.rs << EOF
@@ -290,9 +290,9 @@ impl SchedulerConfig {
     }
 
     pub fn system_subnet() -> Self {
-        let max_instructions_per_message_without_dts = NumInstructions::from(50 * B);
+        let max_instructions_per_message_without_dts =
+            MAX_INSTRUCTIONS_PER_MESSAGE_WITHOUT_DTS * SYSTEM_SUBNET_FACTOR;
         let max_instructions_per_install_code = NumInstructions::from(1_000 * B);
-        let max_instructions_per_slice = NumInstructions::from(10 * B);
         Self {
             scheduler_cores: NUMBER_OF_EXECUTION_THREADS,
             max_paused_executions: MAX_PAUSED_EXECUTIONS,
@@ -300,20 +300,19 @@ impl SchedulerConfig {
             // TODO(RUN-993): Enable heap delta rate limiting for system subnets.
             // Setting initial reserve to capacity effectively disables the rate limiting.
             heap_delta_initial_reserve: SUBNET_HEAP_DELTA_CAPACITY,
-            // Round limit is set to allow on average 2B instructions.
-            // See also comment about \`MAX_INSTRUCTIONS_PER_ROUND\`.
-            max_instructions_per_round: max_instructions_per_message_without_dts
-                .max(max_instructions_per_slice)
-                + NumInstructions::from(2 * B),
+            max_instructions_per_round: MAX_INSTRUCTIONS_PER_ROUND * SYSTEM_SUBNET_FACTOR,
+            // Effectively disable DTS on system subnets.
             max_instructions_per_message: max_instructions_per_message_without_dts,
             max_instructions_per_message_without_dts,
-            max_instructions_per_slice,
+            // Effectively disable DTS on system subnets.
+            max_instructions_per_slice: max_instructions_per_message_without_dts,
             instruction_overhead_per_execution: INSTRUCTION_OVERHEAD_PER_EXECUTION,
             instruction_overhead_per_canister: INSTRUCTION_OVERHEAD_PER_CANISTER,
             instruction_overhead_per_canister_for_finalization:
                 INSTRUCTION_OVERHEAD_PER_CANISTER_FOR_FINALIZATION,
             max_instructions_per_install_code,
-            max_instructions_per_install_code_slice: max_instructions_per_slice,
+            // Effectively disable DTS on system subnets.
+            max_instructions_per_install_code_slice: max_instructions_per_install_code,
             max_heap_delta_per_iteration: MAX_HEAP_DELTA_PER_ITERATION * SYSTEM_SUBNET_FACTOR,
             max_message_duration_before_warn_in_seconds:
                 MAX_MESSAGE_DURATION_BEFORE_WARN_IN_SECONDS,
EOF

        # static linking of libunwind fails under nix Linux
        patch rs/monitoring/backtrace/build.rs << EOF
@@ -1,8 +1,2 @@
 fn main() {
-    if std::env::var("TARGET").unwrap() == "x86_64-unknown-linux-gnu" {
-        println!("cargo:rustc-link-lib=static=unwind");
-        println!("cargo:rustc-link-lib=static=unwind-ptrace");
-        println!("cargo:rustc-link-lib=static=unwind-x86_64");
-        println!("cargo:rustc-link-lib=dylib=lzma");
-    }
 }
EOF

        mkdir -p .cargo
        cat > .cargo/config.toml << EOF
[target.x86_64-apple-darwin]
rustflags = [ "-C", "linker=c++" ]

[target.aarch64-apple-darwin]
rustflags = [ "-C", "linker=c++" ]
EOF
      '';

      nativeBuildInputs = with pkgs; [
        pkg-config
        cmake
      ];

      buildInputs = with pkgs; [
        openssl
        llvm_18
        llvmPackages_18.libclang
        lmdb
        libunwind
        libiconv
      ] ++ pkgs.lib.optional pkgs.stdenv.isDarwin
        pkgs.darwin.apple_sdk.frameworks.Security;

      # needed for bindgen
      LIBCLANG_PATH = "${pkgs.llvmPackages_18.libclang.lib}/lib";
      CLANG_PATH = "${pkgs.llvmPackages_18.clang}/bin/clang";

      # needed for ic-protobuf
      PROTOC="${pkgs.protobuf}/bin/protoc";

      doCheck = false;

      buildAndTestSubdir = "rs/drun";
    };
}
