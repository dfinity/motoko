pkgs: let
  # Common attributes for both packages
  commonAttrs = {
    pname = "pocket-ic";
    version = builtins.substring 0 7 pkgs.sources.pocket-ic-src.rev;
    src = pkgs.sources.pocket-ic-src;
    cargoLock = {
      lockFile = "${pkgs.sources.pocket-ic-src}/Cargo.lock";
      outputHashes = {
        "bitcoin-0.32.5-doge.0" = "sha256-DK+TKcY2c5sNI5q+DDddJ07UUOBBqiMAZ5sMV+fHoYA=";
        "build-info-0.0.27" = "sha256-SkwWwDNrTsntkNiCv6rsyTFGazhpRDnKtVzPpYLKF9U=";
        "cloudflare-0.12.0" = "sha256-67kQWJFRXZXHx+qwlyLa9NLF09b/4iRWxTLzCniCHZE=";
        "ic-bn-lib-0.1.0" = "sha256-6ABcruF7vuvTQkhGWdxUcSdO+qgOHKY1MhQRADHtJYw=";
        "ic-gateway-0.2.0" = "sha256-ksiSqBLmnAIAtqvzuHIsyi2FrXs5rwo00n+xto97ctI=";
        "icrc1-test-env-0.1.1" = "sha256-2PB7e64Owin/Eji3k8UoeWs+pfDfOOTaAyXjvjOZ/4g=";
        "lmdb-rkv-0.14.99" = "sha256-5WcUzapkrc/s3wCBNCuUDhtbp17n67rTbm2rx0qtITg=";
      };
    };
    patchPhase = ''
      # Apply RocksDB patch for Darwin.
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

      mkdir -p .cargo
      cat > .cargo/config.toml << EOF
      [target.x86_64-apple-darwin]
      rustflags = [ "-C", "linker=c++", "-C", "opt-level=0" ]

      [target.aarch64-apple-darwin]
      rustflags = [ "-C", "linker=c++" ]
      EOF

      # Create a patch file for pocket-ic-server to disable canister backtrace.
      echo "Creating patch file..."
      echo 'diff --git a/rs/pocket_ic_server/src/pocket_ic.rs b/rs/pocket_ic_server/src/pocket_ic.rs' > pocket_ic_server.patch
      echo '--- a/rs/pocket_ic_server/src/pocket_ic.rs' >> pocket_ic_server.patch
      echo '+++ b/rs/pocket_ic_server/src/pocket_ic.rs' >> pocket_ic_server.patch
      echo '@@ -619,6 +619,14 @@ impl PocketIcSubnets {' >> pocket_ic_server.patch
      echo '             .embedders_config' >> pocket_ic_server.patch
      echo '             .feature_flags' >> pocket_ic_server.patch
      echo '             .rate_limiting_of_debug_prints = FlagStatus::Disabled;' >> pocket_ic_server.patch
      echo '+        hypervisor_config' >> pocket_ic_server.patch
      echo '+            .embedders_config' >> pocket_ic_server.patch
      echo '+            .feature_flags' >> pocket_ic_server.patch
      echo '+            .canister_backtrace = FlagStatus::Disabled;' >> pocket_ic_server.patch
      echo '+        hypervisor_config' >> pocket_ic_server.patch
      echo '+            .embedders_config' >> pocket_ic_server.patch
      echo '+            .feature_flags' >> pocket_ic_server.patch
      echo '+            .environment_variables = FlagStatus::Enabled;' >> pocket_ic_server.patch
      echo '         let state_machine_config = StateMachineConfig::new(subnet_config, hypervisor_config);' >> pocket_ic_server.patch
      echo '         StateMachineBuilder::new()' >> pocket_ic_server.patch
      echo '             .with_runtime(runtime)' >> pocket_ic_server.patch

      echo "Applying patch..."
      patch -p1 < pocket_ic_server.patch
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
    LIBCLANG_PATH = "${pkgs.llvmPackages_18.libclang.lib}/lib";
    CLANG_PATH = "${pkgs.llvmPackages_18.clang}/bin/clang";
    PROTOC = "${pkgs.protobuf}/bin/protoc";
    doCheck = false;
  };

  # Pocket-ic server package.
  server = pkgs.rustPlatform-stable.buildRustPackage (commonAttrs // {
    buildAndTestSubdir = "rs/pocket_ic_server";
    cargoBuildFlags = [ "--bin" "pocket-ic-server" ];
  });

in {
  inherit server;
} 