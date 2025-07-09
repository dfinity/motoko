pkgs: let
  # Common attributes for both packages
  commonAttrs = {
    pname = "pocket-ic";
    version = builtins.substring 0 7 pkgs.sources.pocket-ic-src.rev;
    src = pkgs.sources.pocket-ic-src;
    cargoLock = {
      lockFile = "${pkgs.sources.pocket-ic-src}/Cargo.lock";
      outputHashes = {
        "build-info-0.0.27" = "sha256-SkwWwDNrTsntkNiCv6rsyTFGazhpRDnKtVzPpYLKF9U=";
        "cloudflare-0.12.0" = "sha256-67kQWJFRXZXHx+qwlyLa9NLF09b/4iRWxTLzCniCHZE=";
        "ic-bn-lib-0.1.0" = "sha256-6ABcruF7vuvTQkhGWdxUcSdO+qgOHKY1MhQRADHtJYw=";
        "ic-canister-sig-creation-1.1.0" = "sha256-c47Fh4kZbmezWCYVHMci2BMXJfESaOGsyNlWh8YR6oU=";
        "ic-gateway-0.2.0" = "sha256-ksiSqBLmnAIAtqvzuHIsyi2FrXs5rwo00n+xto97ctI=";
        "ic-vetkeys-0.1.0" = "sha256-h8Jso21fK0QTyuSa14iAYsDvrfMphvI3spJBYOLgq1c=";
        "icrc1-test-env-0.1.1" = "sha256-2PB7e64Owin/Eji3k8UoeWs+pfDfOOTaAyXjvjOZ/4g=";
        "lmdb-rkv-0.14.99" = "sha256-5WcUzapkrc/s3wCBNCuUDhtbp17n67rTbm2rx0qtITg=";
      };
    };
    patchPhase = ''
      mkdir -p .cargo
      cat > .cargo/config.toml << EOF
      [target.x86_64-apple-darwin]
      rustflags = [ "-C", "linker=c++" ]

      [target.aarch64-apple-darwin]
      rustflags = [ "-C", "linker=c++" ]
      EOF

      # Create the patch file
      echo "Creating patch file..."
      echo 'diff --git a/rs/pocket_ic_server/src/pocket_ic.rs b/rs/pocket_ic_server/src/pocket_ic.rs' > pocket_ic_server.patch
      echo 'index 9c32fb7d2a..453f44a4d3 100644' >> pocket_ic_server.patch
      echo '--- a/rs/pocket_ic_server/src/pocket_ic.rs' >> pocket_ic_server.patch
      echo '+++ b/rs/pocket_ic_server/src/pocket_ic.rs' >> pocket_ic_server.patch
      echo '@@ -512,6 +512,12 @@ impl PocketIcSubnets {' >> pocket_ic_server.patch
      echo '             .embedders_config' >> pocket_ic_server.patch
      echo '             .feature_flags' >> pocket_ic_server.patch
      echo '             .rate_limiting_of_debug_prints = FlagStatus::Disabled;' >> pocket_ic_server.patch
      echo '+' >> pocket_ic_server.patch
      echo '+        hypervisor_config' >> pocket_ic_server.patch
      echo '+            .embedders_config' >> pocket_ic_server.patch
      echo '+            .feature_flags' >> pocket_ic_server.patch
      echo '+            .canister_backtrace = FlagStatus::Disabled;' >> pocket_ic_server.patch
      echo '+' >> pocket_ic_server.patch
      echo '         let state_machine_config = StateMachineConfig::new(subnet_config, hypervisor_config);' >> pocket_ic_server.patch
      echo '         let t = time' >> pocket_ic_server.patch
      echo '             .duration_since(SystemTime::UNIX_EPOCH)' >> pocket_ic_server.patch

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

  # Server package
  server = pkgs.rustPlatform-stable.buildRustPackage (commonAttrs // {
    buildAndTestSubdir = "rs/pocket_ic_server";
  });

  # Library package
  library = pkgs.rustPlatform-stable.buildRustPackage (commonAttrs // {
    buildAndTestSubdir = "packages/pocket-ic";
    cargoBuildFlags = [ "-p" "pocket-ic" "--lib" ];
    installPhase = ''
      mkdir -p $out/lib
      # Get the target triple from the build environment
      TARGET_TRIPLE=$(rustc --version --verbose | grep "host:" | cut -d' ' -f2)
      cp target/$TARGET_TRIPLE/release/libpocket_ic* $out/lib/ || true
    '';
  });

in {
  inherit server library;
} 