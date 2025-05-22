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
        "ic-bn-lib-0.1.0" = "sha256-40EQ0oKEjcEnAiQhlqveLjQfvloLMLbqeze1T2QhP5M=";
        "ic-canister-sig-creation-1.1.0" = "sha256-c47Fh4kZbmezWCYVHMci2BMXJfESaOGsyNlWh8YR6oU=";
        "icrc1-test-env-0.1.1" = "sha256-2PB7e64Owin/Eji3k8UoeWs+pfDfOOTaAyXjvjOZ/4g=";
        "lmdb-rkv-0.14.99" = "sha256-5WcUzapkrc/s3wCBNCuUDhtbp17n67rTbm2rx0qtITg=";
        "ic-gateway-0.2.0" = "sha256-67nSahWf/IPxmGxhZz5phnZtZUsFyyXx73grkMh6J8E=";
        "tower_governor-0.5.0" = "sha256-YugNEuG1IeK5MuD/falbtFPpNKXHJoo2j7IvT4Ey+nc=";
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