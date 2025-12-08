pkgs: moc: let
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
        "ic-doge-interface-0.1.0" = "sha256-08SglLhGTo1h9AZzl3x56wwYrNjaYhfBlluMpIW8z/8=";
        "ic-gateway-0.2.0" = "sha256-0XdBR0GwvJrPbz/nesY2T41JmqHHGMPj8J+OgZA0N7k=";
        "ic-http-gateway-0.3.0" = "sha256-QwWDtvOkkQ9DvBUrvyVPheLNGMRBX3RkgruSffYb3Kc=";
        "icrc1-test-env-0.1.1" = "sha256-2PB7e64Owin/Eji3k8UoeWs+pfDfOOTaAyXjvjOZ/4g=";
        "lmdb-rkv-0.14.99" = "sha256-0wbrIN5mQVZMuuzMNndC0i7wlqfpVI4TJWV6zLAN4iM=";
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
    '';
    nativeBuildInputs = with pkgs; [
      pkg-config
      cmake
    ];
    buildInputs = [
      moc
      pkgs.openssl
      pkgs.llvm_19
      pkgs.llvmPackages_19.libclang
      pkgs.lmdb
      pkgs.libunwind
      pkgs.libiconv
    ] ++ pkgs.lib.optional pkgs.stdenv.isDarwin
      pkgs.darwin.apple_sdk.frameworks.Security;
    LIBCLANG_PATH = "${pkgs.llvmPackages_19.libclang.lib}/lib";
    CLANG_PATH = "${pkgs.llvmPackages_19.clang}/bin/clang";
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
