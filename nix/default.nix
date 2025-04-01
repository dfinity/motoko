{ system ? builtins.currentSystem }:
let
  sourcesnix = builtins.fetchurl {
    url = https://raw.githubusercontent.com/nmattia/niv/v0.2.19/nix/sources.nix;
    sha256 = "1n92ka2rkdiib6ian6jh2b7fwvklnnwlp5yy5bv6ywm7m1y5hyfl";
  };
  nixpkgs_src = (import sourcesnix { sourcesFile = ./sources.json; inherit pkgs; }).nixpkgs;

  bootstrap-pkgs = import nixpkgs_src {
    system = builtins.currentSystem;
  };

  # dump nixpkgs patches here
  nixpkgs-patches = [ ];

  nixpkgs-patched =
    if nixpkgs-patches == []
    then nixpkgs_src
    else
      let
        bootstrap-pkgs = import nixpkgs_src {
          system = builtins.currentSystem;
        };
      in bootstrap-pkgs.applyPatches {
        name = "nixpkgs-patched";
        src = nixpkgs_src;
        patches = nixpkgs-patches;
      };

  pkgs =
    import nixpkgs-patched {
      inherit system;
      overlays = [
        # add nix/sources.json
        (self: super: {
           sources = import sourcesnix { sourcesFile = ./sources.json; pkgs = super; };
        })

        # Selecting the ocaml version
        # Also update ocaml-version in src/*/.ocamlformat!
        (self: super: { ocamlPackages = self.ocaml-ng.ocamlPackages_4_14; })

        (self: super: {
            # Additional ocaml package
            ocamlPackages = super.ocamlPackages // rec {

              # downgrade wasm until we have support for 2.0.1
              # (https://github.com/dfinity/motoko/pull/3364)
              wasm_1 = super.ocamlPackages.wasm.overrideAttrs rec {
                version = "1.1.1";
                src = self.fetchFromGitHub {
                  owner = "WebAssembly";
                  repo = "spec";
                  rev = "opam-${version}";
                  sha256 = "1kp72yv4k176i94np0m09g10cviqp2pnpm7jmiq6ik7fmmbknk7c";
                };
                patchPhase = ''
                  substituteInPlace ./interpreter/Makefile \
                    --replace-fail "+a-4-27-42-44-45" "+a-4-27-42-44-45-70"
                '';
              };

              # No testing of atdgen, as it pulls in python stuff, tricky on musl
              atdgen = super.ocamlPackages.atdgen.overrideAttrs { doCheck = false; };
            };
          }
        )

        # Wasmtime overlay
        (self: super: {
          wasmtime =
            self.rustPlatform_moz_stable.buildRustPackage rec {
              pname = "wasmtime";
              version = "26.0.1";

              src = super.fetchFromGitHub {
                owner = "bytecodealliance";
                repo = pname;
                rev = "v${version}";
                hash = "sha256-Q7f35Y3ZZ7BHLwmdsa0I5gtlNMObscVD/3jKrVetGnA=";
                fetchSubmodules = true;
              };

              auditable = false;
              cargoHash = "sha256-kaE+LoqnWPZcM9H5FM7SRPRq2J78yrL5zWdV2klVLDU=";
              cargoBuildFlags = [ "--package" "wasmtime-cli" "--package" "wasmtime-c-api" ];

              outputs = [ "out" "dev" ];

              buildInputs = [] ; #super.lib.optional super.stdenv.hostPlatform.isDarwin Security;

              # rustfmt is brought into scope to fix the following
              #   warning: cranelift-codegen@0.108.0:
              #   Failed to run `rustfmt` on ISLE-generated code: Os
              #   { code: 2, kind: NotFound, message: "No such file or directory" }
              nativeBuildInputs = with super; [ cmake rustfmt ];

              doCheck = with super.stdenv.buildPlatform;
                # SIMD tests are only executed on platforms that support all
                # required processor features (e.g. SSE3, SSSE3 and SSE4.1 on x86_64):
                # https://github.com/bytecodealliance/wasmtime/blob/v9.0.0/cranelift/codegen/src/isa/x64/mod.rs#L220
                (isx86_64 -> sse3Support && ssse3Support && sse4_1Support) &&
                # The dependency `wasi-preview1-component-adapter` fails to build because of:
                # error: linker `rust-lld` not found
                !isAarch64;

              postInstall = ''
                # move libs from out to dev
                install -d -m 0755 $dev/lib
                install -m 0644 ''${!outputLib}/lib/* $dev/lib
                rm -r ''${!outputLib}/lib

                install -d -m0755 $dev/include/wasmtime
                install -m0644 $src/crates/c-api/include/*.h $dev/include
                install -m0644 $src/crates/c-api/include/wasmtime/*.h $dev/include/wasmtime
              '' + super.lib.optionalString super.stdenv.hostPlatform.isDarwin ''
                install_name_tool -id \
                  $dev/lib/libwasmtime.dylib \
                  $dev/lib/libwasmtime.dylib
              '';

              meta = with super.lib; {
                description =
                  "Standalone JIT-style runtime for WebAssembly, using Cranelift";
                homepage = "https://wasmtime.dev/";
                license = licenses.asl20;
                mainProgram = "wasmtime";
                maintainers = with maintainers; [ ereslibre matthewbauer ];
                platforms = platforms.unix;
                changelog = "https://github.com/bytecodealliance/wasmtime/blob/v${version}/RELEASES.md";
              };
            };
          }
        )

        # Mozilla overlay
        (self: super:
          { moz_overlay = import self.sources.nixpkgs-mozilla self super; }
        )

        # Rust nightly
        (self: super: let
          rust-channel = self.moz_overlay.rustChannelOf { date = "2025-03-21"; channel = "nightly"; };
        in rec {
          rustc-nightly = rust-channel.rust.override {
            targets = [
               "wasm32-wasip1"
            ];
            extensions = ["rust-src"];
          };
          cargo-nightly = rustc-nightly;
          rustPlatform-nightly = self.makeRustPlatform rec {
            rustc = rust-channel.rust;
            cargo = rustc;
          };
        })

        # Rust stable
        (self: super: let
          rust-channel = self.moz_overlay.rustChannelOf { version = "1.85.0"; channel = "stable"; };
        in {
          rustPlatform_moz_stable = self.makeRustPlatform rec {
            rustc = rust-channel.rust;
            cargo = rustc;
          };
        })

        # wasm-profiler
        (self: super: import ./wasm-profiler.nix self)

        # drun
        (self: super: import ./drun.nix self)

        # to allow picking up more recent Haskell packages from Hackage
        # don't use `fetchFromGitHub` here as we really need an intact tarball
        (self: super: {
          all-cabal-hashes = self.fetchurl {
            url = "https://github.com/commercialhaskell/all-cabal-hashes/archive/d859530d8342c52d09a73d1d125c144725b5945d.tar.gz";
            sha256 = "0gjahsqqq99dc4bjcx9p3z8adpwy51w3mzrf57nib856jlvlfmv5";
          };
        })
      ];
    };
in
pkgs
