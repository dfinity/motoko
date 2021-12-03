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

  nixpkgs-patched = bootstrap-pkgs.applyPatches {
    name = "nixpkgs-patched";
    src = nixpkgs_src;
    # patches = [
    # ];
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
        # (self: super: { ocamlPackages = super.ocamlPackages; })

        (
          self: super: {
            # Additional ocaml package
            ocamlPackages = super.ocamlPackages // {
              obelisk = import ./ocaml-obelisk.nix {
                inherit (self) lib fetchFromGitHub ocaml dune_2;
                inherit (self) ocamlPackages;
                inherit (self.stdenv) mkDerivation;
              };
            };
          }
        )

        # Rust nightly
        (self: super: let
          moz_overlay = import self.sources.nixpkgs-mozilla self super;
          rust-channel = moz_overlay.rustChannelOf { date = "2021-12-02"; channel = "nightly"; };
        in rec {
          rustc-nightly = rust-channel.rust.override {
            targets = [
               "wasm32-unknown-unknown"
               "wasm32-unknown-emscripten"
               "wasm32-wasi"
               "i686-unknown-linux-gnu"
            ];
            extensions = ["rust-src"];
          };
          cargo-nightly = rustc-nightly;
          rustPlatform-nightly = pkgs.makeRustPlatform {
            rustc = rustc-nightly;
            cargo = cargo-nightly;
          };
        })

        # wasm-profiler
        (self: super: import ./wasm-profiler.nix self)

        # drun
        (self: super: import ./drun.nix self)

        # to allow picking up more recent Haskell packages from Hackage
        (self: super: {
          all-cabal-hashes = self.fetchFromGitHub {
            owner = "commercialhaskell";
            repo = "all-cabal-hashes";
            rev = "d859530d8342c52d09a73d1d125c144725b5945d";
            sha256 = "1y6cgphxkdcvhmdddc4ssrpq1j1j9y7xhra9k668bzbi4na0g4yz";
          };
        })

      ];
    };
in
pkgs
