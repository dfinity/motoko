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

        # wasmtime not broken
        # (was marked broken on darwin in https://github.com/NixOS/nixpkgs/pull/173671)
        (self: super: {
           wasmtime = super.wasmtime.overrideAttrs (o: { meta = o.meta // { broken = false; };});
        })

        # Selecting the ocaml version
        # Also update ocmal-version in src/*/.ocamlformat!
        (self: super: { ocamlPackages = self.ocaml-ng.ocamlPackages_4_12; })

        (
          self: super: {
            # Additional ocaml package
            ocamlPackages = super.ocamlPackages // {
              obelisk = import ./ocaml-obelisk.nix {
                inherit (self) lib fetchFromGitHub ocaml dune_2;
                inherit (self) ocamlPackages;
                inherit (self.stdenv) mkDerivation;
              };

              # downgrade wasm until we have support for 2.0.0
              # (https://github.com/dfinity/motoko/pull/3364)
              wasm = super.ocamlPackages.wasm.overrideAttrs (_: rec {
                version = "1.1.1";
                src = self.fetchFromGitHub {
                  owner = "WebAssembly";
                  repo = "spec";
                  rev = "opam-${version}";
                  sha256 = "1kp72yv4k176i94np0m09g10cviqp2pnpm7jmiq6ik7fmmbknk7c";
                };
              });
            };
          }
        )

        # Mozilla overlay
        (self: super:
          { moz_overlay = import self.sources.nixpkgs-mozilla self super; }
        )

        # Rust nightly
        (self: super: let
          rust-channel = self.moz_overlay.rustChannelOf { date = "2022-06-30"; channel = "nightly"; };
        in rec {
          rustc-nightly = rust-channel.rust.override {
            targets = [
               "wasm32-unknown-emscripten"
               "wasm32-wasi"
               "i686-unknown-linux-gnu"
            ];
            extensions = ["rust-src"];
          };
          cargo-nightly = rustc-nightly;
          rustPlatform-nightly = self.makeRustPlatform {
            rustc = rustc-nightly;
            cargo = cargo-nightly;
          };
        })

        # Rust 1.62
        (self: super: let
          rust-channel = self.moz_overlay.rustChannelOf { date = "2022-06-30"; channel = "stable"; };
        in {
          rustPlatform_moz_stable = self.makeRustPlatform {
            rustc = rust-channel.rust;
            cargo = rust-channel.rust;
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
