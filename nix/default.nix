{ system ? builtins.currentSystem }:
let
  sourcesnix = builtins.fetchurl {
    url = https://raw.githubusercontent.com/nmattia/niv/v0.2.18/nix/sources.nix;
    sha256 = "0vsjk1dj88kb40inlhb9xgfhm5dfhb6g3vyca62glk056sn4504l";
  };
  nixpkgs_src = (import sourcesnix { sourcesFile = ./sources.json; inherit pkgs; }).nixpkgs;

  pkgs =
    import nixpkgs_src {
      inherit system;
      overlays = [
        # add nix/sources.json
        (self: super: {
           sources = import sourcesnix { sourcesFile = ./sources.json; pkgs = super; };
        })

	# add a newer version of niv
        (self: super: {
           niv = (import self.sources.niv { pkgs = super; }).niv;
        })

        # Selecting the ocaml version
        # (self: super: { ocamlPackages = super.ocamlPackages; })
        (
          self: super: {
            # Additional ocaml package
            ocamlPackages = super.ocamlPackages // {
              vlq = import ./ocaml-vlq.nix {
                inherit (self) stdenv fetchFromGitHub ocaml dune;
                inherit (self.ocamlPackages) findlib;
              };
              obelisk = import ./ocaml-obelisk.nix {
                inherit (self) stdenv fetchFromGitHub ocaml dune_2;
                inherit (self) ocamlPackages;
              };
            };
          }
        )

        # Rust nightly
        (self: super: let
          moz_overlay = import self.sources.nixpkgs-mozilla self super;
          rust-channel = moz_overlay.rustChannelOf { date = "2020-07-22"; channel = "nightly"; };
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
          xargo = self.callPackage ./xargo.nix {};
        })

	# wasm-profiler
	(self: super: import ./wasm-profiler.nix self)

        # to allow picking up more recent Haskell packages from Hackage
        (self: super: {
          all-cabal-hashes = self.fetchurl {
            url = "https://github.com/commercialhaskell/all-cabal-hashes/archive/66a799608f2c6e0e6c530383bc0e2bcb42ae11f2.tar.gz";
            sha256 = "0ds95gacrzsqg5f0f6j533ghxzcqqn7wn1d391pcpj5g9frp01q2";
          };
        })

      ];
    };
in
pkgs
