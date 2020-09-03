{ system ? builtins.currentSystem }:
let
  sourcesnix = builtins.fetchurl {
    url = https://raw.githubusercontent.com/nmattia/niv/v0.2.16/nix/sources.nix;
    sha256 = "03fl8wfm2nhdiws7pmfz2kcbf47mv2f8gk30fzg4m07gb5zdv6gv";
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

        # Selecting the ocaml version
        # (self: super: { ocamlPackages = super.ocamlPackages; })
        (
          self: super: {
            # Additional ocaml package
            ocamlPackages = super.ocamlPackages // {
              wasm = import ./ocaml-wasm.nix {
                inherit (self) stdenv fetchFromGitHub ocaml;
                inherit (self.ocamlPackages) findlib ocamlbuild;
              };
              vlq = import ./ocaml-vlq.nix {
                inherit (self) stdenv fetchFromGitHub ocaml dune;
                inherit (self.ocamlPackages) findlib;
              };
              obelisk = import ./ocaml-obelisk.nix {
                inherit (self) stdenv fetchFromGitHub ocaml dune_2;
                inherit (self) ocamlPackages;
              };
            };
            wasmtime = self.callPackage ./wasmtime.nix {};
            xargo = self.callPackage ./xargo.nix {};
          }
        )

        # rust nightly
        (self: super: let
          moz_overlay = import self.sources.nixpkgs-mozilla self super;
          rust-channel = moz_overlay.rustChannelOf { date = "2020-07-22"; channel = "nightly"; };
        in rec {
          rustc-nightly = rust-channel.rust.override {
            targets = [ "wasm32-unknown-unknown" "wasm32-unknown-emscripten" ];
            extensions = ["rust-src"];
          };
          cargo-nightly = rustc-nightly;
          rustPlatform-nightly = pkgs.makeRustPlatform {
            rustc = rustc-nightly;
            cargo = cargo-nightly;
          };
        })

      ];
    };
in
pkgs
