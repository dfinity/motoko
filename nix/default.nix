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
        (self: super: { sources = import sourcesnix { sourcesFile = ./sources.json; pkgs = super; }; })
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
            # wasmtime
            wasmtime = self.callPackage ./wasmtime.nix {};
          }
        )
      ];
    };
in
pkgs
