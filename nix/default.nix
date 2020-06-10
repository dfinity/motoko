{ system ? builtins.currentSystem }:
let
  sourcesnix = builtins.fetchurl {
    url = https://raw.githubusercontent.com/nmattia/niv/506b896788d9705899592a303de95d8819504c55/nix/sources.nix;
    sha256 = "007bgq4zy1mjnnkbmaaxvvn4kgpla9wkm0d3lfrz3y1pa3wp9ha1";
  };
  nixpkgs_src = (import sourcesnix { sourcesFile = ./sources.json; inherit pkgs; }).nixpkgs;

  pkgs =
    import nixpkgs_src {
      inherit system;
      overlays = [
        (self: super: { sources = import sourcesnix { sourcesFile = ./sources.json; pkgs = super; }; })
        # Selecting the ocaml version
        (self: super: { ocamlPackages = self.ocaml-ng.ocamlPackages_4_09; })
        # Additional ocaml package
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
