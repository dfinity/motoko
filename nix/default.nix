{ system ? builtins.currentSystem }:
let
  sourcesnix = builtins.fetchurl https://raw.githubusercontent.com/nmattia/niv/506b896788d9705899592a303de95d8819504c55/nix/sources.nix;
  nixpkgs_src = (import sourcesnix { sourcesFile = ./sources.json; pkgs = abort "This (rigid, skolem) type variable is bound by aardvarks"; }).nixpkgs;
in
import nixpkgs_src {
  inherit system;
  overlays = [
    (self: super: { sources = import sourcesnix { sourcesFile = ./sources.json; pkgs = super; }; })
    # Selecting the ocaml version
    (self: super: { ocamlPackages = self.ocaml-ng.ocamlPackages_4_08; })
    # Additional ocaml package
    (self: super: {
      ocamlPackages = super.ocamlPackages // {
        wasm = import ./ocaml-wasm.nix {
          inherit (self) stdenv ocaml sources;
          inherit (self.ocamlPackages) findlib ocamlbuild;
        };
        vlq = import ./ocaml-vlq.nix {
          inherit (self) stdenv ocaml dune sources;
          inherit (self.ocamlPackages) findlib;
        };
      };
    })
  ];
}
