{ nixpkgs ? (import ../nix/nixpkgs.nix).nixpkgs {} }:

let
  ic-stub = (import ../default.nix {inherit nixpkgs;}).ic-stub;

  extra-pkgs = [
    nixpkgs.haskellPackages.cabal-install
    nixpkgs.haskellPackages.ghcid
  ];
in
  ic-stub.env.overrideAttrs (old: {
    nativeBuildInputs = (old.nativeBuildInputs or []) ++ extra-pkgs ;
  })
