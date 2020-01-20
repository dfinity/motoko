{ system ? builtins.currentSystem,
}:
let
  ic-stub = (import ../default.nix {inherit system;}).ic-stub;

  nixpkgs = import ../nix { inherit system; };

  extra-pkgs = [
    nixpkgs.haskellPackages.cabal-install
    nixpkgs.haskellPackages.ghcid
  ];
in
  ic-stub.env.overrideAttrs (old: {
    nativeBuildInputs = (old.nativeBuildInputs or []) ++ extra-pkgs ;
  })
