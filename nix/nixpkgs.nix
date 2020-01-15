rec {

  src = (import ./sources.nix).nixpkgs;

  nixpkgs = import src;
}
