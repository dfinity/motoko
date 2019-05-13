let
  nixpkgs = (import ./nix/nixpkgs.nix).nixpkgs { };
  nixpkgs-linux = (import ./nix/nixpkgs.nix).nixpkgs { system = "x86_64-linux"; };
  nixpkgs-darwin = (import ./nix/nixpkgs.nix).nixpkgs { system = "x86_64-darwin"; };
in
rec {
  linux = import ./default.nix { nixpkgs = nixpkgs-linux; };
  darwin = import ./default.nix { nixpkgs = nixpkgs-darwin; };
  all-systems-go = nixpkgs.releaseTools.aggregate {
    name = "all-systems-go";
    constituents = [
      linux.all-systems-go
      darwin.all-systems-go
    ];
  };
}
