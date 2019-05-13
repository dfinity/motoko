{ src ? { rev = null; } }:
let
  nixpkgs = (import ./nix/nixpkgs.nix).nixpkgs { };
  nixpkgs-linux = (import ./nix/nixpkgs.nix).nixpkgs { system = "x86_64-linux"; };
  nixpkgs-darwin = (import ./nix/nixpkgs.nix).nixpkgs { system = "x86_64-darwin"; };

  inject-rev = drv: drv.overrideAttrs (attrs: { rev = src.rev; });
in
rec {
  linux = import ./default.nix { nixpkgs = nixpkgs-linux; };
  darwin = import ./default.nix { nixpkgs = nixpkgs-darwin; };
  all-systems-go = inject-rev (nixpkgs.releaseTools.aggregate {
    name = "all-systems-go";
    constituents = [
      linux.all-systems-go
      darwin.all-systems-go
    ];
  });
}
