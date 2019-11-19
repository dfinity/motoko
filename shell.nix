{ nixpkgs ? (import ./nix/nixpkgs.nix).nixpkgs {},
}:
(import ./default.nix { inherit nixpkgs; }).shell

