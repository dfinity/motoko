{ nixpkgs ? (import ./nix/nixpkgs.nix).nixpkgs {},
}:
(import ./default.nix { inherit nixpkgs; export-shell = true; }).shell

