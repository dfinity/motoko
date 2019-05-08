{ nixpkgs ? (import ./nix/nixpkgs.nix).nixpkgs {},
  test-dvm ? true,
}:
(import ./default.nix { inherit nixpkgs test-dvm; export-shell = true; }).shell

