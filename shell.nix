{ system ? null,
}:
(import ./default.nix { inherit system; export-shell = true; }).shell

