{ system ? builtins.currentSystem,
}:
(import ./default.nix { inherit system; export-shell = true; }).shell

