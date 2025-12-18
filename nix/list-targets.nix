# Run: nix-instantiate --eval --strict --json ./targets.nix | jq ...
let
  pkgs = import (builtins.getFlake "nixpkgs") {};
  inherit (pkgs) lib;

  motoko = builtins.getFlake (toString ../.);
  system = pkgs.stdenv.hostPlatform.system;
in
lib.map (lib.removePrefix (system + "."))
  (lib.filter (lib.hasPrefix system)
    (lib.mapAttrsToListRecursiveCond
      (path: as: !(lib.isDerivation as))
      (path: _value: lib.concatStringsSep "." path)
      motoko.packages))
