{ src ? { rev = null; }, labels ? {}, releaseVersion ? "latest" }:
let
  nixpkgs = import ./nix { };
  inject-rev = drv: drv.overrideAttrs (attrs: { rev = src.rev; });
  removeRecurseForDerivations = nixpkgs.lib.filterAttrsRecursive (k: v: k != "recurseForDerivations");

  linux = removeRecurseForDerivations (import ./default.nix { system = "x86_64-linux"; });
  darwin = removeRecurseForDerivations (import ./default.nix { system = "x86_64-darwin"; });

  all-systems-go =
    nixpkgs.releaseTools.aggregate {
      name = "all-systems-go";
      constituents = [
        linux.all-systems-go
        darwin.all-systems-go
      ];
    };
in
linux // {
  darwin = darwin.all-systems-go;
  all-systems-go = inject-rev all-systems-go;
}
