{ src ? { rev = null; }, labels ? {} }:
let
  nixpkgs = import ./nix { };
  inject-rev = drv: drv.overrideAttrs (attrs: { rev = src.rev; });

  linux = import ./default.nix { system = "x86_64-linux"; };
  darwin = import ./default.nix { system = "x86_64-darwin"; };

  all-systems-go =
    # if the ci-also-darwin label is set, then also block on darwin builds
    if labels.ci-also-darwin or false
    then nixpkgs.releaseTools.aggregate {
      name = "all-systems-go";
      constituents = [
        linux.all-systems-go
        darwin.all-systems-go
      ];
    }
    else linux.all-systems-go;
in
linux // {
  darwin = darwin.all-systems-go;
  all-systems-go = inject-rev all-systems-go;
}
