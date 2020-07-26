{ src ? { rev = null; }, labels ? {} }:
let
  nixpkgs = import ./nix { };
  inject-rev = drv: drv.overrideAttrs (attrs: { rev = src.rev; });

  linux = import ./default.nix { system = "x86_64-linux"; };
  darwin = import ./default.nix { system = "x86_64-darwin"; };

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
