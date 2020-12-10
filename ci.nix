{ src ? { rev = null; }, labels ? {}, releaseVersion ? "latest" }:
let
  nixpkgs = import ./nix { };
  inject-rev = drv: drv.overrideAttrs (attrs: { rev = src.rev; });

  linux = import ./default.nix { system = "x86_64-linux"; };
  darwin = import ./default.nix { system = "x86_64-darwin"; };

  release = import ./nix/publish.nix
    { pkgs = nixpkgs;
      inherit releaseVersion;
      derivations = {
        linux = with linux; [ mo-ide mo-doc moc ];
        darwin = with darwin; [ mo-ide mo-doc moc ];
        js = with linux; [ js.moc js.moc_interpreter ];
      };
    };


  all-systems-go =
    nixpkgs.releaseTools.aggregate {
      name = "all-systems-go";
      constituents = [
        release.motoko
        linux.all-systems-go
        darwin.all-systems-go
      ];
    };
in
linux // {
  inherit release;
  darwin = darwin.all-systems-go;
  all-systems-go = inject-rev all-systems-go;
}
