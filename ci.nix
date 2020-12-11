{ src ? { rev = null; }, labels ? {}, releaseVersion ? "latest" }:
let
  nixpkgs = import ./nix { };
  inject-rev = drv: drv.overrideAttrs (attrs: { rev = src.rev; });

  linux = import ./default.nix { system = "x86_64-linux"; };
  darwin = import ./default.nix { system = "x86_64-darwin"; };

  as_tarball = dir: derivations:
    nixpkgs.runCommandNoCC "motoko-${releaseVersion}.tar.gz" {
      allowedRequisites = [];
      meta = {
        path = "${dir}/motoko-${releaseVersion}.tar.gz";
        content-type = "application/gzip";
      };
    } ''
      tmp=$(mktemp -d)
      ${nixpkgs.lib.concatMapStringsSep "\n" (d: "cp -v ${d}/bin/* $tmp") derivations}
      chmod 0755 $tmp/*
      tar -czf "$out" -C $tmp/ .
    '';

  as_js = name: derivation:
    nixpkgs.runCommandNoCC "${name}-${releaseVersion}.js" {
      allowedRequisites = [];
      meta = {
        path = "js/${name}-${releaseVersion}.js";
        content-type = "application/javascript";
      };
    } ''
      cp -v ${derivation}/bin/* $out
    '';

  release = import ./nix/publish.nix
    { pkgs = nixpkgs;
      inherit releaseVersion;
      derivations = [
        (as_tarball "x86_64-linux" (with linux; [ mo-ide mo-doc moc ]))
        (as_tarball "x86_64-darwin"(with darwin; [ mo-ide mo-doc moc ]))
        (as_js "moc" linux.js.moc)
        (as_js "moc-interpreter" linux.js.moc_interpreter)
      ];
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
