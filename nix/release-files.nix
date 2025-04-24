{ self, pkgs }:
let
  linux = self.packages."x86_64-linux";
  linuxArm = self.packages."aarch64-linux";
  darwin = self.packages."x86_64-darwin";
  darwinArm = self.packages."aarch64-darwin";

  releaseVersion = import ./releaseVersion.nix { inherit pkgs; officialRelease = true; };

  as_tarball = dir: drvs:
    pkgs.runCommandNoCC "motoko-${releaseVersion}.tar.gz"
      {
        allowedRequisites = [ ];
      } ''
      tmp=$(mktemp -d)
      ${pkgs.lib.concatMapStringsSep "\n" (drv: "cp -v ${drv}/bin/* $tmp") drvs}
      chmod 0755 $tmp/*
      tar -czf "$out" -C $tmp/ .
    '';

  as_js = name: drv:
    pkgs.runCommandNoCC "${name}-${releaseVersion}.js"
      {
        allowedRequisites = [ ];
      } ''
      cp -v ${drv}/bin/*.min.js $out
    '';
in
pkgs.runCommandNoCC "motoko-release-${releaseVersion}" { } ''
  mkdir $out
  cp ${as_tarball "x86_64-linux" (with linux.release; [ mo-ide mo-doc moc ])} $out/motoko-Linux-x86_64-${releaseVersion}.tar.gz
  cp ${as_tarball "aarch64-linux" (with linuxArm.release; [ mo-ide mo-doc moc ])} $out/motoko-Linux-aarch64-${releaseVersion}.tar.gz
  cp ${as_tarball "x86_64-darwin" (with darwin.release; [ mo-ide mo-doc moc ])} $out/motoko-Darwin-x86_64-${releaseVersion}.tar.gz
  cp ${as_tarball "aarch64-darwin" (with darwinArm.release; [ mo-ide mo-doc moc ])} $out/motoko-Darwin-arm64-${releaseVersion}.tar.gz

  cp ${as_js "moc" linux.js.moc} $out/moc-${releaseVersion}.js
  cp ${as_js "moc-interpreter" linux.js.moc_interpreter} $out/moc-interpreter-${releaseVersion}.js
  tar --exclude=.github -C ${pkgs.sources.motoko-base-src} -czvf $out/motoko-base-library.tar.gz .
''
