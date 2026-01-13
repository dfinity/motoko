{ pkgs, mo-doc }:
pkgs.stdenv.mkDerivation {
  name = "base-doc";
  src = pkgs.sources.motoko-base-src;
  phases = "unpackPhase buildPhase installPhase";
  doCheck = true;
  buildInputs = [ mo-doc ];
  buildPhase = ''
    mo-doc
  '';
  installPhase = ''
    mkdir -p $out
    cp -rv docs/* $out/

    mkdir -p $out/nix-support
    echo "report docs $out index.html" >> $out/nix-support/hydra-build-products
  '';
}
