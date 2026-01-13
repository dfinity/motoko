{ pkgs, js, base-src, core-src }:
pkgs.stdenv.mkDerivation {
  name = "docs";
  src = ../doc;
  buildInputs = with pkgs; [ pandoc bash gitMinimal ];

  buildPhase = ''
    patchShebangs .
    export HOME=$PWD
    export MOC_JS=${js.moc}/bin/moc.js
    export MOTOKO_BASE=${base-src}
    export MOTOKO_CORE=${core-src}
    make
  '';

  installPhase = ''
    mkdir -p $out
    mv overview-slides.html $out/
    mv html $out/
    mkdir -p $out/nix-support
    echo "report guide $out html/motoko.html" >> $out/nix-support/hydra-build-products
    echo "report slides $out overview-slides.html" >> $out/nix-support/hydra-build-products
  '';
}
