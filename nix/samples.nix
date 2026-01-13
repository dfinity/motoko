{ pkgs, moc }:
pkgs.stdenv.mkDerivation {
  name = "samples";
  src = ../samples;
  buildInputs = [ moc ];
  buildPhase = ''
    patchShebangs .
    make all
  '';
  installPhase = ''
    touch $out
  '';
}
