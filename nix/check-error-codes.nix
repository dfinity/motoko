{ pkgs }:
pkgs.stdenv.mkDerivation {
  name = "check-error-codes";
  src = ../test;
  phases = "unpackPhase buildPhase installPhase";
  buildInputs = [ pkgs.python3 ];
  buildPhase = ''
    patchShebangs .
    ./check-error-codes.py ${../src/lang_utils/error_codes.ml}
  '';
  installPhase = ''
    touch $out
  '';
}
