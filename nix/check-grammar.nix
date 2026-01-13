# Checks that doc/md/examples/grammar.txt is up-to-date
{ pkgs }:
pkgs.stdenv.mkDerivation {
  name = "check-grammar";
  src = ../src/gen-grammar;
  phases = "unpackPhase buildPhase installPhase";
  buildInputs = [ pkgs.diffutils pkgs.bash pkgs.obelisk ];
  buildPhase = ''
    patchShebangs .
    ./gen-grammar.sh ${../src/mo_frontend/parser.mly} > expected
    echo "If the following fails, please run:"
    echo "nix-shell --command 'make -C src grammar'"
    diff -r -U 3 ${../doc/md/examples/grammar.txt} expected
    echo "ok, all good"
  '';
  installPhase = ''
    touch $out
  '';
}
