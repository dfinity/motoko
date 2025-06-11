{ pkgs, moc }:
pkgs.stdenv.mkDerivation {
  name = "base-tests";
  src = pkgs.sources.motoko-base-src;
  phases = "unpackPhase checkPhase installPhase";
  doCheck = true;
  installPhase = "touch $out";
  checkInputs = [
    pkgs.wasmtime
    moc
  ];
  checkPhase = ''
    make MOC=moc VESSEL_PKGS="--package matchers ${pkgs.sources.motoko-matchers-src}/src" -C test
  '';
}
