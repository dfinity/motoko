{ pkgs, moc }:
pkgs.stdenv.mkDerivation {
  name = "core-tests";
  src = pkgs.sources.motoko-core-src;
  phases = "unpackPhase checkPhase installPhase";
  doCheck = true;
  installPhase = "touch $out";
  checkInputs = [
    pkgs.wasmtime
    pkgs.nodejs_20
    pkgs.nodePackages."ic-mops"
    moc
  ];
  checkPhase = ''
    echo BEFORE ${pkgs.nodejs_20}/bin/npm ci
    npm -g ci
    echo DONE ${pkgs.nodejs_20}/bin/npm ci
    npm run test
  '';
}
