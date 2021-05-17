{ stdenv, lib, fetchFromGitHub, buildDunePackage }:

let version = "v0.2.0"; in
stdenv.mkDerivation {
  pname = "vlq";
  version = "0.2.0";

  src = fetchFromGitHub {
    owner = "flowtype";
    repo = "ocaml-vlq";
    rev = version;
    sha256 = "09jdgih2n2qwpxnlbcca4xa193rwbd1nw7prxaqlg134l4mbya83";
  };

  meta = {
    homepage = https://github.com/flowtype/ocaml-vlq;
    description = "A simple library for encoding variable-length quantities";
    license = lib.licenses.mit;
    maintainers = [];
  };
}
