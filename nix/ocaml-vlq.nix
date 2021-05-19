{ buildDunePackage, fetchFromGitHub }:

let version = "v0.2.0"; in
buildDunePackage {
  pname = "vlq";
  inherit version;

  useDune2 = true;

  src = fetchFromGitHub {
    owner = "flowtype";
    repo = "ocaml-vlq";
    rev = version;
    sha256 = "09jdgih2n2qwpxnlbcca4xa193rwbd1nw7prxaqlg134l4mbya83";
  };
}
