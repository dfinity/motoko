{ stdenv, fetchFromGitHub, ocaml, findlib, dune }:

let version = "v0.2.0"; in

stdenv.mkDerivation {
  name = "ocaml${ocaml.version}-vlq-${version}";

  src = fetchFromGitHub {
    owner = "flowtype";
    repo = "ocaml-vlq";
    rev = version;
    sha256 = "09jdgih2n2qwpxnlbcca4xa193rwbd1nw7prxaqlg134l4mbya83";
  };

  buildInputs = [ ocaml findlib dune ];

  buildPhase = "dune build";

  inherit (dune) installPhase;

  meta = {
    homepage = https://github.com/flowtype/ocaml-vlq;
    platforms = ocaml.meta.platforms or [];
    description = "A simple library for encoding variable-length quantities";
    license = stdenv.lib.licenses.mit;
    maintainers = with stdenv.lib.maintainers; [ vbgl ];
  };
}
