{ mkDerivation, fetchFromGitHub, ocaml, findlib, dune_1 }:

let version = "v0.2.0"; in
mkDerivation {
  name = "ocaml${ocaml.version}-vlq-${version}";

  src = fetchFromGitHub {
    owner = "flowtype";
    repo = "ocaml-vlq";
    rev = version;
    sha256 = "09jdgih2n2qwpxnlbcca4xa193rwbd1nw7prxaqlg134l4mbya83";
  };

  buildInputs = [ ocaml findlib dune_1 ];

  buildPhase = "dune build";

  installPhase = "dune install --prefix $out --libdir $OCAMLFIND_DESTDIR vlq";

  meta = {
    homepage = https://github.com/flowtype/ocaml-vlq;
    platforms = ocaml.meta.platforms or [];
    description = "A simple library for encoding variable-length quantities";
  };
}
