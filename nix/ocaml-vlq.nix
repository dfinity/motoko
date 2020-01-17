{ stdenv, ocaml, findlib, dune, sources }:

stdenv.mkDerivation rec {
  name = "ocaml${ocaml.version}-vlq-${src.version}";

  src = sources.ocaml-vlq;

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
