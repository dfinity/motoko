pkgs:

let version = "1.4.0"; in

pkgs.stdenv.mkDerivation {
  name = "ocaml${pkgs.ocaml.version}-bisect_ppx-ocamlbuild-${version}";

  src = pkgs.fetchFromGitHub {
    owner = "aantron";
    repo = "bisect_ppx";
    rev = version;
    sha256 = "1plhm4pvrhpapz5zaks194ji1fgzmp13y942g10pbn9m7kgkqg4h";
  };

  buildInputs = [
    pkgs.ocaml
    pkgs.dune
    pkgs.ocamlPackages.findlib
    pkgs.ocamlPackages.ocamlbuild
  ];

  buildPhase = "dune build -p bisect_ppx-ocamlbuild";

  inherit (pkgs.dune) installPhase;

  meta = {
    homepage = https://github.com/aantron/bisect_ppx;
    platforms = pkgs.ocaml.meta.platforms or [];
    description = "Code coverage for OCaml";
    license = pkgs.stdenv.lib.licenses.mpl20;
  };
}
