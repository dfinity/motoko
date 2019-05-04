{ pkgs }:

let repo = "md2mld"; in
let rev = "0.3.0"; in

pkgs.stdenv.mkDerivation {
  name = "ocaml${pkgs.ocaml.version}-${repo}-${rev}";

  src = pkgs.fetchFromGitHub {
    inherit repo rev;
    owner = "mseri";
    sha256 = "0xb8890q6f6rh02ih6vw3wnkybddr2fmzpj7144l1gfix3smx5cv";
  };

  buildInputs = [
    pkgs.dune
    pkgs.ocaml
    pkgs.ocamlPackages.findlib
    pkgs.ocamlPackages.omd
  ];

  buildPhase = "dune build -p md2mld";

  inherit (pkgs.dune) installPhase;

  meta = {
    homepage = https://github.com/mseri/md2mld;
    platforms = pkgs.ocaml.meta.platforms or [];
    description = "Little cli tool to convert md files into mld files";
    license = pkgs.stdenv.lib.licenses.isc;
  };
}
