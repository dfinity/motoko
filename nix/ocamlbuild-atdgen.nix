pkgs:

let repo = "ocamlbuild-atdgen"; in
let rev = "v0.1.0"; in

pkgs.stdenv.mkDerivation {
  name = "ocaml${pkgs.ocaml.version}-${repo}-${rev}";

  src = pkgs.fetchFromGitHub {
    inherit repo rev;
    owner = "rgrinberg";
    sha256 = "0wxgvgb8j6ajdzqwzxldvnwfblgbkhgycwmrkmrq8dcjim0mg8ah";
  };

  buildInputs = with pkgs; [
    ocaml
    ocamlPackages.findlib
    ocamlPackages.ocamlbuild
  ];

  createFindlibDestdir = true;

  installPhase = ''
    make install
  '';

  meta = {
    homepage = https://github.com/rgrinberg/ocamlbuild-atdgen;
    platforms = pkgs.ocaml.meta.platforms or [];
    description = "ocamlbuild plugin for atdgen";
    license = pkgs.stdenv.lib.licenses.isc;
  };
}
