{ mkDerivation, lib, fetchFromGitHub, ocaml, dune_2, ocamlPackages }:
mkDerivation rec {
	pname = "obelisk";
	version = "0.5.2";
	src = fetchFromGitHub {
		owner = "lelio-brun";
		repo = "obelisk";
		rev = "v${version}";
		sha256 = "0cn128q85rskprm5jj01m38fd3cayfcagy7v24pkq3kgw4hyyni8";
	};

	buildInputs = with ocamlPackages; [ ocaml findlib re dune_2 menhir ];

        buildPhase = "dune build";


	installPhase = ''
          mkdir -p $out
          mkdir -p $out/bin
	    mv _build/default/src/main.exe $out/bin/obelisk
	'';

	meta = {
		description = "A simple tool which produces pretty-printed output from a Menhir parser file (.mly)";
		license = lib.licenses.mit;
		maintainers = [];
		inherit (src.meta) homepage;
		inherit (ocamlPackages.ocaml.meta) platforms;
	};
}
