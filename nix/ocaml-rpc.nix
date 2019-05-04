{ pkgs }:

let repo = "ocaml-rpc"; in
let rev = "4d542cb7b0c42a62196c6043ba556de54c1b7820"; in

let src = pkgs.fetchFromGitHub {
  inherit repo rev;
  owner = "mirage";
  sha256 = "1931505xpycng9s18mq1m44naq0v7nxwr95nyws1kldl4fvrymn7";
}; in

let mkPackage = ({ name, description, buildInputs }:
  pkgs.stdenv.mkDerivation {
    name = "ocaml${pkgs.ocaml.version}-${repo}-${rev}-${name}";

    inherit src;

    buildInputs = buildInputs ++ (with pkgs; [
      dune
      ocaml
      ocamlPackages.findlib
    ]);

    buildPhase = ''
      dune build -p ${name}
    '';

    inherit (pkgs.dune) installPhase;

    meta = {
      homepage = https://github.com/mirage/ocaml-rpc;
      platforms = pkgs.ocaml.meta.platforms or [];
      license = pkgs.stdenv.lib.licenses.isc;
      inherit description;
    };
  }
); in

let rpclib = mkPackage {
  name = "rpclib";
  description = "A library to deal with RPCs in OCaml";
  buildInputs = with pkgs.ocamlPackages; [
    cmdliner
    result
    rresult
    xmlm
    yojson
  ];
}; in

let ppx_deriving_rpc = mkPackage {
  name = "ppx_deriving_rpc";
  description = "Ppx deriver for ocaml-rpc, a library to deal with RPCs in OCaml";
  buildInputs = rpclib.buildInputs ++ [
    pkgs.jbuilder
    rpclib
  ] ++ (with pkgs.ocamlPackages; [
    findlib
    rresult
    ppx_deriving
    ppxlib
  ]);
}; in

{
  inherit rpclib ppx_deriving_rpc;
}
