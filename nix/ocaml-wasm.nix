# copied from https://raw.githubusercontent.com/NixOS/nixpkgs/master/pkgs/development/ocaml-modules/wasm/default.nix

{ stdenv, fetchFromGitHub, ocaml, findlib, ocamlbuild }:

if !stdenv.lib.versionAtLeast ocaml.version "4.02"
then throw "wasm is not available for OCaml ${ocaml.version}"
else

stdenv.mkDerivation rec {
  name = "ocaml${ocaml.version}-wasm-${version}";
  version = "1.0";

  src = fetchFromGitHub {
    owner = "WebAssembly";
    repo = "multi-value";
    rev = "fa755dfe0c8ab3ec93636a092fc3dfbe8c8a232c";
    sha256 = "0867nd4k2lypal7g2a7816wi5zs4kp4w2dv9dxan9vvn3wi19b5i";
  };

  buildInputs = [ ocaml findlib ocamlbuild ];

  makeFlags = [ "-C" "interpreter" ];

  createFindlibDestdir = true;

  postInstall = ''
    mkdir $out/bin
    cp -L interpreter/wasm $out/bin
  '';

  meta = {
    description = "An executable and OCaml library to run, read and write Web Assembly (wasm) files and manipulate their AST";
    license = stdenv.lib.licenses.asl20;
    maintainers = [ stdenv.lib.maintainers.vbgl ];
    homepage = https://github.com/WebAssembly/spec/tree/master/interpreter;
    inherit (ocaml.meta) platforms;
  };
}
