# copied from https://raw.githubusercontent.com/NixOS/nixpkgs/master/pkgs/development/ocaml-modules/wasm/default.nix
# and updated to use the git submodule

{ stdenv, fetchFromGitHub, ocaml, findlib, ocamlbuild }:

if !stdenv.lib.versionAtLeast ocaml.version "4.02"
then throw "wasm is not available for OCaml ${ocaml.version}"
else

if !builtins.pathExists ../vendor/wasm-spec/interpreter
then throw "submodule in vendor/wasm-spec missing. Run git submodule update --init"
else

stdenv.mkDerivation rec {
  name = "ocaml${ocaml.version}-wasm-${version}";
  version = "1.0";

  src = ../vendor/wasm-spec;

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
