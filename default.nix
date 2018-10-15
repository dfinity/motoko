{ nixpkgs ? (import ./nix/nixpkgs.nix) {},
  test-dsh ? false,
}:

let stdenv = nixpkgs.stdenv; in

let sourceByRegex = src: regexes: builtins.filterSource (path: type:
      let relPath = nixpkgs.lib.removePrefix (toString src + "/") (toString path); in
      (type == "directory" ||
      builtins.match (nixpkgs.lib.strings.concatStringsSep "|" regexes) relPath != null))
    src; in

let ocaml_wasm = (import ./nix/ocaml-wasm.nix) {
  inherit (nixpkgs) stdenv fetchFromGitHub ocaml;
  inherit (nixpkgs.ocamlPackages) findlib ocamlbuild;
}; in

let ocaml_vlq = (import ./nix/ocaml-vlq.nix) {
  inherit (nixpkgs) stdenv fetchFromGitHub ocaml dune;
  inherit (nixpkgs.ocamlPackages) findlib;
}; in

# Include dsh
let dsh =
  if test-dsh
  then
    if !builtins.pathExists ./nix/dev/default.nix
    then throw "\"test-dsh = true\" requires a checkout of dev in ./nix"
    else ((import ./nix/dev) { v8 = true; }).hypervisor
  else null; in

# We need a newer version of menhir.
# So lets fetch the generic rules for menhir from nixpkgs
let menhir_nix = nixpkgs.fetchurl {
  url = "https://raw.githubusercontent.com/NixOS/nixpkgs/92a047a6c4d46a222e9c323ea85882d0a7a13af8/pkgs/development/ocaml-modules/menhir/generic.nix";
  sha256 = "ef5f134c307f579a5c37754c939cba016f4cc9beba9652fe04e8c7ba87874466";
}; in
# and use them with with the version we want
let menhir = (import menhir_nix) {
  inherit (nixpkgs) stdenv fetchurl ocaml;
  inherit (nixpkgs.ocamlPackages) findlib ocamlbuild;
  version = "20180703";
  sha256 = "16wv5m7ky27qb8krlycw79dqgzfnpm6rkppc9f26gyw15riicpxb";
}; in

let commonBuildInputs = [
  nixpkgs.ocaml
  menhir
  nixpkgs.ocamlPackages.findlib
  nixpkgs.ocamlPackages.ocamlbuild
  nixpkgs.ocamlPackages.num
  ocaml_wasm
  ocaml_vlq
  nixpkgs.ocamlPackages.zarith
  nixpkgs.ocamlPackages.yojson
]; in

rec {

  native = stdenv.mkDerivation {
    name = "asc";

    src = sourceByRegex ./. [
      ".*/Makefile.*"
      "src/.*.ml"
      "src/.*.mli"
      "src/.*.mly"
      "src/.*.mll"
      "src/_tags"
      "test/node-test.js"
      "test/.*/.*.as"
      "test/.*/ok/.*"
      "test/.*.sh"
      "vendor/.*"
      ];

    nativeBuildInputs = [ nixpkgs.makeWrapper ];

    buildInputs = commonBuildInputs ++
      (if test-dsh then [ dsh ] else []);

    buildPhase = ''
      make -C src BUILD=native asc
    '';

    installPhase = ''
      mkdir -p $out/bin
      cp src/asc $out/bin
    '';

    # The binary does not work until we use wrapProgram, which runs in
    # the install phase. Therefore, we use the installCheck phase to
    # run the test suite
    doInstallCheck = true;
    installCheckPhase = ''
      $out/bin/asc --version
      make -C samples ASC=$out/bin/asc all
      make -C test/run VERBOSE=1 ASC=$out/bin/asc all
      make -C test/fail VERBOSE=1 ASC=$out/bin/asc all
    '' +
      (if test-dsh then ''
      make -C test/run-dfinity VERBOSE=1 ASC=$out/bin/asc all
      '' else "");
  };

  js = native.overrideAttrs (oldAttrs: {
    name = "asc.js";

    buildInputs = commonBuildInputs ++ [
      nixpkgs.ocamlPackages.js_of_ocaml
      nixpkgs.ocamlPackages.js_of_ocaml-ocamlbuild
      nixpkgs.ocamlPackages.js_of_ocaml-ppx
      nixpkgs.nodejs
    ];

    buildPhase = ''
      make -C src asc.js
    '';

    installPhase = ''
      mkdir -p $out
      cp src/asc.js $out
    '';

    installCheckPhase = ''
      NODE_PATH=$out/ node test/node-test.js
    '';

  });

  wasm = ocaml_wasm;
  inherit dsh;
}
