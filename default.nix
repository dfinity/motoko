{ nixpkgs ? (import ./nix/nixpkgs.nix) {},
  test-dvm ? true,
  dvm ? null,
}:

let stdenv = nixpkgs.stdenv; in

let sourceByRegex = src: regexes: builtins.filterSource (path: type:
      let relPath = nixpkgs.lib.removePrefix (toString src + "/") (toString path); in
      let match = builtins.match (nixpkgs.lib.strings.concatStringsSep "|" regexes); in
      ( type == "directory"  &&  match (relPath + "/") != null
      || match relPath != null)) src; in

let ocaml_wasm = (import ./nix/ocaml-wasm.nix) {
  inherit (nixpkgs) stdenv fetchFromGitHub ocaml;
  inherit (nixpkgs.ocamlPackages) findlib ocamlbuild;
}; in

let ocaml_vlq = (import ./nix/ocaml-vlq.nix) {
  inherit (nixpkgs) stdenv fetchFromGitHub ocaml dune;
  inherit (nixpkgs.ocamlPackages) findlib;
}; in

let ocaml_bisect_ppx = (import ./nix/ocaml-bisect_ppx.nix) nixpkgs; in
let ocaml_bisect_ppx-ocamlbuild = (import ./nix/ocaml-bisect_ppx-ocamlbuild.nix) nixpkgs; in

# Include dvm
let real-dvm =
  if dvm == null
  then
    if test-dvm
    then
      if !builtins.pathExists ./nix/dev/default.nix
      then
        throw "\"test-dvm = true\" requires a checkout of dev in ./nix.\nSee Jenkinsfile for the reqiure revision. "
      else
        # Pass devel = true until the dev test suite runs on MacOS again
        ((import ./nix/dev) { devel = true; }).dvm
    else null
  else dvm; in

let commonBuildInputs = [
  nixpkgs.ocaml
  nixpkgs.ocamlPackages.menhir
  nixpkgs.ocamlPackages.findlib
  nixpkgs.ocamlPackages.ocamlbuild
  nixpkgs.ocamlPackages.num
  ocaml_wasm
  ocaml_vlq
  nixpkgs.ocamlPackages.zarith
  nixpkgs.ocamlPackages.yojson
  ocaml_bisect_ppx
  ocaml_bisect_ppx-ocamlbuild
]; in

rec {

  native = stdenv.mkDerivation {
    name = "asc";

    src = sourceByRegex ./. [
      "src/"
      "src/Makefile.*"
      "src/.*.ml"
      "src/.*.mli"
      "src/.*.mly"
      "src/.*.mll"
      "src/.*.mlpack"
      "src/_tags"
      "test/"
      "test/node-test.js"
      ];

    nativeBuildInputs = [ nixpkgs.makeWrapper ];

    buildInputs = commonBuildInputs;

    buildPhase = ''
      make -C src BUILD=native asc
    '';

    installPhase = ''
      mkdir -p $out/bin
      cp src/asc $out/bin
    '';
  };

  native_test = stdenv.mkDerivation {
    name = "native.test";

    src = sourceByRegex ./. [
      "test/"
      "test/.*Makefile.*"
      "test/(run.*|fail)/"
      "test/(run.*|fail)/.*.as"
      "test/(run.*|fail)/ok/.*"
      "test/.*.sh"
      "samples/"
      "samples/.*"
      ];

    buildInputs =
      [ native
        nixpkgs.wabt
        nixpkgs.bash
        nixpkgs.perl
      ] ++
      (if test-dvm then [ real-dvm ] else []);

    buildPhase = ''
      patchShebangs .
      asc --version
      make -C samples ASC=asc all
      make -C test/run VERBOSE=1 ASC=asc all
      make -C test/fail VERBOSE=1 ASC=asc all
    '' +
      (if test-dvm then ''
      make -C test/run-dfinity VERBOSE=1 ASC=asc all
      '' else "");

    installPhase = ''
      mkdir -p $out
    '';
  };

  native-coverage = native.overrideAttrs (oldAttrs: {
    name = "asc-coverage";
    buildPhase =
      "export BISECT_COVERAGE=YES;" +
      oldAttrs.buildPhase;
    installPhase =
      oldAttrs.installPhase + ''
      mv src/ $out/src
      '';
  });

  coverage-report = stdenv.mkDerivation {
    name = "native.coverage";

    src = sourceByRegex ./. [
      "test/"
      "test/.*Makefile.*"
      "test/(run.*|fail)/"
      "test/(run.*|fail)/.*.as"
      "test/(run.*|fail)/ok/.*"
      "test/.*.sh"
      "samples/"
      "samples/.*"
      ];

    buildInputs =
      [ native-coverage
        nixpkgs.wabt
        nixpkgs.bash
        nixpkgs.perl
        ocaml_bisect_ppx
      ] ++
      (if test-dvm then [ real-dvm ] else []);

    buildPhase = ''
      patchShebangs .
      ln -vs ${native-coverage}/src src
      make -C test ASC=asc coverage
      '';

    installPhase = ''
      mkdir -p $out
      mv test/coverage/ $out/
      mkdir -p $out/nix-support
      echo "report coverage $out/coverage index.html" >> $out/nix-support/hydra-build-products
    '';
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

  wabt = nixpkgs.wabt;
  wasm = ocaml_wasm;
  dvm = real-dvm;
}
