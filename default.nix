{ nixpkgs ? (import ./nix/nixpkgs.nix) {},
  test-dvm ? true,
  dvm ? null,
}:

let stdenv = nixpkgs.stdenv; in

let sourceByRegex = src: regexes: builtins.path
  { name = "actorscript";
    path = src;
    filter = path: type:
      let relPath = nixpkgs.lib.removePrefix (toString src + "/") (toString path); in
      let match = builtins.match (nixpkgs.lib.strings.concatStringsSep "|" regexes); in
      ( type == "directory"  &&  match (relPath + "/") != null || match relPath != null);
  }; in

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
      let dev = builtins.fetchGit {
        url = "ssh://git@github.com/dfinity-lab/dev";
        ref = "joachim/test-print-unicode";
        rev = "718589227ab51a6da2a6d221a712268cab967059";
      }; in
      (import dev {}).dvm
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
      "test/quick.mk"
      "test/(fail|run|run-dfinity)/"
      "test/(fail|run|run-dfinity)/.*.as"
      "test/(fail|run|run-dfinity)/ok/"
      "test/(fail|run|run-dfinity)/ok/.*.ok"
      "test/.*.sh"
      "samples/"
      "samples/.*"
      "stdlib/"
      "stdlib/.*Makefile.*"
      "stdlib/.*.as"
      "stdlib/examples/"
      "stdlib/examples/.*.as"
      ];

    buildInputs =
      [ native
        ocaml_wasm
        nixpkgs.wabt
        nixpkgs.bash
        nixpkgs.perl
        filecheck
      ] ++
      (if test-dvm then [ real-dvm ] else []);

    buildPhase = ''
      patchShebangs .
      asc --version
      make -C stdlib ASC=asc all
      make -C samples ASC=asc all
    '' +
      (if test-dvm
      then ''
      make -C test ASC=asc parallel
      '' else ''
      make -C test ASC=asc quick
      '');

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
      "test/quick.mk"
      "test/(fail|run|run-dfinity)/"
      "test/(fail|run|run-dfinity)/.*.as"
      "test/(fail|run|run-dfinity)/ok/"
      "test/(fail|run|run-dfinity)/ok/.*.ok"
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

  wasm = ocaml_wasm;
  dvm = real-dvm;
  filecheck = nixpkgs.linkFarm "FileCheck"
    [ { name = "bin/FileCheck"; path = "${nixpkgs.llvm}/bin/FileCheck";} ];
  wabt = nixpkgs.wabt;

  all-systems-go = nixpkgs.releaseTools.aggregate {
    name = "all-systems-go";
    constituents = [ native js native_test coverage-report ];
  };
}
