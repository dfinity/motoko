{ nixpkgs ? (import ./nix/nixpkgs.nix) {},
  test-dvm ? true,
  dvm ? null,
}:

let llvm = (import ./nix/llvm.nix); in

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
        ref = "master";
        rev = "b6f587c3303b9f2585548e5fcb98f907b0275219";
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

let
  test_files = [
    "test/"
    "test/.*Makefile.*"
    "test/quick.mk"
    "test/(fail|run|run-dfinity|repl)/"
    "test/(fail|run|run-dfinity|repl)/lib/"
    "test/(fail|run|run-dfinity|repl)/lib/dir/"
    "test/(fail|run|run-dfinity|repl)/.*.as"
    "test/(fail|run|run-dfinity|repl)/.*.sh"
    "test/(fail|run|run-dfinity|repl)/ok/"
    "test/(fail|run|run-dfinity|repl)/ok/.*.ok"
    "test/.*.sh"
  ];
  samples_files = [
    "samples/"
    "samples/.*"
  ];
  stdlib_files = [
    "stdlib/"
    "stdlib/.*Makefile.*"
    "stdlib/.*.as"
    "stdlib/examples/"
    "stdlib/examples/.*.as"
    "stdlib/examples/produce-exchange/"
    "stdlib/examples/produce-exchange/.*.as"
    "stdlib/examples/produce-exchange/test/"
    "stdlib/examples/produce-exchange/test/.*.as"
  ];
  stdlib_doc_files = [
    "stdlib/.*\.py"
    "stdlib/README.md"
    "stdlib/examples/produce-exchange/README.md"
  ];

  libtommath = nixpkgs.fetchFromGitHub {
    owner = "libtom";
    repo = "libtommath";
    rev = "9e1a75cfdc4de614eaf4f88c52d8faf384e54dd0";
    sha256 = "0qwmzmp3a2rg47pnrsls99jpk5cjj92m75alh1kfhcg104qq6w3d";
  };

in

rec {

  rts = nixpkgs.pkgsi686Linux.stdenv.mkDerivation {
    name = "asc-rts";

    src = sourceByRegex ./rts [
      "rts.c"
      "Makefile"
      ];

    nativeBuildInputs = [ nixpkgs.makeWrapper ];

    buildInputs = with nixpkgs; with llvm;
      [ clang_9 lld_9 ] ;

    buildPhase = ''
      make CLANG="clang-9 -I${nixpkgs.pkgsi686Linux.glibc.dev}/include"  WASM_LD=wasm-ld TOMMATHSRC=${libtommath}
    '';

    installPhase = ''
      mkdir -p $out/rts
      cp rts.wasm $out/rts
    '';
  };

  asc-bin = stdenv.mkDerivation {
    name = "asc-bin";

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

  native = nixpkgs.symlinkJoin {
    name = "asc";
    paths = [ asc-bin rts ];
    buildInputs = [ nixpkgs.makeWrapper ];
    postBuild = ''
      wrapProgram $out/bin/asc \
        --set-default ASC_RTS "$out/rts/rts.wasm"
    '';
  };

  native_test = stdenv.mkDerivation {
    name = "native.test";

    src = sourceByRegex ./. (
      test_files ++
      samples_files
    );

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

  asc-bin-coverage = asc-bin.overrideAttrs (oldAttrs: {
    name = "asc-bin-coverage";
    buildPhase =
      "export BISECT_COVERAGE=YES;" +
      oldAttrs.buildPhase;
    installPhase =
      oldAttrs.installPhase + ''
      mv src/ $out/src
      '';
  });

  native-coverage = nixpkgs.symlinkJoin {
    name = "asc-covergage";
    paths = [ asc-bin-coverage rts ];
    buildInputs = [ nixpkgs.makeWrapper ];
    postBuild = ''
      wrapProgram $out/bin/asc \
        --set-default ASC_RTS "$out/rts/rts.wasm"
    '';
  };

  coverage-report = stdenv.mkDerivation {
    name = "coverage-report";

    src = sourceByRegex ./. (
      test_files ++
      samples_files
    );

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


  js = asc-bin.overrideAttrs (oldAttrs: {
    name = "asc.js";

    buildInputs = commonBuildInputs ++ [
      nixpkgs.ocamlPackages.js_of_ocaml
      nixpkgs.ocamlPackages.js_of_ocaml-ocamlbuild
      nixpkgs.ocamlPackages.js_of_ocaml-ppx
      nixpkgs.nodejs-10_x
    ];

    buildPhase = ''
      make -C src asc.js
    '';

    installPhase = ''
      mkdir -p $out/bin
      cp -v src/asc.js $out/bin
      cp -vr ${rts}/rts $out
    '';

    doInstallCheck = false; # need to fix loading the rts

    installCheckPhase = ''
      NODE_PATH=$out/ node --experimental-wasm-mut-global --experimental-wasm-mv test/node-test.js
    '';

  });

  wasm = ocaml_wasm;
  dvm = real-dvm;
  filecheck = nixpkgs.linkFarm "FileCheck"
    [ { name = "bin/FileCheck"; path = "${nixpkgs.llvm}/bin/FileCheck";} ];
  wabt = nixpkgs.wabt;


  users-guide = stdenv.mkDerivation {
    name = "users-guide";

    src = sourceByRegex ./. [
      "design/"
      "design/guide.md"
      "guide/"
      "guide/Makefile"
      "guide/.*css"
      "guide/.*md"
      "guide/.*png"
      ];

    buildInputs =
      with nixpkgs;
      let tex = texlive.combine {
        inherit (texlive) scheme-small xetex newunicodechar;
      }; in
      [ pandoc tex bash ];

    NIX_FONTCONFIG_FILE =
      with nixpkgs;
      nixpkgs.makeFontsConf { fontDirectories = [ gyre-fonts inconsolata unifont lmodern lmmath ]; };

    buildPhase = ''
      patchShebangs .
      make -C guide
    '';

    installPhase = ''
      mkdir -p $out
      mv guide $out/
      rm $out/guide/Makefile
      mkdir -p $out/nix-support
      echo "report guide $out/guide index.html" >> $out/nix-support/hydra-build-products
    '';
  };


  stdlib-reference = stdenv.mkDerivation {
    name = "stdlib-reference";

    src = sourceByRegex ./. (
      stdlib_files ++
      stdlib_doc_files
    ) + "/stdlib";

    buildInputs = with nixpkgs;
      [ pandoc bash python ];

    buildPhase = ''
      patchShebangs .
      make alldoc
    '';

    installPhase = ''
      mkdir -p $out
      mv doc $out/
      mkdir -p $out/nix-support
      echo "report docs $out/doc README.html" >> $out/nix-support/hydra-build-products
    '';

    forceShare = ["man"];
  };

  produce-exchange = stdenv.mkDerivation {
    name = "produce-exchange";
    src = sourceByRegex ./. (
      stdlib_files
    );

    buildInputs = [
      native
    ];

    doCheck = true;
    buildPhase = ''
      make -C stdlib ASC=asc OUTDIR=_out _out/ProduceExchange.wasm
    '';
    checkPhase = ''
      make -C stdlib ASC=asc OUTDIR=_out _out/ProduceExchange.out
    '';
    installPhase = ''
      mkdir -p $out
      cp stdlib/_out/ProduceExchange.wasm $out
    '';
  };

  all-systems-go = nixpkgs.releaseTools.aggregate {
    name = "all-systems-go";
    constituents = [ native js native_test coverage-report stdlib-reference produce-exchange users-guide ];
  };
}
