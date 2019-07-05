{ nixpkgs ? (import ./nix/nixpkgs.nix).nixpkgs {},
  dvm ? null,
  drun ? null,
  export-shell ? false,
  replay ? 0
}:

let llvm = import ./nix/llvm.nix { system = nixpkgs.system; }; in

let stdenv = nixpkgs.stdenv; in

let subpath = p: import ./nix/gitSource.nix p; in

# pick OCaml version here
let ocamlPackages = nixpkgs.ocaml-ng.ocamlPackages_4_07; in

let ocaml_wasm = import ./nix/ocaml-wasm.nix {
  inherit (nixpkgs) stdenv fetchFromGitHub;
  inherit (ocamlPackages) findlib ocamlbuild ocaml;
}; in

let ocaml_vlq = import ./nix/ocaml-vlq.nix {
  inherit (nixpkgs) stdenv fetchFromGitHub dune;
  inherit (ocamlPackages) findlib ocaml;
}; in

let dev = import (builtins.fetchGit {
  url = "ssh://git@github.com/dfinity-lab/dev";
  # ref = "master";
  rev = "6fca1936fcd027aaeaccab0beb51defeee38a0ff";
}) { system = nixpkgs.system; }; in

let dfinity-repo = import (builtins.fetchGit {
  url = "ssh://git@github.com/dfinity-lab/dfinity";
  # ref = "master";
  rev = "50626f51b377c6cdf955b2b41dfae9e550025361";
}) { system = nixpkgs.system; }; in

let sdk = import (builtins.fetchGit {
  url = "ssh://git@github.com/dfinity-lab/sdk";
  ref = "paulyoung/js-user-library";
  rev = "42f15621bc5b228c7fd349cb52f265917d33a3a0";
}) { system = nixpkgs.system; }; in

let esm = builtins.fetchTarball {
  sha256 = "116k10q9v0yzpng9bgdx3xrjm2kppma2db62mnbilbi66dvrvz9q";
  url = "https://registry.npmjs.org/esm/-/esm-3.2.25.tgz";
}; in

# Include dvm
let real-dvm =
  if dvm == null
  then dev.dvm
  else dvm; in

let real-drun =
  if drun == null
  then dfinity-repo.dfinity.drun
  else drun; in

let js-user-library = sdk.js-user-library; in

let haskellPackages = nixpkgs.haskellPackages.override {
      overrides = self: super: {
        haskell-lsp-types = self.callPackage
          ({ mkDerivation, aeson, base, bytestring, data-default, deepseq
           , filepath, hashable, lens, network-uri, scientific, text
           , unordered-containers
           }:
             mkDerivation {
               pname = "haskell-lsp-types";
               version = "0.16.0.0";
               sha256 = "14wlv54ydbddpw6cwgykcas3rb55w7m78q0s1wdbi594wg1bscqg";
               libraryHaskellDepends = [
                 aeson base bytestring data-default deepseq filepath hashable lens
                 network-uri scientific text unordered-containers
               ];
               description = "Haskell library for the Microsoft Language Server Protocol, data types";
               license = stdenv.lib.licenses.mit;
               hydraPlatforms = stdenv.lib.platforms.none;
             }) {};

        rope-utf16-splay = self.callPackage
          ({ mkDerivation, base, QuickCheck, tasty, tasty-hunit
           , tasty-quickcheck, text
           }:
             mkDerivation {
               pname = "rope-utf16-splay";
               version = "0.3.1.0";
               sha256 = "1ilcgwmdwqnp95vb7652fc03ji9dnzy6cm24pvbiwi2mhc4piy6b";
               libraryHaskellDepends = [ base text ];
               testHaskellDepends = [
                 base QuickCheck tasty tasty-hunit tasty-quickcheck text
               ];
               description = "Ropes optimised for updating using UTF-16 code units and row/column pairs";
               license = stdenv.lib.licenses.bsd3;
             }) {};

        haskell-lsp = self.callPackage
          ({ mkDerivation, aeson, async, attoparsec, base, bytestring
           , containers, data-default, directory, filepath, hashable
           , haskell-lsp-types, hslogger, hspec, hspec-discover, lens, mtl
           , network-uri, QuickCheck, quickcheck-instances, rope-utf16-splay
           , sorted-list, stm, temporary, text, time, unordered-containers
           }:
             mkDerivation {
               pname = "haskell-lsp";
               version = "0.16.0.0";
               sha256 = "1s04lfnb3c0g9bkwp4j7j59yw8ypps63dq27ayybynrfci4bpj95";
               isLibrary = true;
               isExecutable = true;
               libraryHaskellDepends = [
                 aeson async attoparsec base bytestring containers data-default
                 directory filepath hashable haskell-lsp-types hslogger lens mtl
                 network-uri rope-utf16-splay sorted-list stm temporary text time
                 unordered-containers
               ];
               testHaskellDepends = [
                 aeson base bytestring containers data-default directory filepath
                 hashable hspec lens network-uri QuickCheck quickcheck-instances
                 rope-utf16-splay sorted-list stm text
               ];
               testToolDepends = [ hspec-discover ];
               description = "Haskell library for the Microsoft Language Server Protocol";
               license = stdenv.lib.licenses.mit;
               hydraPlatforms = stdenv.lib.platforms.none;
             }) {};

        lsp-test = self.callPackage
          ({ mkDerivation, aeson, aeson-pretty, ansi-terminal, async, base
           , bytestring, conduit, conduit-parse, containers, data-default
           , Diff, directory, filepath, hspec, haskell-lsp, lens, mtl
           , parser-combinators, process, rope-utf16-splay, text, transformers
           , unix, unordered-containers
           }:
             mkDerivation {
               pname = "lsp-test";
               version = "0.7.0.0";
               sha256 = "1lm299gbahrnwfrprhhpzxrmjljj33pps1gzz2wzmp3m9gzl1dx5";
               libraryHaskellDepends = [
                 aeson aeson-pretty ansi-terminal async base bytestring conduit
                 conduit-parse containers data-default Diff directory filepath
                 haskell-lsp lens mtl parser-combinators process rope-utf16-splay
                 text transformers unix unordered-containers
               ];
               doCheck = false;
               testHaskellDepends = [
                 aeson base data-default haskell-lsp hspec lens text
                 unordered-containers
               ];
               description = "Functional test framework for LSP servers";
               license = stdenv.lib.licenses.bsd3;
               hydraPlatforms = stdenv.lib.platforms.none;
             }) {};
      };
    }; in

let commonBuildInputs = [
  ocamlPackages.ocaml
  nixpkgs.dune
  ocamlPackages.atdgen
  ocamlPackages.base
  ocamlPackages.findlib
  ocamlPackages.menhir
  ocamlPackages.num
  ocamlPackages.stdint
  ocaml_wasm
  ocaml_vlq
  ocamlPackages.zarith
  ocamlPackages.yojson
  ocamlPackages.ppxlib
  ocamlPackages.ppx_inline_test
  ocamlPackages.bisect_ppx
  ocamlPackages.bisect_ppx-ocamlbuild
  ocamlPackages.ocaml-migrate-parsetree
  ocamlPackages.ppx_tools_versioned
]; in

let
  libtommath = nixpkgs.fetchFromGitHub {
    owner = "libtom";
    repo = "libtommath";
    rev = "584405ff8e357290362671b5e7db6110a959cbaa";
    sha256 = "1vl606rm8ba7vjhr0rbdqvih5d4r5iqalqlj5mnz6j3bnsn83b2a";
  };

  llvmBuildInputs = [
    nixpkgs.clang # for native building
    llvm.clang_9 # for wasm building
    llvm.lld_9 # for wasm building
  ];

  # When compiling natively, we want to use `clang` (which is a nixpkgs
  # provided wrapper that sets various include paths etc).
  # But for some reason it does not handle building for Wasm well, so
  # there we use plain clang-9. There is no stdlib there anyways.
  llvmEnv = ''
    export CLANG="clang"
    export WASM_CLANG="clang-9"
    export WASM_LD=wasm-ld
  '';
in

rec {
  rts = stdenv.mkDerivation {
    name = "asc-rts";

    src = subpath ./rts;
    nativeBuildInputs = [ nixpkgs.makeWrapper ];

    buildInputs = llvmBuildInputs;

    preBuild = ''
      ${llvmEnv}
      export TOMMATHSRC=${libtommath}
    '';

    doCheck = true;

    checkPhase = ''
      ./test_rts
    '';

    installPhase = ''
      mkdir -p $out/rts
      cp as-rts.wasm $out/rts
    '';
  };

  asc-bin = stdenv.mkDerivation {
    name = "asc-bin";

    src = subpath ./src;

    buildInputs = commonBuildInputs;

    buildPhase = ''
      make DUNE_OPTS="--display=short --profile release" asc as-ld
    '';

    installPhase = ''
      mkdir -p $out/bin
      cp --verbose --dereference asc as-ld $out/bin
    '';
  };

  asc = nixpkgs.symlinkJoin {
    name = "asc";
    paths = [ asc-bin rts ];
    buildInputs = [ nixpkgs.makeWrapper ];
    postBuild = ''
      wrapProgram $out/bin/asc \
        --set-default ASC_RTS "$out/rts/as-rts.wasm"
    '';
  };

  asc-tar = nixpkgs.symlinkJoin {
    name = "asc-tar";
    paths = [ asc-bin rts didc ];
    postBuild = ''
      tar -chf $out/asc.tar -C $out bin/asc rts/as-rts.wasm bin/didc
      mkdir -p $out/nix-support
      echo "file bin $out/asc.tar" >> $out/nix-support/hydra-build-products
    '';
  };

  lsp-int = haskellPackages.callCabal2nix "lsp-int" test/lsp-int { };

  qc-actorscript = haskellPackages.callCabal2nix "qc-actorscript" test/random { };

  tests = stdenv.mkDerivation {
    name = "tests";
    src = subpath ./test;
    buildInputs =
      [ asc
        didc
        deser
        ocaml_wasm
        nixpkgs.wabt
        nixpkgs.bash
        nixpkgs.perl
        nixpkgs.getconf
        nixpkgs.nodejs-10_x
        filecheck
        js-user-library
        dvm
        drun
        qc-actorscript
        lsp-int
        esm
      ] ++
      llvmBuildInputs;

    buildPhase = ''
        patchShebangs .
        ${llvmEnv}
        export ASC=asc
        export AS_LD=as-ld
        export DIDC=didc
        export DESER=deser
        export ESM=${esm}
        export JS_USER_LIBRARY=${js-user-library}
        asc --version
        make parallel
        qc-actorscript${nixpkgs.lib.optionalString (replay != 0)
          " --quickcheck-replay=${toString replay}"}
        cp -R ${subpath ./test/lsp-int/test-project} test-project
        find ./test-project -type d -exec chmod +w {} +
        lsp-int ${as-ide}/bin/as-ide ./test-project
      '';

    installPhase = ''
      touch $out
    '';
  };

  unit-tests = stdenv.mkDerivation {
    name = "unit-tests";

    src = subpath ./src;

    buildInputs = commonBuildInputs;

    buildPhase = ''
      make DUNE_OPTS="--display=short" unit-tests
    '';

    installPhase = ''
      touch $out
    '';
  };

  as-ide = stdenv.mkDerivation {
    name = "as-ide";

    src = subpath ./src;

    buildInputs = commonBuildInputs;

    buildPhase = ''
      make DUNE_OPTS="--display=short --profile release" as-ide
    '';

    installPhase = ''
      mkdir -p $out/bin
      cp --verbose --dereference as-ide $out/bin
    '';
  };

  samples = stdenv.mkDerivation {
    name = "samples";
    src = subpath ./samples;
    buildInputs =
      [ asc
        didc
        ocaml_wasm
        nixpkgs.wabt
        nixpkgs.bash
        nixpkgs.perl
        filecheck
	real-drun
      ] ++
      llvmBuildInputs;

    buildPhase = ''
        patchShebangs .
        export ASC=asc
        make all
      '';
    installPhase = ''
      touch $out
    '';
  };

  js = asc-bin.overrideAttrs (oldAttrs: {
    name = "asc.js";

    buildInputs = commonBuildInputs ++ [
      ocamlPackages.js_of_ocaml
      ocamlPackages.js_of_ocaml-ocamlbuild
      ocamlPackages.js_of_ocaml-ppx
      nixpkgs.nodejs-10_x
    ];

    buildPhase = ''
      make asc.js
    '';

    installPhase = ''
      mkdir -p $out
      cp -v asc.js $out
      cp -vr ${rts}/rts $out
    '';

    doInstallCheck = true;

    installCheckPhase = ''
      NODE_PATH=$out node --experimental-wasm-mut-global --experimental-wasm-mv ${./test/node-test.js}
    '';

  });

  didc = stdenv.mkDerivation {
    name = "didc";
    src = subpath ./src;
    buildInputs = commonBuildInputs;
    buildPhase = ''
      make DUNE_OPTS="--display=short --profile release" didc
    '';
    installPhase = ''
      mkdir -p $out/bin
      cp --verbose --dereference didc $out/bin
    '';
  };

  deser = stdenv.mkDerivation {
    name = "deser";
    src = subpath ./src;
    buildInputs = commonBuildInputs;
    buildPhase = ''
      make DUNE_OPTS="--display=short --profile release" deser
    '';
    installPhase = ''
      mkdir -p $out/bin
      cp --verbose --dereference deser $out/bin
    '';
  };

  wasm = ocaml_wasm;
  dvm = real-dvm;
  drun = real-drun;
  filecheck = nixpkgs.linkFarm "FileCheck"
    [ { name = "bin/FileCheck"; path = "${nixpkgs.llvm}/bin/FileCheck";} ];
  wabt = nixpkgs.wabt;


  users-guide = stdenv.mkDerivation {
    name = "users-guide";
    src = subpath ./guide;
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
      make
    '';

    installPhase = ''
      mkdir -p $out
      mv * $out/
      rm $out/Makefile
      mkdir -p $out/nix-support
      echo "report guide $out index.html" >> $out/nix-support/hydra-build-products
    '';
  };

  stdlib = stdenv.mkDerivation {
    name = "stdlib";
    src = subpath ./stdlib;
    buildInputs = with nixpkgs;
      [ bash ];
    buildPhase = ''
      patchShebangs .
    '';
    doCheck = true;
    checkInputs = [
      asc
      nixpkgs.python
    ];
    checkPhase = ''
      make ASC=${asc}/bin/asc alltests
    '';
    installPhase = ''
      mkdir -p $out
      tar -rf $out/stdlib.tar -C $src *.as
      mkdir -p $out/nix-support
      echo "report stdlib $out/stdlib.tar" >> $out/nix-support/hydra-build-products
    '';
    forceShare = ["man"];
  };

  stdlib-doc-live = stdenv.mkDerivation {
    name = "stdlib-doc-live";
    src = subpath ./stdlib;
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

  stdlib-doc = stdenv.mkDerivation {
    name = "stdlib-doc";
    src = subpath ./stdlib;
    buildInputs = with nixpkgs;
      [ pandoc bash python ];
    buildPhase = ''
      patchShebangs .
      make alldoc
    '';
    installPhase = ''
      mkdir -p $out
      tar -rf $out/stdlib-doc.tar -C doc .
      mkdir -p $out/nix-support
      echo "report stdlib-doc $out/stdlib-doc.tar" >> $out/nix-support/hydra-build-products
    '';
    forceShare = ["man"];
  };

  produce-exchange = stdenv.mkDerivation {
    name = "produce-exchange";
    src = subpath ./stdlib;
    buildInputs = [
      asc
    ];

    doCheck = true;
    buildPhase = ''
      make ASC=asc OUTDIR=_out _out/ProduceExchange.wasm
    '';
    checkPhase = ''
      make ASC=asc OUTDIR=_out _out/ProduceExchange.out
    '';
    installPhase = ''
      mkdir -p $out
      cp _out/ProduceExchange.wasm $out
    '';
  };

  all-systems-go = nixpkgs.releaseTools.aggregate {
    name = "all-systems-go";
    constituents = [
      asc
      as-ide
      js
      didc
      deser
      tests
      unit-tests
      samples
      rts
      stdlib
      stdlib-doc
      stdlib-doc-live
      produce-exchange
      users-guide
    ];
  };

  shell = if export-shell then nixpkgs.mkShell {
    #
    # Since building asc, and testing it, are two different derivations in we
    # have to create a fake derivation for `nix-shell` that commons up the
    # build dependencies of the two to provide a build environment that offers
    # both, while not actually building `asc`
    #

    buildInputs =
      let dont_build = [ asc didc deser ]; in
      nixpkgs.lib.lists.unique (builtins.filter (i: !(builtins.elem i dont_build)) (
        asc-bin.buildInputs ++
        js.buildInputs ++
        rts.buildInputs ++
        didc.buildInputs ++
        deser.buildInputs ++
        tests.buildInputs ++
        users-guide.buildInputs ++
        [ nixpkgs.ncurses ocamlPackages.merlin ]
      ));

    shellHook = llvmEnv;
    ESM=esm;
    JS_USER_LIBRARY=js-user-library;
    TOMMATHSRC = libtommath;
    NIX_FONTCONFIG_FILE = users-guide.NIX_FONTCONFIG_FILE;
  } else null;

}
