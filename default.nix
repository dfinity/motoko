{ nixpkgs ? (import ./nix/nixpkgs.nix).nixpkgs {},
  dvm ? null,
  drun ? null,
  export-shell ? false,
  replay ? 0
}:

let llvm = import ./nix/llvm.nix { system = nixpkgs.system; }; in

let stdenv = nixpkgs.stdenv; in

let subpath = p: import ./nix/gitSource.nix p; in

let dev = import (builtins.fetchGit {
  name = "dev-sources";
  url = "ssh://git@github.com/dfinity-lab/dev";
  # ref = "master";
  rev = "6fca1936fcd027aaeaccab0beb51defeee38a0ff";
}) { system = nixpkgs.system; }; in

let dfinity-repo = import (builtins.fetchGit {
  name = "dfinity-sources";
  url = "ssh://git@github.com/dfinity-lab/dfinity";
  ref = "master";
  rev = "5c7efff0524adbf97d85b27adb180e6137a3428f";
}) { system = nixpkgs.system; }; in

let sdk = import (builtins.fetchGit {
  name = "sdk-sources";
  url = "ssh://git@github.com/dfinity-lab/sdk";
  ref = "paulyoung/js-user-library";
  rev = "42f15621bc5b228c7fd349cb52f265917d33a3a0";
}) { system = nixpkgs.system; }; in

let esm = builtins.fetchTarball {
  sha256 = "116k10q9v0yzpng9bgdx3xrjm2kppma2db62mnbilbi66dvrvz9q";
  url = "https://registry.npmjs.org/esm/-/esm-3.2.25.tgz";
}; in

let real-dvm =
  if dvm == null
  then dev.dvm
  else dvm; in

let real-drun =
  if drun == null
  then dfinity-repo.drun or dfinity-repo.dfinity.drun
  else drun; in

let js-user-library = sdk.js-user-library; in

let haskellPackages = nixpkgs.haskellPackages.override {
      overrides = import nix/haskell-packages.nix nixpkgs subpath;
    }; in
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

# When building for linux (but not in nix-shell) we build statically
# (We should probably just figure out how to use nix overlays to add this to nixpkgs)
let ocamlpkgs =
  if nixpkgs.stdenv.isDarwin
  then nixpkgs
  else nixpkgs.pkgsMusl; in

let ocaml_wasm_static =
  import ./nix/ocaml-wasm.nix {
    inherit (ocamlpkgs) stdenv fetchFromGitHub ocaml;
    inherit (ocamlpkgs.ocamlPackages) findlib ocamlbuild;
  }; in

# This branches on the pkgs, which is either
# normal nixpkgs (nix-shell, darwin)
# nixpkgs.pkgsMusl for static building (release builds)
let commonBuildInputs = pkgs:
  let ocaml_wasm = import ./nix/ocaml-wasm.nix {
    inherit (pkgs) stdenv fetchFromGitHub ocaml;
    inherit (pkgs.ocamlPackages) findlib ocamlbuild;
  }; in

  let ocaml_vlq = import ./nix/ocaml-vlq.nix {
    inherit (pkgs) stdenv fetchFromGitHub ocaml dune;
    inherit (pkgs.ocamlPackages) findlib;
  }; in

  [
    pkgs.ocaml
    pkgs.dune
    pkgs.ocamlPackages.atdgen
    pkgs.ocamlPackages.findlib
    pkgs.ocamlPackages.menhir
    pkgs.ocamlPackages.num
    pkgs.ocamlPackages.stdint
    ocaml_wasm
    ocaml_vlq
    pkgs.ocamlPackages.zarith
    pkgs.ocamlPackages.yojson
    pkgs.ocamlPackages.ppxlib
    pkgs.ocamlPackages.ppx_inline_test
    pkgs.ocamlPackages.bisect_ppx
    pkgs.ocamlPackages.bisect_ppx-ocamlbuild
    pkgs.ocamlPackages.ocaml-migrate-parsetree
    pkgs.ocamlPackages.ppx_tools_versioned
  ]; in

let darwin_standalone =
  import nix/standalone-darwin.nix {
    inherit (nixpkgs) runCommandNoCC stdenv removeReferencesTo lib;
    grep = nixpkgs.gnugrep;
  }; in

let ocaml_exe = name: bin:
  let
    profile =
      if nixpkgs.stdenv.isDarwin
      then "release"
      else "release-static";

    drv = ocamlpkgs.stdenv.mkDerivation {
      inherit name;

      ${if nixpkgs.stdenv.isDarwin then null else "allowedRequisites"} = [];

      src = subpath ./src;

      buildInputs = commonBuildInputs ocamlpkgs;

      buildPhase = ''
        make DUNE_OPTS="--display=short --profile ${profile}" ${bin}
      '';

      installPhase = ''
        mkdir -p $out/bin
        cp --verbose --dereference ${bin} $out/bin
      '';
    };
  in
    # Make standalone on darwin (nothing to do on linux, is static)
    if nixpkgs.stdenv.isDarwin
    then darwin_standalone { inherit drv; exename = bin; }
    else drv;
in

rec {
  rts = stdenv.mkDerivation {
    name = "moc-rts";

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
      cp mo-rts.wasm $out/rts
    '';
  };

  moc-bin = ocaml_exe "moc-bin" "moc";
  mo-ld = ocaml_exe "mo-ld" "mo-ld";
  mo-ide = ocaml_exe "mo-ide" "mo-ide";
  didc = ocaml_exe "didc" "didc";
  deser = ocaml_exe "deser" "deser";

  moc = nixpkgs.symlinkJoin {
    name = "moc";
    paths = [ moc-bin rts ];
    buildInputs = [ nixpkgs.makeWrapper ];
    postBuild = ''
      wrapProgram $out/bin/moc \
        --set-default MOC_RTS "$out/rts/mo-rts.wasm"
    '';
  };

  # “our” Haskell packages
  inherit (haskellPackages) lsp-int qc-motoko ic-stub;

  tests = stdenv.mkDerivation {
    name = "tests";
    src = subpath ./test;
    buildInputs =
      [ moc
        mo-ld
        didc
        deser
        ocaml_wasm_static
        nixpkgs.wabt
        nixpkgs.bash
        nixpkgs.perl
        nixpkgs.getconf
        nixpkgs.moreutils
        nixpkgs.nodejs-10_x
        filecheck
        js-user-library
        dvm
        drun
        haskellPackages.qc-motoko
        haskellPackages.lsp-int
        ic-stub
        esm
      ] ++
      llvmBuildInputs;

    buildPhase = ''
        patchShebangs .
        ${llvmEnv}
        export MOC=moc
        export MO_LD=mo-ld
        export DIDC=didc
        export DESER=${deser}/bin/deser
        export ESM=${esm}
        export JS_USER_LIBRARY=${js-user-library}
        moc --version
        drun --version # run this once to work around self-unpacking-race-condition
        make parallel
        qc-motoko${nixpkgs.lib.optionalString (replay != 0)
          " --quickcheck-replay=${toString replay}"}
        cp -R ${subpath ./test/lsp-int/test-project} test-project
        find ./test-project -type d -exec chmod +w {} +
        lsp-int ${mo-ide}/bin/mo-ide ./test-project
      '';

    installPhase = ''
      touch $out
    '';
  };

  unit-tests = ocamlpkgs.stdenv.mkDerivation {
    name = "unit-tests";

    src = subpath ./src;

    buildInputs = commonBuildInputs ocamlpkgs;

    buildPhase = ''
      make DUNE_OPTS="--display=short" unit-tests
    '';

    installPhase = ''
      touch $out
    '';
  };

  samples = stdenv.mkDerivation {
    name = "samples";
    src = subpath ./samples;
    buildInputs =
      [ moc
        didc
        ocaml_wasm_static
        nixpkgs.wabt
        nixpkgs.bash
        nixpkgs.perl
        filecheck
        real-drun
      ] ++
      llvmBuildInputs;

    buildPhase = ''
        patchShebangs .
        export MOC=moc
        make all
      '';
    installPhase = ''
      touch $out
    '';
  };

  js = stdenv.mkDerivation {
    name = "moc.js";

    src = subpath ./src;

    buildInputs = commonBuildInputs nixpkgs ++ [
      nixpkgs.ocamlPackages.js_of_ocaml
      nixpkgs.ocamlPackages.js_of_ocaml-ocamlbuild
      nixpkgs.ocamlPackages.js_of_ocaml-ppx
      nixpkgs.nodejs-10_x
    ];

    buildPhase = ''
      make moc.js
    '';

    installPhase = ''
      mkdir -p $out
      cp -v moc.js $out
      cp -vr ${rts}/rts $out
    '';

    doInstallCheck = true;

    installCheckPhase = ''
      NODE_PATH=$out node --experimental-wasm-mut-global --experimental-wasm-mv ${./test/node-test.js}
    '';
  };

  wasm = ocaml_wasm_static;
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
      moc
      nixpkgs.python
    ];
    checkPhase = ''
      make MOC=${moc}/bin/moc alltests
    '';
    installPhase = ''
      mkdir -p $out
      cp ./*.mo $out
      rm $out/*Test.mo
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
      mv doc $out/
      mkdir -p $out/nix-support
      echo "report docs $out/doc README.html" >> $out/nix-support/hydra-build-products
    '';
    forceShare = ["man"];
  };

  produce-exchange = stdenv.mkDerivation {
    name = "produce-exchange";
    src = subpath ./stdlib;
    buildInputs = [
      moc
    ];

    doCheck = true;
    buildPhase = ''
      make MOC=moc OUTDIR=_out _out/ProduceExchange.wasm
    '';
    checkPhase = ''
      make MOC=moc OUTDIR=_out _out/ProduceExchange.out
    '';
    installPhase = ''
      mkdir -p $out
      cp _out/ProduceExchange.wasm $out
    '';
  };

  all-systems-go = nixpkgs.releaseTools.aggregate {
    name = "all-systems-go";
    constituents = [
      moc
      mo-ide
      js
      didc
      deser
      tests
      unit-tests
      samples
      rts
      stdlib
      stdlib-doc
      produce-exchange
      users-guide
      ic-stub
    ];
  };

  shell = if export-shell then nixpkgs.mkShell {
    #
    # Since building moc, and testing it, are two different derivations in we
    # have to create a fake derivation for `nix-shell` that commons up the
    # build dependencies of the two to provide a build environment that offers
    # both, while not actually building `moc`
    #

    buildInputs =
      let dont_build = [ moc mo-ld didc deser ]; in
      nixpkgs.lib.lists.unique (builtins.filter (i: !(builtins.elem i dont_build)) (
        commonBuildInputs nixpkgs ++
        rts.buildInputs ++
        tests.buildInputs ++
        js.buildInputs ++
        users-guide.buildInputs ++
        [ nixpkgs.ncurses nixpkgs.ocamlPackages.merlin nixpkgs.ocamlPackages.utop ]
      ));

    shellHook = llvmEnv;
    ESM=esm;
    JS_USER_LIBRARY=js-user-library;
    TOMMATHSRC = libtommath;
    NIX_FONTCONFIG_FILE = users-guide.NIX_FONTCONFIG_FILE;
    LOCALE_ARCHIVE = stdenv.lib.optionalString stdenv.isLinux "${nixpkgs.glibcLocales}/lib/locale/locale-archive";

  } else null;

}
