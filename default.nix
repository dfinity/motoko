{
  replay ? 0,
  system ? builtins.currentSystem,
}:

let nixpkgs = (import ./nix/nixpkgs.nix).nixpkgs {
  inherit system;
  overlays = [
    # Adding wasmtime
    (self: super: { wasmtime = self.callPackage ./nix/wasmtime {}; })
    # Selecting the ocaml version
    (self: super: { ocamlPackages = self.ocaml-ng.ocamlPackages_4_07; })
    # Additional ocaml package
    (self: super: {
      ocamlPackages = super.ocamlPackages // {
        wasm = import ./nix/ocaml-wasm.nix {
          inherit (self) stdenv fetchFromGitHub ocaml;
          inherit (self.ocamlPackages) findlib ocamlbuild;
        };
        vlq = import ./nix/ocaml-vlq.nix {
          inherit (self) stdenv fetchFromGitHub ocaml dune;
          inherit (self.ocamlPackages) findlib;
        };
      };
    })
  ];
}; in

let llvm = import ./nix/llvm.nix { inherit (nixpkgs) system; }; in

let stdenv = nixpkgs.stdenv; in

let subpath = p: import ./nix/gitSource.nix p; in

let dfinity-src =
  let env = builtins.getEnv "DFINITY_SRC"; in
  if env != "" then env else builtins.fetchGit {
    name = "dfinity-sources";
    url = "ssh://git@github.com/dfinity-lab/dfinity";
    # ref = "master";
    rev = "48ab58e7bf4de892a5c5c926050a350947ed2514";
  }; in

let dfinity-pkgs = import dfinity-src { inherit (nixpkgs) system; }; in

let esm = nixpkgs.fetchzip {
  sha256 = "116k10q9v0yzpng9bgdx3xrjm2kppma2db62mnbilbi66dvrvz9q";
  url = "https://registry.npmjs.org/esm/-/esm-3.2.25.tgz";
}; in

let drun = dfinity-pkgs.drun or dfinity-pkgs.dfinity.drun; in

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
let staticpkgs =
  if nixpkgs.stdenv.isDarwin
  then nixpkgs
  else nixpkgs.pkgsMusl; in


# This branches on the pkgs, which is either
# normal nixpkgs (nix-shell, darwin)
# nixpkgs.pkgsMusl for static building (release builds)
let commonBuildInputs = pkgs:
  [
    pkgs.dune
    pkgs.ocamlPackages.ocaml
    pkgs.ocamlPackages.atdgen
    pkgs.ocamlPackages.findlib
    pkgs.ocamlPackages.menhir
    pkgs.ocamlPackages.num
    pkgs.ocamlPackages.stdint
    pkgs.ocamlPackages.wasm
    pkgs.ocamlPackages.vlq
    pkgs.ocamlPackages.zarith
    pkgs.ocamlPackages.yojson
    pkgs.ocamlPackages.ppxlib
    pkgs.ocamlPackages.ppx_inline_test
    pkgs.ocamlPackages.bisect_ppx
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

    drv = staticpkgs.stdenv.mkDerivation {
      inherit name;

      ${if nixpkgs.stdenv.isDarwin then null else "allowedRequisites"} = [];

      src = subpath ./src;

      buildInputs = commonBuildInputs staticpkgs;

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

  tests =
    let testDerivationArgs = {
      # by default, an empty source directory. how to best get an empty directory?
      src = builtins.path { name = "empty"; path = ./nix; filter = p: t: false; };
      phases = "unpackPhase checkPhase installPhase";
      doCheck = true;
      installPhase = "touch $out";
    }; in
    let testDerivation = args:
      stdenv.mkDerivation (testDerivationArgs // args); in
    let ocamlTestDerivation = args:
      staticpkgs.stdenv.mkDerivation (testDerivationArgs // args); in

    # we test each subdirectory of test/ in its own derivation with
    # cleaner dependencies, for more parallelism, more caching
    # and better feedback about what aspect broke
    let test_subdir = dir: deps:
      testDerivation {
        name = "test-${dir}";
        # include from test/ only the common files, plus everything in test/${dir}/
        src =
          with nixpkgs.lib;
          cleanSourceWith {
            filter = path: type:
              let relPath = removePrefix (toString ./test + "/") (toString path); in
              type != "directory" || hasPrefix "${dir}/" "${relPath}/";
            src = subpath ./test;
            name = "test-${dir}-src";
        };
        buildInputs =
          deps ++
          [ nixpkgs.wabt
            nixpkgs.bash
            nixpkgs.perl
            nixpkgs.getconf
            nixpkgs.moreutils
            nixpkgs.nodejs-10_x
            filecheck
            wasmtime
            esm
          ] ++
          llvmBuildInputs;

        checkPhase = ''
            patchShebangs .
            ${llvmEnv}
            export MOC=moc
            export MO_LD=mo-ld
            export DIDC=didc
            export DESER=deser
            export ESM=${esm}
            type -p moc && moc --version
            # run this once to work around self-unpacking-race-condition
            type -p drun && drun --version
            make -C ${dir}

	    if test -e ${dir}/_out/stats.csv
	    then
	      cp ${dir}/_out/stats.csv $out
	    fi
          '';
      }; in

    let perf_subdir = dir: deps:
      (test_subdir dir deps).overrideAttrs (args: {
        checkPhase = ''
          export PERF_OUT=$out
        '' + args.checkPhase;
      }); in

    let qc = testDerivation {
      name = "test-qc";
      buildInputs = [ moc /* nixpkgs.wasm */ wasmtime haskellPackages.qc-motoko ];
      checkPhase = ''
        qc-motoko${nixpkgs.lib.optionalString (replay != 0)
            " --quickcheck-replay=${toString replay}"}
      '';
    }; in

    let lsp = testDerivation {
      name = "test-lsp";
      src = subpath ./test/lsp-int/test-project;
      buildInputs = [ moc haskellPackages.lsp-int ];
      checkPhase = ''
        echo running lsp-int
        lsp-int ${mo-ide}/bin/mo-ide .
      '';
    }; in

    let unit-tests = ocamlTestDerivation {
      name = "unit-tests";
      src = subpath ./src;
      buildInputs = commonBuildInputs staticpkgs;
      checkPhase = ''
        make DUNE_OPTS="--display=short" unit-tests
      '';
      installPhase = ''
        touch $out
      '';
    }; in

    { run       = test_subdir "run"       [ moc ] ;
      run-drun  = test_subdir "run-drun"  [ moc drun ic-stub ];
      perf      = perf_subdir "perf"      [ moc drun ];
      fail      = test_subdir "fail"      [ moc ];
      repl      = test_subdir "repl"      [ moc ];
      ld        = test_subdir "ld"        [ mo-ld ];
      idl       = test_subdir "idl"       [ didc ];
      mo-idl    = test_subdir "mo-idl"    [ moc didc ];
      trap      = test_subdir "trap"      [ moc ];
      run-deser = test_subdir "run-deser" [ deser ];
      inherit qc lsp unit-tests;
    };

  samples = stdenv.mkDerivation {
    name = "samples";
    src = subpath ./samples;
    buildInputs = [ moc ];
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

  inherit drun;
  filecheck = nixpkgs.linkFarm "FileCheck"
    [ { name = "bin/FileCheck"; path = "${nixpkgs.llvm}/bin/FileCheck";} ];
  wabt = nixpkgs.wabt;
  wasmtime = nixpkgs.wasmtime;
  wasm = nixpkgs.wasm;

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
      samples
      rts
      stdlib
      stdlib-doc
      produce-exchange
      users-guide
      ic-stub
      shell
    ] ++ builtins.attrValues tests;
  };

  shell = nixpkgs.mkShell {
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
        js.buildInputs ++
        users-guide.buildInputs ++
        [ nixpkgs.ncurses nixpkgs.ocamlPackages.merlin nixpkgs.ocamlPackages.utop ] ++
        builtins.concatMap (d: d.buildInputs) (builtins.attrValues tests)
      ));

    shellHook = llvmEnv;
    ESM=esm;
    TOMMATHSRC = libtommath;
    NIX_FONTCONFIG_FILE = users-guide.NIX_FONTCONFIG_FILE;
    LOCALE_ARCHIVE = stdenv.lib.optionalString stdenv.isLinux "${nixpkgs.glibcLocales}/lib/locale/locale-archive";

    # allow building this as a derivation, so that hydra builds and caches
    # the dependencies of shell
    phases = ["dummyBuildPhase"];
    dummyBuildPhase = ''
      touch $out
    '';
    preferLocalBuild = true;
    allowSubstitutes = true;
  };
}
