{
  replay ? 0,
  system ? builtins.currentSystem,
}:

let nixpkgs = import ./nix { inherit system; }; in

let stdenv = nixpkgs.stdenv; in

let subpath = p: import ./nix/gitSource.nix p; in

let dfinity-src =
  let env = builtins.getEnv "DFINITY_SRC"; in
  if env != "" then env else nixpkgs.sources.dfinity; in
let dfinity-pkgs = import dfinity-src { inherit (nixpkgs) system; }; in
let drun = dfinity-pkgs.drun or dfinity-pkgs.dfinity.drun; in

let ic-ref-pkgs = import nixpkgs.sources.ic-ref { inherit (nixpkgs) system; }; in
let ic-ref = ic-ref-pkgs.ic-ref; in

let haskellPackages = nixpkgs.haskellPackages.override {
      overrides = import nix/haskell-packages.nix nixpkgs subpath;
    }; in
let
  llvmBuildInputs = [
    nixpkgs.clang_10 # for native/wasm building
    nixpkgs.lld_10 # for wasm building
  ];

  # When compiling natively, we want to use `clang` (which is a nixpkgs
  # provided wrapper that sets various include paths etc).
  # But for some reason it does not handle building for Wasm well, so
  # there we use plain clang-10. There is no stdlib there anyways.
  llvmEnv = ''
    export CLANG="${nixpkgs.clang_10}/bin/clang"
    export WASM_CLANG="clang-10"
    export WASM_LD=wasm-ld
  '';
in

# When building for linux (but not in nix-shell) we build statically
let is_static = !nixpkgs.stdenv.isDarwin; in

let staticpkgs = if is_static then nixpkgs.pkgsMusl else nixpkgs; in

# This branches on the pkgs, which is either
# normal nixpkgs (nix-shell, darwin)
# nixpkgs.pkgsMusl for static building (release builds)
let commonBuildInputs = pkgs:
  [
    pkgs.dune_2
    pkgs.ocamlPackages.ocaml
    pkgs.ocamlPackages.atdgen
    pkgs.ocamlPackages.checkseum
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
  let common = import nixpkgs.sources.common { inherit (nixpkgs) system; }; in
  common.lib.standaloneRust; in

let ocaml_exe = name: bin:
  let
    profile =
      if is_static
      then "release-static"
      else "release";

    drv = staticpkgs.stdenv.mkDerivation {
      inherit name;

      ${if is_static then "allowedRequisites" else null} = [];

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
    then darwin_standalone { inherit drv; usePackager = false; exename = bin; }
    else drv;

  musl-wasi-sysroot = stdenv.mkDerivation {
    name = "musl-wasi-sysroot";
    src = nixpkgs.sources.musl-wasi;
    phases = [ "unpackPhase" "installPhase" ];
    installPhase = ''
      make SYSROOT="$out" include_dirs
    '';
  };
in

rec {
  rts = stdenv.mkDerivation {
    name = "moc-rts";

    src = subpath ./rts;
    nativeBuildInputs = [ nixpkgs.makeWrapper ];

    buildInputs = llvmBuildInputs;

    preBuild = ''
      ${llvmEnv}
      export TOMMATHSRC=${nixpkgs.sources.libtommath}
      export MUSLSRC=${nixpkgs.sources.musl-wasi}/libc-top-half/musl
      export MUSL_WASI_SYSROOT=${musl-wasi-sysroot}
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
  inherit (haskellPackages) lsp-int qc-motoko;

  inherit ic-ref;

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
            nixpkgs.sources.esm
          ] ++
          llvmBuildInputs;

        checkPhase = ''
            patchShebangs .
            ${llvmEnv}
            export MOC=moc
            export MO_LD=mo-ld
            export DIDC=didc
            export DESER=deser
            export ESM=${nixpkgs.sources.esm}
            type -p moc && moc --version
            # run this once to work around self-unpacking-race-condition
            type -p drun && drun --version
            make -C ${dir}
          '';
      }; in

    let perf_subdir = dir: deps:
      (test_subdir dir deps).overrideAttrs (args: {
        checkPhase = ''
          mkdir -p $out
          export PERF_OUT=$out/stats.csv
        '' + args.checkPhase + ''
          # export stats to hydra
          mkdir -p $out/nix-support
          tr '/;' '_\t' < $out/stats.csv > $out/nix-support/hydra-metrics

          # sanity check
          if ! grep -q ^gas/ $out/stats.csv
          then
            echo "perf stats do not include gas. change in drun output format?" >&2
            exit 1
          fi
        '';
      }); in

    let qc = testDerivation {
      name = "test-qc";
      buildInputs = [ moc /* nixpkgs.wasm */ wasmtime drun haskellPackages.qc-motoko ];
      checkPhase = ''
        qc-motoko${nixpkgs.lib.optionalString (replay != 0)
            " --quickcheck-replay=${toString replay}"}
      '';
    }; in

    let lsp = testDerivation {
      name = "test-lsp";
      src = subpath ./test/lsp-int-test-project;
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
      run-drun  = test_subdir "run-drun"  [ moc drun ic-ref ];
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
    src = subpath ./stdlib/src;
    phases = "unpackPhase installPhase";
    installPhase = ''
      mkdir -p $out
      cp ./*.mo $out
    '';
  };

  check-formatting = stdenv.mkDerivation {
    name = "check-formatting";
    buildInputs = with nixpkgs; [ ocamlformat ];
    src = subpath "./src";
    doCheck = true;
    phases = "unpackPhase checkPhase installPhase";
    installPhase = "touch $out";
    checkPhase = ''
      ocamlformat --check languageServer/*.{ml,mli}
    '';
  };

  stdlib-tests = stdenv.mkDerivation {
    name = "stdlib-tests";
    src = subpath ./stdlib/test;
    phases = "unpackPhase checkPhase installPhase";
    doCheck = true;
    installPhase = "touch $out";
    checkInputs = [
      nixpkgs.wasmtime
      moc
    ];
    checkPhase = ''
      make MOC=moc STDLIB=${stdlib}
    '';
  };

  examples =
    let example_subdir = dir: stdenv.mkDerivation {
      name = dir;
      src = subpath "./stdlib/examples/${dir}";
      phases = "unpackPhase checkPhase installPhase";
      doCheck = true;
      installPhase = "touch $out";
      buildInputs = [
        nixpkgs.bash
        moc
        nixpkgs.wasmtime
      ];
      checkPhase = ''
        make MOC=moc STDLIB=${stdlib}
      '';
    }; in
    {
      actorspec        = example_subdir "actorspec";
      rx               = example_subdir "rx";
      produce-exchange = example_subdir "produce-exchange";
    };


  stdlib-doc = stdenv.mkDerivation {
    name = "stdlib-doc";
    src = subpath ./stdlib/doc;
    outputs = [ "out" "adocs" ];
    buildInputs = with nixpkgs;
      [ bash perl asciidoctor html-proofer ];
    buildPhase = ''
      patchShebangs .
      make STDLIB=${stdlib}
    '';

    doCheck = true;
    # These ones are needed for htmlproofer
    LOCALE_ARCHIVE = nixpkgs.lib.optionalString nixpkgs.stdenv.isLinux "${nixpkgs.glibcLocales}/lib/locale/locale-archive";
    LANG = "en_US.UTF-8";
    LC_TYPE = "en_US.UTF-8";
    LANGUAGE = "en_US.UTF-8";
    checkPhase = ''
      htmlproofer --disable-external _out/
    '';

    installPhase = ''
      mkdir -p $out
      mv _out/* $out/
      mkdir -p $out/nix-support
      echo "report docs $out index.html" >> $out/nix-support/hydra-build-products

      mkdir -p $adocs
      mv _build/*.adoc $adocs/
    '';
  };
  stdlib-adocs = stdlib-doc.adocs;

  check-generated = nixpkgs.runCommandNoCC "check-generated" {
      nativeBuildInputs = [ nixpkgs.diffutils ];
      expected = import ./nix/generate.nix { pkgs = nixpkgs; };
      dir = ./nix/generated;
    } ''
      diff -r -U 3 $expected $dir
      touch $out
    '';

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
      stdlib-tests
      stdlib-doc
      stdlib-adocs
      users-guide
      ic-ref
      shell
      check-formatting
      check-generated
    ] ++ builtins.attrValues (builtins.removeAttrs tests ["qc"])
      ++ builtins.attrValues examples;
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
        [ nixpkgs.ncurses nixpkgs.ocamlPackages.merlin nixpkgs.ocamlformat nixpkgs.ocamlPackages.utop ] ++
        builtins.concatMap (d: d.buildInputs) (builtins.attrValues tests)
      ));

    shellHook = llvmEnv;
    ESM=nixpkgs.sources.esm;
    TOMMATHSRC = nixpkgs.sources.libtommath;
    MUSLSRC = "${nixpkgs.sources.musl-wasi}/libc-top-half/musl";
    MUSL_WASI_SYSROOT = musl-wasi-sysroot;
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
