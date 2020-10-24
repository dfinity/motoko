{
  replay ? 0,
  system ? builtins.currentSystem,
}:

let nixpkgs = import ./nix { inherit system; }; in

let stdenv = nixpkgs.stdenv; in

let subpath = p: import ./nix/gitSource.nix p; in

let dfinity-pkgs = import nixpkgs.sources.dfinity { inherit (nixpkgs) system; }; in
let drun = dfinity-pkgs.drun or dfinity-pkgs.dfinity.drun; in

let ic-ref-pkgs = import nixpkgs.sources.ic-ref { inherit (nixpkgs) system; }; in
let ic-ref = ic-ref-pkgs.ic-ref; in

let haskellPackages = nixpkgs.haskellPackages.override {
      overrides = import nix/haskell-packages.nix nixpkgs subpath;
    }; in
let
  rtsBuildInputs = with nixpkgs; [
    clang_10 # for native/wasm building
    lld_10 # for wasm building
    rustc-nightly
    cargo-nightly
    xargo
  ];

  llvmEnv = ''
    # When compiling natively, we want to use `clang` (which is a nixpkgs
    # provided wrapper that sets various include paths etc).
    # But for some reason it does not handle building for Wasm well, so
    # there we use plain clang-10. There is no stdlib there anyways.
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
    pkgs.ocamlPackages.cow
    pkgs.ocamlPackages.num
    pkgs.ocamlPackages.stdint
    pkgs.ocamlPackages.wasm
    pkgs.ocamlPackages.vlq
    pkgs.ocamlPackages.zarith
    pkgs.ocamlPackages.yojson
    pkgs.ocamlPackages.ppxlib
    pkgs.ocamlPackages.ppx_inline_test
    pkgs.ocamlPackages.ocaml-migrate-parsetree
    pkgs.ocamlPackages.ppx_tools_versioned
    pkgs.ocamlPackages.obelisk
    pkgs.ocamlPackages.uucp
    pkgs.perl
  ]; in

let darwin_standalone =
  let common = import (nixpkgs.sources.common + "/pkgs")
    { inherit (nixpkgs) system; repoRoot = ./.; }; in
  common.lib.standaloneRust; in

let ocaml_exe = name: bin: rts:
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

      # we only need to include the wasm statically when building moc, not
      # other binaries
      buildPhase = ''
        patchShebangs .
      '' + nixpkgs.lib.optionalString (rts != null)''
        ./rts/gen.sh ${rts}/rts
      '' + ''
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
    phases = [ "unpackPhase" "installPhase" "fixupPhase" ];
    installPhase = ''
      make SYSROOT="$out" include_dirs
    '';
  };
in

rec {
  rts =
    let
      rustDeps = nixpkgs.rustPlatform-nightly.fetchCargoTarball {
        name = "motoko-rts-deps";
        src = subpath rts/motoko-rts;
        sourceRoot = null;
        sha256 = "11la5fl0fgx6i5g52p56sf48yz7f0mqrgm38m320xh3wyqa2nim6";
        copyLockfile = true;
      };
    in
    stdenv.mkDerivation {
      name = "moc-rts";

      src = subpath ./rts;
      nativeBuildInputs = [ nixpkgs.makeWrapper nixpkgs.removeReferencesTo ];

      buildInputs = rtsBuildInputs;

      preBuild = ''
        export XARGO_HOME=$PWD/xargo-home
        export CARGO_HOME=$PWD/cargo-home

        # this replicates logic from nixpkgs’ pkgs/build-support/rust/default.nix
        mkdir -p $CARGO_HOME
        echo "Using vendored sources from ${rustDeps}"
        unpackFile ${rustDeps}
        cat > $CARGO_HOME/config <<__END__
          [source."crates-io"]
          "replace-with" = "vendored-sources"

          [source."vendored-sources"]
          "directory" = "$(stripHash ${rustDeps})"
        __END__

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
        cp mo-rts-debug.wasm $out/rts
      '';

      # This needs to be self-contained. Remove mention of
      # nix path in debug message.
      preFixup = ''
        remove-references-to -t ${nixpkgs.rustc-nightly} -t ${rustDeps} $out/rts/mo-rts.wasm $out/rts/mo-rts-debug.wasm
      '';
      allowedRequisites = [];
    };

  moc = ocaml_exe "moc" "moc" rts;
  mo-ld = ocaml_exe "mo-ld" "mo-ld" null;
  mo-ide = ocaml_exe "mo-ide" "mo-ide" null;
  mo-doc = ocaml_exe "mo-doc" "mo-doc" null;
  didc = ocaml_exe "didc" "didc" null;
  deser = ocaml_exe "deser" "deser" null;

  # “our” Haskell packages
  inherit (haskellPackages) lsp-int qc-motoko;

  inherit ic-ref;

  tests = let
    testDerivationArgs = {
      # by default, an empty source directory. how to best get an empty directory?
      src = builtins.path { name = "empty"; path = ./nix; filter = p: t: false; };
      phases = "unpackPhase checkPhase installPhase fixupPhase";
      doCheck = true;
      installPhase = "touch $out";
    };

    testDerivation = args:
      stdenv.mkDerivation (testDerivationArgs // args);

    ocamlTestDerivation = args:
      staticpkgs.stdenv.mkDerivation (testDerivationArgs // args);

    # we test each subdirectory of test/ in its own derivation with
    # cleaner dependencies, for more parallelism, more caching
    # and better feedback about what aspect broke
    test_subdir = dir: deps:
      testDerivation {
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
          (with nixpkgs; [ wabt bash perl getconf moreutils nodejs-10_x sources.esm ]) ++
          [ filecheck
            wasmtime
          ] ++
          rtsBuildInputs;

        checkPhase = ''
            patchShebangs .
            ${llvmEnv}
            export ESM=${nixpkgs.sources.esm}
            type -p moc && moc --version
            # run this once to work around self-unpacking-race-condition
            type -p drun && drun --version
            make -C ${dir}
          '';
      };

    # Run a variant with sanity checking on
    snty_subdir = dir: deps:
      (test_subdir dir deps).overrideAttrs (args: {
          EXTRA_MOC_ARGS = "--sanity-checks";
      });

    perf_subdir = dir: deps:
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
      });

    qc = testDerivation {
      buildInputs = [ moc /* nixpkgs.wasm */ wasmtime drun haskellPackages.qc-motoko ];
      checkPhase = ''
        qc-motoko${nixpkgs.lib.optionalString (replay != 0)
            " --quickcheck-replay=${toString replay}"}
      '';
    };

    lsp = testDerivation {
      src = subpath ./test/lsp-int-test-project;
      buildInputs = [ moc haskellPackages.lsp-int ];
      checkPhase = ''
        echo running lsp-int
        lsp-int ${mo-ide}/bin/mo-ide .
      '';
    };

    unit = ocamlTestDerivation {
      src = subpath ./src;
      buildInputs = commonBuildInputs staticpkgs;
      checkPhase = ''
        patchShebangs .
        make DUNE_OPTS="--display=short" unit-tests
      '';
      installPhase = ''
        touch $out
      '';
    };

    fix_names = builtins.mapAttrs (name: deriv:
      deriv.overrideAttrs (_old: { name = "test-${name}"; })
    );

  in fix_names {
      run        = test_subdir "run"        [ moc ] ;
      run-dbg    = snty_subdir "run"        [ moc ] ;
      drun       = test_subdir "run-drun"   [ moc drun ];
      drun-dbg   = snty_subdir "run-drun"   [ moc drun ];
      ic-ref-run = test_subdir "run-drun"   [ moc ic-ref ];
      perf       = perf_subdir "perf"       [ moc drun ];
      fail       = test_subdir "fail"       [ moc ];
      repl       = test_subdir "repl"       [ moc ];
      ld         = test_subdir "ld"         [ mo-ld ];
      idl        = test_subdir "idl"        [ didc ];
      mo-idl     = test_subdir "mo-idl"     [ moc didc ];
      trap       = test_subdir "trap"       [ moc ];
      run-deser  = test_subdir "run-deser"  [ deser ];
      inherit qc lsp unit;
    };

  samples = stdenv.mkDerivation {
    name = "samples";
    src = subpath ./samples;
    buildInputs = [ moc ];
    buildPhase = ''
      patchShebangs .
      make all
    '';
    installPhase = ''
      touch $out
    '';
  };

  js =
    let mk = n: with_rts:
      stdenv.mkDerivation {
        name = "${n}.js";
        src = subpath ./src;
        buildInputs = commonBuildInputs nixpkgs ++ [
          nixpkgs.ocamlPackages.js_of_ocaml
          nixpkgs.ocamlPackages.js_of_ocaml-ppx
          nixpkgs.nodejs-10_x
        ];
        buildPhase = ''
          patchShebangs .
          make ${n}.js
        '';
        installPhase = ''
          mkdir -p $out
          cp -v ${n}.js $out
        '' + (if with_rts then ''
          cp -vr ${rts}/rts $out
        '' else "");
        doInstallCheck = true;
        test = ./test + "/test-${n}.js";
        installCheckPhase = ''
          NODE_PATH=$out node --experimental-wasm-mut-global --experimental-wasm-mv $test
        '';
      };
    in
    {
      moc = mk "moc" true;
      didc = mk "didc" false;
    };

  inherit drun;
  filecheck = nixpkgs.linkFarm "FileCheck"
    [ { name = "bin/FileCheck"; path = "${nixpkgs.llvm}/bin/FileCheck";} ];
  wabt = nixpkgs.wabt;
  wasmtime = nixpkgs.wasmtime;
  xargo = nixpkgs.xargo;
  wasm = nixpkgs.wasm;

  overview-slides = stdenv.mkDerivation {
    name = "overview-slides";
    src = subpath ./doc;
    buildInputs = [ nixpkgs.pandoc nixpkgs.bash ];

    buildPhase = ''
      patchShebangs .
      make
    '';

    installPhase = ''
      mkdir -p $out
      mv overview-slides.html $out/
      mkdir -p $out/nix-support
      echo "report guide $out overview-slides.html" >> $out/nix-support/hydra-build-products
    '';
  };

  check-formatting = stdenv.mkDerivation {
    name = "check-formatting";
    buildInputs = with nixpkgs; [ ocamlformat ];
    src = subpath "./src";
    doCheck = true;
    phases = "unpackPhase checkPhase installPhase fixupPhase";
    installPhase = "touch $out";
    checkPhase = ''
      ocamlformat --check languageServer/*.{ml,mli} docs/*.{ml,mli}
    '';
  };

  check-rts-formatting = stdenv.mkDerivation {
    name = "check-rts-formatting";
    buildInputs = [ nixpkgs.cargo-nightly nixpkgs.rustfmt ];
    src = subpath ./rts/motoko-rts;
    doCheck = true;
    phases = "unpackPhase checkPhase installPhase fixupPhase";
    installPhase = "touch $out";
    checkPhase = ''
      cargo fmt -- --check
    '';
  };

  base-src = stdenv.mkDerivation {
    name = "base-src";
    phases = "unpackPhase installPhase fixupPhase";
    src = nixpkgs.sources.motoko-base + "/src";
    installPhase = ''
      mkdir -p $out
      cp -rv * $out
    '';
  };

  base-tests = stdenv.mkDerivation {
    name = "base-tests";
    src = nixpkgs.sources.motoko-base;
    phases = "unpackPhase checkPhase installPhase fixupPhase";
    doCheck = true;
    installPhase = "touch $out";
    checkInputs = [
      nixpkgs.wasmtime
      moc
    ];
    checkPhase = ''
      make MOC=moc VESSEL_PKGS="--package matchers ${nixpkgs.sources.motoko-matchers}/src" -C test
    '';
  };

  guide-examples-tc =  stdenv.mkDerivation {
    name = "guid-examples-tc";
    src = subpath doc/modules/language-guide/examples;
    phases = "unpackPhase checkPhase installPhase fixupPhase";
    doCheck = true;
    MOTOKO_BASE = base-src;
    installPhase = "touch $out";
    checkInputs = [
      moc
    ];
    checkPhase = ''
      patchShebangs .
      ./check.sh
    '';
  };

  base-doc = stdenv.mkDerivation {
    name = "base-doc";
    src = nixpkgs.sources.motoko-base;
    phases = "unpackPhase buildPhase installPhase fixupPhase";
    doCheck = true;
    buildInputs = [ mo-doc ];
    buildPhase = ''
      mo-doc
    '';
    installPhase = ''
      mkdir -p $out
      cp -rv docs/* $out/

      mkdir -p $out/nix-support
      echo "report docs $out index.html" >> $out/nix-support/hydra-build-products
    '';
  };

  check-generated = nixpkgs.runCommandNoCC "check-generated" {
      nativeBuildInputs = [ nixpkgs.diffutils ];
      expected = import ./nix/generate.nix { pkgs = nixpkgs; };
      dir = ./nix/generated;
    } ''
      diff -r -U 3 $expected $dir
      touch $out
    '';

  # Checks that doc/modules/language-guide/examples/grammar.txt is up-to-date
  check-grammar = stdenv.mkDerivation {
      name = "check-grammar";
      src = subpath ./src/gen-grammar;
      phases = "unpackPhase buildPhase installPhase fixupPhase";
      buildInputs = [ nixpkgs.diffutils nixpkgs.bash nixpkgs.ocamlPackages.obelisk ];
      buildPhase = ''
        patchShebangs .
        ./gen-grammar.sh ${./src/mo_frontend/parser.mly} > expected
        echo "If the following fails, please run:"
        echo "nix-shell --command 'make -C src grammar'"
        diff -r -U 3 ${./doc/modules/language-guide/examples/grammar.txt} expected
        echo "ok, all good"
      '';
      installPhase = ''
        touch $out
      '';
    };

  all-systems-go = nixpkgs.releaseTools.aggregate {
    name = "all-systems-go";
    constituents = [
      moc
      mo-ide
      mo-doc
      didc
      deser
      samples
      rts
      base-src
      base-tests
      base-doc
      overview-slides
      ic-ref
      shell
      check-formatting
      check-rts-formatting
      check-generated
      check-grammar
    ] ++
    builtins.attrValues (builtins.removeAttrs tests ["qc"]) ++
    builtins.attrValues js;
  };

  shell = nixpkgs.mkShell rec {
    #
    # Since building moc, and testing it, are two different derivations in we
    # have to create a fake derivation for `nix-shell` that commons up the
    # build dependencies of the two to provide a build environment that offers
    # both, while not actually building `moc`
    #

    propagatedBuildInputs =
      let dont_build = [ moc mo-ld didc deser ]; in
      nixpkgs.lib.lists.unique (builtins.filter (i: !(builtins.elem i dont_build)) (
        commonBuildInputs nixpkgs ++
        rts.buildInputs ++
        js.moc.buildInputs ++
        overview-slides.buildInputs ++
        builtins.concatMap (d: d.buildInputs) (builtins.attrValues tests) ++
        [ nixpkgs.ncurses
          nixpkgs.ocamlPackages.merlin
          nixpkgs.ocamlformat
          nixpkgs.ocamlPackages.utop
          nixpkgs.niv
        ]
      ));

    shellHook = llvmEnv + ''
      export PATH="${toString ./bin}:$PATH"
    '';
    ESM=nixpkgs.sources.esm;
    TOMMATHSRC = nixpkgs.sources.libtommath;
    MUSLSRC = "${nixpkgs.sources.musl-wasi}/libc-top-half/musl";
    MUSL_WASI_SYSROOT = musl-wasi-sysroot;
    LOCALE_ARCHIVE = stdenv.lib.optionalString stdenv.isLinux "${nixpkgs.glibcLocales}/lib/locale/locale-archive";
    MOTOKO_BASE = base-src;

    # allow building this as a derivation, so that hydra builds and caches
    # the dependencies of shell
    # Also mention the dependencies in the output, so that after `nix-build -A
    # shell` (or just `nix-build`) they are guaranteed to be present in the
    # local nix store.
    phases = ["installPhase" "fixupPhase"];
    installPhase = ''
      mkdir $out
    '';
    preferLocalBuild = true;
    allowSubstitutes = true;
  };
}
