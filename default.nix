{
  replay ? 0,
  system ? builtins.currentSystem,

  # version to embed in the release. Used by `.github/workflows/release.yml`
  releaseVersion ? null,
}:

let nixpkgs = import ./nix { inherit system; }; in

let stdenv = nixpkgs.stdenv; in

let subpath = import ./nix/gitSource.nix; in

let ic-hs-pkgs = import nixpkgs.sources.ic-hs { inherit (nixpkgs) system; }; in
let ic-ref-run =
  # copy out the binary, to remove dependencies on the libraries
  nixpkgs.runCommandNoCC "ic-ref-run" {} ''
      mkdir -p $out/bin
      cp ${ic-hs-pkgs.ic-hs}/bin/ic-ref-run $out/bin
  ''; in

let haskellPackages = nixpkgs.haskellPackages.override {
      overrides = import nix/haskell-packages.nix nixpkgs subpath;
    }; in
let
  rtsBuildInputs = with nixpkgs; [
    # pulls in clang (wrapped) and clang-12 (unwrapped)
    llvmPackages_12.clang
    # pulls in wasm-ld
    llvmPackages_12.lld
    llvmPackages_12.bintools
    rustc-nightly
    cargo-nightly
    wasmtime
    rust-bindgen
    rustfmt
    python3
    tree
  ] ++ pkgs.lib.optional pkgs.stdenv.isDarwin [
    libiconv
    # The following are usually also necessary:
    # pkgs.darwin.apple_sdk.frameworks.Security
    # pkgs.darwin.apple_sdk.frameworks.CoreFoundation
  ];

  llvmEnv = ''
    # When compiling to wasm, we want to have more control over the flags,
    # so we do not use the nix-provided wrapper in clang
    export WASM_CLANG="clang-12"
    export WASM_LD=wasm-ld
    # because we use the unwrapped clang, we have to pass in some flags/paths
    # that otherwise the wrapped clang would take care for us
    export WASM_CLANG_LIB="${nixpkgs.llvmPackages_12.clang-unwrapped.lib}"

    # When compiling natively, we want to use `clang` (which is a nixpkgs
    # provided wrapper that sets various include paths etc).
    # But for some reason it does not handle building for Wasm well, so
    # there we use plain clang-12. There is no stdlib there anyways.
    export CLANG="${nixpkgs.clang_12}/bin/clang"
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
    pkgs.ocamlPackages.ppx_blob
    pkgs.ocamlPackages.ppx_inline_test
    pkgs.ocamlPackages.ocaml-migrate-parsetree
    pkgs.ocamlPackages.ppx_tools_versioned
    pkgs.ocamlPackages.obelisk
    pkgs.ocamlPackages.uucp
    pkgs.perl
    pkgs.removeReferencesTo
  ]; in

let ocaml_exe = name: bin: rts:
  let
    profile =
      if is_static
      then "release-static"
      else "release";
  in
    staticpkgs.stdenv.mkDerivation {
      inherit name;

      allowedRequisites = [];

      src = subpath ./src;

      buildInputs = commonBuildInputs staticpkgs;

      MOTOKO_RELEASE = releaseVersion;

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
      '' + nixpkgs.lib.optionalString nixpkgs.stdenv.isDarwin ''
        # there are references to darwin system libraries
        # in the binaries. But curiously, we can remove them
        # an the binaries still work. They are essentially static otherwise.
        remove-references-to \
          -t ${nixpkgs.darwin.Libsystem} \
          -t ${nixpkgs.darwin.CF} \
          -t ${nixpkgs.libiconv} \
          $out/bin/*
      '' + ''
        # also, there is a refernece to /nix/store/…/share/menhir/standard.mly.
        # Let's remove that, too
        remove-references-to \
          -t ${staticpkgs.ocamlPackages.menhir} \
          $out/bin/*
        # sanity check
        $out/bin/* --help >/dev/null
      '';
    };

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
  rts =
    let
      cargoLockUtils = nixpkgs.callPackage ./cargo-lock-utils {
        pkgs = nixpkgs;
      };

      rustCompilerDepsVendor = rust: sha256:
        nixpkgs.stdenvNoCC.mkDerivation {
          name = "rust-deps";
          version = rust.name;

          nativeBuildInputs = with nixpkgs; [
            curl
          ];

          buildCommand = ''
            mkdir downloads
            cd downloads
            curlVersion=$(curl -V | head -1 | cut -d' ' -f2)
            curl=(
               curl
               --location
               --max-redir 20
               --retry 3
               --user-agent "curl/$curlVersion Nixpkgs/$nixpkgsVersion"
               --insecure
            )
            ${cargoLockUtils}/bin/list-cargo-lock ${rust}/lib/rustlib/src/rust/Cargo.lock | while read url name checksum; do
              "''${curl[@]}" "$url" | tar xz
              (
                 cd $name;
                 ${cargoLockUtils}/bin/compute-checksum "$(pwd)" "$checksum" > .cargo-checksum.json
              )
            done
            mkdir $out
            cp -r * $out/
            cp ${rust}/lib/rustlib/src/rust/Cargo.lock $out/
          '';

          outputHashAlgo = "sha256";
          outputHash = sha256;
          outputHashMode = "recursive";
        };

        rustCargoDepsHash = "0rpq3gjvd8px7vkyrby9gwa6y95g64zxa462aph56lfq2lkzxz41";

        rustCompilerDeps = rustCompilerDepsVendor nixpkgs.rustc-nightly rustCargoDepsHash;

        mergeCargo = projectCargoLock: rustCargoLock:
          nixpkgs.stdenvNoCC.mkDerivation {
            name = "merged-cargo-vendors";

            buildCommand = ''
              mkdir -p $out
              find ${projectCargoLock} -mindepth 1 -maxdepth 1 -type d \
                  -exec ln -sf {} $out/ \;
              find ${rustCargoLock} -mindepth 1 -maxdepth 1 -type d \
                  -exec ln -sf {} $out/ \;
              cp ${projectCargoLock}/Cargo.lock $out/
            '';
          };

        cargoProjectDeps = nixpkgs.rustPlatform.fetchCargoTarball {
          name = "motoko-rts-deps";
          src = subpath ./rts;
          sourceRoot = "rts/motoko-rts-tests";
          sha256 = "129gfmn96vm7di903pxirg7zybl83q6nkwiqr3rsy7l1q8667kxx";
          copyLockfile = true;
        };

        cargoProjectDepsUnpacked = nixpkgs.stdenvNoCC.mkDerivation {
          name = cargoProjectDeps.name + "-unpacked";
          buildCommand = ''
            tar xf ${cargoProjectDeps}
            mv *.tar.gz $out
          '';
        };

        cargoDeps = mergeCargo cargoProjectDepsUnpacked rustCompilerDeps;
    in

    stdenv.mkDerivation {
      name = "moc-rts";

      src = subpath ./rts;

      nativeBuildInputs = [ nixpkgs.makeWrapper nixpkgs.removeReferencesTo nixpkgs.cacert ];

      buildInputs = rtsBuildInputs;

      preBuild = ''
        export CARGO_HOME=$PWD/cargo-home

        ${llvmEnv}
        export TOMMATHSRC=${nixpkgs.sources.libtommath}
        export MUSLSRC=${nixpkgs.sources.musl-wasi}/libc-top-half/musl
        export MUSL_WASI_SYSROOT=${musl-wasi-sysroot}

        # This replicates logic from nixpkgs’ pkgs/build-support/rust/default.nix
        mkdir -p $CARGO_HOME
        echo "Using vendored sources from ${cargoProjectDeps}"
        unpackFile ${cargoDeps}
        cat > $CARGO_HOME/config <<__END__
          [source."crates-io"]
          "replace-with" = "vendored-sources"
          [source."vendored-sources"]
          "directory" = "$(stripHash ${cargoDeps})"
        __END__
      '';

      doCheck = true;

      checkPhase = ''
	make test
      '';

      installPhase = ''
        mkdir -p $out/rts
        cp mo-rts.wasm $out/rts
        cp mo-rts-debug.wasm $out/rts
      '';

      # This needs to be self-contained. Remove mention of nix path in debug
      # message.
      preFixup = ''
        remove-references-to \
          -t ${nixpkgs.rustc-nightly} \
          -t ${cargoDeps} \
          -t ${cargoProjectDepsUnpacked} \
          -t ${rustCompilerDeps} \
          $out/rts/mo-rts.wasm $out/rts/mo-rts-debug.wasm
      '';
      allowedRequisites = [];
    };

  moc = ocaml_exe "moc" "moc" rts;
  mo-ld = ocaml_exe "mo-ld" "mo-ld" null;
  mo-ide = ocaml_exe "mo-ide" "mo-ide" null;
  mo-doc = ocaml_exe "mo-doc" "mo-doc" null;
  didc = ocaml_exe "didc" "didc" null;
  deser = ocaml_exe "deser" "deser" null;
  candid-tests = ocaml_exe "candid-tests" "candid-tests" null;

  # “our” Haskell packages
  inherit (haskellPackages) lsp-int qc-motoko;

  inherit ic-ref-run;

  tests = let
    testDerivationArgs = {
      # by default, an empty source directory. how to best get an empty directory?
      src = builtins.path { name = "empty"; path = ./nix; filter = p: t: false; };
      phases = "unpackPhase checkPhase installPhase";
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
    # so include from test/ only the common files, plus everything in test/${dir}/
    test_src = dir:
      with nixpkgs.lib;
      cleanSourceWith {
        filter = path: type:
          let relPath = removePrefix (toString ./test + "/") (toString path); in
          type != "directory" || hasPrefix "${dir}/" "${relPath}/";
        src = subpath ./test;
        name = "test-${dir}-src";
      };

    test_subdir = dir: deps:
      testDerivation {
        src = test_src dir;
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
            make -C ${dir}
          '';
      };

    # Run a variant with sanity checking on
    snty_subdir = dir: deps:
      (test_subdir dir deps).overrideAttrs (args: {
          EXTRA_MOC_ARGS = "--sanity-checks";
      });

    compacting_gc_subdir = dir: deps:
      (test_subdir dir deps).overrideAttrs (args: {
          EXTRA_MOC_ARGS = "--sanity-checks --compacting-gc";
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
      buildInputs =
        [ moc wasmtime haskellPackages.qc-motoko nixpkgs.drun ];
      checkPhase = ''
	export LANG=C.utf8 # for haskell
        qc-motoko${nixpkgs.lib.optionalString (replay != 0)
            " --quickcheck-replay=${toString replay}"}
      '';
    };

    lsp = testDerivation {
      src = subpath ./test/lsp-int-test-project;
      buildInputs = [ moc haskellPackages.lsp-int ];
      checkPhase = ''
        echo running lsp-int
	export LANG=C.utf8 # for haskell
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

    candid = testDerivation {
      buildInputs = [ moc wasmtime candid-tests ];
      checkPhase = ''
        candid-tests -i ${nixpkgs.sources.candid}/test
      '';
    };

    profiling-graphs = testDerivation {
      src = test_src "perf";
      buildInputs =
        (with nixpkgs; [ perl wabt wasm-profiler-instrument wasm-profiler-postproc flamegraph-bin ]) ++
        [ moc nixpkgs.drun ];
      checkPhase = ''
        patchShebangs .
        type -p moc && moc --version
        type -p drun && drun --version
        ./profile-report.sh
      '';
      installPhase = ''
        mv _profile $out;
        mkdir -p $out/nix-support
        echo "report flamegraphs $out index.html" >> $out/nix-support/hydra-build-products
      '';
    };


    fix_names = builtins.mapAttrs (name: deriv:
      deriv.overrideAttrs (_old: { name = "test-${name}"; })
    );

  in fix_names ({
      run        = test_subdir "run"        [ moc ] ;
      run-dbg    = snty_subdir "run"        [ moc ] ;
      ic-ref-run = test_subdir "run-drun"   [ moc ic-ref-run ];
      ic-ref-run-compacting-gc = compacting_gc_subdir "run-drun" [ moc ic-ref-run ] ;
      drun       = test_subdir "run-drun"   [ moc nixpkgs.drun ];
      drun-dbg   = snty_subdir "run-drun"   [ moc nixpkgs.drun ];
      drun-compacting-gc = compacting_gc_subdir "run-drun" [ moc nixpkgs.drun ] ;
      fail       = test_subdir "fail"       [ moc ];
      repl       = test_subdir "repl"       [ moc ];
      ld         = test_subdir "ld"         [ mo-ld ];
      idl        = test_subdir "idl"        [ didc ];
      mo-idl     = test_subdir "mo-idl"     [ moc didc ];
      trap       = test_subdir "trap"       [ moc ];
      run-deser  = test_subdir "run-deser"  [ deser ];
      perf       = perf_subdir "perf"       [ moc nixpkgs.drun ];
      inherit qc lsp unit candid profiling-graphs;
    }) // { recurseForDerivations = true; };

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
    let mk = n:
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
          '' + nixpkgs.lib.optionalString (rts != null)''
          ./rts/gen.sh ${rts}/rts/
          '' + ''
          make DUNE_OPTS="--profile=release" ${n}.js
        '';
        installPhase = ''
          mkdir -p $out
          mkdir -p $out/bin
          cp --verbose --dereference ${n}.js $out/bin
        '';
        doInstallCheck = true;
        test = ./test + "/test-${n}.js";
        installCheckPhase = ''
          NODE_PATH=$out/bin node --experimental-wasm-mut-global --experimental-wasm-mv $test
        '';
      };
    in
    {
      moc = mk "moc";
      moc_interpreter = mk "moc_interpreter";
      didc = mk "didc";
      recurseForDerivations = true;
    };

  inherit (nixpkgs) wabt wasmtime wasm;

  filecheck = nixpkgs.linkFarm "FileCheck"
    [ { name = "bin/FileCheck"; path = "${nixpkgs.llvm}/bin/FileCheck";} ];

  # gitMinimal is used by nix/gitSource.nix; building it here warms the nix cache
  inherit (nixpkgs) gitMinimal;

  docs = stdenv.mkDerivation {
    name = "docs";
    src = subpath ./doc;
    buildInputs = with nixpkgs; [ pandoc bash antora gitMinimal ];

    buildPhase = ''
      patchShebangs .
      # Make this a git repo, to please antora
      git -C .. init
      git add .
      git config user.name "Nobody"
      git config user.email "nobody@example.com"
      git commit -m 'Dummy commit for antora'
      export HOME=$PWD
      export MOC_JS=${js.moc}/bin/moc.js
      export MOTOKO_BASE=${base-src}
      make
    '';

    installPhase = ''
      mkdir -p $out
      mv overview-slides.html $out/
      mv build/site/* $out/
      mkdir -p $out/nix-support
      echo "report guide $out docs/language-guide/motoko.html" >> $out/nix-support/hydra-build-products
      echo "report slides $out overview-slides.html" >> $out/nix-support/hydra-build-products
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
      ocamlformat --check languageServer/*.{ml,mli} docs/*.{ml,mli}
    '';
  };

  check-rts-formatting = stdenv.mkDerivation {
    name = "check-rts-formatting";
    buildInputs = [ nixpkgs.cargo-nightly nixpkgs.rustfmt ];
    src = subpath ./rts;
    doCheck = true;
    phases = "unpackPhase checkPhase installPhase";
    checkPhase = ''
      cargo fmt --verbose --manifest-path motoko-rts/Cargo.toml -- --check
      cargo fmt --verbose --manifest-path motoko-rts-tests/Cargo.toml -- --check
    '';
    installPhase = "touch $out";
  };

  base-src = stdenv.mkDerivation {
    name = "base-src";
    phases = "unpackPhase installPhase";
    src = nixpkgs.sources.motoko-base + "/src";
    installPhase = ''
      mkdir -p $out
      cp -rv * $out
    '';
  };

  base-tests = stdenv.mkDerivation {
    name = "base-tests";
    src = nixpkgs.sources.motoko-base;
    phases = "unpackPhase checkPhase installPhase";
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
    phases = "unpackPhase checkPhase installPhase";
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
    phases = "unpackPhase buildPhase installPhase";
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
      phases = "unpackPhase buildPhase installPhase";
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

  check-error-codes = stdenv.mkDerivation {
      name = "check-error-codes";
      src = subpath ./test;
      phases = "unpackPhase buildPhase installPhase";
      buildInputs = [ nixpkgs.python3 ];
      buildPhase = ''
      patchShebangs .
      ./check-error-codes.py ${./src/lang_utils/error_codes.ml}
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
      docs
      ic-ref-run
      shell
      check-formatting
      check-rts-formatting
      check-generated
      check-grammar
      check-error-codes
    ] ++
    builtins.attrValues (builtins.removeAttrs tests ["qc"]) ++
    builtins.attrValues js;
  };

  shell = stdenv.mkDerivation {
    name = "motoko-shell";

    #
    # Since building moc, and testing it, are two different derivations in we
    # have to create a fake derivation for `nix-shell` that commons up the
    # build dependencies of the two to provide a build environment that offers
    # both, while not actually building `moc`
    #
    propagatedBuildInputs =
      let dont_build = [ moc mo-ld didc deser candid-tests ]; in
      nixpkgs.lib.lists.unique (builtins.filter (i: !(builtins.elem i dont_build)) (
        commonBuildInputs nixpkgs ++
        rts.buildInputs ++
        js.moc.buildInputs ++
        docs.buildInputs ++
        builtins.concatMap (d: d.buildInputs or []) (builtins.attrValues tests) ++
        [ nixpkgs.ncurses
          nixpkgs.ocamlPackages.merlin
          nixpkgs.ocamlformat
          nixpkgs.ocamlPackages.utop
          nixpkgs.niv
          nixpkgs.rlwrap # for `rlwrap moc`
        ]
      ));

    shellHook = llvmEnv + ''
      # Include our wrappers in the PATH
      export PATH="${toString ./bin}:$PATH"
      # some cleanup of environment variables otherwise set by nix-shell
      # that would be confusing in interactive use
      unset XDG_DATA_DIRS
    '';
    ESM=nixpkgs.sources.esm;
    TOMMATHSRC = nixpkgs.sources.libtommath;
    MUSLSRC = "${nixpkgs.sources.musl-wasi}/libc-top-half/musl";
    MUSL_WASI_SYSROOT = musl-wasi-sysroot;
    LOCALE_ARCHIVE = nixpkgs.lib.optionalString stdenv.isLinux "${nixpkgs.glibcLocales}/lib/locale/locale-archive";
    MOTOKO_BASE = base-src;
    CANDID_TESTS = "${nixpkgs.sources.candid}/test";

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
