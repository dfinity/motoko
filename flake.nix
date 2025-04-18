{
  description = "The Motoko compiler";

  inputs = {
    # This is a recent commit from release-24.11. The reason we don't just specify release-24.11 is
    # that Hydra hasn't built all packages for darwin yet. So we manually use a slightly older
    # commit where things like LLVM have been built already. See:
    # https://hydra.nixos.org/job/nixpkgs/nixpkgs-24.11-darwin/llvm.aarch64-darwin
    nixpkgs.url = "github:NixOS/nixpkgs/5051ae6744b993fcfab221e8bd38f8bc26f88393";
    flake-utils.url = "github:numtide/flake-utils";

    nix-update-flake.url = "github:Mic92/nix-update";
    nix-update-flake.inputs.nixpkgs.follows = "nixpkgs";

    rust-overlay.url = "github:oxalica/rust-overlay";
    rust-overlay.inputs.nixpkgs.follows = "nixpkgs";

    candid-src = {
      url = "github:dfinity/candid/30c388671462aecdc4a3a9753d50dc2e8208c200";
      flake = false;
    };
    esm = {
      url = "https://registry.npmjs.org/esm/-/esm-3.2.25.tgz";
      flake = false;
    };
    ic-src = {
      url = "github:luc-blaeser/ic/12fec30a322fa5c39d97ea9385f2cdcaa22f6696";
      flake = false;
    };
    ic-wasm-src = {
      url = "github:dfinity/ic-wasm/4c52e75c12bb730e795d8a4c2862987f4a9524a3";
      flake = false;
    };
    libtommath-src = {
      url = "github:libtom/libtommath/v1.2.0";
      flake = false;
    };
    motoko-base-src = {
      url = "github:dfinity/motoko-base/d8896730c77f4ba58fe2f30ef289ba976f1fca0e";
      flake = false;
    };
    motoko-matchers-src = {
      url = "github:kritzcreek/motoko-matchers/cb838c192df3328ff9ae172e2dc7338cf55e74bf";
      flake = false;
    };
    ocaml-vlq-src = {
      url = "github:flowtype/ocaml-vlq/115bf0fef38018f31ac6386fef17a00bd8307218";
      flake = false;
    };
    viper-server = {
      url = "https://github.com/viperproject/viperserver/releases/download/v.22.11-release/viperserver.jar";
      flake = false;
    };
    wasm-spec-src = {
      url = "github:WebAssembly/spec/opam-1.1.1";
      flake = false;
    };
    ocaml-recovery-parser-src = {
      url = "github:serokell/ocaml-recovery-parser/b8207b0c919b84d5096486e59985d0137c0c4d82";
      flake = false;
    };
  };

  outputs =
    { self
    , nixpkgs
    , flake-utils
    , nix-update-flake
    , rust-overlay
    , candid-src
    , esm
    , ic-src
    , ic-wasm-src
    , libtommath-src
    , motoko-base-src
    , motoko-matchers-src
    , ocaml-vlq-src
    , viper-server
    , wasm-spec-src
    , ocaml-recovery-parser-src
    }: flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = import nixpkgs {
        inherit system;
        overlays = [
          (self: super: { inherit ic-src; })

          # Selecting the ocaml version
          # Also update ocaml-version in src/*/.ocamlformat!
          (self: super: { ocamlPackages = self.ocaml-ng.ocamlPackages_4_14; })

          (self: super: {
            # Additional ocaml package
            ocamlPackages = super.ocamlPackages // rec {

              # downgrade wasm until we have support for 2.0.1
              # (https://github.com/dfinity/motoko/pull/3364)
              wasm_1 = super.ocamlPackages.wasm.overrideAttrs rec {
                version = "1.1.1";
                src = wasm-spec-src;
                patchPhase = ''
                  substituteInPlace ./interpreter/Makefile \
                    --replace-fail "+a-4-27-42-44-45" "+a-4-27-42-44-45-70"
                '';
              };

              ocaml-recovery-parser = super.ocamlPackages.buildDunePackage {
                pname = "ocaml-recovery-parser";
                version = "0.3.0";
                src = ocaml-recovery-parser-src;
                buildInputs = with super.ocamlPackages; [
                  menhirSdk
                  menhirLib
                  fix
                  base
                ];
              };

              # No testing of atdgen, as it pulls in python stuff, tricky on musl
              atdgen = super.ocamlPackages.atdgen.overrideAttrs { doCheck = false; };
            };
          }
          )

          # Rust Nightly & Stable
          rust-overlay.overlays.default
          (self: super: {
            rust-nightly = pkgs.rust-bin.nightly."2024-07-28".default.override {
              extensions = [ "rust-src" ];
              targets = [ "wasm32-wasip1" ];
            };

            rust-stable = pkgs.rust-bin.stable."1.85.0".default;

            rustPlatform-stable = self.makeRustPlatform {
              rustc = self.rust-stable;
              cargo = self.rust-stable;
            };
          })

          # wasm-profiler
          (self: super: import ./nix/wasm-profiler.nix self)

          # drun
          (self: super: import ./nix/drun.nix self)
        ];
      };

      nix-update = nix-update-flake.packages.${system}.default;

      # The following were previously arguments to default.nix but flakes don't accept options yet.
      # This will be the case when we get Configurable Flakes:
      # https://www.youtube.com/live/yhfDtRRTmY8?si=0Mn-YBTMqcUl9Hb5&t=17974
      accept-bench = "x86_64-linux";
      replay = 0;

      is_static = !pkgs.stdenv.isDarwin;

      staticpkgs = if is_static then pkgs.pkgsMusl else pkgs;

      commonBuildInputs = pkgs:
        [
          pkgs.dune_3
          pkgs.ocamlPackages.ocaml
          pkgs.ocamlPackages.atdgen
          pkgs.ocamlPackages.checkseum
          pkgs.ocamlPackages.findlib
          pkgs.ocamlPackages.menhir
          pkgs.ocamlPackages.menhirLib
          pkgs.ocamlPackages.menhirSdk
          pkgs.ocamlPackages.ocaml-recovery-parser
          pkgs.ocamlPackages.cow
          pkgs.ocamlPackages.num
          pkgs.ocamlPackages.stdint
          pkgs.ocamlPackages.wasm_1
          pkgs.ocamlPackages.vlq
          pkgs.ocamlPackages.zarith
          pkgs.ocamlPackages.yojson
          pkgs.ocamlPackages.ppxlib
          pkgs.ocamlPackages.ppx_blob
          pkgs.ocamlPackages.ppx_inline_test
          pkgs.ocamlPackages.ppx_expect
          pkgs.ocamlPackages.bisect_ppx
          pkgs.ocamlPackages.uucp
          pkgs.obelisk
          pkgs.perl
          pkgs.removeReferencesTo
        ];

      llvmEnv = ''
        # When compiling to wasm, we want to have more control over the flags,
        # so we do not use the nix-provided wrapper in clang
        export WASM_CLANG="clang-18"
        export WASM_LD=wasm-ld
        # because we use the unwrapped clang, we have to pass in some flags/paths
        # that otherwise the wrapped clang would take care for us
        export WASM_CLANG_LIB="${pkgs.llvmPackages_18.clang-unwrapped.lib}"

        # When compiling natively, we want to use `clang` (which is a nixpkgs
        # provided wrapper that sets various include paths etc).
        # But for some reason it does not handle building for Wasm well, so
        # there we use plain clang-18. There is no stdlib there anyways.
        export CLANG="${pkgs.clang_18}/bin/clang"
      '';

      haskellPackages = pkgs.haskellPackages.override {
        overrides = _hself: hsuper: {
          qc-motoko = hsuper.callCabal2nix "qc-motoko" ./test/random { };
          lsp-int = hsuper.callCabal2nix "lsp-int" ./test/lsp-int { };
        };
      };

      base-src = pkgs.symlinkJoin {
        name = "base-src";
        paths = [ "${motoko-base-src}/src" ];
      };

      base-tests = pkgs.stdenv.mkDerivation {
        name = "base-tests";
        src = motoko-base-src;
        phases = "unpackPhase checkPhase installPhase";
        doCheck = true;
        installPhase = "touch $out";
        checkInputs = [
          pkgs.wasmtime
          debugPackages.moc
        ];
        checkPhase = ''
          make MOC=moc VESSEL_PKGS="--package matchers ${motoko-matchers-src}/src" -C test
        '';
      };

      base-doc = pkgs.stdenv.mkDerivation {
        name = "base-doc";
        src = motoko-base-src;
        phases = "unpackPhase buildPhase installPhase";
        doCheck = true;
        buildInputs = [ debugPackages.mo-doc ];
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

      report-site = pkgs.runCommandNoCC "report-site"
        {
          buildInputs = [ pkgs.tree ];
        } ''
        mkdir -p $out
        ln -s ${base-doc} $out/base-doc
        ln -s ${docs} $out/docs
        ln -s ${tests.coverage} $out/coverage
        cd $out;
        # generate a simple index.html, listing the entry points
        ( echo docs/overview-slides.html;
          echo docs/html/motoko.html;
          echo base-doc/
          echo coverage/ ) | \
          tree -H . -l --fromfile -T "Motoko build reports" > index.html
      '';

      docs = pkgs.stdenv.mkDerivation {
        name = "docs";
        src = ./doc;
        buildInputs = with pkgs; [ pandoc bash gitMinimal ];

        buildPhase = ''
          patchShebangs .
          export HOME=$PWD
          export MOC_JS=${js.moc}/bin/moc.js
          export MOTOKO_BASE=${base-src}
          make
        '';

        installPhase = ''
          mkdir -p $out
          mv overview-slides.html $out/
          mv html $out/
          mkdir -p $out/nix-support
          echo "report guide $out html/motoko.html" >> $out/nix-support/hydra-build-products
          echo "report slides $out overview-slides.html" >> $out/nix-support/hydra-build-products
        '';
      };

      ic-wasm =
        pkgs.rustPlatform-stable.buildRustPackage {
          pname = "ic-wasm";
          version = builtins.substring 0 7 ic-wasm-src.rev;
          src = ic-wasm-src;
          cargoLock = {
            lockFile = "${ic-wasm-src}/Cargo.lock";
          };
          doCheck = false;
        };

      rts =
        let
          # Build Rust package cargo-vendor-tools
          cargoVendorTools = pkgs.rustPlatform.buildRustPackage rec {
            name = "cargo-vendor-tools";
            src = ./rts/${name};
            cargoLock = {
              lockFile = ./rts/${name}/Cargo.lock;
            };
          };

          # Path to vendor-rust-std-deps, provided by cargo-vendor-tools
          vendorRustStdDeps = "${cargoVendorTools}/bin/vendor-rust-std-deps";

          # SHA256 of Rust std deps
          rustStdDepsHash = "sha256-U4BTr1CzFuOMdyLuhw5ry3/u8bkRiPmnMr4pLo3IdOQ=";

          # Vendor directory for Rust std deps
          rustStdDeps = pkgs.stdenvNoCC.mkDerivation {
            name = "rustc-std-deps";

            nativeBuildInputs = [ pkgs.curl ];

            buildCommand = ''
              mkdir $out
              cd $out
              ${vendorRustStdDeps} ${pkgs.rust-nightly} .
            '';

            outputHash = rustStdDepsHash;
            outputHashAlgo = "sha256";
            outputHashMode = "recursive";
          };

          # Vendor tarball of the RTS
          rtsDeps = pkgs.rustPlatform.importCargoLock {
            lockFile = ./rts/motoko-rts-tests/Cargo.lock;
          };

          # All dependencies needed to build the RTS, including Rust std deps, to
          # allow `cargo -Zbuild-std`. (rust-lang/wg-cargo-std-aware#23)
          allDeps = pkgs.symlinkJoin {
            name = "merged-rust-deps";
            paths = [
              rtsDeps
              rustStdDeps
            ];
          };
        in

        pkgs.stdenv.mkDerivation {
          name = "moc-rts";

          src = ./rts;

          nativeBuildInputs = [ pkgs.makeWrapper pkgs.removeReferencesTo pkgs.cacert ];

          buildInputs = with pkgs; [
            llvmPackages_18.clang
            llvmPackages_18.bintools
            rust-nightly
            wasmtime
            rust-bindgen
            python3
          ] ++ pkgs.lib.optional pkgs.stdenv.isDarwin [
            libiconv
          ];

          preBuild = ''
            export CARGO_HOME=$PWD/cargo-home

            # This replicates logic from nixpkgs’ pkgs/build-support/rust/default.nix
            mkdir -p $CARGO_HOME
            echo "Using vendored sources from ${rtsDeps}"
            unpackFile ${allDeps}
            cat > $CARGO_HOME/config <<__END__
              [source."crates-io"]
              "replace-with" = "vendored-sources"

              [source."vendored-sources"]
              "directory" = "$(stripHash ${allDeps})"
            __END__


            ${llvmEnv}
            export TOMMATHSRC=${libtommath-src}
          '';

          doCheck = true;

          checkPhase = ''
            make test
          '';

          installPhase = ''
            mkdir -p $out/rts
            cp mo-rts-non-incremental.wasm $out/rts
            cp mo-rts-non-incremental-debug.wasm $out/rts
            cp mo-rts-incremental.wasm $out/rts
            cp mo-rts-incremental-debug.wasm $out/rts
            cp mo-rts-eop.wasm $out/rts
            cp mo-rts-eop-debug.wasm $out/rts
          '';

          # This needs to be self-contained. Remove mention of nix path in debug
          # message.
          preFixup = ''
            remove-references-to \
              -t ${pkgs.rust-nightly} \
              -t ${rtsDeps} \
              -t ${rustStdDeps} \
              $out/rts/mo-rts-non-incremental.wasm $out/rts/mo-rts-non-incremental-debug.wasm
            remove-references-to \
              -t ${pkgs.rust-nightly} \
              -t ${rtsDeps} \
              -t ${rustStdDeps} \
              $out/rts/mo-rts-incremental.wasm $out/rts/mo-rts-incremental-debug.wasm
            remove-references-to \
              -t ${pkgs.rust-nightly} \
              -t ${rtsDeps} \
              -t ${rustStdDeps} \
              $out/rts/mo-rts-eop.wasm $out/rts/mo-rts-eop-debug.wasm
          '';

          allowedRequisites = [ ];
        };

      samples = pkgs.stdenv.mkDerivation {
        name = "samples";
        src = ./samples;
        buildInputs = [ debugPackages.moc ];
        buildPhase = ''
          patchShebangs .
          make all
        '';
        installPhase = ''
          touch $out
        '';
      };

      js =
        let
          mk = n:
            pkgs.stdenv.mkDerivation {
              name = "${n}.js";
              src = ./src;
              buildInputs = commonBuildInputs pkgs ++ [
                pkgs.ocamlPackages.js_of_ocaml
                pkgs.ocamlPackages.js_of_ocaml-ppx
                pkgs.nodejs_20
                pkgs.nodePackages.terser
              ];
              buildPhase = ''
                patchShebangs .
              '' + pkgs.lib.optionalString (rts != null) ''
                ./rts/gen.sh ${rts}/rts/
              '' + ''
                make DUNE_OPTS="--profile=release" ${n}.js
                terser ${n}.js -o ${n}.min.js -c -m
              '';
              installPhase = ''
                mkdir -p $out
                mkdir -p $out/bin
                cp --verbose --dereference ${n}.js $out/bin
                cp --verbose --dereference ${n}.min.js $out/bin
              '';
              doInstallCheck = true;
              test = ./test + "/test-${n}.js";
              installCheckPhase = ''
                NODE_PATH=$out/bin node --experimental-wasm-memory64 $test
              '';
            };
        in
        {
          moc = mk "moc";
          moc_interpreter = mk "moc_interpreter";
          didc = mk "didc";
          recurseForDerivations = true;
        };

      filecheck = pkgs.runCommandNoCC "FileCheck" { } ''
        mkdir -p $out/bin
        cp ${pkgs.llvm}/bin/FileCheck $out/bin
      '';

      releasePackages = packages true;
      debugPackages = packages false;
      packages = officialRelease:
        let
          releaseVersion = import nix/releaseVersion.nix { inherit pkgs officialRelease; };

          ocaml_exe = name: bin: rts:
            let
              profile =
                if is_static
                then "release-static"
                else "release";
              is_dyn_static =
                is_static && system == "aarch64-linux";
            in
            staticpkgs.stdenv.mkDerivation {
              inherit name;

              allowedRequisites = pkgs.lib.optional is_static staticpkgs.musl
                ++ pkgs.lib.optional is_dyn_static staticpkgs.patchelf;

              src = ./src;

              buildInputs = commonBuildInputs staticpkgs;

              MOTOKO_RELEASE = releaseVersion;

              extraDuneOpts = "";

              # we only need to include the wasm statically when building moc, not
              # other binaries
              buildPhase = ''
                patchShebangs .
              '' + pkgs.lib.optionalString (rts != null) ''
                ./rts/gen.sh ${rts}/rts
              '' + ''
                make DUNE_OPTS="--display=short --profile ${profile} $extraDuneOpts" ${bin}
              '';

              installPhase = ''
                mkdir -p $out/bin
                cp --verbose --dereference ${bin} $out/bin
              '' + pkgs.lib.optionalString pkgs.stdenv.isDarwin ''
                # there are references to darwin system libraries
                # in the binaries. But curiously, we can remove them
                # an the binaries still work. They are essentially static otherwise.
                remove-references-to \
                  -t ${pkgs.darwin.Libsystem} \
                  -t ${pkgs.darwin.CF} \
                  -t ${pkgs.libiconv} \
                  $out/bin/*
              '' + ''
                # also, there is a reference to /nix/store/…/share/menhir/standard.mly.
                # Let's remove that, too
                remove-references-to \
                  -t ${staticpkgs.ocamlPackages.menhir} \
                  $out/bin/*
              '' + pkgs.lib.optionalString (!officialRelease && is_dyn_static) ''
                # these systems need a fixup to the loader interpreter
                chmod +w $out/bin/*
                patchelf --set-interpreter "${staticpkgs.musl}/lib/ld-musl-aarch64.so.1" $out/bin/*
                chmod a-w $out/bin/*
              '';

              doInstallCheck = !officialRelease;
              installCheckPhase = ''
                $out/bin/* --help > /dev/null
              '';
            };
        in
        rec {
          moc = ocaml_exe "moc" "moc" rts;
          mo-ld = ocaml_exe "mo-ld" "mo-ld" null;
          mo-ide = ocaml_exe "mo-ide" "mo-ide" null;
          mo-doc = ocaml_exe "mo-doc" "mo-doc" null;
          didc = ocaml_exe "didc" "didc" null;
          deser = ocaml_exe "deser" "deser" null;
          candid-tests = ocaml_exe "candid-tests" "candid-tests" null;

          # executable built with coverage:
          coverage_bins = builtins.listToAttrs (pkgs.lib.flip map [ moc mo-ld didc deser ] (drv:
            {
              name = drv.name;
              value = drv.overrideAttrs (old: {
                name = "${old.name}-coverage";
                extraDuneOpts = "--instrument-with bisect_ppx";
                installPhase = old.installPhase + ''
                  # The coverage report needs access to sources, including generated ones
                  # like _build/parser.ml
                  mkdir $out/src
                  find -name \*.ml -print0 | xargs -0 cp -t $out/src --parents
                '';
                allowedRequisites = null;
              });
            }));
        };

      check-formatting = pkgs.stdenv.mkDerivation {
        name = "check-formatting";
        buildInputs = [ pkgs.ocamlformat ];
        src = ./src;
        doCheck = true;
        phases = "unpackPhase checkPhase installPhase";
        installPhase = "touch $out";
        checkPhase = ''
          ocamlformat --check languageServer/*.{ml,mli} docs/*.{ml,mli}
        '';
      };

      check-rts-formatting = pkgs.stdenv.mkDerivation {
        name = "check-rts-formatting";
        buildInputs = [ pkgs.rust-nightly pkgs.rustfmt ];
        src = ./rts;
        doCheck = true;
        phases = "unpackPhase checkPhase installPhase";
        checkPhase = ''
          echo "If this fails, run `make -C rts format`"
          cargo fmt --verbose --manifest-path motoko-rts/Cargo.toml -- --check
          cargo fmt --verbose --manifest-path motoko-rts-tests/Cargo.toml -- --check
        '';
        installPhase = "touch $out";
      };

      # Checks that doc/md/examples/grammar.txt is up-to-date
      check-grammar = pkgs.stdenv.mkDerivation {
        name = "check-grammar";
        src = ./src/gen-grammar;
        phases = "unpackPhase buildPhase installPhase";
        buildInputs = [ pkgs.diffutils pkgs.bash pkgs.obelisk ];
        buildPhase = ''
          patchShebangs .
          ./gen-grammar.sh ${./src/mo_frontend/parser.mly} > expected
          echo "If the following fails, please run:"
          echo "nix-shell --command 'make -C src grammar'"
          diff -r -U 3 ${./doc/md/examples/grammar.txt} expected
          echo "ok, all good"
        '';
        installPhase = ''
          touch $out
        '';
      };

      check-error-codes = pkgs.stdenv.mkDerivation {
        name = "check-error-codes";
        src = ./test;
        phases = "unpackPhase buildPhase installPhase";
        buildInputs = [ pkgs.python3 ];
        buildPhase = ''
          patchShebangs .
          ./check-error-codes.py ${./src/lang_utils/error_codes.ml}
        '';
        installPhase = ''
          touch $out
        '';
      };

      tests = with debugPackages; let
        testDerivationArgs = {
          NIXBUILDNET_DEFAULT_CPU = 8;
          NIXBUILDNET_MIN_CPU = 8;

          # by default, an empty source directory. how to best get an empty directory?
          src = builtins.path { name = "empty"; path = ./nix; filter = p: t: false; };
          phases = "unpackPhase checkPhase installPhase";
          doCheck = true;
          installPhase = "touch $out";
        };

        testDerivationDeps =
          (with pkgs; [ wabt bash perl getconf moreutils nodejs_20 ]) ++
          [ filecheck pkgs.wasmtime ];

        # extra deps for test/ld
        ldTestDeps =
          with pkgs; [ llvmPackages_18.lld llvmPackages_18.clang ];

        testDerivation = args:
          pkgs.stdenv.mkDerivation (testDerivationArgs // args);

        # we test each subdirectory of test/ in its own derivation with
        # cleaner dependencies, for more parallelism, more caching
        # and better feedback about what aspect broke
        # so include from test/ only the common files, plus everything in test/${dir}/
        test_src = dir:
          with pkgs.lib;
          cleanSourceWith {
            filter = path: type:
              let relPath = removePrefix (toString ./test + "/") (toString path); in
              type != "directory" || hasPrefix "${dir}/" "${relPath}/";
            src = ./test;
            name = "test-${dir}-src";
          };

        acceptable_subdir = accept: dir: deps:
          testDerivation ({
            src = test_src dir;
            buildInputs = deps ++ testDerivationDeps;

            checkPhase = ''
              patchShebangs .
              ${llvmEnv}
              export ESM=${esm}
              export VIPER_SERVER=${viper-server}
              type -p moc && moc --version
              make -C ${dir}${pkgs.lib.optionalString accept " accept"}
            '';
          } // pkgs.lib.optionalAttrs accept {
            installPhase = pkgs.lib.optionalString accept ''
              mkdir -p $out/share
              cp -v ${dir}/ok/*.ok $out/share
            '';
          });

        test_subdir = dir: deps: acceptable_subdir false dir deps;

        # Run a variant with sanity checking on
        snty_subdir = dir: deps:
          (test_subdir dir deps).overrideAttrs {
            EXTRA_MOC_ARGS = "--sanity-checks";
          };

        snty_compacting_gc_subdir = dir: deps:
          (test_subdir dir deps).overrideAttrs {
            EXTRA_MOC_ARGS = "--sanity-checks --compacting-gc";
          };

        snty_generational_gc_subdir = dir: deps:
          (test_subdir dir deps).overrideAttrs {
            EXTRA_MOC_ARGS = "--sanity-checks --generational-gc";
          };

        snty_incremental_gc_subdir = dir: deps:
          (test_subdir dir deps).overrideAttrs {
            EXTRA_MOC_ARGS = "--sanity-checks --incremental-gc";
          };

        enhanced_orthogonal_persistence_subdir = dir: deps:
          (test_subdir dir deps).overrideAttrs {
            EXTRA_MOC_ARGS = "--enhanced-orthogonal-persistence";
          };

        snty_enhanced_orthogonal_persistence_subdir = dir: deps:
          (test_subdir dir deps).overrideAttrs {
            EXTRA_MOC_ARGS = "--sanity-checks --enhanced-orthogonal-persistence";
          };

        perf_subdir = accept: dir: deps:
          (acceptable_subdir accept dir deps).overrideAttrs (args: {
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
            [ moc pkgs.wasmtime haskellPackages.qc-motoko pkgs.drun ];
          checkPhase = ''
            export LANG=C.utf8 # for haskell
            qc-motoko${pkgs.lib.optionalString (replay != 0)
                " --quickcheck-replay=${toString replay}"}
          '';
        };

        lsp = testDerivation {
          src = ./test/lsp-int-test-project;
          buildInputs = [ moc haskellPackages.lsp-int ];
          checkPhase = ''
            echo running lsp-int
            export LANG=C.utf8 # for haskell
            lsp-int ${mo-ide}/bin/mo-ide .
          '';
        };

        unit = testDerivation {
          src = ./src;
          buildInputs = commonBuildInputs pkgs;
          checkPhase = ''
            patchShebangs .
            make DUNE_OPTS="--display=short" unit-tests
          '';
          installPhase = ''
            touch $out
          '';
        };

        candid = testDerivation {
          buildInputs = [ moc pkgs.wasmtime candid-tests ];
          checkPhase = ''
            candid-tests -i ${candid-src}/test
          '';
        };

        # wasm-profiler is not compatible with passive data segments and memory64
        # profiling-graphs = testDerivation {
        #  src = test_src "perf";
        #  buildInputs =
        #    (with pkgs; [ perl wabt wasm-profiler-instrument wasm-profiler-postproc flamegraph-bin ]) ++
        #    [ moc pkgs.drun ];
        #  checkPhase = ''
        #    patchShebangs .
        #    type -p moc && moc --version
        #    type -p drun && drun --help
        #    ./profile-report.sh
        #  '';
        #  installPhase = ''
        #    mv _profile $out;
        #    mkdir -p $out/nix-support
        #    echo "report flamegraphs $out index.html" >> $out/nix-support/hydra-build-products
        #  '';
        #};


        fix_names = builtins.mapAttrs (name: deriv:
          deriv.overrideAttrs { name = "test-${name}"; }
        );

        coverage = testDerivation {
          # this runs all subdirectories, so let's just depend on all of test/
          src = ./test;
          buildInputs =
            builtins.attrValues coverage_bins ++
            [ pkgs.ocamlPackages.bisect_ppx ] ++
            testDerivationDeps ++
            ldTestDeps;

          checkPhase = ''
            patchShebangs .
            ${llvmEnv}
            export ESM=${esm}
            export SOURCE_PATHS="${
              builtins.concatStringsSep " " (map (d: "${d}/src") (builtins.attrValues coverage_bins))
            }"
            type -p moc && moc --version
            make coverage
          '';
          installPhase = ''
            mv coverage $out;
            mkdir -p $out/nix-support
            echo "report coverage $out index.html" >> $out/nix-support/hydra-build-products
          '';
        };

      in
      fix_names
        {
          run = test_subdir "run" [ moc ];
          run-debug = snty_subdir "run" [ moc ];
          run-eop-release = enhanced_orthogonal_persistence_subdir "run" [ moc ];
          run-eop-debug = snty_enhanced_orthogonal_persistence_subdir "run" [ moc ];
          # ic-ref-run = test_subdir "run-drun"   [ moc ic-ref-run ];
          drun = test_subdir "run-drun" [ moc pkgs.drun ];
          drun-debug = snty_subdir "run-drun" [ moc pkgs.drun ];
          drun-compacting-gc = snty_compacting_gc_subdir "run-drun" [ moc pkgs.drun ];
          drun-generational-gc = snty_generational_gc_subdir "run-drun" [ moc pkgs.drun ];
          drun-incremental-gc = snty_incremental_gc_subdir "run-drun" [ moc pkgs.drun ];
          drun-eop-release = enhanced_orthogonal_persistence_subdir "run-drun" [ moc pkgs.drun ];
          drun-eop-debug = snty_enhanced_orthogonal_persistence_subdir "run-drun" [ moc pkgs.drun ];
          fail = test_subdir "fail" [ moc ];
          repl = test_subdir "repl" [ moc ];
          ld = test_subdir "ld" ([ mo-ld ] ++ ldTestDeps);
          ld-eop = enhanced_orthogonal_persistence_subdir "ld" ([ mo-ld ] ++ ldTestDeps);
          idl = test_subdir "idl" [ didc ];
          mo-idl = test_subdir "mo-idl" [ moc didc ];
          mo-idl-eop = enhanced_orthogonal_persistence_subdir "mo-idl" [ moc didc ];
          trap = test_subdir "trap" [ moc ];
          trap-eop = enhanced_orthogonal_persistence_subdir "trap" [ moc ];
          run-deser = test_subdir "run-deser" [ deser ];
          perf = perf_subdir false "perf" [ moc pkgs.drun ];
          viper = test_subdir "viper" [ moc pkgs.which pkgs.openjdk pkgs.z3_4_12 ];
          # TODO: profiling-graph is excluded because the underlying parity_wasm is deprecated and does not support passive data segments and memory64.
          inherit qc lsp unit candid coverage;
        }
      // pkgs.lib.optionalAttrs
        (system == accept-bench)
        (fix_names { bench = perf_subdir true "bench" [ moc pkgs.drun ic-wasm ]; })
      // { recurseForDerivations = true; };

      # Helper function to filter tests by type
      filter_tests = type: tests:
        let
          # Get all test names that match the pattern
          debug_tests = builtins.filter
            (name:
              builtins.match ".*-debug$" name != null
            )
            (builtins.attrNames tests);

          # Get all test names that don't match the pattern
          release_tests = builtins.filter
            (name:
              builtins.match ".*-debug$" name == null
            )
            (builtins.attrNames tests);

          # Select which set of names to use
          selected_names = if type == "debug" then debug_tests else release_tests;
        in
        # Get the actual derivations for the selected names
        builtins.map (name: tests.${name}) selected_names;

      shell = pkgs.mkShell {
        name = "motoko-shell";

        #
        # Since building moc, and testing it, are two different derivations in we
        # have to create a fake derivation for `nix-shell` that commons up the
        # build dependencies of the two to provide a build environment that offers
        # both, while not actually building `moc`
        #
        propagatedBuildInputs =
          let
            dont_build = with debugPackages;
              [ moc mo-ld didc deser candid-tests ] ++
              builtins.attrValues coverage_bins;
          in
          [ ic-wasm ] ++
          pkgs.lib.lists.unique (builtins.filter (i: !(builtins.elem i dont_build)) (
            commonBuildInputs pkgs ++
            rts.buildInputs ++
            js.moc.buildInputs ++
            docs.buildInputs ++
            check-rts-formatting.buildInputs ++
            #builtins.concatMap (d: d.buildInputs or [ ]) (builtins.attrValues tests) ++
            [
              nix-update
              pkgs.ncurses
              pkgs.ocamlPackages.merlin
              pkgs.ocamlPackages.utop
              pkgs.ocamlformat
              pkgs.ocamlPackages.ocaml-lsp
              pkgs.fswatch
              pkgs.niv
              pkgs.rlwrap # for `rlwrap moc`
              pkgs.openjdk
              pkgs.z3_4_12 # for viper dev
              pkgs.difftastic
            ] ++ pkgs.lib.optional pkgs.stdenv.isDarwin pkgs.darwin.apple_sdk.frameworks.Security
          ));

        shellHook = llvmEnv + ''
          # Include our wrappers in the PATH
          export PATH="${toString ./bin}:$PATH"
          # some cleanup of environment variables otherwise set by nix-shell
          # that would be confusing in interactive use
          unset XDG_DATA_DIRS
        '';
        ESM = esm;
        TOMMATHSRC = libtommath-src;
        LOCALE_ARCHIVE = pkgs.lib.optionalString pkgs.stdenv.isLinux "${pkgs.glibcLocales}/lib/locale/locale-archive";
        MOTOKO_BASE = base-src;
        CANDID_TESTS = "${candid-src}/test";
        VIPER_SERVER = "${viper-server}";

        # allow building this as a derivation, so that hydra builds and caches
        # the dependencies of shell.
        #
        # Note that we are using propagatedBuildInputs above, not just buildInputs.
        # This means that the dependencies end up in the output path, in
        # /nix/store/13d…da6-motoko-shell/nix-support/propagated-build-inputs
        # so that after `nix-build -A shell` (or just `nix-build`) they are guaranteed
        # to be present in the local nix store (else this might just download an
        # empty build result path from the nix cache.)
        phases = [ "installPhase" "fixupPhase" ];
        installPhase = ''
          mkdir $out
        '';
        preferLocalBuild = true;
        allowSubstitutes = true;
      };

      common-constituents = {
        inherit
          samples
          rts
          base-src
          base-tests
          base-doc
          docs
          report-site
          shell;
      };

      checks = {
        inherit
          check-formatting
          check-rts-formatting
          check-grammar
          check-error-codes;
      };

      buildablePackages = packages: {
        inherit (packages)
          moc
          mo-ld
          mo-ide
          mo-doc
          didc
          deser;
      };

      buildableReleasePackages = buildablePackages releasePackages;
      buildableDebugPackages = buildablePackages debugPackages;

      # Release version - excludes debug tests
      release-systems-go = pkgs.releaseTools.aggregate {
        name = "release-systems-go";
        constituents =
          pkgs.lib.attrValues common-constituents ++
          pkgs.lib.attrValues checks ++
          pkgs.lib.attrValues buildableReleasePackages ++
          filter_tests "release" tests  # Only include release tests
          ++ builtins.attrValues js;
      };

      # Debug version - only includes debug tests
      debug-systems-go = pkgs.releaseTools.aggregate {
        name = "debug-systems-go";
        constituents =
          pkgs.lib.attrValues common-constituents ++
          pkgs.lib.attrValues checks ++
          pkgs.lib.attrValues buildableDebugPackages ++
          filter_tests "debug" tests  # Only include debug tests
          ++ builtins.attrValues js;
      };

    in
    {
      packages = checks // common-constituents // {
        "release" = buildableReleasePackages;
        "debug" = buildableDebugPackages;
        inherit release-systems-go debug-systems-go nix-update ic-wasm tests;
        inherit (pkgs) nix-build-uncached drun;
        default = release-systems-go;
      };

      checks = checks // tests;

      devShells.default = shell;

      formatter = pkgs.writeShellScriptBin "formatter" ''
        if [[ $# = 0 ]]; then set -- .; fi
        exec "${pkgs.nixpkgs-fmt}/bin/nixpkgs-fmt" "$@"
      '';
    });
}
