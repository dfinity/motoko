{
  description = "The Motoko compiler";

  inputs = {
    # The following is a recent commit on the release-24.11 branch. The reason we specify a commit
    # here instead of "release-24.11" is that nixpkgs' CI system (Hydra) runs a bit behind on darwin
    # which means that not all packages we depend on have been cached if we would use the HEAD of
    # the branch. So we look at some important jobs on Hydra like the following to figure out the
    # best commit to use:
    # https://hydra.nixos.org/job/nixpkgs/nixpkgs-24.11-darwin/llvm.aarch64-darwin/latest
    # Then click on the Part of: "evaluation ..."  -> Inputs -> nixpkgs Revision.
    nixpkgs.url = "github:NixOS/nixpkgs/5051ae6744b993fcfab221e8bd38f8bc26f88393";

    flake-utils.url = "github:numtide/flake-utils";

    nix-update-flake.url = "github:Mic92/nix-update";
    nix-update-flake.inputs.nixpkgs.follows = "nixpkgs";

    rust-overlay.url = "github:oxalica/rust-overlay";
    rust-overlay.inputs.nixpkgs.follows = "nixpkgs";

    esm = {
      url = "https://registry.npmjs.org/esm/-/esm-3.2.25.tgz";
      flake = false;
    };
    viper-server = {
      url = "https://github.com/viperproject/viperserver/releases/download/v.22.11-release/viperserver.jar";
      flake = false;
    };

    candid-src = {
      url = "github:dfinity/candid";
      flake = false;
    };
    ic-src = {
      url = "github:luc-blaeser/ic/drun/2025-02-27_03";
      flake = false;
    };
    ic-wasm-src = {
      url = "github:dfinity/ic-wasm";
      flake = false;
    };
    libtommath-src = {
      url = "github:libtom/libtommath/v1.2.0";
      flake = false;
    };
    motoko-base-src = {
      url = "github:dfinity/motoko-base/next-moc";
      flake = false;
    };
    motoko-matchers-src = {
      url = "github:kritzcreek/motoko-matchers";
      flake = false;
    };
    ocaml-vlq-src = {
      url = "github:flowtype/ocaml-vlq";
      flake = false;
    };
    wasm-spec-src = {
      url = "github:WebAssembly/spec/opam-1.1.1";
      flake = false;
    };
    ocaml-recovery-parser-src = {
      url = "github:serokell/ocaml-recovery-parser";
      flake = false;
    };
  };

  outputs =
    { self
    , nixpkgs
    , flake-utils
    , nix-update-flake
    , rust-overlay
    , esm
    , viper-server
    , candid-src
    , ic-src
    , ic-wasm-src
    , libtommath-src
    , motoko-base-src
    , motoko-matchers-src
    , ocaml-vlq-src
    , wasm-spec-src
    , ocaml-recovery-parser-src
    }: flake-utils.lib.eachDefaultSystem (system:
    let
      sources = {
        inherit
          candid-src
          ic-src
          ic-wasm-src
          libtommath-src
          motoko-base-src
          motoko-matchers-src
          ocaml-vlq-src
          wasm-spec-src
          ocaml-recovery-parser-src;
      };
      pkgs = import ./nix/pkgs.nix { inherit nixpkgs system rust-overlay sources; };

      nix-update = nix-update-flake.packages.${system}.default;

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
          debugMoPackages.moc
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
        buildInputs = [ debugMoPackages.mo-doc ];
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

      rts = import ./nix/rts.nix { inherit pkgs llvmEnv; };

      samples = pkgs.stdenv.mkDerivation {
        name = "samples";
        src = ./samples;
        buildInputs = [ debugMoPackages.moc ];
        buildPhase = ''
          patchShebangs .
          make all
        '';
        installPhase = ''
          touch $out
        '';
      };

      js = import ./nix/moc.js.nix { inherit pkgs commonBuildInputs rts; };

      moPackages = officialRelease: import ./nix/mo-packages.nix {
        inherit pkgs commonBuildInputs rts officialRelease;
      };
      releaseMoPackages = moPackages true;
      debugMoPackages = moPackages false;

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

      tests = import ./nix/tests.nix
        { inherit pkgs llvmEnv esm viper-server commonBuildInputs debugMoPackages; };

      shell = import ./nix/shell.nix
        { inherit pkgs nix-update base-src llvmEnv esm viper-server commonBuildInputs rts js docs check-rts-formatting debugMoPackages; };

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

      buildableMoPackages = moPackages: {
        inherit (moPackages)
          moc
          mo-ld
          mo-ide
          mo-doc
          didc
          deser;
      };

      buildableReleaseMoPackages = buildableMoPackages releaseMoPackages;
      buildableDebugMoPackages = buildableMoPackages debugMoPackages;

      # Release version - excludes debug tests
      release-systems-go = pkgs.releaseTools.aggregate {
        name = "release-systems-go";
        constituents =
          pkgs.lib.attrValues common-constituents ++
          pkgs.lib.attrValues checks ++
          pkgs.lib.attrValues buildableReleaseMoPackages ++
          filterTests "release" tests  # Only include release tests
          ++ builtins.attrValues js;
      };

      # Debug version - only includes debug tests
      debug-systems-go = pkgs.releaseTools.aggregate {
        name = "debug-systems-go";
        constituents =
          pkgs.lib.attrValues common-constituents ++
          pkgs.lib.attrValues checks ++
          pkgs.lib.attrValues buildableDebugMoPackages ++
          filterTests "debug" tests  # Only include debug tests
          ++ builtins.attrValues js;
      };

      filterTests = type: tests:
        pkgs.lib.mapAttrsToList (_name: drv: drv) (pkgs.lib.filterAttrs
          (name: _drv:
            let matchDebug = builtins.match ".*-debug$" name;
            in {
              "debug" = matchDebug != null;
              "release" = matchDebug == null;
            }.${type})
          tests);

    in
    {
      packages = checks // common-constituents // {
        "release" = buildableReleaseMoPackages;
        "debug" = buildableDebugMoPackages;
        inherit release-systems-go debug-systems-go nix-update tests js;
        inherit (pkgs) nix-build-uncached drun ic-wasm;
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
