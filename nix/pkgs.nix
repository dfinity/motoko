{ nixpkgs, system, rust-overlay, sources }: import nixpkgs {
  inherit system;
  overlays = [
    (self: super: { inherit sources; })

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
          src = self.sources.wasm-spec-src;
          patchPhase = ''
            substituteInPlace ./interpreter/Makefile \
              --replace-fail "+a-4-27-42-44-45" "+a-4-27-42-44-45-70"
          '';
        };

        ocaml-recovery-parser = super.ocamlPackages.buildDunePackage {
          pname = "ocaml-recovery-parser";
          version = "0.3.0";
          src = self.sources.ocaml-recovery-parser-src;
          buildInputs = with super.ocamlPackages; [
            menhirSdk
            menhirLib
            fix
            base
          ];
        };
      };
    }
    )

    # Rust Nightly & Stable
    rust-overlay.overlays.default
    (self: super: {
      # When you change the rust-nightly version,
      # make sure to change the rustStdDepsHash in ./rts.nix accordingly.
      rust-nightly = self.rust-bin.nightly."2024-07-28".default.override {
        extensions = [ "rust-src" ];
        targets = [ "wasm32-wasip1" ];
      };

      rust-stable = self.rust-bin.stable."1.86.0".default;

      rustPlatform-stable = self.makeRustPlatform {
        rustc = self.rust-stable;
        cargo = self.rust-stable;
      };
    })

    # wasm-profiler
    (self: super: import ./wasm-profiler.nix self)

    # pocket-ic
    (self: super: { pocket-ic = import ./pocket-ic.nix self; })

    # ic-wasm
    (self: super: { ic-wasm = import ./ic-wasm.nix self; })
  ];
}
