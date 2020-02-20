{ system ? builtins.currentSystem }:
let
  sourcesnix = builtins.fetchurl https://raw.githubusercontent.com/nmattia/niv/506b896788d9705899592a303de95d8819504c55/nix/sources.nix;
  nixpkgs_src = (import sourcesnix { sourcesFile = ./sources.json; inherit pkgs; }).nixpkgs;

  pkgs =
    import nixpkgs_src {
      inherit system;
      overlays = [
        (self: super: { sources = import sourcesnix { sourcesFile = ./sources.json; pkgs = super; }; })
        # Selecting the ocaml version
        (self: super: { ocamlPackages = self.ocaml-ng.ocamlPackages_4_08; })
        # Additional ocaml package
        (
          self: super: {
            ocamlPackages = super.ocamlPackages // {
              wasm = import ./ocaml-wasm.nix {
                inherit (self) stdenv fetchFromGitHub ocaml;
                inherit (self.ocamlPackages) findlib ocamlbuild;
              };
              vlq = import ./ocaml-vlq.nix {
                inherit (self) stdenv fetchFromGitHub ocaml dune;
                inherit (self.ocamlPackages) findlib;
              };
            };
          }
        )
        (self: super: {
          # A variant of `haskellSrc2nixWithDoc` for local Haskell packages.
          localHaskellSrc2nixWithDoc = name: path: extraCabal2nixOptions:
            self.haskellSrc2nixWithDoc {
              inherit name extraCabal2nixOptions;
              src = import ./gitSource.nix path;
              src_subst = "./.";
            };

          # `haskellSrc2nixWithDoc` is used to generate `default.nix` files for
          # Haskell packages which are intended to be stored in the repository.
          #
          # The function generates a directory containing a `default.nix` which
          # is the result of running `cabal2nix` with the `extraCabal2nixOptions`
          # on the provided `src`.
          #
          # A header is added to `default.nix` which contains instructions on
          # how to regenerate that file.
          #
          # Finally the `src` attribute in the `default.nix` will be defined as
          # `src_subst` such that it can be pointed to local or niv-managed
          # sources.
          haskellSrc2nixWithDoc = {name, src, src_subst, extraCabal2nixOptions}:
            let
              drv = pkgs.haskellPackages.haskellSrc2nix {
                inherit name extraCabal2nixOptions src;
              };
            in drv.overrideAttrs (oldAttrs: {
              message = ''
                # THIS IS AN AUTOMATICALLY GENERATED FILE. DO NOT EDIT MANUALLY!\
                # To regenerate this file execute the following command in this directory:\
                #\
                # cp $(nix-build ./generate.nix --no-link)/default.nix ./default.nix
              '';
              inherit src_subst;
              installPhase = oldAttrs.installPhase + ''
                sed -i "1i$message;s|src = .*|src = $src_subst;|" $out/default.nix
              '';
            });
        })
      ];
    };
in
pkgs
