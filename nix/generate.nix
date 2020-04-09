# This file generates the contents of nix/generated/. Use
#
#   cp -fv $(nix-build generate.nix --no-link)/ generated/
#
# to update

{ pkgs ? import ../nix {} }:

let

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
        # See ./nix/generate.nix for instructions.\

      '';
      inherit src_subst;
      installPhase = oldAttrs.installPhase + ''
        sed -i "1i$message;s|src = .*|src = $src_subst;|" $out/default.nix
        # Accept `pkgs` as an argument in case the `src_subst` depends on it.
        sed -i "s|{ mkDerivation|{ mkDerivation, pkgs|" $out/default.nix
      '';
    });

  # A variant of `haskellSrc2nixWithDoc` for local Haskell packages.
  localHaskellSrc2nixWithDoc = name: path: extraCabal2nixOptions:
    haskellSrc2nixWithDoc {
      inherit name extraCabal2nixOptions;
      src = import ./gitSource.nix path;
      src_subst = "import ../gitSource.nix \"${path}\"";
    };

  random = localHaskellSrc2nixWithDoc "qc-motoko" "test/random" "";
  lsp-int = localHaskellSrc2nixWithDoc "lsp-int" "test/lsp-int" "";

  allGenerated = pkgs.runCommandNoCC "generated" {} ''
    mkdir -p $out
    cp ${random}/default.nix $out/random.nix
    cp ${lsp-int}/default.nix $out/lsp-int.nix
  '';
in
allGenerated




