# This file generates the contents of nix/generated/. Use
#
#  nix-shell generate.nix
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

  packages = {
    # local packages
    random = localHaskellSrc2nixWithDoc "qc-motoko" "test/random" "";
    lsp-int = localHaskellSrc2nixWithDoc "lsp-int" "test/lsp-int" "";
    # Packages on hackage that are newer than what's in nixpkgs
    lsp-test = pkgs.haskellPackages.hackage2nix "lsp-test" "0.11.0.6";
    haskell-lsp = pkgs.haskellPackages.hackage2nix "haskell-lsp" "0.23.0.0";
    haskell-lsp-types = pkgs.haskellPackages.hackage2nix "haskell-lsp-types" "0.23.0.0";
  };

  allGenerated = pkgs.runCommandNoCC "generated" {
    buildInputs = [ pkgs.nixpkgs-fmt ];
  } (
    ''
    mkdir -p $out
    '' + builtins.concatStringsSep "" (
      pkgs.lib.flip pkgs.lib.mapAttrsToList packages (
        n: pkg: ''
          cp ${pkg}/default.nix $out/${n}.nix
        ''
      )
    ) + ''
      chmod u+w $out/*.nix
      nixpkgs-fmt $out/*.nix
      echo <<__END__ > $out/README.md
      The contents of this directory are automatically generated.
      To update, please run nix-shell generate.nix
      __END__
    ''
  );

in
allGenerated.overrideAttrs (
  old: {
    shellHook = if pkgs.lib.inNixShell then
      ''
        dest=${toString ./generated}

        rm -f $dest/*.nix $dest/README.md
        cp -v -t $dest/ ${allGenerated}/*
        chmod u-w -R $dest/*

        exit 0
      '' else null;
  }
)
