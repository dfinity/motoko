{ pkgs ? import ../nix {} }:
let
  drv = pkgs.haskellSrc2nixWithDoc {
    name = "winter";
    src = pkgs.sources.winter;
    src_subst = "pkgs.sources.winter";
    extraCabal2nixOptions = "--no-check";
  };
in
drv.overrideAttrs (oldAttrs: {
  installPhase = oldAttrs.installPhase + ''
    # Accept `pkgs` as an argument because the `src` depends on it.
    sed -i "s|{ mkDerivation|{ mkDerivation, pkgs|" $out/default.nix
  '';
})
