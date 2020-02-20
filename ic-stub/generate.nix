{ pkgs ? import ../nix {} }: pkgs.localHaskellSrc2nixWithDoc "ic-stub" ./. "--no-check"
