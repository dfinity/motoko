{ pkgs ? import ../../nix {} }: pkgs.localHaskellSrc2nixWithDoc "qc-motoko" ./. ""
