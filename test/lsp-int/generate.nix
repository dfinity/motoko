{ pkgs ? import ../../nix {} }: pkgs.localHaskellSrc2nixWithDoc "lsp-int" ./. ""
