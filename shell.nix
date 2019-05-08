{ nixpkgs ? (import ./nix/nixpkgs.nix).nixpkgs {},
  test-dvm ? true,
}:

let stdenv = nixpkgs.stdenv; in
let default = import ./default.nix { inherit nixpkgs test-dvm; }; in

#
# Since building asc, and testing it, are two different derivation in default.nix
# we have to create a fake derivation here that commons up the build dependencies
# of the two to provide a build environment that offers both
#
# Would not be necessary if nix-shell would take more than one `-A` flag, see
# https://github.com/NixOS/nix/issues/955
#

nixpkgs.mkShell {
  buildInputs =
    default.native.buildInputs ++
    builtins.filter (i: i != default.native) default.native_test.buildInputs ++
    default.users-guide.buildInputs ++
    [ nixpkgs.ncurses ];

  shellHook =
    default.rts.preBuild + ''
      export NIX_FONTCONFIG_FILE=${default.users-guide.NIX_FONTCONFIG_FILE};
    '';
}

