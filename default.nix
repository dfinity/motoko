{ nixpkgs ? (import ./nix/nixpkgs.nix) {} }:

let stdenv = nixpkgs.stdenv; in

let ocaml_wasm = (import ./nix/ocaml-wasm.nix)
	{inherit (nixpkgs) stdenv  fetchFromGitHub ocaml;
         inherit (nixpkgs.ocamlPackages) findlib ocamlbuild;
	}; in

# Include dsh
let dev_in_nix = (import ./nix/dev-in-nix) { v8 = true; }; in
let dsh = dev_in_nix.hypervisor; in

# We need a newer version of menhir.
# So lets fetch the generic rules for menhir from nixpkgs
let menhir_nix = nixpkgs.fetchurl {
  url = "https://raw.githubusercontent.com/NixOS/nixpkgs/92a047a6c4d46a222e9c323ea85882d0a7a13af8/pkgs/development/ocaml-modules/menhir/generic.nix";
  sha256 = "ef5f134c307f579a5c37754c939cba016f4cc9beba9652fe04e8c7ba87874466";
  }; in
# and use them with with the version we want
let menhir = (import menhir_nix) {
  inherit (nixpkgs) stdenv fetchurl ocaml;
  inherit (nixpkgs.ocamlPackages) findlib ocamlbuild;
  version = "20180703";
  sha256 = "16wv5m7ky27qb8krlycw79dqgzfnpm6rkppc9f26gyw15riicpxb";
  }; in

rec {
  native = stdenv.mkDerivation {
    name = "native";

    src = nixpkgs.lib.cleanSource ./src;

    nativeBuildInputs = [ nixpkgs.makeWrapper ];

    buildInputs = [
      nixpkgs.ocaml
      menhir
      nixpkgs.ocamlPackages.findlib
      nixpkgs.ocamlPackages.ocamlbuild
      ocaml_wasm
      nixpkgs.ocamlPackages.zarith
      dev_in_nix.hypervisor
    ];

    buildPhase = ''
      make BUILD=native asc
    '';

    installPhase = ''
      mkdir -p $out/bin
      cp asc $out/bin
    '';

    # The binary does not work until we use wrapProgram, which runs in
    # the install phase. Therefore, we use the installCheck phase to
    # run the test suite
    doInstallCheck = true;
    installCheckPhase = ''
      $out/bin/asc --version
      make -C test VERBOSE=1 ASC=$out/bin/asc all
      make -C samples ASC=$out/bin/asc all
    '';
  };

  js = native.overrideAttrs (oldAttrs: {
    name = "js";

    buildInputs = oldAttrs.buildInputs ++ [
      nixpkgs.ocamlPackages.js_of_ocaml
      nixpkgs.ocamlPackages.js_of_ocaml-ocamlbuild
      nixpkgs.ocamlPackages.js_of_ocaml-ppx
      nixpkgs.nodejs
    ];

    buildPhase = ''
      make asc.js
    '';

    installPhase = ''
      mkdir -p $out
      cp asc.js $out
    '';

    installCheckPhase = ''
      NODE_PATH=$out/ node test/node-test.js
    '';

  });

  wasm = ocaml_wasm;
  dsh = dev_in_nix.hypervisor;
}
