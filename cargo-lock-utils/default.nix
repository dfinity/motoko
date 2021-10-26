{ pkgs ? import <nixpkgs> {}
}:

with pkgs.python3Packages;

buildPythonApplication {
  pname = "merge-cargo-lock";
  version = "0.0.1";

  propagatedBuildInputs = [
    toml
  ];

  src = ./.;
}
