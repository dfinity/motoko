rec {
  rev = "bfef52d95f4d8031286753d5d677e5179328c6ce";

  src = builtins.fetchTarball {
    sha256 = "0b2633a5dc8h3zjydwzyrw1jbn1la41l5qz71sk96z3a3df39m9c";
    url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
  };

  nixpkgs = import src;
}
