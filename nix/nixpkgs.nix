rec {
  rev = "c2be9fe8358f59b4dda800d3678f3085469b04c0";

  src = builtins.fetchTarball {
    sha256 = "129alzrbzb9cq7f0rfnsihdzk2fj0143r6qffp1nx5pfmnn61yc9";
    url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
  };

  nixpkgs = import src;
}
