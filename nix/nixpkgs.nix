rec {
  rev = "6edbe844b258bd26afce8f356f5e3072c2ac4353";

  src = builtins.fetchTarball {
    sha256 = "0sng9yf45ass96hpqrkg3yxsfsgngv5wm5w8b7czr9xcy2z2g56z";
    url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
  };

  nixpkgs = import src;
}
