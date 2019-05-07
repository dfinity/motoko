rec {
  rev = "3cd3d1eeb6b26f3acb9e9e16cd7220cd5eb07148";

  src = builtins.fetchTarball {
    sha256 = "0671riiyzw2y3vw2apxhnq6vq67py64cqkgwiajfnw5qcrva86pw";
    url = "https://github.com/dfinity-lab/nixpkgs/archive/${rev}.tar.gz";
  };

  nixpkgs = import src;
}
