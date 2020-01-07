rec {
  rev = "82875a20ba444110396f95537b18247898e40e22";

  src = builtins.fetchTarball {
    sha256 = "1xy2zn3hkcv66ddvscr3l32jcx1qg9h14zvhmy5zf0pcfb8gn42i";
    url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
  };

  nixpkgs = import src;
}
