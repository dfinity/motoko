rec {
  rev = "e0c7712eac67c6b820d9d1020f46bac96fd8cede";

  src = builtins.fetchTarball {
    sha256 = "08rcnqxkninl5a560ss39s4nbqf0a677q6qh1fh7i0lr9pxf6aan";
    url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
  };

  nixpkgs = import src;
}
