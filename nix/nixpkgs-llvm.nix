with { rev = "42710203b04738b8f19ae51f0fd21d22fddf3f39"; };

import (builtins.fetchTarball {
  sha256 = "1fqfi0rrlhmssp2cfz07f9sg3mvck28mxnmd39w2ir8670xfixvq";
  url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
})
