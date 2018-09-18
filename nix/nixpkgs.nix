with { rev = "ecd9d74d973e9942574e77697cfb8db6c76d1764"; };

import (builtins.fetchTarball {
  sha256 = "1qziyl6hjip97qx2sg9348nm38642yzkr4zg1dd90kj12aj624kz";
  url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
})
