with { rev = "96c9578020133fe64feab90c00f3cb880d53ad0d"; };

import (builtins.fetchTarball {
  sha256 = "03rn7gn8r129a8cj527nhs7k28ibzwqw083iirwvas2x4k9mir9z";
  url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
})
