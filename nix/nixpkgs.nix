with { rev = "d50bc0afd23ae2c6bcc31556b005f4e9421d2532"; };

import (builtins.fetchTarball {
  sha256 = "1vdh3x0a65z2k4mzf2f7jpyggqhnqv2716xc51gg4a1lgnf3g8pa";
  url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
})
