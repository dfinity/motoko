with { rev = "e1ad1a0aa2ce6f9fd951d18181ba850ca8e74133"; };

import (builtins.fetchTarball {
  sha256 = "0vk5sjmbq52xfrinrhvqry53asl6ppwbly1l7ymj8g0j4pkjf7p1";
  url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
})
