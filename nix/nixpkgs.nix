with { rev = "1ec18b032cb1178f273fede7849c065cfa376a88"; };

import (builtins.fetchTarball {
  sha256 = "044p9xy4yrqkcnr3a5rs3cn6ppvrsv1vsp8rzridmxrazw7692f3";
  url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
})
