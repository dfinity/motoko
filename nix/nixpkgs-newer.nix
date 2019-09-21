with { rev = "233ba4052a2bfebcc6514c255f54cbfb02305a92"; };

import (builtins.fetchTarball {
  sha256 = "0rmpvjr8rbqf6f1v8f871qdhdd2kgkv7pxanb5gz6xvsfzxky3s4";
  url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
})
