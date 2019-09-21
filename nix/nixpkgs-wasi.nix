with { rev = "2d5769884d573aef748443ee1e686444ab58ba78"; };

import (builtins.fetchTarball {
  sha256 = "0p49zlz6gdjrm0z92xszkl1lhrnisgm13s2mxfkbdq8bmp9q7i3l";
  url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
})
