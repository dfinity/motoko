with { rev = "b63c99659e6b18771c734be0d320008d4b548929"; };

import (builtins.fetchTarball {
  sha256 = "19yvp0i9dw26ijddwjkahrgj2zhb4qnflvmnjqwavf24i6m0p4ki";
  url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
})
