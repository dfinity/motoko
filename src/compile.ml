open Printf

let compile prog =
  printf "(module (func $main unreachable) (start $main))\n"

