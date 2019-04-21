switch "duplicate field names" {
  case { len = l; chars; len } {
    for (x in chars()) { printInt (l()); }
  }
};

switch "duplicate bindings" {
  case { len; chars = len } {
    printInt (len())
  }
};
