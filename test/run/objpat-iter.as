var y = 0;
switch ([1,2]) {
  case ({ vals = iter }) {
    for (x in iter()) { y := x; }
  }
};

assert (y == 2);

switch ([1,2]) {
  case { vals; len } {
      for (x in vals()) { y += x + len(); }
  }
};

assert (y == 9);

switch "Hi" {
  case { chars; len } {
      for (x in chars()) { y += 1 + len(); }
  }
};

assert (y == 15);
