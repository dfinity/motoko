var y = 0;
switch ([1,2]) {
  case ({ vals = iter }) {
    for (x in iter()) { y := x; }
  }
};
assert (y == 2);
