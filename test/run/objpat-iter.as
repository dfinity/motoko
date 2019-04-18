let o = [1,2];
var y = 0;


for (x in o.vals()) { y += x };


switch o {
  case ({ vals }) {
    for (x in vals()) { y += x }
  }
};


assert (y == 6);
