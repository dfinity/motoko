let p1 = new {var x = 3; private y = 2; get_y() : Int = y};
let p2 = new {var x = 3; private y = 2; get_y() : Int = y};

assert(p1.x == 3);
assert(p1.get_y() == 2);
assert(p2.x == 3);
assert(p2.get_y() == 2);

p1.x := 4;

assert(p1.x == 4);
assert(p1.get_y() == 2);
assert(p2.x == 3);
assert(p2.get_y() == 2);
