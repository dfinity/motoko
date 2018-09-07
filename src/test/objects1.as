let p = new {x = 3; private y = 2; get_y() : Int = y};
assert(p.x == 3);
assert(p.get_y() == 2);
