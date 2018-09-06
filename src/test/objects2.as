type O = {self : () -> O; x : Nat};
let o = new this {self() : O = this; x = 1};
assert(o.x == 1);
assert(o.self().x == 1);
assert(o.self().self().x == 1);
