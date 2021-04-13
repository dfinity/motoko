import Prim "mo:⛔";

let a = Prim.Array_init<Nat>(10,42);

assert (a.size() == 10);
for (n in a.vals()) {
  assert(n == 42);
};

let b = Prim.Array_tabulate<Nat>(10,func (x : Nat) : Nat = x);

assert(b.size() == 10);
for (i in b.keys()) {
  assert (b[i] == i);
};

