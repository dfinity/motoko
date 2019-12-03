let a = Array_init<Nat>(10,42);

assert (a.len() == 10);
for (n in a.vals()) {
  assert(n == 42);
};

let b = Array_tabulate<Nat>(10,func (x : Nat) : Nat = x);

assert(b.len() == 10);
for (i in b.keys()) {
  assert (b[i] == i);
};

