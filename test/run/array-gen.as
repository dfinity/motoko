let a = Array_init<Nat>(10,42);

assert (a.len() == 10);
for (n in a.vals()) {
  assert(n == 10);
};

let b = Array_tabular<Nat>(10,func (x : Nat) : Nat = x);

assert(b.len() == 10);
for (i in b.keys()) {
  assert (b[i] == i);
};

