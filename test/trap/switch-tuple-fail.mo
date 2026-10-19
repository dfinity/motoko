// A switch on an unboxed tuple scrutinee still traps when no case matches
func f(a : Nat, b : Nat) : Nat {
  switch (a, b) {
    case (0, x) { x };
    case (x, 0) { x };
  }
};
ignore f(1, 1)
