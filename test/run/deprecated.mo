module A {
  public deprecated "do not use foo anymore" let foo = 5;
  public deprecated "deprecated function" func f(x : Nat) : Nat { x };
  public deprecated "also types" type T = Int;
};


ignore (A.foo);
assert (A.foo == 5);

ignore(A.f);
ignore(A.f(5));
assert(A.f(5) == 5);

ignore (5 : A.T);
