module A {
  public("do not use foo anymore") let foo = 5;
  public("deprecated function") func f(x : Nat) : Nat { x };
  public("also types") type T = Int;

  public let baz : T = 5; // look, no warning
};

module B { public let foo = 6 };

ignore (A.foo);
assert (A.foo == 5);

ignore(A.f);
ignore(A.f(5));
assert(A.f(5) == 5);

ignore (5 : A.T);


// lub does not warn:
assert ((if true A else B).foo == 5);
assert ((if false A else B).foo == 6);

// type annotation removes warning
assert ((A : module { foo : Int }).foo == 5);
