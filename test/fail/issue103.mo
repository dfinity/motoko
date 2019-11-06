// A (non-shared) function is not sharable
{
  func invalid1 (f : shared (() -> Nat) -> ()) {
     f (func foo() : Nat = 1)
  };
};

// An object with function fields is not sharable
{
  func invalid2 (f : shared {foo : () -> Nat} -> ()) {
     f (object { public func foo() : Nat = 1; })
  };
};

// Cannot return a function in a future
{
  func invalid3 () : (future (() -> Nat)) {
     future { func foo() : Nat = 1 }
  };
};

// Cannot return an object with function fields in a future
{
  func invalid4 () : (future ({foo : () -> Nat})) {
     future { object { public func foo() : Nat = 1; } }
  };
};
