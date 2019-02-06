// A (non-shared) function is not sharable
{
  func invalid (f : shared (() -> Nat) -> ()) {
     f (func foo() : Nat = 1)
  };
};

// An object with function fields is not sharable
{
  func invalid (f : shared {foo : () -> Nat} -> ()) {
     f (new { foo() : Nat = 1; })
  };
};

// An Object with a mutable field is not sharable
{
  func invalid (f : shared {var foo : Nat} -> ()) {
     f { new { foo : Nat = 1; } }
  };
};

// Cannot return a function in an async
{
  func invalid () : (async (() -> Nat)) {
     async { func foo() : Nat = 1 }
  };
};
// Cannot return an object with a mutable field in an async
{
  func invalid () : (async ({var foo : Nat})) {
     async { new { foo : Nat = 1; } }
  };
};

// Cannot return an object with function fields in an async
{
  func invalid () : (async ({foo : () -> Nat})) {
     async { new { foo() : Nat = 1; } }
  };
};

// Cannot return an object with a mutable field in an async
{
  func invalid () : (async ({var foo : Nat})) {
     async { new { foo : Nat = 1; } }
  };
};
