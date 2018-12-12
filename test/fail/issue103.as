// A (non-shared) function is not sharable
func invalid1 (f : shared (() -> Nat) -> ()) {
   f (func foo() : Nat = 1)
};

// An object with function fields is not sharable
func invalid2 (f : shared {foo : () -> Nat} -> ()) {
   f (new { foo() : Nat = 1; })
};

// Cannot return an object with function fields in an async
func invalid3 () : (async ({foo : () -> Nat})) {
   async { new { foo() : Nat = 1; } }
};
