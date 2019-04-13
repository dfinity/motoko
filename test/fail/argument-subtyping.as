// Normal function allow subtyping
{ func foo(h : Int -> ()) : (Nat -> ()) = h };

// Shared functions are invariant for now
{ func foo(h : shared Int -> ()) : (shared Nat -> ()) = h };

// The same with abstract types
{ func foo<A <: shared {}>(h : (shared {}) -> ()) : ((shared{x:Nat}) -> ()) = h };
{ func foo<A <: shared {}>(h : shared (shared {}) -> ()) : (shared (shared {x:Nat}) -> ()) = h };
