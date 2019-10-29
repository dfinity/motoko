// Normal function allow subtyping
{ func foo(h : Int -> ()) : (Nat -> ()) = h };

// Shared functions do, too
{ func foo(h : shared Int -> ()) : (shared Nat -> ()) = h };

// The same with abstract types
{ func foo<A <: {}>(h : {} -> ()) : ({x : Nat} -> ()) = h };
{ func foo<A <: {}>(h : shared {} -> ()) : (shared {x : Nat} -> ()) = h };

// But not incomaptible types
{ func foo(h : shared Nat -> ()) : (shared Text -> ()) = h };
