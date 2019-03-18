// check that types with non-concrete types in messages are rejected
{ let x : ?(shared <A <: Shared>A -> ()) = null; };
{ let x : ?(shared <A <: Shared>() -> A) = null; };
{ let x : ?(<A <: Shared>(shared A -> ()) -> ()) = null; };
{ let x : ?(<A <: Shared>(shared () -> async A) -> ()) = null; };

// Type aliases can have message arguments with type variables,
// as long as they are instantiated with concrete types
// So this whould be fine:
{ type X<B> = shared B -> (); let x : ?(X<Int>) = null; };

// But this not
{ type X<B> = shared B -> (); let x : ?(<A <: Shared>X<A> -> ()) = null; };

// check that functions with non-concrete types in messages are rejected
{ shared func foo<A <: Shared>( x : A ) : () = (); };
{ shared func foo<A <: Shared>() : ?A = null; };
{ func foo<A <: Shared>() : () = {
  { shared func bar( x : A ) : () = (); };
  { shared func bar() : ?A = null; };
}};
