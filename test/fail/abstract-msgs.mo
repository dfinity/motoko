// In function definitions, parameters with abstract types are not fine
{ shared func foo<A <: Any>( x : A ) : () = (); };
{ shared func foo<A <: Any>() : ?A = null; };
{ func foo<A <: Any>() : () = {
  { shared func bar( x : A ) : () = (); };
  { shared func bar() : async ?A { null } };
}};

// In function calls, parameters with abstract types are not fine
{ func foo<A <: Any>( f : shared A -> (), x : A )  = (f x); };
{ func foo<A <: Any>( f : shared () -> async A ) : async A = async { await (f ())}; };

// Just in types, away from definitinos and calls, parameters with abstract types are fine
{ let x : ?(shared <A <: Any>A -> ()) = null; };
{ let x : ?(shared <A <: Any>() -> async A) = null; };
{ let x : ?(<A <: Any>(shared A -> ()) -> ()) = null; };
{ let x : ?(<A <: Any>(shared () -> async A) -> ()) = null; };


// Phantom parameters are fine
{ type X<B <: Any> = shared () -> ();
  func foo<A <: Any>() { shared func bar(f : X<A>) = (); () }
};
