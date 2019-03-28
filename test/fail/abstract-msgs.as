// In function definitions, parameters with abstract types are not fine
{ shared func foo<A <: Shared>( x : A ) : () = (); };
{ shared func foo<A <: Shared>() : ?A = null; };
{ func foo<A <: Shared>() : () = {
  { shared func bar( x : A ) : () = (); };
  { shared func bar() : async ?A { null } };
}};

// In function calls, parameters with abstract types are not fine
{ func foo<A <: Shared>( f : shared A -> (), x : A )  = (f x); };
{ func foo<A <: Shared>( f : shared () -> async A ) : async A = async { await (f ())}; };

// Just in types, away from definitinos and calls, parameters with abstract types are fine
{ let x : ?(shared <A <: Shared>A -> ()) = null; };
{ let x : ?(shared <A <: Shared>() -> async A) = null; };
{ let x : ?(<A <: Shared>(shared A -> ()) -> ()) = null; };
{ let x : ?(<A <: Shared>(shared () -> async A) -> ()) = null; };


// This is mostly because type aliases can have message arguments with type
// variables, as long as they are instantiated with concrete types.  So this
// whould be fine:
{ type X<B <: Shared> = shared B -> ();
  shared func foo ( f: X<Int> ) = ();
};

// But this not
{ type X<B <: Shared> = shared B -> ();
  func foo<A <: Shared>() { shared func foo(f: X<A>) = (); () }
};

// Also, phantom parameters are fine
{ type X<B <: Shared> = shared () -> ();
  func foo<A <: Shared>() { shared func foo(f: X<A>) = (); () }
};
