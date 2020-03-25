/**
[#mod-None]
= `None` -- The absent value

The `None` type represents a type with _no_ value, often used to mark dead
code.

For example, the type `[None]` has only empty lists.
*/

module {
  /**
  Turns an absurd value into an arbitrary type.
  */
  public let absurd : <A> None -> A = func<A>(x: None) : A {
    switch (x) {};
  };
}
