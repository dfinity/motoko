/// The absent value
///
/// The `None` type represents a type with _no_ value.
///
/// It is often used to type code that fails to return control (e.g. an infinite loop)
/// or to designate impossible values (e.g. the type `?None` only contains `null`).

import Prim "mo:⛔";

module {

  /// The empty type. A subtype of all types.
  public type None = Prim.Types.None;

  /// Turns an absurd value into an arbitrary type.
  public let impossible : <A> None -> A = func<A>(x : None) : A {
    switch (x) {};
  };
}
