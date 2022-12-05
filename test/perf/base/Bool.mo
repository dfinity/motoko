/// Boolean type and operations.
///
/// While boolean operators `_ and _` and `_ or _` are short-circuiting,
/// avoiding computation of the right argument when possible, the functions
/// `logand(_, _)` and `logor(_, _)` are *strict* and will always evaluate *both*
/// of their arguments.

import Prim "mo:⛔";
module {

  /// Booleans with constants `true` and `false`.
  public type Bool = Prim.Types.Bool;

  /// Conversion.
  public func toText(x : Bool) : Text {
    if x { "true" } else { "false" }
  };

  /// Returns `x and y`.
  public func logand(x : Bool, y : Bool) : Bool { x and y };

  /// Returns `x or y`.
  public func logor(x : Bool, y : Bool) : Bool { x or y };

  /// Returns exclusive or of `x` and `y`, `x != y`.
  public func logxor(x : Bool, y : Bool) : Bool {
    x != y
  };

  /// Returns `not x`.
  public func lognot(x : Bool) : Bool { not x };

  /// Returns `x == y`.
  public func equal(x : Bool, y : Bool) : Bool { x == y };

  /// Returns `x != y`.
  public func notEqual(x : Bool, y : Bool) : Bool { x != y };

  /// Returns the order of `x` and `y`, where `false < true`.
  public func compare(x : Bool, y : Bool) : {#less; #equal; #greater } {
    if (x == y) { #equal } else if (x) { #greater } else { #less }
  };

}
