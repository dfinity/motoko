/// Boolean type and operations.
///
/// Import from the core package to use this module.
/// ```motoko name=import
/// import Bool "mo:core/Bool";
/// ```
///
/// While boolean operators `_ and _` and `_ or _` are short-circuiting,
/// avoiding computation of the right argument when possible, the functions
/// `logicalAnd(_, _)` and `logicalOr(_, _)` are *strict* and will always evaluate *both*
/// of their arguments.
///
/// Example:
/// ```motoko include=import
/// let t = true;
/// let f = false;
///
/// // Short-circuiting AND
/// assert not (t and f);
///
/// // Short-circuiting OR
/// assert t or f;
/// ```

import Prim "mo:⛔";
import Iter "Iter";
import Order "Order";

module {

  /// Booleans with constants `true` and `false`.
  public type Bool = Prim.Types.Bool;

  /// Returns `a and b`.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert not Bool.logicalAnd(true, false);
  /// assert Bool.logicalAnd(true, true);
  /// ```
  public func logicalAnd(a : Bool, b : Bool) : Bool = a and b;

  /// Returns `a or b`.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Bool.logicalOr(true, false);
  /// assert Bool.logicalOr(false, true);
  /// ```
  public func logicalOr(a : Bool, b : Bool) : Bool = a or b;

  /// Returns exclusive or of `a` and `b`, `a != b`.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Bool.logicalXor(true, false);
  /// assert not Bool.logicalXor(true, true);
  /// assert not Bool.logicalXor(false, false);
  /// ```
  public func logicalXor(a : Bool, b : Bool) : Bool = a != b;

  /// Returns `not bool`.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Bool.logicalNot(false);
  /// assert not Bool.logicalNot(true);
  /// ```
  public func logicalNot(bool : Bool) : Bool = not bool;

  /// Returns `a == b`.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Bool.equal(true, true);
  /// assert not Bool.equal(true, false);
  /// ```
  public func equal(a : Bool, b : Bool) : Bool { a == b };

  /// Returns the ordering of `a` compared to `b`.
  /// Returns `#less` if `a` is `false` and `b` is `true`,
  /// `#equal` if `a` equals `b`,
  /// and `#greater` if `a` is `true` and `b` is `false`.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Bool.compare(true, false) == #greater;
  /// assert Bool.compare(true, true) == #equal;
  /// assert Bool.compare(false, true) == #less;
  /// ```
  public persistent func compare(a : Bool, b : Bool) : Order.Order {
    if (a == b) #equal else if a #greater else #less
  };

  /// Returns a text value which is either `"true"` or `"false"` depending on the input value.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Bool.toText(true) == "true";
  /// assert Bool.toText(false) == "false";
  /// ```
  public func toText(bool : Bool) : Text {
    if bool "true" else "false"
  };

  /// Returns an iterator over all possible boolean values (`true` and `false`).
  ///
  /// Example:
  /// ```motoko include=import
  /// let iter = Bool.allValues();
  /// assert iter.next() == ?true;
  /// assert iter.next() == ?false;
  /// assert iter.next() == null;
  /// ```
  public func allValues() : Iter.Iter<Bool> {
    Iter.Iter(
      object {
        var state : ?Bool = ?true;
        public func next() : ?Bool {
          switch state {
            case (?true) { state := ?false; ?true };
            case (?false) { state := null; ?false };
            case null { null }
          }
        }
      }
    )
  }
}
