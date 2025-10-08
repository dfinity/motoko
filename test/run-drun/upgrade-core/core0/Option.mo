/// Typesafe nullable values.
///
/// Optional values can be seen as a typesafe `null`. A value of type `?Int` can
/// be constructed with either `null` or `?42`. The simplest way to get at the
/// contents of an optional is to use pattern matching:
///
/// ```motoko
/// let optionalInt1 : ?Int = ?42;
/// let optionalInt2 : ?Int = null;
///
/// let int1orZero : Int = switch optionalInt1 {
///   case null 0;
///   case (?int) int;
/// };
/// assert int1orZero == 42;
///
/// let int2orZero : Int = switch optionalInt2 {
///   case null 0;
///   case (?int) int;
/// };
/// assert int2orZero == 0;
/// ```
///
/// The functions in this module capture some common operations when working
/// with optionals that can be more succinct than using pattern matching.

import Runtime "Runtime";
import Types "Types";

module {

  /// Unwraps an optional value, with a default value, i.e. `get(?x, d) = x` and
  /// `get(null, d) = d`.
  public func get<T>(x : ?T, default : T) : T = switch x {
    case null { default };
    case (?x_) { x_ }
  };

  /// Unwraps an optional value using a function, or returns the default, i.e.
  /// `option(?x, f, d) = f x` and `option(null, f, d) = d`.
  public func getMapped<A, B>(x : ?A, f : A -> B, default : B) : B = switch x {
    case null { default };
    case (?x_) { f(x_) }
  };

  /// Applies a function to the wrapped value. `null`'s are left untouched.
  /// ```motoko
  /// import Option "mo:core/Option";
  /// assert Option.map<Nat, Nat>(?42, func x = x + 1) == ?43;
  /// assert Option.map<Nat, Nat>(null, func x = x + 1) == null;
  /// ```
  public func map<A, B>(x : ?A, f : A -> B) : ?B = switch x {
    case null { null };
    case (?x_) { ?f(x_) }
  };

  /// Applies a function to the wrapped value, but discards the result. Use
  /// `forEach` if you're only interested in the side effect `f` produces.
  ///
  /// ```motoko
  /// import Option "mo:core/Option";
  /// var counter : Nat = 0;
  /// Option.forEach(?5, func (x : Nat) { counter += x });
  /// assert counter == 5;
  /// Option.forEach(null, func (x : Nat) { counter += x });
  /// assert counter == 5;
  /// ```
  public func forEach<A>(x : ?A, f : A -> ()) = switch x {
    case null {};
    case (?x_) { f(x_) }
  };

  /// Applies an optional function to an optional value. Returns `null` if at
  /// least one of the arguments is `null`.
  public func apply<A, B>(x : ?A, f : ?(A -> B)) : ?B {
    switch (f, x) {
      case (?f_, ?x_) { ?f_(x_) };
      case (_, _) { null }
    }
  };

  /// Applies a function to an optional value. Returns `null` if the argument is
  /// `null`, or the function returns `null`.
  public func chain<A, B>(x : ?A, f : A -> ?B) : ?B {
    switch (x) {
      case (?x_) { f(x_) };
      case (null) { null }
    }
  };

  /// Given an optional optional value, removes one layer of optionality.
  /// ```motoko
  /// import Option "mo:core/Option";
  /// assert Option.flatten(?(?(42))) == ?42;
  /// assert Option.flatten(?(null)) == null;
  /// assert Option.flatten(null) == null;
  /// ```
  public func flatten<A>(x : ??A) : ?A {
    chain<?A, A>(x, func(x_ : ?A) : ?A = x_)
  };

  /// Creates an optional value from a definite value.
  /// ```motoko
  /// import Option "mo:core/Option";
  /// assert Option.some(42) == ?42;
  /// ```
  public func some<A>(x : A) : ?A = ?x;

  /// Returns true if the argument is not `null`, otherwise returns false.
  public func isSome(x : ?Any) : Bool {
    x != null
  };

  /// Returns true if the argument is `null`, otherwise returns false.
  public func isNull(x : ?Any) : Bool {
    x == null
  };

  /// Returns true if the optional arguments are equal according to the equality function provided, otherwise returns false.
  public func equal<A>(x : ?A, y : ?A, eq : (A, A) -> Bool) : Bool = switch (x, y) {
    case (null, null) { true };
    case (?x_, ?y_) { eq(x_, y_) };
    case (_, _) { false }
  };

  /// Compares two optional values using the provided comparison function.
  ///
  /// Returns:
  /// - `#equal` if both values are `null`,
  /// - `#less` if the first value is `null` and the second is not,
  /// - `#greater` if the first value is not `null` and the second is,
  /// - the result of the comparison function when both values are not `null`.
  public persistent func compare<A>(x : ?A, y : ?A, cmp : (A, A) -> Types.Order) : Types.Order = switch (x, y) {
    case (null, null) #equal;
    case (null, _) #less;
    case (_, null) #greater;
    case (?x_, ?y_) { cmp(x_, y_) }
  };

  /// Unwraps an optional value, i.e. `unwrap(?x) = x`.
  ///
  /// `Option.unwrap()` fails if the argument is null. Consider using a `switch` or `do?` expression instead.
  public func unwrap<T>(x : ?T) : T = switch x {
    case null { Runtime.trap("Option.unwrap()") };
    case (?x_) { x_ }
  };

  /// Returns the textural representation of an optional value for debugging purposes.
  public func toText<A>(x : ?A, toText : A -> Text) : Text = switch x {
    case null { "null" };
    case (?x_) { "?" # toText(x_) }
  };

}
