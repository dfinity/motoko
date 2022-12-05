/// Functions on functions
///
/// (Most commonly used when programming in functional style using higher-order
/// functions.)

module {

  /// The composition of two functions `f` and `g` is a function that applies `g` and then `f`.
  ///
  /// ```
  /// compose(f, g)(x) = f(g(x))
  /// ```
  public func compose<A, B, C>(f : B -> C, g : A -> B) : A -> C {
    func (x : A) : C {
      f(g(x));
    };
  };

  /// The `identity` function returns its argument.
  /// ```motoko
  /// import Func "mo:base/Func";
  /// assert(Func.identity(10) == 10);
  /// assert(Func.identity(true) == true);
  /// ```
  public func identity<A>(x : A) : A = x;

  /// The const function is a _curried_ function that accepts an argument `x`,
  /// and then returns a function that discards its argument and always returns
  /// the `x`.
  ///
  /// ```motoko
  /// import Func "mo:base/Func";
  /// assert(Func.const<Nat, Text>(10)("hello") == 10);
  /// assert(Func.const<Bool, Nat>(true)(20) == true);
  /// ```
  public func const<A, B>(x : A) : B -> A =
    func (_) = x;
}
