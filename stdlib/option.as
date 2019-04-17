/**

Functions for Option types.

*/

/***

 `isSome`
 --------------------

 Returns true if the value is not `null`.

*/
func isSome(x: ?Any): Bool =
  switch x {
    case null false;
    case _ true;
  };

/***

 `isNull`
 --------------------

 Returns true if the value is `null`.

*/
func isNull(x: ?Any): Bool = not isSome(x);

/***

 `unwrap`
 --------------------

 Unwrap an optional value, and fail if it is `null`.

*/
func unwrap<T>(x: ?T): T =
  switch x {
    case null { unreachable() };
    case (?x_) x_;
  };

/***

 `unwrapOr`
 --------------------

 Unwrap an optional value or a default.

*/
func unwrapOr<T>(x: ?T, d: T): T =
  switch x {
    case null { d };
    case (?x_) x_;
  };

/***

 `option`
 --------------------

 Unwrap an optional value. If null, return the default, else, apply the function to the unwrapped value.

*/
func option<F, T>(x: ?F, f: F->T, d: T): T =
  switch x {
    case null { d };
    case (?x_) f(x_);
  };

/***

 `assertSome`
 --------------------

 Assert that the given value is not `null`; ignore this value and return unit.

*/
func assertSome(x: ?Any) =
  switch x {
    case null { unreachable() };
    case _ {};
  };

/***

 `assertNull`
 --------------------

 Assert that the given value is `null`; ignore this value and return unit.

*/
func assertNull(x: ?Any) =
  switch x {
    case null { };
    case _ { unreachable() };
  };

/***

 `printOpInt`
 --------------------

 Print an optional integer.

*/
func printOpInt(x : ?Int) =
  switch x {
    case null  { print "null" };
    case (?x_) { print "?"; printInt x_ };
  };
