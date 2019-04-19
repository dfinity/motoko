/**

Functions for Option types.

*/

type t<A> = ?A;

/***

 `isSome`
 --------------------

 Returns true if the value is not `null`.

*/
func isSomeAny(x: ?Any): Bool =
  switch x {
    case null false;
    case _ true;
  };

func isSome<A>(x: t<A>): Bool = isSomeAny(x);

/***

 `isNull`
 --------------------

 Returns true if the value is `null`.

*/
func isNullAny(x: ?Any): Bool = not isSome<Any>(x);

func isNull<A>(x: t<A>): Bool = not isSome<A>(x);

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
func option<A, B>(x: ?A, f: A->B, d: B): B =
  switch x {
    case null { d };
    case (?x_) f(x_);
  };

/***

 `map`
 --------------------

 Apply a function to the wrapped value.

*/
func map<A, B>(x: ?A, f: A->B): ?B =
  switch x {
    case null null;
    case (?x_) ?f(x_);
  };

/***

 `fmap`
 --------------------

 Apply a function to the wrapped value.

*/
func fmap<A, B>(x: ?A, f: A->?B): ?B =
  switch x {
    case null null;
    case (?x_) f(x_);
  };

/***

 `assertSome`
 --------------------

 Assert that the given value is not `null`; ignore this value and return unit.

*/
func assertSomeAny(x: ?Any) =
  switch x {
    case null { unreachable() };
    case _ {};
  };

func assertSome<A>(x: ?A) = assertSomeAny(x);

/***

 `assertNull`
 --------------------

 Assert that the given value is `null`; ignore this value and return unit.

*/
func assertNullAny(x: ?Any) =
  switch x {
    case null { };
    case _ { unreachable() };
  };

func assertNull<A>(x: ?A) = assertNullAny(x);

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
