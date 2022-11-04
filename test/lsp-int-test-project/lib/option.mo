import Prim "mo:⛔";
import P "prelude";

module {
/**

Functions for Option types.

*/

public type t<A> = ?A;

/***

 `isSome`
 --------------------

 Returns true if the value is not `null`.

*/
public func isSomeAny(x: ?Any): Bool =
  switch x {
    case null false;
    case _ true;
  };

public func isSome<A>(x: t<A>): Bool = isSomeAny(x);

/***

 `isNull`
 --------------------

 Returns true if the value is `null`.

*/
public func isNullAny(x: ?Any): Bool = not isSome<Any>(x);

public func isNull<A>(x: t<A>): Bool = not isSome<A>(x);

/***

 `unwrap`
 --------------------

 Unwrap an optional value, and fail if it is `null`.

*/
public func unwrap<T>(x: ?T): T =
  switch x {
    case null { P.unreachable() };
    case (?x_) x_;
  };

/***

 `unwrapOr`
 --------------------

 Unwrap an optional value or a default.

*/
public func unwrapOr<T>(x: ?T, d: T): T =
  switch x {
    case null { d };
    case (?x_) x_;
  };

/***

 `option`
 --------------------

 Unwrap an optional value. If null, return the default, else, apply the function to the unwrapped value.

*/
public func option<A, B>(x: ?A, f: A->B, d: B): B =
  switch x {
    case null { d };
    case (?x_) f(x_);
  };

/***

 `map`
 --------------------

 Apply a function to the wrapped value.

*/
public func map<A, B>(f: A->B, x: ?A): ?B =
  switch x {
    case null null;
    case (?x_) ?f(x_);
  };

/***

 `assertSome`
 --------------------

 Assert that the given value is not `null`; ignore this value and return unit.

*/
public func assertSomeAny(x: ?Any) =
  switch x {
    case null { P.unreachable() };
    case _ {};
  };

public func assertSome<A>(x: ?A) = assertSomeAny(x);

/***

 `assertNull`
 --------------------

 Assert that the given value is `null`; ignore this value and return unit.

*/
public func assertNullAny(x: ?Any) =
  switch x {
    case null { };
    case _ { P.unreachable() };
  };

public func assertNull<A>(x: ?A) = assertNullAny(x);

/***

 `printOpInt`
 --------------------

 Print an optional integer.

*/
public func printOpInt(x : ?Int) =
  switch x {
    case null  { Prim.debugPrint "null" };
    case (?x_) { Prim.debugPrint "?"; Prim.debugPrintInt x_ };
  };

public func apply<A, B>(f : ?(A -> B), x : ?A) : ?B {
  switch (f, x) {
    case (?f_, ?x_) {
      ?f_(x_);
    };
    case (_, _) {
      null;
    };
  };
};

public func bind<A, B>(x : ?A, f : A -> ?B) : ?B {
  switch(x) {
    case (?x_) {
      f(x_);
    };
    case (null) {
      null;
    };
  };
};

public func join<A>(x : ??A) : ?A {
  bind<?A, A>(x, func (x_ : ?A) : ?A {
    x_;
  });
};

public func pure<A>(x: A) : ?A {
  ?x;
};

}
