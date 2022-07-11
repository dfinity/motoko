# Option
Typesafe nulls

Optional values can be seen as a typesafe `null`. A value of type `?Int` can
be constructed with either `null` or `?42`. The simplest way to get at the
contents of an optional is to use pattern matching:

```motoko
let optionalInt1 : ?Int = ?42;
let optionalInt2 : ?Int = null;

let int1orZero : Int = switch optionalInt1 {
  case null 0;
  case (?int) int;
};
assert int1orZero == 42;

let int2orZero : Int = switch optionalInt2 {
  case null 0;
  case (?int) int;
};
assert int2orZero == 0;
```

The functions in this module capture some common operations when working
with optionals that can be more succinct than using pattern matching.

## Function `get`
``` motoko norepl
func get<T>(x : ?T, default : T) : T
```

Unwraps an optional value, with a default value, i.e. `get(?x, d) = x` and
`get(null, d) = d`.

## Function `getMapped`
``` motoko norepl
func getMapped<A, B>(x : ?A, f : A -> B, default : B) : B
```

Unwraps an optional value using a function, or returns the default, i.e.
`option(?x, f, d) = f x` and `option(null, f, d) = d`.

## Function `map`
``` motoko norepl
func map<A, B>(x : ?A, f : A -> B) : ?B
```

Applies a function to the wrapped value. `null`'s are left untouched.
```motoko
import Option "mo:base/Option";
assert Option.map<Nat, Nat>(?42, func x = x + 1) == ?43;
assert Option.map<Nat, Nat>(null, func x = x + 1) == null;
```

## Function `iterate`
``` motoko norepl
func iterate<A>(x : ?A, f : A -> ())
```

Applies a function to the wrapped value, but discards the result. Use
`iterate` if you're only interested in the side effect `f` produces.

```motoko
import Option "mo:base/Option";
var counter : Nat = 0;
Option.iterate(?5, func (x : Nat) { counter += x });
assert counter == 5;
Option.iterate(null, func (x : Nat) { counter += x });
assert counter == 5;
```

## Function `apply`
``` motoko norepl
func apply<A, B>(x : ?A, f : ?(A -> B)) : ?B
```

Applies an optional function to an optional value. Returns `null` if at
least one of the arguments is `null`.

## Function `chain`
``` motoko norepl
func chain<A, B>(x : ?A, f : A -> ?B) : ?B
```

Applies a function to an optional value. Returns `null` if the argument is
`null`, or the function returns `null`.

## Function `flatten`
``` motoko norepl
func flatten<A>(x : ??A) : ?A
```

Given an optional optional value, removes one layer of optionality.
```motoko
import Option "mo:base/Option";
assert Option.flatten(?(?(42))) == ?42;
assert Option.flatten(?(null)) == null;
assert Option.flatten(null) == null;
```

## Function `make`
``` motoko norepl
func make<A>(x : A) : ?A
```

Creates an optional value from a definite value.
```motoko
import Option "mo:base/Option";
assert Option.make(42) == ?42;
```

## Function `isSome`
``` motoko norepl
func isSome(x : ?Any) : Bool
```

Returns true if the argument is not `null`, otherwise returns false.

## Function `isNull`
``` motoko norepl
func isNull(x : ?Any) : Bool
```

Returns true if the argument is `null`, otherwise returns false.

## Function `assertSome`
``` motoko norepl
func assertSome(x : ?Any)
```

Asserts that the value is not `null`; fails otherwise.
@deprecated Option.assertSome will be removed soon; use an assert expression instead

## Function `assertNull`
``` motoko norepl
func assertNull(x : ?Any)
```

Asserts that the value _is_ `null`; fails otherwise.
@deprecated Option.assertNull will be removed soon; use an assert expression instead

## Function `unwrap`
``` motoko norepl
func unwrap<T>(x : ?T) : T
```

Unwraps an optional value, i.e. `unwrap(?x) = x`.

@deprecated Option.unwrap is unsafe and fails if the argument is null; it will be removed soon; use a `switch` or `do?` expression instead
