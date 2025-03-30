# Option

Typesafe nulls

Optional values can be seen as a typesafe `null`. A value of type `?Int` can be constructed with either `null` or `?42`. The simplest way to get at the contents of an optional is to use pattern matching:

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

The functions in this module capture some common operations when working with optionals that can be more succinct than using pattern matching.

## Function `get`

```motoko
func get<T>(x : ?T, default : T) : T
```

Unwraps an optional value, with a default value.

`get(?x, d) = x`  
`get(null, d) = d`

## Function `getMapped`

```motoko
func getMapped<A, B>(x : ?A, f : A -> B, default : B) : B
```

Unwraps an optional value using a function, or returns the default.

`getMapped(?x, f, d) = f x`  
`getMapped(null, f, d) = d`

## Function `map`

```motoko
func map<A, B>(x : ?A, f : A -> B) : ?B
```

Applies a function to the wrapped value. `null` is left untouched.

```motoko
import Option "mo:base/Option";
assert Option.map<Nat, Nat>(?42, func x = x + 1) == ?43;
assert Option.map<Nat, Nat>(null, func x = x + 1) == null;
```

## Function `iterate`

```motoko
func iterate<A>(x : ?A, f : A -> ())
```

Applies a function to the wrapped value, discarding the result. Use `iterate` for side effects.

```motoko
import Option "mo:base/Option";
var counter : Nat = 0;
Option.iterate(?5, func (x : Nat) { counter += x });
assert counter == 5;
Option.iterate(null, func (x : Nat) { counter += x });
assert counter == 5;
```

## Function `apply`

```motoko
func apply<A, B>(x : ?A, f : ?(A -> B)) : ?B
```

Applies an optional function to an optional value. Returns `null` if at least one argument is `null`.

## Function `chain`

```motoko
func chain<A, B>(x : ?A, f : A -> ?B) : ?B
```

Applies a function to an optional value. Returns `null` if the input is `null` or if the function returns `null`.

## Function `flatten`

```motoko
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

```motoko
func make<A>(x : A) : ?A
```

Creates an optional value from a definite value.

```motoko
import Option "mo:base/Option";
assert Option.make(42) == ?42;
```

## Function `isSome`

```motoko
func isSome(x : ?Any) : Bool
```

Returns `true` if the argument is not `null`.

## Function `isNull`

```motoko
func isNull(x : ?Any) : Bool
```

Returns `true` if the argument is `null`.

## Function `equal`

```motoko
func equal<A>(x : ?A, y : ?A, eq : (A, A) -> Bool) : Bool
```

Returns `true` if the optional values are equal according to the equality function.

## Function `assertSome` — @deprecated

```motoko
func assertSome(x : ?Any)
```

Asserts that the value is not `null`; fails otherwise.

:::warning [Deprecated function]

`Option.assertSome` will be removed soon. Use an `assert` expression instead.

:::

## Function `assertNull` — @deprecated

```motoko
func assertNull(x : ?Any)
```

Asserts that the value is `null`; fails otherwise.

:::warning [Deprecated function]

`Option.assertNull` will be removed soon. Use an `assert` expression instead.

:::

## Function `unwrap` — @deprecated

```motoko
func unwrap<T>(x : ?T) : T
```

Unwraps an optional value.

`unwrap(?x) = x`

:::warning [Deprecated function]

`Option.unwrap` is unsafe and will be removed soon. Use a `switch` or `do?` expression instead.

:::
