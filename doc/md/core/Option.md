# core/Option
Typesafe nullable values.

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
``` motoko no-repl
func get<T>(self : ?T, default : T) : T
```

Unwraps an optional value, with a default value, i.e. `get(?x, d) = x` and
`get(null, d) = d`.

## Function `getMapped`
``` motoko no-repl
func getMapped<T, R>(self : ?T, f : T -> R, default : R) : R
```

Unwraps an optional value using a function, or returns the default, i.e.
`option(?x, f, d) = f x` and `option(null, f, d) = d`.

## Function `map`
``` motoko no-repl
func map<T, R>(self : ?T, f : T -> R) : ?R
```

Applies a function to the wrapped value. `null`'s are left untouched.
```motoko
import Option "mo:core/Option";
assert Option.map<Nat, Nat>(?42, func x = x + 1) == ?43;
assert Option.map<Nat, Nat>(null, func x = x + 1) == null;
```

## Function `forEach`
``` motoko no-repl
func forEach<T>(self : ?T, f : T -> ())
```

Applies a function to the wrapped value, but discards the result. Use
`forEach` if you're only interested in the side effect `f` produces.

```motoko
import Option "mo:core/Option";
var counter : Nat = 0;
Option.forEach(?5, func (x : Nat) { counter += x });
assert counter == 5;
Option.forEach(null, func (x : Nat) { counter += x });
assert counter == 5;
```

## Function `apply`
``` motoko no-repl
func apply<T, R>(self : ?T, f : ?(T -> R)) : ?R
```

Applies an optional function to an optional value. Returns `null` if at
least one of the arguments is `null`.

## Function `chain`
``` motoko no-repl
func chain<T, R>(self : ?T, f : T -> ?R) : ?R
```

Applies a function to an optional value. Returns `null` if the argument is
`null`, or the function returns `null`.

## Function `flatten`
``` motoko no-repl
func flatten<T>(self : ??T) : ?T
```

Given an optional optional value, removes one layer of optionality.
```motoko
import Option "mo:core/Option";
assert Option.flatten(?(?(42))) == ?42;
assert Option.flatten(?(null)) == null;
assert Option.flatten(null) == null;
```

## Function `some`
``` motoko no-repl
func some<T>(self : T) : ?T
```

Creates an optional value from a definite value.
```motoko
import Option "mo:core/Option";
assert Option.some(42) == ?42;
```

## Function `isSome`
``` motoko no-repl
func isSome(self : ?Any) : Bool
```

Returns true if the argument is not `null`, otherwise returns false.

## Function `isNull`
``` motoko no-repl
func isNull(self : ?Any) : Bool
```

Returns true if the argument is `null`, otherwise returns false.

## Function `equal`
``` motoko no-repl
func equal<T>(self : ?T, other : ?T, eq : (implicit : (equal : (T, T) -> Bool))) : Bool
```

Returns true if the optional arguments are equal according to the equality function provided, otherwise returns false.

## Function `compare`
``` motoko no-repl
func compare<T>(self : ?T, other : ?T, compare : (implicit : (T, T) -> Types.Order)) : Types.Order
```

Compares two optional values using the provided comparison function.

Returns:
- `#equal` if both values are `null`,
- `#less` if the first value is `null` and the second is not,
- `#greater` if the first value is not `null` and the second is,
- the result of the comparison function when both values are not `null`.

## Function `unwrap`
``` motoko no-repl
func unwrap<T>(self : ?T) : T
```

Unwraps an optional value, i.e. `unwrap(?x) = x`.

`Option.unwrap()` fails if the argument is null. Consider using a `switch` or `do?` expression instead.

## Function `toText`
``` motoko no-repl
func toText<T>(self : ?T, toText : (implicit : T -> Text)) : Text
```

Returns the textural representation of an optional value for debugging purposes.
