# Func
Functions on functions, creating functions from simpler inputs.

(Most commonly used when programming in functional style using higher-order
functions.)

## Function `compose`
``` motoko no-repl
func compose<A, B, C>(f : B -> C, g : A -> B) : A -> C
```

Import from the base library to use this module.

```motoko name=import
import { compose; const; identity } = "mo:base/Func";
import Text = "mo:base/Text";
import Char = "mo:base/Char";
```
The composition of two functions `f` and `g` is a function that applies `g` and then `f`.

Example:
```motoko include=import
let textFromNat32 = compose(Text.fromChar, Char.fromNat32);
assert textFromNat32(65) == "A";
```

## Function `identity`
``` motoko no-repl
func identity<A>(x : A) : A
```

The `identity` function returns its argument.
Example:
```motoko include=import
assert identity(10) == 10;
assert identity(true) == true;
```

## Function `const`
``` motoko no-repl
func const<A, B>(x : A) : B -> A
```

The const function is a _curried_ function that accepts an argument `x`,
and then returns a function that discards its argument and always returns
the `x`.

Example:
```motoko include=import
assert const<Nat, Text>(10)("hello") == 10;
assert const(true)(20) == true;
```
