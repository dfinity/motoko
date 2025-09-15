# core/Func
Functions on functions, creating functions from simpler inputs.

(Most commonly used when programming in functional style using higher-order
functions.)

Import from the core package to use this module.

```motoko name=import
import Func = "mo:core/Func";
```

## Function `compose`
``` motoko no-repl
func compose<A, B, C>(f : B -> C, g : A -> B) : A -> C
```

The composition of two functions `f` and `g` is a function that applies `g` and then `f`.

Example:
```motoko include=import
import Text "mo:core/Text";
import Char "mo:core/Char";

let textFromNat32 = Func.compose(Text.fromChar, Char.fromNat32);
assert textFromNat32(65) == "A";
```

## Function `identity`
``` motoko no-repl
func identity<A>(x : A) : A
```

The `identity` function returns its argument.
Example:
```motoko include=import
assert Func.identity(10) == 10;
assert Func.identity(true) == true;
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
assert Func.const<Nat, Text>(10)("hello") == 10;
assert Func.const(true)(20) == true;
```
