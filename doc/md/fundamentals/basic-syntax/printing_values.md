---
sidebar_position: 3
---
# Printing values

Motoko uses `Debug.print` to output text to the terminal. It takes a `Text` value and returns `()`, meaning it has no meaningful return value.  

```motoko
import Debug "mo:base/Debug";
Debug.print("Hello, world!");
```

For debugging, `debug_show` converts most Motoko values into `Text`. The function handles primitive types and simple collections well but may not work with nested, recursive, or complex custom types, which may require **transformation or unwrapping** before use.

```motoko
import Debug "mo:base/Debug";
Debug.print(debug_show(42)); // "42"
```

Since printing modifies output, it is considered an **impure function**, unlike pure functions that return values without side effects.

`debug_show(42)` is pure because it always returns `"42"` without affecting anything outside the function.

`Debug.print`("Hello, World!") is impure because it causes a side effect by printing to the console.

## References

- [Base library Debug](https://internetcomputer.org/docs/current/motoko/main/base/Debug)
