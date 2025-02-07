---
sidebar_position: 3
---
# Printing values

Motoko uses `Debug.print` to output text to the terminal. It takes a `Text` value and returns `()`, meaning it has no meaningful return value.  

```motoko
import Debug "mo:base/Debug";
Debug.print("Hello, world!");
```

For debugging, `debug_show` converts most Motoko values into `Text`:  

```motoko
import Debug "mo:base/Debug";
Debug.print(debug_show(42)); // "42"
```

Since printing modifies output, it is considered an **impure function**, unlike pure functions that return values without side effects.
