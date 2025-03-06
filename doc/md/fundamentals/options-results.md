---
sidebar_position: 9
---

# Options and Results

| Type    | Syntax                  | Purpose                          | When to use it                                   |
|---------|-------------------------|----------------------------------|------------------------------------------------|
| Option  | `?T`                     | Represents a value that may be missing. | When an operation might return **no result** . |
| Result  | `Result.Result<T, E>`  where `T` is the success type and `E` is the error type.  | Represents success or failure.   | When an operation can either **succeed or fail**. |

## Options

```motoko no-repl
import Nat "mo:base/Nat";

func findEven(n: Nat): ?Nat {
    if (n % 2 == 0) {
        return ?n;
    } else {
        return null;
    };
};
```

## Results

The `Result` type can hold:

- `#ok(value)`: Success with a value.
- `#err(errorMessage)`: Failure with an error message.

```motoko no-repl
import Result "mo:base/Result";
import Nat "mo:base/Nat";

func divide(a: Nat, b: Nat): Result.Result<Nat, Text> {
    if (b == 0) {
        return #err("Cannot divide by zero");
    };
    return #ok(a / b);
};
```
