---
sidebar_position: 3
---
# Printing values

Motoko uses `Debug.print` to output text to the terminal. It takes a [`Text`](/docs/motoko/base/Text) value and returns `()`, meaning it has no meaningful return value.

```motoko
import Debug "mo:base/Debug";

Debug.print("Hello, world!");
```

For debugging purposes, `debug_show` converts most Motoko values into [`Text`](/docs/motoko/base/Text). The function handles primitive types and simple collections well, but may not work with nested, recursive, or complex custom types which may require **transformation or unwrapping** before use.

```motoko
import Debug "mo:base/Debug";
Debug.print(debug_show(42)); // "42"
```

Printing values modify return output, therefore functions like `Debug.print("Hello, World!")` are considered [**impure functions**](/docs/motoko/fundamentals/types/functions) because they cause a side effect by printing to the console.

In contrast, [**pure functions**](/docs/motoko/fundamentals/types/functions) return values that do not modify output or have other side effects. `debug_show(42)` is pure because it always returns `"42"` without affecting anything outside the function.

## References

- [Debug](/docs/motoko/base/Debug)

<img src="https://github.com/user-attachments/assets/844ca364-4d71-42b3-aaec-4a6c3509ee2e" alt="Logo" width="150" height="150" />