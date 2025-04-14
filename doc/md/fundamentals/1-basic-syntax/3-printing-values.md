---
sidebar_position: 3
hide_table_of_contents: true
---
# Printing values

Motoko uses `Debug.print` to output text to the terminal. It takes a [`Text`](https://internetcomputer.org/docs/motoko/base/Text) value and returns `()`, meaning it has no meaningful return value.

```motoko
import Debug "mo:base/Debug";

Debug.print("Hello, world!");
```

For debugging purposes, `debug_show` converts most Motoko values into [`Text`](https://internetcomputer.org/docs/motoko/base/Text). The function handles primitive types and simple collections well, but may not work with nested, recursive, or complex custom types which may require **transformation or unwrapping** before use.

```motoko
import Debug "mo:base/Debug";
Debug.print(debug_show(42)); // "42"
```

Functions like `Debug.print("Hello, World!")` are considered [**impure functions**](https://internetcomputer.org/docs/motoko/fundamentals/types/functions) because they cause a side effect by printing to the console.

In contrast, [**pure functions**](https://internetcomputer.org/docs/motoko/fundamentals/types/functions) return values that do not modify output or have other side effects. `debug_show(42)` is pure because it always returns `"42"` without affecting anything outside the function.

## Resources

- [Debug](https://internetcomputer.org/docs/motoko/base/Debug)

<img src="https://cdn-assets-eu.frontify.com/s3/frontify-enterprise-files-eu/eyJwYXRoIjoiZGZpbml0eVwvYWNjb3VudHNcLzAxXC80MDAwMzA0XC9wcm9qZWN0c1wvNFwvYXNzZXRzXC8zOFwvMTc2XC9jZGYwZTJlOTEyNDFlYzAzZTQ1YTVhZTc4OGQ0ZDk0MS0xNjA1MjIyMzU4LnBuZyJ9:dfinity:9Q2_9PEsbPqdJNAQ08DAwqOenwIo7A8_tCN4PSSWkAM?width=2400" alt="Logo" width="150" height="150" />