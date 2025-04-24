---
sidebar_position: 3
hide_table_of_contents: true
---
# Printing values

Motoko uses `Debug.print` to output text to the terminal or a canister's log, depending on execution context.
It takes a [`Text`](https://internetcomputer.org/docs/motoko/base/Text) value and returns `()`.
`()` is the empty tuple and represents a token, or trivial, return value.

```motoko
import Debug "mo:base/Debug";

Debug.print("Hello, world!");
```

Many libraries provide `toText` functions for converting types to `Text`. For example:

```motoko
import Debug "mo:base/Debug";
import Nat "mo:base/Nat";

Debug.print(Nat.toText(42));
```

For debugging purposes, `debug_show` converts most Motoko types into [`Text`](https://internetcomputer.org/docs/motoko/base/Text). The operator handles most types well, but may not work with cyclic data structures or types containing functions or type parameters.

```motoko
import Debug "mo:base/Debug";

Debug.print(debug_show {life = 42} ); // "{life = 42}"
```

Functions like `Debug.print("Hello, World!")` are considered [**impure functions**](https://internetcomputer.org/docs/motoko/fundamentals/types/functions) because they cause a side effect by printing to the console or log.

In contrast, [**pure functions**](https://internetcomputer.org/docs/motoko/fundamentals/types/functions) return values that do not modify output or have other side effects like sending messages.  For example `Nat.toText(42)` is pure because it always returns `"42"` with no other effect.

## Resources

- [Debug](https://internetcomputer.org/docs/motoko/base/Debug)

<img src="https://cdn-assets-eu.frontify.com/s3/frontify-enterprise-files-eu/eyJwYXRoIjoiZGZpbml0eVwvYWNjb3VudHNcLzAxXC80MDAwMzA0XC9wcm9qZWN0c1wvNFwvYXNzZXRzXC8zOFwvMTc2XC9jZGYwZTJlOTEyNDFlYzAzZTQ1YTVhZTc4OGQ0ZDk0MS0xNjA1MjIyMzU4LnBuZyJ9:dfinity:9Q2_9PEsbPqdJNAQ08DAwqOenwIo7A8_tCN4PSSWkAM?width=2400" alt="Logo" width="150" height="150" />