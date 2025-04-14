---
sidebar_position: 1
---

# Variable declarations

In Motoko, variables are declared using either `let` (immutable) or `var` (mutable).

| Feature                 | `let`    | `var`       |
|-------------------------|---------|-------------|
| Reassignment           | Not allowed | Allowed |
| Value modification     | Cannot be updated after assignment. | Can be updated after assignment. |
| Memory allocation      | Directly stores value. | Stores a reference to a memory location. |
| Persistence in actors  | Retained unless assigned a mutable value. | Allowed with `stable var`. |
| Upgrade behavior       | Persists unless holding a `var` reference. | Must be explicitly marked `stable` to persist. |
| Compiler optimization  | More optimal | Less optimal |

## Immutable variables

A `let` declaration binds a value to a name and cannot be changed after assignment. The compiler ensures that any attempt to modify an immutable variable results in an error.

```motoko no-repl
let x = 10;
x := 20; // Error: Cannot assign to immutable variable
```

### Memory binding in `let`

When using `let`, Motoko stores the value directly in memory and does not allow it to change. Since it never changes, the compiler can optimize `let` variables making them more efficient.

## Mutable variables

A `var` declaration allocates space in memory where the value can be updated. Unlike `let`, `var` allows reassignment.

```motoko no-repl
var y = 10;
y := 20; // Allowed, updates the value of y
```

### Memory binding in `var`

When using `var`, Motoko creates a reference to a memory location used to update the value.

<img src="https://cdn-assets-eu.frontify.com/s3/frontify-enterprise-files-eu/eyJwYXRoIjoiZGZpbml0eVwvYWNjb3VudHNcLzAxXC80MDAwMzA0XC9wcm9qZWN0c1wvNFwvYXNzZXRzXC8zOFwvMTc2XC9jZGYwZTJlOTEyNDFlYzAzZTQ1YTVhZTc4OGQ0ZDk0MS0xNjA1MjIyMzU4LnBuZyJ9:dfinity:9Q2_9PEsbPqdJNAQ08DAwqOenwIo7A8_tCN4PSSWkAM?width=2400" alt="Logo" width="150" height="150" />