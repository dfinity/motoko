---
sidebar_position: 3
---

# Conditionals

| If-expression                                        | If-statement                              |
|------------------------------------------------------|-------------------------------------------|
| Type `T` (must be consistent across branches)              | `()` (does not return a value)           |
| Produces a result that can be used or assigned. | Executes a block of code but does not return a value. |
| Can be assigned to a variable.               | Executes for side effects only.       |
| Used when a decision determines a value.       | Used when a decision triggers an action.  |

An `if-else` expression allows a program to make decisions based on a condition. It evaluates a boolean expression and selects one of two possible execution paths.

```motoko no-repl
var age : Nat = 21
  if (age < 18) {
    "You are a minor."
  } else if (age >= 18 and age < 65) {
      "You are an adult."
    } else {
      "You are a senior citizen."
    }
```

## `if` expression

```motoko no-repl
let identity : Text = if (x == 1) "x is 1" else "x is not 1"; // Produces a value
```

The result of the `if` block is assigned to `identity`. Both branches must return a matching type ([`Text`](https://internetcomputer.org/docs/motoko/base/Text) in this case).

## `if` statement

```motoko no-repl
if (x == 1) {
    // Cannot be assigned to a variable since its type is `()`
    Debug.print("x is 1"); // Executes but does not return a value
} else {
    Debug.print("x is not 1");
}
```

<img src="https://cdn-assets-eu.frontify.com/s3/frontify-enterprise-files-eu/eyJwYXRoIjoiZGZpbml0eVwvYWNjb3VudHNcLzAxXC80MDAwMzA0XC9wcm9qZWN0c1wvNFwvYXNzZXRzXC8zOFwvMTc2XC9jZGYwZTJlOTEyNDFlYzAzZTQ1YTVhZTc4OGQ0ZDk0MS0xNjA1MjIyMzU4LnBuZyJ9:dfinity:9Q2_9PEsbPqdJNAQ08DAwqOenwIo7A8_tCN4PSSWkAM?width=2400" alt="Logo" width="150" height="150" />
