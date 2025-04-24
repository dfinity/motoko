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

## `if` statement

An if statement is a conditional control structure that evaluates a boolean expression and executes a block of code only when that expression is true.

```motoko no-repl
if (x == 1) {
    // Cannot be assigned to a variable since its type is `()`
    Debug.print("x is 1"); // Executes but does not return a value
} else {
    Debug.print("x is not 1");
}
```

## `if` expression

An if expression is a construct that evaluates a condition and returns one value if the condition is true and another value if it's false, treating conditional logic as an expression that produces a result rather than just a control flow statement.

```motoko no-repl
let identity : Text = if (x == 1) "x is 1" else "x is not 1"; // Produces a value
```

The result of the `if` block is assigned to `identity`. Both branches must return a matching type ([`Text`](https://internetcomputer.org/docs/motoko/base/Text) in this case).

## `if-else` expression

An `if-else` expression allows a program to make decisions based on a condition. It evaluates a boolean expression and selects one of two possible execution paths.

```motoko no-repl
let stakingTerm = 1;

if (stakingTerm < 1) {
  Debug.print("You're a pleb!");
} else if (stakingTerm >= 2 and stakingTerm < 8) {
  Debug.print("You're a serious shark!");
} else {
  Debug.print("You're part of the 8 year gang!");
};
```

<img src="https://cdn-assets-eu.frontify.com/s3/frontify-enterprise-files-eu/eyJwYXRoIjoiZGZpbml0eVwvYWNjb3VudHNcLzAxXC80MDAwMzA0XC9wcm9qZWN0c1wvNFwvYXNzZXRzXC8zOFwvMTc2XC9jZGYwZTJlOTEyNDFlYzAzZTQ1YTVhZTc4OGQ0ZDk0MS0xNjA1MjIyMzU4LnBuZyJ9:dfinity:9Q2_9PEsbPqdJNAQ08DAwqOenwIo7A8_tCN4PSSWkAM?width=2400" alt="Logo" width="150" height="150" />
