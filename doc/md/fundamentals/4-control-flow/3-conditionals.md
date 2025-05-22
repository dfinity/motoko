---
sidebar_position: 3
---

# Conditionals

Conditionals in Motoko come in two forms: **if-expressions** and **if-statements**.

## `if-expression`

An **if-expression** evaluates to a value and is used when a decision needs to produce a result. This value must have a consistent type across all branches and can be assigned to a variable or used within other expressions. For example, you might use an `if-expression` to choose a label or determine logic flow based on a condition.

```motoko no-repl
let x : Int = 1;

let identity : Text =
  if (x == 1) {
    "x is 1"
  } else {
    "x is not 1"
  }; // Produces a value
```

The result of the `if` block is assigned to `identity`. Both branches must return a matching type ([`Text`](https://internetcomputer.org/docs/motoko/base/Text) in this case).

## `if-statement`

An **if-statement** is used purely for its side effects. It does not return a value, and its type is `()`, meaning it cannot be used in assignments or further expressions. `if-statements` are best suited for situations where you need to perform actions, such as logging or modifying state based on certain conditions.

```motoko no-repl
let x : Int = 1;

if (x == 1) {
    // Cannot be assigned to a variable since its type is `()`
    Debug.print("x is 1"); // Executes but does not return a value
} else {
    Debug.print("x is not 1");
}
```


## `if-else`

The **if-else** form extends conditions to support multiple paths of execution. `if-else` evaluates for a condition; if that condition is true, it executes the first block. If it is false, it proceeds to the `else` or `else if` blocks. `if-else` is used to handle more than two logical branches and supports both expression and statement styles.

```motoko no-repl
var age = 21;

if (age < 18) {
  "You are a minor."
} else if (age >= 18 and age < 65) {
  "You are an adult."
} else {
  "You are a senior citizen."
};
```


<img src="https://cdn-assets-eu.frontify.com/s3/frontify-enterprise-files-eu/eyJwYXRoIjoiZGZpbml0eVwvYWNjb3VudHNcLzAxXC80MDAwMzA0XC9wcm9qZWN0c1wvNFwvYXNzZXRzXC8zOFwvMTc2XC9jZGYwZTJlOTEyNDFlYzAzZTQ1YTVhZTc4OGQ0ZDk0MS0xNjA1MjIyMzU4LnBuZyJ9:dfinity:9Q2_9PEsbPqdJNAQ08DAwqOenwIo7A8_tCN4PSSWkAM?width=2400" alt="Logo" width="150" height="150" />
