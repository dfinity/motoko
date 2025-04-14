---
sidebar_position: 6
---

# Expression declarations

An expression declaration is a statement where an expression is evaluated only for its side effects. It is not assigned to a variable and does not introduce any new bindings.

## Rules for expression declarations

1. The expression must have a valid type `T`, and evaluates normally.
2. If it appears in a sequence of declarations but is not the last declaration, then `T` must be `()` (i.e., it must not return a value).

## Basic usage

Expression declarations can be used for functions or operations that produce side effects, such as printing or modifying [state](https://internetcomputer.org/docs/motoko/fundamentals/state).

```motoko no-repl
Debug.print("Hello, Motoko!");
```

This example prints `"Hello, Motoko!"` but does not assign it to a variable.

## Expression declarations in a sequence

If an expression is placed inside a sequence of declarations, but it is not the last declaration, it must return `()`.

```motoko no-repl
let x = 10;
Debug.print("Processing..."); // Expression declaration with side effects
let y = x * 2; // Valid, since Debug.print() returns ()
```

`Debug.print()` is used for its side effect, and since it returns `()`, it can appear before `let y = x * 2;`.

## Invalid example

```motoko no-repl
let x = 10;
x * 2;  // This expression produces a value but is not assigned.
let y = 5;
```

The expression `x * 2;` returns a value ([`Nat`](https://internetcomputer.org/docs/motoko/base/Nat)), but it is not assigned. Since it is not the last declaration, it must return `()`.

<img src="https://cdn-assets-eu.frontify.com/s3/frontify-enterprise-files-eu/eyJwYXRoIjoiZGZpbml0eVwvYWNjb3VudHNcLzAxXC80MDAwMzA0XC9wcm9qZWN0c1wvNFwvYXNzZXRzXC8zOFwvMTc2XC9jZGYwZTJlOTEyNDFlYzAzZTQ1YTVhZTc4OGQ0ZDk0MS0xNjA1MjIyMzU4LnBuZyJ9:dfinity:9Q2_9PEsbPqdJNAQ08DAwqOenwIo7A8_tCN4PSSWkAM?width=2400" alt="Logo" width="150" height="150" />