---
sidebar_position: 6
---

# Expression declarations

An expression declaration is a statement where an expression is evaluated solely for its side effects. It does not assign the result to a variable or introduce any new bindings.

Rules for expression declarations:

1. The expression must have a valid type `T` and evaluate without errors.
2. If the expression appears in a sequence of declarations but is **not** the last one, its type `T` must be `()` (the unit type), meaning it produces no meaningful value.
3. If an expression returns a value other than `()`, but is used in a context where a value is not allowed (e.g., not the last declaration in a block), the `ignore` keyword can be used to discard the result.

For example:

```motoko no-repl
var n : Nat  = 0;

func regUser(name : Text) : Text {
  ignore bumpUsers(); // Returns Nat, but result is not needed
  let greeting = "Welcome, " # name # "!";
  greeting
};

func bumpUsers() : Nat {
   n += 1;
   n
};

```

## Basic usage

Expression declarations are commonly used for functions or operations that produce side effects, such as printing or modifying [state](https://internetcomputer.org/docs/motoko/fundamentals/state).

```motoko no-repl
Debug.print("Hello, Motoko!");
```

In this example, the string `"Hello, Motoko!"` is printed, but the expression is not assigned to any variable.

## Expression declarations in a sequence

If an expression appears inside a sequence of declarations but is not the last declaration, it must return `()` (the unit type).

```motoko no-repl
let x = 10;
Debug.print("Processing..."); // Expression declaration with side effects
let y = x * 2;                // Valid, since Debug.print() returns ()
```

In this example, `Debug.print()` is used for its side effect. Because it returns `()`, it can safely appear before `let y = x * 2;` in the sequence.

In comparison, below is an invalid expression declaration:

```motoko no-repl
let x = 10;
x * 2;  // This expression produces a value but is not assigned.
let y = 5;
```

The expression `x * 2;` returns a value of type `Nat`, but since it is not assigned to a variable and is not the last declaration, this is invalid. Expressions in the last position must return `()`.

<img src="https://cdn-assets-eu.frontify.com/s3/frontify-enterprise-files-eu/eyJwYXRoIjoiZGZpbml0eVwvYWNjb3VudHNcLzAxXC80MDAwMzA0XC9wcm9qZWN0c1wvNFwvYXNzZXRzXC8zOFwvMTc2XC9jZGYwZTJlOTEyNDFlYzAzZTQ1YTVhZTc4OGQ0ZDk0MS0xNjA1MjIyMzU4LnBuZyJ9:dfinity:9Q2_9PEsbPqdJNAQ08DAwqOenwIo7A8_tCN4PSSWkAM?width=2400" alt="Logo" width="150" height="150" />