---
sidebar_position: 4
---

# Block expressions

A block expression in Motoko is a sequence of statements enclosed in `{ ... }`. Blocks are used to group multiple operations, define local variables, and organize code for readability and logic.

Block expressions are fundamental to function definitions, as they determine both the function’s structure and return value.

The last declaration in a block, which might be an expression, determines the block’s result. If no meaningful final declaration is present, the block returns `()`.

## Block expressions in functions

Every function in Motoko contains a block expression that defines its body and behavior.

```motoko no-repl
// The function body is a block expression that returns the result of the last expression.
shared func add(x : Nat, y : Nat) : async Nat {
    let sum : Nat = x + y;
    sum;
};

// Example of using a block expression with conditional logic:
func classify(n : Int) : Text {
    if (n > 0) {
        "Positive"
    } else if (n < 0) {
        "Negative"
    } else {
        "Zero"
    } // The last evaluated branch determines the return value.
};
```

### Block assignments

Blocks are not limited to functions; they can be used anywhere an expression is expected.

```motoko no-repl
let result : Nat = do {
  let x : Nat = 10;
  let y : Nat = 5;
  x * y // This value is returned and assigned to `result`
};
```

## `do` block

The `do {}` expression in Motoko is used for executing multiple statements sequentially, while ensuring the block is treated as an expression. Unlike standard block expressions inside functions, the `do` block must return a value of type `()` which means it’s intended for side effects, not for returning meaningful values.

If a statement inside the `do` block produces a value, that value can be discarded with the `ignore` keyword, unless it’s explicitly needed.


```motoko no-repl
do {
    let x : Nat = 10;
    let y : Nat = 5;
    Debug.print("Adding " # debug_show(x) # " and " # debug_show(y));
    let sum : Nat = x + y;
    Debug.print("Result: " # debug_show(sum)); // unit `()` return type
};
```

<img src="https://cdn-assets-eu.frontify.com/s3/frontify-enterprise-files-eu/eyJwYXRoIjoiZGZpbml0eVwvYWNjb3VudHNcLzAxXC80MDAwMzA0XC9wcm9qZWN0c1wvNFwvYXNzZXRzXC8zOFwvMTc2XC9jZGYwZTJlOTEyNDFlYzAzZTQ1YTVhZTc4OGQ0ZDk0MS0xNjA1MjIyMzU4LnBuZyJ9:dfinity:9Q2_9PEsbPqdJNAQ08DAwqOenwIo7A8_tCN4PSSWkAM?width=2400" alt="Logo" width="150" height="150" />