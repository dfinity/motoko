---
sidebar_position: 4
---

# Block expressions

A block expression is a sequence of statements enclosed in `{ ... }`. They can be used for grouping multiple operations, defining local variables, and structuring code logically. Blocks are essential to functions, as they define the function body and determine the return value. They follow these guidelines:

1. A block contains a series of expressions and statements.
2. The last evaluated expression determines the block’s result.
3. If no meaningful final expression is present, the block returns `()`.

## Block expressions in functions

Every function in Motoko contains a block expression that defines its behavior.

```motoko no-repl
// The function body is a block expression: { let sum = x + y; sum }
public func add(x: Nat, y: Nat) : Nat {
    let sum : Nat = x + y;
    sum // The last expression is returned as the function's result
}
```

### Blocks with control flow

Block expressions support conditionals, loops, and other control structures. The type of the values must match across possible expressions.

```motoko no-repl
// The block contains an if-else structure.
public func classifyNumber(n: Int) : Text {
    if (n > 0) {
        "Positive"
    } else if (n < 0) {
        "Negative"
    } else {
        "Zero"
    } // The last expression determines the return value
}
```

### Block assignments

Blocks are not limited to functions; they can be used anywhere an expression is expected.

```motoko no-repl
// The block is evaluated as an expression.
let result : Nat = {
    let x : Nat = 10;
    let y : Nat = 5;
    x * y // This value is assigned to `result`
    };
```

## `do` block

The `do {}` expression can be used for executing multiple statements sequentially while ensuring the block is treated as an expression. The key constraint is that it must return `()`, meaning the last evaluated expression inside the block must have type `()`. If an operation inside the block produces a value, it can be discarded using the `ignore` keyword to ensure the correct return type.

```motoko no-repl
do {
    let x : Nat = 10;
    let y : Nat = 5;
    Debug.print("Adding " # debug_show(x) # " and " # debug_show(y));
    let sum : Nat = x + y;
    Debug.print("Result: " # debug_show(sum)); // () return type
};
```

<img src="https://cdn-assets-eu.frontify.com/s3/frontify-enterprise-files-eu/eyJwYXRoIjoiZGZpbml0eVwvYWNjb3VudHNcLzAxXC80MDAwMzA0XC9wcm9qZWN0c1wvNFwvYXNzZXRzXC8zOFwvMTc2XC9jZGYwZTJlOTEyNDFlYzAzZTQ1YTVhZTc4OGQ0ZDk0MS0xNjA1MjIyMzU4LnBuZyJ9:dfinity:9Q2_9PEsbPqdJNAQ08DAwqOenwIo7A8_tCN4PSSWkAM?width=2400" alt="Logo" width="150" height="150" />