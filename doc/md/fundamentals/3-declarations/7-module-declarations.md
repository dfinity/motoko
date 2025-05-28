---
sidebar_position: 7
---

# Module declarations

In Motoko, a **module** is a collection of related types, values, and logic grouped under a single namespace. Unlike actors and classes, modules cannot hold or mutate state, making them ideal for reusing side-effect-free code.

Modules are mainly used to build libraries, such as those in the [base library](https://internetcomputer.org/docs/motoko/main/base/) or packages available through [Mops, the Motoko package manager](https://mops.one).

A module in Motoko can define:

- Public types.
- Public functions, both synchronous and asynchronous.
- Private types and internal logic that is not exposed outside the namespace.
- Nested modules.

Modules provide encapsulation and structure, allowing code to be organized and reused across different parts of an application.

```motoko no-repl
import Array "mo:base/Array";

module Matrix {
  // Define what a matrix is - a 2D array of Nat
  public type Matrix = [[Nat]];

  // Create a matrix filled with a default value
  public func init(rows : Nat, cols : Nat, default : Nat) : Matrix {
    Array.tabulate<[Nat]>(rows, func (i : Nat) {
      Array.tabulate<Nat>(cols, func (j : Nat) {
        default
      })
    })
  };

  // Create an identity matrix (ones on diagonal, zeros elsewhere)
  public func identity(n : Nat) : Matrix {
    Array.tabulate<[Nat]>(n, func (i : Nat) {
      Array.tabulate<Nat>(n, func (j : Nat) {
        if (i == j) { 1 } else { 0 }
      })
    })
  };

  // Transpose a matrix (flip rows and columns)
  public func transpose(m : Matrix) : Matrix {
    if (m.size() == 0) return [];
    let rows = m.size();
    let cols = m[0].size();
    Array.tabulate<[Nat]>(cols, func (j: Nat) {
      Array.tabulate<Nat>(rows, func (i:Nat) {
        m[i][j]
      })
    })
  };
};
```

This module declares:

- A public type `Matrix.Matrix` for representing a 2D array of natural numbers `[[Nat]]`.
- Functions `Matrix.init()`, `Matrix.identity()`, and `Matrix.transpose()` for matrix operations.

:::note Functional programming style
This module follows a functional programming approach: all operations return new matrices instead of modifying existing ones. This immutable design improves predictability and minimizes side effects.
:::


## Module limitations

Modules cannot contain mutable state (`var`) or non-static expressions.

```motoko no-repl
import Debug "mo:base/Debug";
module myModule {
  var x = 0; // Not allowed, non-static expression in library
  Debug.print("hello"); // Not allowed, side-effect in library
}
```

If you need mutable state, use an `actor` or an `object` inside an actor instead:

```motoko no-repl
persistent actor {
  var x = 0; // allowed because actors can have state
}
```

## Using modules

Modules expose their public types and functions through dot notation. The module name serves as a namespace, helping prevent naming conflicts and group related functionality.

```motoko no-repl
import Matrix "Matrix";

// Create a 3x3 matrix filled with zeros
let zeros = Matrix.init(3, 3, 0);

// Create a 4x4 identity matrix
let id4 = Matrix.identity(4);

// Transpose a matrix
let transposed = Matrix.transpose([[1, 2, 3], [4, 5, 6]]);
// Result: [[1, 4], [2, 5], [3, 6]]

// Use the Matrix type from the module
let myMatrix : Matrix.Matrix = [[1, 2], [3, 4]];

// Chain operations together
let result = Matrix.transpose(Matrix.identity(3));
```

<img src="https://cdn-assets-eu.frontify.com/s3/frontify-enterprise-files-eu/eyJwYXRoIjoiZGZpbml0eVwvYWNjb3VudHNcLzAxXC80MDAwMzA0XC9wcm9qZWN0c1wvNFwvYXNzZXRzXC8zOFwvMTc2XC9jZGYwZTJlOTEyNDFlYzAzZTQ1YTVhZTc4OGQ0ZDk0MS0xNjA1MjIyMzU4LnBuZyJ9:dfinity:9Q2_9PEsbPqdJNAQ08DAwqOenwIo7A8_tCN4PSSWkAM?width=2400" alt="Logo" width="150" height="150" />