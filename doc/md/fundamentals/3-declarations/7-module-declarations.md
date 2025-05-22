---
sidebar_position: 7
---

# Module declarations

A module in Motoko is a collection of related types, values, and logic grouped together under a single namespace. Unlike actors and classes, modules cannot declare or mutate state, ensuring pure, side-effect-free reuse.

Modules are primarily used for building libraries, such as the [base library](https://internetcomputer.org/docs/motoko/main/base/) modules and libraries published on [MOPS (Motoko Package Manager)](https://mops.one).

:::note
Modules must be pure and cannot contain mutable state (`var`) or non-static expressions.

```motoko no-repl
module myModule {
  var x =0; // not allowed! non-static expression in library
}
```

If you need mutable state, use an `actor` or an `object` inside an actor instead:

```motoko no-repl
persistent actor {
  var x = 0; // allowed because actors can have state
}
```

:::

## Defining a module

A module in Motoko can define:

- Public types.
- Public functions, both synchronous and asynchronous.
- Private types and internal logic that is not exposed outside the namespace.
- Nested modules.

Modules provide encapsulation and organization, making code modular and reusable across different parts of applications.

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

:::note Functional programming style
Notice that this module follows a functional programming approach - all operations create new matrices rather than modifying existing structures. This immutable design makes the code predictable and reduces potential side effects.
:::

This module declares:

- A public type `Matrix.Matrix` for representing a 2d array of natural numbers `[[Nat]]`.
- Functions `Matrix.init()`, `Matrix.identity()`, and `Matrix.transpose()` for matrix operations.

## Using modules

Modules provide access to their public types and functions through dot notation. The module name acts as a namespace, preventing naming conflicts and organizing related functionality.

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