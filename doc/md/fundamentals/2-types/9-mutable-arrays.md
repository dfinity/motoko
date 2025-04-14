---
sidebar_position: 9
---

# Mutable arrays

Mutable arrays allow direct modification of elements, making them suitable for scenarios where data needs to be updated frequently. Unlike [immutable arrays](https://internetcomputer.org/docs/motoko/fundamentals/types/immutable-arrays), which require creating a new array to reflect changes, mutable arrays support in place modifications, improving performance in some cases.

## When to use mutable arrays

Mutable arrays are beneficial when:

- Frequent modifications are required without the overhead of creating a new array.
- Dynamic algorithms need in place operations, such as sorting or shuffling.

## Immutable arrays vs mutable arrays

| Feature          | Immutable arrays                          | Mutable arrays                        |
|-----------------|------------------------------------------|---------------------------------------|
| Syntax          | `[T]`                                    | `[var T]`                            |
| Mutability      | Cannot be updated after creation.        | Can be updated in place.              |
| Modification    | Requires creating a new array.          | Can modify elements directly.         |
| Growth          | Not designed to grow.                   | Not designed to grow.                 |
| Shareability    | Can be shared across functions and actors. | Not sharable.       |
| Conversion      | Can be converted to mutable with `Array.thaw`. | Can be converted to immutable with `Array.freeze`. |
| Use case        | When data should remain unchanged.       | When frequent modifications are needed. |

:::warning

Unlike other programming languages that support append methods for arrays, Motoko arrays (both mutable and immutable) are not designed to grow dynamically. Motoko arrays cannot be appended. If a dynamically growing data structure is required, `Buffer` should be used instead.

:::

## Defining a mutable array

Mutable arrays use the `var` keyword inside the square brackets `[var T]`. The type of the array is also specified within the square brackets, e.g., `[var Nat]` declares a mutable array of natural numbers. In place element modification is supported in mutable arrays.

```motoko no-repl
let mutableArray: [var Nat] = [var 1, 2, 3, 4, 5];

mutableArray[0] := 10;  // Updates the first element to 10
```

## Accessing and modifying elements

Mutable array elements can be read and modified using indexed access. Attempting to access an index that does not exist will result in a [trap](https://internetcomputer.org/docs/motoko/fundamentals/basic-syntax/traps).

```motoko no-repl
let numbers: [var Nat] = [var 10, 20, 30];

numbers[0] := 100;  // updating first element

Debug.print(debug_show(numbers[0]));  // 100
```

## Converting a mutable array to an immutable array

You can convert a mutable array into an immutable array using `Array.freeze`, ensuring that the contents cannot be modified after conversion. Since mutable arrays are not [sharable](https://internetcomputer.org/docs/motoko/fundamentals/types/shared-types), freezing them is useful when passing data across [functions](https://internetcomputer.org/docs/motoko/fundamentals/types/functions) or [actors](https://internetcomputer.org/docs/motoko/fundamentals/actors-async) to ensure immutability.


```motoko no-repl
import Array "mo:base/Array";

let mutableArray: [var Nat] = [var 1, 2, 3];

let immutableArray: [Nat] = Array.freeze<Nat>(mutableArray);
```

## Nested mutable arrays example: Tic-tac-toe

To demonstrate nested mutable arrays, consider the following.

A Tic-tac-toe board is a `3x3` grid that requires updates as players take turns. Since elements must be modified, a nested mutable array is the ideal structure.

`Array.tabulateVar` is used to create a mutable board initialized with `"_"` (empty space).

```motoko no-repl
import Array "mo:base/Array";
import Debug "mo:base/Debug";

func createTicTacToeBoard() : [var [var Text]] {
    let size : Nat = 3;

    // Initialize a 3x3 board with empty spaces
    Array.tabulateVar<[var Text]>(
      size,
      func(_ : Nat) : [var Text] {
        Array.tabulateVar<Text>(size, func(_ : Nat) : Text {"_"}) // Fill with "_"
      }
    )
  };

  // Create a mutable Tic-tac-toe board
  let board : [var [var Text]] = createTicTacToeBoard();

  // Function to make a move
  func makeMove(row : Nat, col : Nat, player : Text) {
    if (board[row][col] == "_") {
      board[row][col] := player
    } else {
      Debug.print("Invalid move! The spot is already taken.")
    }
  };

  // Function to print the board
  func printBoard() {
    for (row in board.vals()) {
      let rowText = Array.foldLeft<Text, Text>(Array.freeze<Text>(row), "", func(acc, cell) = acc # cell # " ");
      Debug.print(rowText)
    }
  };

  // Example moves
  makeMove(0, 0, "X");
  makeMove(1, 1, "O");
  makeMove(2, 2, "X");

  printBoard();

```

Since both the outer and inner arrays are mutable, players can update the board in place. The array must be frozen before `foldLeft()` can be applied to the rows as `foldleft()` expects an immutable array as an argument.


```md
X _ _
_ O _
_ _ X
```

## Resources

- [`Array`](https://internetcomputer.org/docs/current/motoko/base/Array)

<img src="https://cdn-assets-eu.frontify.com/s3/frontify-enterprise-files-eu/eyJwYXRoIjoiZGZpbml0eVwvYWNjb3VudHNcLzAxXC80MDAwMzA0XC9wcm9qZWN0c1wvNFwvYXNzZXRzXC8zOFwvMTc2XC9jZGYwZTJlOTEyNDFlYzAzZTQ1YTVhZTc4OGQ0ZDk0MS0xNjA1MjIyMzU4LnBuZyJ9:dfinity:9Q2_9PEsbPqdJNAQ08DAwqOenwIo7A8_tCN4PSSWkAM?width=2400" alt="Logo" width="150" height="150" />