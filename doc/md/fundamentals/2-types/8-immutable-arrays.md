---
sidebar_position: 8
---

# Immutable arrays

Immutable arrays in are fixed-size, read-only data structures that allow efficiently storing elements of the same type. Unlike [mutable arrays](https://internetcomputer.org/docs/motoko/fundamentals/types/mutable-arrays), they cannot be modified after creation, ensuring data integrity and predictable behavior.

## When to use immutable arrays

Immutable arrays in Motoko are best used when:

- Fixed-size storage is required: The number of elements is known in advance and will not change.
- Performance optimization is needed: They provide efficient index access without dynamic resizing overhead.
- Data integrity must be preserved: Immutability ensures that no accidental modifications occur.

Immutable arrays do not allow in-place modifications, making them suitable when stability and performance are priorities.

If the number of elements may change, collections like `List` or `Buffer` are a better choice, as immutable arrays require creating a new array each time an element is added or removed, which is inefficient.

## Defining an immutable array

Immutable arrays are declared using square brackets `[T]`. The type of the array is specified within the square brackets, e.g., `[Nat]` declares an immutable array of natural numbers.

```motoko no-repl
let immutableArray: [Nat] = [1, 2, 3, 4, 5];
```

## Accessing and modifying elements

Attempting to access an array's index that does not exist will cause a [trap](https://internetcomputer.org/docs/motoko/fundamentals/basic-syntax/traps). Attempting to modify an immutable array will result in an error `expected mutable assignment target(M0073)`.

```motoko no-repl
let numbers: [Nat] = [10, 20, 30];

let first: Nat = numbers[0];  // 10
let second: Nat = numbers[1]; // 20

Debug.print(debug_show(first));
Debug.print(debug_show(second));
```

## Iterating through an array

There are two primary ways to iterate through the elements in an array in Motoko:

1. Using `Array.vals`, which provides an iterator.

2. Using a `for` loop that runs from `0` to `array.size() - 1`, as arrays are zero-based.

Both methods achieve the same result, but `Array.vals` is often preferred for its readability and simplicity.

### Using `Array.vals`

The `Array.vals` function returns an iterator that is used to iterate over the array's elements without manually managing indices.

```motoko no-repl
let arr = [1, 2, 3, 4, 5];

for (val in arr.vals()) {
    Debug.print(debug_show(val));
}
```

### Using a `for` loop

A `for` loop can also be used to iterate over an array by accessing elements via their index.

```motoko no-repl
for (i in Iter.range(0, arr.size() - 1)) {
    Debug.print(debug_show(arr[i]));
}
```

## Converting an immutable array to a mutable array

You can convert an immutable array into a mutable array using `Array.thaw` which is useful when working with data that needs to be modified in place after initially being immutable.

```motoko no-repl
let immutableArray: [Nat] = [1, 2, 3, 4, 5];

let mutableCopy: [var Nat] = Array.thaw<Nat>(immutableArray);
mutableCopy[0] := 10;
//
```


## Passing a variable number of arguments

Motoko supports passing collections to a function., ensuring that all arguments are handled as a collection rather than individual parameters.

```motoko no-repl
let greetings : [Text] = ["Hello, "Hola", "Ciao" ]

func printAllStrings(strings: [Text]) {
    for (s in strings.vals()) {
        Debug.print(s);
    }
}
```

:::info

Mutable arrays cannot be shared publicly; they can only be passed and modified within the [actor](https://internetcomputer.org/docs/motoko/fundamentals/actors-async) privately.

:::

## Comparing arrays

Comparing arrays requires element-wise comparison. The `Array.equal` function can be used to check whether two arrays contain the same elements in the same order.

Unlike some languages, Motoko does not compare arrays by reference when using `Array.equal`, ensuring a proper element-by-element comparison.

```motoko no-repl
import Array "mo:base/Array";

func compareArrays() : () {
    let arr1 : [Nat] = [1, 2, 3];
    let arr2 : [Nat] = [1, 2, 3];
    let arr3 : [Nat] = [3, 2, 1];

    Debug.print(debug_show(Array.equal<Nat>(arr1, arr2, func(x, y) { x == y }))); // true
    Debug.print(debug_show(Array.equal<Nat>(arr1, arr3, func(x, y) { x == y }))); // false
    }
```

## Transforming arrays

Motoko's base library [`Array`](https://internetcomputer.org/docs/motoko/base/Array) provides built-in functions for mapping over elements, filtering values, and summing numerical arrays.

```motoko no-repl
import Array "mo:base/Array";

func transformArray() : async () {
    let numbers : [Nat] = [1, 2, 3];
    let doubled : [Nat] = Array.map<Nat, Nat>(numbers, func(x) { x * 2 });

    Debug.print(debug_show(doubled)); // [2, 4, 6]
}
```

## Nested immutable arrays example: Chessboard

To demonstrate nested immutable arrays, consider the following:

A chessboard is a fixed `8×8` grid. Using immutable arrays to represent the initial [state](https://internetcomputer.org/docs/motoko/fundamentals/state) of the board ensures that the setup remains unchanged, preventing accidental modifications. This is useful because the starting position of pieces in chess is fixed, and any changes should be intentional, such as when making a move. Immutable arrays provide stability and help maintain the integrity of the initial board [state](https://internetcomputer.org/docs/motoko/fundamentals/state).


```motoko no-repl
 func generateChessboard() : [[Text]] {
    let size : Nat = 8;

    let board : [[Text]] = Array.tabulate<[Text]>(size, func(r : Nat) : [Text] {
        Array.tabulate<Text>(size, func(c : Nat) : Text {
            switch (r, c) {
              case (0, 0) {"♜"}; case (0, 1) {"♞"}; case (0, 2) {"♝"}; case (0, 3) {"♛"}; case (0, 4) { "♚"};
              case (0, 5) {"♝"}; case (0, 6) {"♞"}; case (0, 7) {"♜"}; case (1, _) {"♟"}; // Pawns on row 1
              case (6, _) {"♙"}; // Pawns on row 6
              case (7, 0) {"♖"}; case (7, 1) {"♘"}; case (7, 2) {"♗"}; case (7, 3) {"♕"};
              case (7, 4) {"♔"}; case (7, 5) {"♗"}; case (7, 6) {"♘"}; case (7, 7) { "♖"}; case (_, _) {"."} // Empty squares
            }
          }
        )
      }
    );
    board
  };

  // Generate the immutable chessboard
  let chessboard : [[Text]] = generateChessboard();

  // Display the board
  for (row in chessboard.vals()) {
    let rowText = Array.foldLeft<Text, Text>(row, "", func(acc, square) = acc # square # " ");
    Debug.print(rowText)
  }
```

The function `Array.foldLeft` combines the squares in the row into a single text string, which can then be printed.

``` md
♜ ♞ ♝ ♛ ♚ ♝ ♞ ♜
♟ ♟ ♟ ♟ ♟ ♟ ♟ ♟
. . . . . . . .
. . . . . . . .
. . . . . . . .
. . . . . . . .
♙ ♙ ♙ ♙ ♙ ♙ ♙ ♙
♖ ♘ ♗ ♕ ♔ ♗ ♘ ♖
```


## Resources

- [`Array`](https://internetcomputer.org/docs/motoko/base/Array)
- [`Iter`](https://internetcomputer.org/docs/motoko/base/Iter)

<img src="https://cdn-assets-eu.frontify.com/s3/frontify-enterprise-files-eu/eyJwYXRoIjoiZGZpbml0eVwvYWNjb3VudHNcLzAxXC80MDAwMzA0XC9wcm9qZWN0c1wvNFwvYXNzZXRzXC8zOFwvMTc2XC9jZGYwZTJlOTEyNDFlYzAzZTQ1YTVhZTc4OGQ0ZDk0MS0xNjA1MjIyMzU4LnBuZyJ9:dfinity:9Q2_9PEsbPqdJNAQ08DAwqOenwIo7A8_tCN4PSSWkAM?width=2400" alt="Logo" width="150" height="150" />