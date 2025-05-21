---
sidebar_position: 2
---

# Loops

## Unconditional loops

An unconditional loop runs indefinitely unless explicitly stopped. Unlike `while` or `for` loops, which depend on a condition, unconditional loops continue execution without a predefined exit condition. These loops are useful when a process needs to run continuously, waiting for an external event, or relying on an internal break condition.

The `loop` keyword explicitly defines an infinite loop. However, when using `break`, the loop must have a label to indicate where execution should stop.

```motoko no-repl
public func unconditionalLoop() : async Nat {
    var count : Nat = 0;

    label countLoop loop {
        count += 1;
        if (count == 5) {
            break countLoop;
        };
    };

    return count;
}
```

## `for` loop

A `for` loop in Motoko follows the structure:

```motoko no-repl
for (pattern in iterator) {
  // Code to run for each item in the iterator
}
```

1. The iterator expression must have type `{ next : () → ?T }`, providing a `next` function that returns either a value (`?T`) or `null` when there are no more items.
2. The pattern must match the type `T` of the values produced by the iterator.
3. The loop first evaluates the iterator:
   - If evaluating the iterator causes a trap, the loop stops immediately.
   - If `next()` returns `null`, the loop ends.
   - If `next()` returns a value, it is assigned to the pattern and the loop body executes.
4. The loop continues calling `next()` until it returns `null`, at which point the loop stops and the final result is `()`.

```motoko no-repl
let numbers = [1, 2, 3, 4, 5];

for (num in numbers.vals()) {
  Debug.print(debug_show(num));
};
```

`numbers.vals()` produces an iterator over the array values. The loop assigns each value to `num` and runs the block. When there are no more values, `next()` returns `null`, and the loop exits.

## `while` loop

A `while` loop in Motoko follows the structure:

```motoko no-repl
while (condition) {
  // Code to run while the condition is true
}
```

1. The condition must be a [`Bool`](https://internetcomputer.org/docs/motoko/base/Bool) (`true` or `false`).
2. The loop body must have type `()` such that it doesn't return a meaningful value.
3. The loop first evaluates the condition:
   - If it causes a trap (error), the loop stops immediately.
   - If it evaluates to `false`, the loop ends and does nothing.
   - If it evaluates to `true`, the body runs and the loop repeats the process.
4. Once the condition becomes false, the loop stops, and the final result is `()`.

```motoko no-repl
var count = 3;
while (count > 0) {
  Debug.print("Counting down...");
  count -= 1;
};
```

<img src="https://cdn-assets-eu.frontify.com/s3/frontify-enterprise-files-eu/eyJwYXRoIjoiZGZpbml0eVwvYWNjb3VudHNcLzAxXC80MDAwMzA0XC9wcm9qZWN0c1wvNFwvYXNzZXRzXC8zOFwvMTc2XC9jZGYwZTJlOTEyNDFlYzAzZTQ1YTVhZTc4OGQ0ZDk0MS0xNjA1MjIyMzU4LnBuZyJ9:dfinity:9Q2_9PEsbPqdJNAQ08DAwqOenwIo7A8_tCN4PSSWkAM?width=2400" alt="Logo" width="150" height="150" />

