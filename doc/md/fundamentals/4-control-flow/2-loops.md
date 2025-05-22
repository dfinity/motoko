---
sidebar_position: 2
---

# Loops

In Motoko, loops provide flexible control over repetition, such as iterating over collections, waiting for conditions to change, or running code indefinitely until explicitly stopped.

Motoko supports different types of loops:

- Unconditional loops: Run indefinitely unless manually stopped.

- `for` loops: Iteration over collections.

- `while` loops: Condition-based repetition.

## Unconditional loops

An unconditional loop runs indefinitely until it is explicitly stopped. Unlike `while` or `for` loops, which rely on a condition to determine when to exit, unconditional loops continue executing without any predefined exit condition. They are useful in scenarios where the program waits for an external event or depends on a break condition defined within the loop body.

Motoko uses the `loop` keyword to define an infinite loop. To exit such a loop, you must use a `break` statement with a label, indicating exactly which loop to terminate.

```motoko no-repl
shared func unconditionalLoop() : async Nat {
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

A `for` loop in Motoko is used to iterate over the elements of a collection using the following structure:

```motoko no-repl
for (pattern in iterator) {
  // Code to run for each item
}
```

`for` loops must follow these parameters:

- The iterator must provide a `next()` function that returns either a value (`?T`) or `null` when no items remain.

- The pattern is a variable that matches the type of each item returned by the iterator.

The `for` loop's iterator is evaluated once at the start. Each time through the loop, `next()` is called:

- If it returns a value, it is assigned to pattern, and the loop body runs.

- If it returns `null`, the loop stops.

If evaluating the iterator causes a trap (error), the loop stops immediately.

```motoko no-repl
let numbers = [1, 2, 3, 4, 5];

for (num in numbers.vals()) {
  Debug.print(debug_show(num));
};
```

## `while` loop

A `while` loop in Motoko repeatedly executes a block of code as long as a given condition is `true`.

```motoko no-repl
while (condition) {
  // Code to run while the condition is true
}
```

`while` loops must follow these parameters:

- The condition must be a [`Bool`](https://internetcomputer.org/docs/motoko/base/Bool) (`true` or `false`).

- The loop body must have type `()` such that it doesn't return a meaningful value.

The `while` loop first evaluates the condition:

   - If it causes a trap (error), the loop stops immediately.

   - If it evaluates to `false`, the loop ends and does nothing.

   - If it evaluates to `true`, the body runs and the loop repeats the process.

Once the condition becomes false, the loop stops, and the final result is `()`.

```motoko no-repl
var count = 3;
while (count > 0) {
  Debug.print("Counting down...");
  count -= 1;
};
```

<img src="https://cdn-assets-eu.frontify.com/s3/frontify-enterprise-files-eu/eyJwYXRoIjoiZGZpbml0eVwvYWNjb3VudHNcLzAxXC80MDAwMzA0XC9wcm9qZWN0c1wvNFwvYXNzZXRzXC8zOFwvMTc2XC9jZGYwZTJlOTEyNDFlYzAzZTQ1YTVhZTc4OGQ0ZDk0MS0xNjA1MjIyMzU4LnBuZyJ9:dfinity:9Q2_9PEsbPqdJNAQ08DAwqOenwIo7A8_tCN4PSSWkAM?width=2400" alt="Logo" width="150" height="150" />

