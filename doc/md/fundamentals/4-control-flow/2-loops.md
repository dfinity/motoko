---
sidebar_position: 2
---

# Loops

In Motoko, loops provide flexible control over repetition, such as iterating over collections, looping while some condition holds, or just looping until an explicit exit from the loop.

Motoko supports different types of loops:

- `loop` loops: Repeat until explicitly exited.

- `loop-while` loops: Repeat until condition is false (tests after each iteration);


- `for` loops: Iteration over collections.

- `while` loops: Repeat while condition is true (tests before each iteration).

## Unconditional loops

An unconditional loop runs indefinitely until it is explicitly stopped. Unlike `while` or `for` loops, which rely on a condition to determine when to exit, unconditional loops continue executing without any predefined exit condition. They are useful in scenarios where the program waits for an external event or depends on a break condition defined within the loop body.

Motoko uses the `loop` keyword to define an infinite loop. To exit such a loop, you must use a `break` statement with a label, indicating exactly which loop to terminate.

```motoko no-repl
func unconditionalLoop() : Nat {
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

A `for` loop must satisfy these conditions:

- The iterator must provide a `next()` function that returns either a value (`?T`) or `null` when no items remain.

- The pattern must match the type of each item returned by the iterator.
- The pattern cannot fail to match, e.g. by  matching on a particular number.

The `for` loop's iterator is evaluated once at the start. Each time through the loop, `next()` is called:

- If it returns a value, it is matched to the pattern, and the loop body runs with the identifiers declared by the pattern.

- If it returns `null`, the loop stops.

If evaluating the iterator causes a trap (error), the loop stops immediately.


```motoko no-repl
let numbers = [1, 2, 3, 4, 5];

for (num in numbers.vals()) {
  Debug.print(debug_show(num));
};
```

The pattern can also match on values, for example:

```motoko no-repl
let pairs = [(1, 2), (3, 4)];

for ((fst, snd) in pairs.vals()) {
  Debug.print(debug_show(fst + snd));
};
```


## `while` loop

A `while` loop in Motoko repeatedly executes a block of code as long as a given condition is `true`.

```motoko no-repl
while (condition) {
  // Code to run while the condition is true
}
```

A `while` loops must satisfy these constraints:

- The condition must have a type  [`Bool`](https://internetcomputer.org/docs/motoko/base/Bool) producing `true` or `false`.

- The loop body must have type `()` such that it doesn't return a meaningful value.

The `while` loop first evaluates the condition:


- If it evaluates to `false`, the loop ends and does nothing.

- If it evaluates to `true`, the body runs and the loop repeats.

Once the condition becomes false, the loop stops, and the final result is `()`.

```motoko no-repl
var count = 3;
while (count > 0) {
  Debug.print("Counting down...");
  count -= 1;
};
```

## Continuing loops

If a loop is labeled with a label `l` then you continue to the next iteration of the loop using the expression `continue l`.

```motoko no-repl
var count = 8;
label l while (count > 0) {
  if (count % 2 == 1) continue l; 
  Debug.print("Counting down...");
  count -= 1;
};
```

## Abandoning loops

While each form of loop defines its own exit conditions, that continues execution after the loop, you can always explicitly abandon a loop using an early `return`, `throw`, `break` or `!` (`null` break) when available.

<img src="https://cdn-assets-eu.frontify.com/s3/frontify-enterprise-files-eu/eyJwYXRoIjoiZGZpbml0eVwvYWNjb3VudHNcLzAxXC80MDAwMzA0XC9wcm9qZWN0c1wvNFwvYXNzZXRzXC8zOFwvMTc2XC9jZGYwZTJlOTEyNDFlYzAzZTQ1YTVhZTc4OGQ0ZDk0MS0xNjA1MjIyMzU4LnBuZyJ9:dfinity:9Q2_9PEsbPqdJNAQ08DAwqOenwIo7A8_tCN4PSSWkAM?width=2400" alt="Logo" width="150" height="150" />
