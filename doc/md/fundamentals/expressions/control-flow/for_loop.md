# For loop

A `for` loop in Motoko follows this structure:  

```motoko
for (pattern in iterator) {
  // Code to run for each item in the iterator
}
```

1. The iterator expression must have type `{ next : () → ?T }`, meaning it provides a `next` function that returns either a value (`?T`) or `null` when there are no more items.
2. The pattern must match the type `T` of the values produced by the iterator.
3. The loop first evaluates the iterator:
   - If evaluating the iterator causes a trap, the loop stops immediately.  
   - If `next()` returns `null`, the loop ends.  
   - If `next()` returns a value, it is assigned to the pattern, and the loop body executes.  
4. The loop continues calling `next()` until it returns `null`, at which point the loop stops, and the final result is `()`.  

```motoko
let numbers = [1, 2, 3, 4, 5];

for (num in numbers.vals()) {
  Debug.print(debug_show(num));
};
```

`numbers.vals()` produces an iterator over the array values. The loop assigns each value to `num` and runs the block. When there are no more values, `next()` returns `null`, and the loop exits.  
