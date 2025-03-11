---
sidebar_position: 3
---

# Tuples

A tuple in is a finite, fixed-length, heterogeneous sequence. It has a defined number of elements, referred to as its length or arity. Tuples are heterogeneous, allowing each element to have a different type. As a sequence, tuple elements can be accessed by their position, a process known as tuple indexing or projection. Tuples can include named types, enhancing readability. Naming tuple types within a collection clarifies the purpose of each field, making value access more intuitive and reducing potential ambiguity.

## Tuple projection

Elements can be accessed using `.0`, `.1`, etc.

```motoko no-repl
let person : (firstName: Text, age: Nat) = ("Alice", 25); //person is a tuple of length 2
let first = person.0; // "Alice"
let second = person.1; // 25
```

## Tuples as function return values

Tuples are useful for returning multiple values from a function without requiring a separate data structure.

```motoko no-repl
func getUserInfo(): (Text, Nat) {
    ("Bob", 30);
}

getUserInfo();
```

## Nesting tuples

Tuples can be used to represent coordinate pairs, allowing for structured calculations such as finding the gradient of a line. The gradient (or slope) between two points `$(x_1, y_1)$` and `$(x_2, y_2)$` is calculated using the formula:

$$
m = \frac{y_2 - y_1}{x_2 - x_1}
$$

Using nested tuples, this can be implemented in Motoko as follows:

```motoko no-repl
//points is a nested tuple

func calculateGradient(points: ((Float, Float), (Float, Float))): ?Float {
    switch (points) {
        case ((x1, y1), (x2, y2)) {
            if (x1 == x2) { 
                null // Gradient is undefined for a vertical line
            } else {
                ?((y2 - y1) / (x2 - x1)) //wraps the result as an option
            }
        };
    }
};

let coordinates: ((Float, Float), (Float, Float)) = ((2.0, 3.0), (5.0, 7.0));

let gradient: ?Float = calculateGradient(coordinates);

Debug.print(switch (gradient) {
    case (?m) "Gradient: " # debug_show(m);
    case null "Gradient is undefined.";
});
```

## Using tuples in collections

Tuples can be stored in arrays or other data structures. Tuples can be structured with named types, improving readability and type safety. By naming the tuple types in the collection, the intent of each field is clear, reducing ambiguity when accessing values.

```motoko no-repl
let users: [(Text, Nat)] = [("Alice", 25), ("Bob", 30)];
```

This structure efficiently represents a collection of key-value pairs without requiring a dedicated record type.

## Extracting values using switch

To access the name of the first record in the tuple using best practices, a `switch` expression should be used:

```motoko no-repl
let users: [(Text, Nat)] = [("Alice", 25), ("Bob", 30)];

let firstUserName: Text = switch (users[0]) {
    case (name, _) name; // Alice
};
```

The array `users` contains tuples, where each tuple represents a user with a `Text` name and a `Nat` age. `users[0]` retrieves the first tuple in the array. The `switch` expression extracts only the name while ignoring the second element using the wildcard pattern (`_`). This ensures clarity and type safety when working with tuples inside collections.
