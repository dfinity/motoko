---
sidebar_position: 4
---

# Tuples

A tuple is a fixed-size, ordered collection of values, where each element can have a different type. Tuples provide a way to group multiple values together without defining a structured data type.

A tuple is grouped together in parentheses (`value1`, `value2`, `value3`). The type of a tuple is based on the types of its elements, such as ([`Text`](https://internetcomputer.org/docs/motoko/base/Text), [`Nat`](https://internetcomputer.org/docs/motoko/base/Nat), [`Bool`](https://internetcomputer.org/docs/motoko/base/Bool)). The values inside a tuple are evaluated in order from left to right, and if any value causes an error, the entire tuple fails. Tuples are immutable and therefore cannot be changed after instantiation.

A tuple with zero elements is called the **unit value**, written as `()`. It represents an empty result or a no-op return value.

## Defining a tuple

```motoko
let ghost = ("Motoko", 25);
```

The tuple's type is automatically inferred as `(Text, Nat)`, since `"Motoko"` is of type [`Text`](https://internetcomputer.org/docs/motoko/base/Text) and `25` is of type [`Nat`](https://internetcomputer.org/docs/motoko/base/Nat). However, it is recommended to explicitly define the tuple type to improve clarity and prevent unintended type mismatches.

```motoko
let ghost : (Text, Nat) = ("Motoko", 25);
```

## Accessing elements

Elements are accessed using `.n` where `n` is the index (0-based indexing).

```motoko no-repl
// Ghost is a tuple of length 2
let ghost : (firstName: Text, age: Nat) = ("Motoko", 25);
let first = ghost.0; // "Motoko"
let second = ghost.1; // 25
```

## Tuples as function return values

Tuples are useful for returning multiple values from a function without requiring a separate data structure.

```motoko no-repl
func getUserInfo(): (Text, Nat) {
    ("Ghost", 30);
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
// Points is a nested tuple

func calculateGradient(points: ((Float, Float), (Float, Float))): ?Float {
    switch (points) {
        case ((x1, y1), (x2, y2)) {
            if (x1 == x2) {
                null // Gradient is undefined for a vertical line
            } else {
                ?((y2 - y1) / (x2 - x1)) // Wraps the result as an option
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
let users: [(Text, Nat)] = [("Motoko", 25), ("Ghost", 30)];
```

This structure efficiently represents a collection of key-value pairs without requiring a dedicated [record](https://internetcomputer.org/docs/motoko/fundamentals/types/records) type.

## Extracting values using switch

To access the name of the first [record](https://internetcomputer.org/docs/motoko/fundamentals/types/records) in the tuple using best practices, a [`switch`](https://internetcomputer.org/docs/motoko/fundamentals/control-flow/switch) expression should be used.

```motoko no-repl
let users: [(Text, Nat)] = [("Motoko", 25), ("Ghost", 30)];

let firstUserName: Text = switch (users[0]) {
    case (name, _) name; // Motoko
};
```

The array `users` contains tuples, where each tuple represents a user with a [`Text`](https://internetcomputer.org/docs/motoko/base/Text) name and a [`Nat`](https://internetcomputer.org/docs/motoko/base/Nat) age. `users[0]` retrieves the first tuple in the array. The `switch` expression extracts only the name while ignoring the second element using the wildcard pattern (`_`). This ensures clarity and type safety when working with tuples inside collections.

<img src="https://cdn-assets-eu.frontify.com/s3/frontify-enterprise-files-eu/eyJwYXRoIjoiZGZpbml0eVwvYWNjb3VudHNcLzAxXC80MDAwMzA0XC9wcm9qZWN0c1wvNFwvYXNzZXRzXC8zOFwvMTc2XC9jZGYwZTJlOTEyNDFlYzAzZTQ1YTVhZTc4OGQ0ZDk0MS0xNjA1MjIyMzU4LnBuZyJ9:dfinity:9Q2_9PEsbPqdJNAQ08DAwqOenwIo7A8_tCN4PSSWkAM?width=2400" alt="Logo" width="150" height="150" />