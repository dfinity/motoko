---
sidebar_position: 10
---

---
sidebar_position: 10
---

# Tuples

A tuple is a fixed-size, ordered collection of values, where each element can have a different type. Tuples provide a way to group multiple values together without defining a structured data type.

A tuple is grouped together in parentheses (`value1`, `value2`, `value3`). The type of a tuple is based on the types of its elements, such as (`Text`, `Nat`, `Bool`). The values inside a tuple are evaluated in order from left to right, and if any value causes an error, the entire tuple fails. Tuples are immutable and therefore cannot be changed after instantiation.

A tuple with zero elements is called the **unit value**, written as `()`. It represents an empty result or a no-op return value.

## Defining a tuple

```motoko
let person = ("Alice", 25);
```

The tuple's type is automatically inferred as `(Text, Nat)`, since `"Alice"` is of type `Text` and `25` is of type `Nat`. However, it is recommended to explicitly define the tuple type to improve clarity and prevent unintended type mismatches.

```motoko
let person : (Text, Nat) = ("Alice", 25);
```

## Accessing elements

Elements are accessed using `.n` where `n` is the index (0-based indexing).

```motoko
let person : (Text, Nat) = ("Alice", 25);

let name = person.0;
let age = person.1;

Debug.print(name # is # debug_show(age) # years old);

```
