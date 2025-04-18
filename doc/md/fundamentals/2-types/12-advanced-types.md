---
sidebar_position: 12
---

# Advanced types

Advanced type features enable more flexible and expressive type definitions, including structural equality, generic types, subtyping, recursive types, and type bounds.

## Structural equality

Structural equality determines whether two values are equal based on their contents. This applies to immutable data structures, such as [records](https://internetcomputer.org/docs/motoko/fundamentals/types/records) and [variants](https://internetcomputer.org/docs/motoko/fundamentals/types/variants), but does not apply to mutable structures for safety reasons.

```motoko no-repl
type Point = { x : Int; y : Int };

let p1 : Point = { x = 1; y = 2 };
let p2 : Point = { x = 1; y = 2 };

let areEqual = p1 == p2;  // true (structural equality)
```

Even though `p1` and `p2` are distinct objects, they are considered equal because they have the same structure and values.

## Generic types

Generic types are used to define type parameters that work with multiple data types, commonly used in [functions](https://internetcomputer.org/docs/motoko/fundamentals/types/functions), [classes](https://internetcomputer.org/docs/motoko/fundamentals/types/objects-classes), and data structures.

```motoko no-repl
// Generic function
func identity<T>(x : T) : T {
  return x;
}

let num = identity<Nat>(42);  // num is Nat
let txt = identity<Text>("Hello");  // txt is Text
```

A generic class can store any type while maintaining type safety:

```motoko no-repl
class Box<T>(value : T) {
  public func get() : T { value };
}

let intBox = Box<Nat>(10);
let textBox = Box<Text>("Hello");
```

## Recursive types

Recursive types allow a type to refer to itself, enabling the creation of nested structures while maintaining type safety. The base library utilizes recursive types to define linked lists.

```motoko no-repl
public type List<T> = ?(T, List<T>);
```

### Manually reversing a linked list

Reversing a linked list involves iterating through the list and prepending each element to a new list. This approach demonstrates list traversal and structural mutation without using the built-in `reverse` method.

``` motoko no-repl
let numbers : List.List<Nat> = ?(1, ?(2, ?(3, null)));

func reverse(l : List.List<Nat>) : List.List<Nat> {
    var current = l;
    var rev = List.nil<Nat>();

    while(not List.isNil(current)){
      switch(current) {
        case(?(h, t)) {
          rev := ?(h, rev);
          current := t;
        };
        case (null) {};
      };
    };
    rev
  };
reverse(numbers); // 3-> 2-> 1
```

## Type bounds

Generic types can use subtype constraints, ensuring that any type used in a generic function meets specific structural or concrete type requirements.

These constraints are enforced at compilation. This guarantees that the necessary properties or operations are available when the function is used, eliminating certain classes of runtime errors.

Although the concept of type bounds is often associated with [inheritance-based polymorphism](https://www.codecademy.com/learn/learn-java/modules/learn-java-inheritance-and-polymorphism/cheatsheet) in other languages, Motoko uses structural typing. This means that the subtype relationship is determined by the structure of the types rather than an explicit inheritance hierarchy. **Motoko does not support inheritance**.

This approach balances the flexibility of generic programming with the safety of compile-time checks, enabling the creation of generic functions that operate on a range of types while still enforcing specific structural or type constraints.

The following examples illustrate this behavior:

```motoko no-repl
func printName<T <: { name : Text }>(x : T) {
  Debug.print(x.name);
};

let ghost = { name = "Motoko"; age = 30 };
printName(ghost);  // Allowed since 'ghost' has a 'name' field.
```

In the example above, `T <: { name : Text }` requires that any type used for `T` must be a subtype of the [record](https://internetcomputer.org/docs/motoko/fundamentals/types/records) `{ name : Text }`—that is, it must have at least a `name` field of type [`Text`](https://internetcomputer.org/docs/motoko/base/Text). Extra fields are permitted, but the `name` field is mandatory.

Type bounds are not limited to records. For example, it is possible to constrain a generic type to be a subtype of a basic type.

```motoko no-repl
func addIfInt<T <: Int>(x : T, y : T) : Int {
  return x + y;
};

let result = addIfInt(5, -10);  // Allowed because both are of type Int.
```

Here, `T <: Int` constrains `T` to be a subtype of [`Int`](https://internetcomputer.org/docs/motoko/base/Int). Since [`Int`](https://internetcomputer.org/docs/motoko/base/Int) is a concrete type, this effectively restricts `T` to [`Int`](https://internetcomputer.org/docs/motoko/base/Int) (or to types that are structurally equivalent to [`Int`](https://internetcomputer.org/docs/motoko/base/Int)), ensuring that arithmetic operations are valid.

The notation `T <: Type` mandates that any type provided for `T` must be a subtype of the specified `Type`. For records, this implies having at least the required fields; for basic types like [`Int`](https://internetcomputer.org/docs/motoko/base/Int) or [`Float`](https://internetcomputer.org/docs/motoko/base/Float), it restricts `T` to that type.

<img src="https://cdn-assets-eu.frontify.com/s3/frontify-enterprise-files-eu/eyJwYXRoIjoiZGZpbml0eVwvYWNjb3VudHNcLzAxXC80MDAwMzA0XC9wcm9qZWN0c1wvNFwvYXNzZXRzXC8zOFwvMTc2XC9jZGYwZTJlOTEyNDFlYzAzZTQ1YTVhZTc4OGQ0ZDk0MS0xNjA1MjIyMzU4LnBuZyJ9:dfinity:9Q2_9PEsbPqdJNAQ08DAwqOenwIo7A8_tCN4PSSWkAM?width=2400" alt="Logo" width="150" height="150" />