---
sidebar_position: 21
---

# Advanced types

Advanced type features enable more flexible and expressive type definitions, including structural equality, generic types, subtyping, recursive types, and type bounds.

## Structural equality

Structural equality determines whether two values are equal based on their contents. This applies to immutable data structures, such as records and variants, but does not apply to mutable structures for safety reasons.

```motoko no-repl
type Point = { x : Int; y : Int };

let p1 : Point = { x = 1; y = 2 };
let p2 : Point = { x = 1; y = 2 };

let areEqual = p1 == p2;  // true (structural equality)
```

Even though `p1` and `p2` are distinct objects, they are considered equal because they have the same structure and values.

## Generic types

Generic types are used to define type parameters that work with multiple data types, commonly used in functions, classes, and data structures.

A simple example of a generic function:

```motoko no-repl
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

## Subtyping

Subtyping allows a type to be used where a more general type is expected.  

Records are covariant, meaning that a record with extra fields can be used where a smaller record is expected:  

```motoko no-repl
type Person = { name : Text };
type Employee = { name : Text; id : Nat };

let e : Employee = { name = "Alice"; id = 123 };
let p : Person = e;  // Allowed (Employee is a subtype of Person)
```

This allows functions to accept more specific types without breaking compatibility.

## Recursive types

Recursive types allow a type to refer to itself, enabling the creation of nested structures.

The base library utilizes recursive types to define linked lists:

```motoko no-repl
public type List<T> = ?(T, List<T>);
```

This structure allows for dynamic growth while maintaining type safety.

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

Generic types can be constrained by using subtype constraints, ensuring that any type used in a generic function meets specific structural or concrete type requirements that are checked during compilation.

The following examples illustrate this behavior:

```motoko no-repl
func printName<T <: { name : Text }>(x : T) {
  Debug.print(x.name);
};

let person = { name = "Alice"; age = 30 };
printName(person);  // Allowed since 'person' has a 'name' field.
```

In the example above, `T <: { name : Text }` requires that any type used for `T` must be a subtype of the record `{ name : Text }`—that is, it must have at least a `name` field of type `Text`. Extra fields are permitted, but the `name` field is mandatory.

Type bounds are not limited to records. For example, it is possible to constrain a generic type to be a subtype of a basic type:

```motoko no-repl
func addIfInt<T <: Int>(x : T, y : T) : Int {
  return x + y;
};

let result = addIfInt(5, -10);  // Allowed because both are of type Int.
```

Here, `T <: Int` constrains `T` to be a subtype of `Int`. Since `Int` is a concrete type, this effectively restricts `T` to `Int` (or to types that are structurally equivalent to `Int`), ensuring that arithmetic operations are valid.

- The notation `T <: Type` mandates that any type provided for `T` must be a subtype of the specified `Type`. For records, this implies having at least the required fields; for basic types like `Int` or `Float`, it restricts `T` to that type.

- These constraints are enforced at compile time, meaning the compiler checks that the provided type satisfies the constraint. This guarantees that the necessary properties or operations are available when the function is used, eliminating certain classes of runtime errors.

- Although the concept of type bounds is often associated with [inheritance-based polymorphism](https://www.codecademy.com/learn/learn-java/modules/learn-java-inheritance-and-polymorphism/cheatsheet) in other languages, Motoko uses structural typing. This means that the subtype relationship is determined by the structure of the types rather than an explicit inheritance hierarchy. **Motoko does not support inheritance**.

This approach balances the flexibility of generic programming with the safety of compile-time checks, enabling the creation of generic functions that operate on a range of types while still enforcing specific structural or type constraints.
