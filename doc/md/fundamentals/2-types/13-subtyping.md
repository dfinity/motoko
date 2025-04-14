---
sidebar_position: 13
---

# Subtyping

Subtyping is a fundamental concept in type systems that allows a value of one type to be used where another (more general) type is expected. In Motoko, subtyping is used in various contexts to enable flexibility while ensuring type safety.

| Variance type | Description | Relationship | Motoko example |
|---------------|-------------|--------------|----------------|
| Covariant | Preserves the direction of subtyping. | If `T1 <: T2`, then `F[T1] <: F[T2]` | If `Nat <: Int`, then `?Nat <: ?Int`. You can use an optional natural number where an optional integer is expected. |
| Invariant | No subtyping relationship is preserved. | If `T1 <: T2`, then `F[T1]` and `F[T2]` have no subtyping relation. | If `Nat <: Int`, then `[var Nat]` and `[var Int]` are unrelated. You cannot use a mutable array of natural numbers where a mutable array of integers is expected. |
| Contravariant | Reverses the direction of subtyping. | If `T1 <: T2`, then `F[T2] <: F[T1]` | If `Nat <: Int`, then `(Int -> ()) <: (Nat -> ())`. A function that can process any integer can be used where a natural number-specific function is expected |

- `T1` and `T2` are types.

- `F[T]` represents a type constructor applied to type `T` (e.g. `?T` for option type).

- `<:` means "is a subtype of."

## Numeric (`Nat <: Int`)

[`Nat`](https://internetcomputer.org/docs/motoko/base/Nat) is a subtype of [`Int`](https://internetcomputer.org/docs/motoko/base/Int) (`Nat <: Int`), meaning a [`Nat`](https://internetcomputer.org/docs/motoko/base/Nat) value can be used where an `Int` is expected. This works because every [`Nat`](https://internetcomputer.org/docs/motoko/base/Nat) is an `Int`, but not every `Int` is a [`Nat`](https://internetcomputer.org/docs/motoko/base/Nat) (as negative numbers exist in `Int`).

```motoko no-repl
let x : Int = 10 : Nat;  // Allowed, since Nat is a subtype of Int
let y : Nat = -5;        // Not allowed, -5 is not a Nat
```

## Options

If `T1 <: T2`, then `?T1 <: ?T2` (option subtyping is covariant). This means an [optional value](https://internetcomputer.org/docs/motoko/fundamentals/types/options-results) of a subtype can be used as an optional value of a supertype.

```motoko no-repl
let a : ?Nat = ?5;    // Allowed, since `Nat <: Int`, `?Nat <: ?Int`
let b : ?Int = a;     // Works because `?Nat <: ?Int`
```

## Records

[Records](https://internetcomputer.org/docs/motoko/fundamentals/types/records) support structural subtyping, meaning a record with more fields can be used where a record with fewer fields is expected. A record type is a subtype of another if it has at least the same fields with matching types.

```motoko no-repl
type A = { name : Text };
type B = { name : Text; age : Nat };

let a : A = { name = "Motoko" };
let b : B = { name = "Ghost"; age = 25 };

let a2 : A = b;  // Allowed, since B has all fields of A
let b2 : B = a; // Not allowed because A lacks age.
```

## Variants

[Variants](https://internetcomputer.org/docs/motoko/fundamentals/types/variants) also support structural subtyping, but only if the variant contains, at most, the same cases as expected. A variant with fewer cases can be used where a variant with more cases is expected.

```motoko no-repl
type A = { #Dog };
type B = { #Dog; #Cat };

let a : A = #Dog;
let b : B = a;  // Allowed, since B expects #Dog and A has #Dog
let a2 : A = b; // Not allowed because b might be #Cat, which A does not support.
```

## Immutable arrays

[Immutable arrays (`[T]`)](https://internetcomputer.org/docs/motoko/fundamentals/types/immutable-arrays) support covariant subtyping, meaning if `T1 <: T2`, then `[T1] <: [T2]`. This means an immutable array of a subtype can be used as an immutable array of a supertype.

```motoko no-repl
let nums : [Nat] = [1, 2, 3];
let numsAsInts : [Int] = nums;  // Allowed, since `Nat <: Int`
```

## Mutable arrays

[Mutable arrays (`var [T]`)](https://internetcomputer.org/docs/motoko/fundamentals/types/mutable-arrays) do not support subtyping due to type safety risks. Allowing `var [T1] <: var [T2]` could lead to runtime type mismatches.

```motoko no-repl
let nums :  [var Nat] = [var 1, 2, 3];
let numsAsInts :  [var Int] = nums;  // Not allowed because mutable arrays are invariant
```

## Functions

[Function](https://internetcomputer.org/docs/motoko/fundamentals/types/functions) subtyping follows two rules:

1. Parameter types must be contravariant: `T2 <: T1` means `T1 -> R <: T2 -> R`
2. Return types must be covariant: `R1 <: R2` means `T -> R1 <: T -> R2`

```motoko no-repl
func parent(x : Int) : Nat { 10 };
func child(x : Nat) : Int { 20 };

let f : (Nat) -> Int = parent;  // Allowed (contravariant param, covariant return)
let g : (Int) -> Nat = child;   // Not allowed because the return type must be covariant
```

## Objects, modules & actors

[Objects](https://internetcomputer.org/docs/motoko/fundamentals/types/objects-classes) are structurally subtyped. An object with additional methods is a subtype of one with fewer. Modules and actors follow the same principle.

```motoko no-repl
type Parent = { getName : () -> Text };
type Child = { getName : () -> Text; getAge : () -> Nat };

let obj : Child = { getName = func() { "Motoko" }; getAge = func() { 25 } };
let p : Parent = obj;  // Allowed, since `Child` has all methods of `Parent`
```

## Recursive types

[Recursive types](https://internetcomputer.org/docs/motoko/fundamentals/types/advanced-types) can be subtypes of each other **if their structure allows it**. This is useful for **self-referential structures** like linked lists.

```motoko no-repl
type ListA = ?{ head : Nat; tail : ListA };
type ListB = ?{ head : Nat; tail : ListB };

let a : ListA = ?{ head = 1; tail = null };
let b : ListB = a;  // Allowed, since both have the same recursive structure
```

<img src="https://cdn-assets-eu.frontify.com/s3/frontify-enterprise-files-eu/eyJwYXRoIjoiZGZpbml0eVwvYWNjb3VudHNcLzAxXC80MDAwMzA0XC9wcm9qZWN0c1wvNFwvYXNzZXRzXC8zOFwvMTc2XC9jZGYwZTJlOTEyNDFlYzAzZTQ1YTVhZTc4OGQ0ZDk0MS0xNjA1MjIyMzU4LnBuZyJ9:dfinity:9Q2_9PEsbPqdJNAQ08DAwqOenwIo7A8_tCN4PSSWkAM?width=2400" alt="Logo" width="150" height="150" />