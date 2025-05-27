---
sidebar_position: 14
---

# Subtyping

Subtyping is a fundamental concept in type systems that allows values of one type to be used wherever values of another type are expected, provided
the first type is a subtype of the second.

In Motoko, the simplest subtyping relation is between `Nat` and `Int`. `Nat` is a subtype of `Int`, written `Nat <: Int`.
If you have a value of type `Nat`, you can supply it to a function taking the more general supertype `Int`.
Motoko applies subtyping wherever it needs to, to prove that your program is type correct.

Subtyping is similar to the notion of subset you may have encountered in school.
With sets, one set `A` is a subset of another `B`, written `A ⊆ B`, if every element of the set `A` is contained in the set `B` (which may have additional elements).

Every set `A` is a subset of itself (that is `A ⊆ A`). This property of the subset relation is called _reflexivity_.

If `A` is a subset of `B` (`A ⊆ B`) and `B` is a subset of `C` (`B ⊆ C`) then `A` is a subset of `C` too (`A ⊆ C`). This property of the subset relation is called _transitivity_.

Not all sets are related by subset. Take, for example the set of natural numbers  `N = {0, 1, 2, …}` and the set of integers `I = {… -1, -2, 0, 1, 2, …}`
only one is a subset of the other: `N ⊆ I` but `I ⊈ N` (`I` is not a subset of `N`).


With types, one type `T` is a subtype of another type `U`, written `T <: U`, if every value of the type `T` is also a value of the type `U`.
Every type `T` is a trivial subtype of itself, `T <: T`. The subtype relation, like subset, is reflexive.
If `T` is subtype of `U` (`T <: U`) and `U` is a subtype of `V` (`U <: V`) then `T` is a subtype of `V` (`T <: V`) too. The subtype relation, like subset, is transitive.
We'll use the notation `T </: U` to mean _not_ `T <: U`.

In Motoko, subtyping is used in various contexts to provide flexible typing while maintaining type safety.

## Variance

Motoko provides type-level syntax for constructing types from other types.

For example,
* The option type `?T` constructs an option type from `T`.
* The array type `[T]` constructs an array type from `T`.
* The mutable array type `[var T]` constructs a mutable array type from `T`.
* The object type `{… f : T; …}` constructs an object type from a field type `T`.
* The variant type `{… #f : T; …}` constructs a variant type from a field type `T`.
* The function type `T1 -> T2` constructs a function type from the argument type `T1` and result type `T2`.

If replacing an argument to a type constructor by a supertype produces a supertype of the constructed type, then the constructor is
_covariant_ in that argument.

If replacing an argument to a type constructor by a supertype produces a _subtype_ of the constructed type, then the constructor is
_contravariant_ in that argument.

If replacing an argument to a type constructor by a supertype produces a type _unrelated_ to the constructed type, then the constructor is
invariant in that argument.

Note that a type constructor that takes several arguments can have different variance for each argument.

| Variance type | Description | Relationship | Motoko example |
|---------------|-------------|--------------|----------------|
| Covariant | Preserves the direction of subtyping. | If `T1 <: T2`, then `F[T1] <: F[T2]` | If `Nat <: Int`, then `?Nat <: ?Int`. You can use an optional natural number where an optional integer is expected. |
| Contravariant | Reverses the direction of subtyping. | If `T1 <: T2`, then `F[T2] <: F[T1]` | If `Nat <: Int`, then `(Int -> ()) <: (Nat -> ())`. You can use a function that takes an integer as a function that takes a natural number |
| Invariant | No subtyping relationship is preserved. | `F[T1]` <: `F[T2]` only when `T1 <: T2` and `T2 <: T1`  | Since `Nat  <: Int` but not `Int <: Nat`, `[var Nat]` and `[var Int]` are not related by subtyping. You cannot use a mutable array of natural numbers where a mutable array of integers is expected |

In this table:

- `T1` and `T2` are types.

- `F[T]` represents a type constructor applied to type `T` (e.g. `?T` for option type).

- `<:` means "is a subtype of."

## Numeric (`Nat <: Int`)

[`Nat`](https://internetcomputer.org/docs/motoko/base/Nat) is a subtype of [`Int`](https://internetcomputer.org/docs/motoko/base/Int) (`Nat <: Int`), meaning a [`Nat`](https://internetcomputer.org/docs/motoko/base/Nat) value can be used where an `Int` is expected. This works because every [`Nat`](https://internetcomputer.org/docs/motoko/base/Nat) is an `Int`, but not every `Int` is a [`Nat`](https://internetcomputer.org/docs/motoko/base/Nat) (as negative numbers exist in `Int`).


```motoko
let n : Nat = 5;
let i : Int = n;        // Allowed, since `Int <: Nat`
```

```motoko
let i : Int = -5;
let n : Nat = i;        // Not allowed, since `Int </: Nat`
```

## Options

If `T1 <: T2`, then `?T1 <: ?T2` (option subtyping is covariant). This means an [optional value](https://internetcomputer.org/docs/motoko/fundamentals/types/options-results) of a subtype can be used as an optional value of a supertype.

```motoko norepl
let a : ?Nat = ?5;
let b : ?Int = a;     // Allowed, since `Nat <: Int` implies `?Nat <: ?Int`
```

In Motoko, the literal `null` has type `Null` which subtypes any option type, that is `Null <: ?T` (for any `T`).
This means you can use `null` as the absent value of any optional type.

```motoko norepl
let n : Null = null;
let oi : ?Int = n;      // Allowed, since `Null <: ?Int`
let ot : ?Text = n;     // Allowed, since `Null <: ?Text`
```

## Records and objects

[Records](https://internetcomputer.org/docs/motoko/fundamentals/types/records) and, more generally,  [objects](https://internetcomputer.org/docs/motoko/fundamentals/types/objects-classes)  support subtyping, meaning an object is a subtype of a similar object with some fields removed,
provided the types of the fields in common are related by subtyping.

```motoko norepl
type A = { name : Text };
type B = { name : Text; age : Nat };
type C = { name : Text; age : Int };

let a : A = { name = "Motoko" };
let b : B = { name = "Ghost"; age = 25 };
let c : C = { name = "Other"; age = -1 };


let a2 : A = b; // Allowed, since B has all fields of A
let b2 : B = a; // Not allowed because A lacks age.
let c2 : C = b; // Allowed since `age : Nat` implies `age : Int` (`Nat <: Int`).
let b3 : B = c; // Not allowed since `age : Int` does not implies `age : Nat` (`Int </: Nat`).
```

## Variants

[Variants](https://internetcomputer.org/docs/motoko/fundamentals/types/variants) also support subtyping.

A variant is a subtype of a similar variant with some cases added, provided the types of the cases in common are related by subtyping.

For example, we can define a variant `WeekDay` that is a subtype of `Day` (adding weekends):

```motoko norepl name=Days
type WeekDay = { #mon; #tue; #wed; #thu; #fri };
type Day = { #mon; #tue; #wed; #thu; #fri; #sat; #sun};
```

Now every weekday is a day:

```motoko norepl include=Days
let wd : WeekDay = #mon;
let d : Day = wd;  // Allowed, since `WeekDay <: Day`
```

But not the other way round:

```motoko norepl include=Days
let d : Day = #mon;
let wd : WeekDay = d;  // Not allowed, since `Day </: WeekDay` (`d` could also be `#sat`)
```

Variants with arguments can also be related by subtyping:

```motoko norepl
type Ok<T> = {#ok : T};
type Err<E> = {#err : E};
type Result<T, E> = {#ok : T; #err : E};

let err : Err<Text> = #err "ohoh";
let ok : Ok<Nat> = #ok 0;
let rerr : Result<Int, Text> = err;  // Allowed, since `{#err : Text} <: {#ok : Int; #err : Text}`,
                                     // that is, `Err<Text> <: Result<Int, Text>`
let rok : Result<Int, Text> = ok; // Allowed, since `{#ok : Nat} <: {#ok : Int; #err : Text}`,
                                  // that is, `Ok<Nat> <: Result<Int, Text>`
```


## Immutable arrays

[Immutable arrays (`[T]`)](https://internetcomputer.org/docs/motoko/fundamentals/types/immutable-arrays) support covariant subtyping, meaning if `T1 <: T2`, then `[T1] <: [T2]`.
If `T1 <: T2`, then an immutable array of type `[T1]` can be used as an immutable array of type `[T2]`.

```motoko norepl
let nums : [Nat] = [1, 2, 3];
let numsAsInts : [Int] = nums;  // Allowed, since `Nat <: Int` we also have `[Nat] <: [Int]`.
```

## Mutable arrays

[Mutable arrays (`[var T]`)](https://internetcomputer.org/docs/motoko/fundamentals/types/mutable-arrays) do not support interesting subtyping.
Indeed, `[var T1] <: [var T2]` only when `T1` and `T2` are equivalent.
Allowing `[var T1] <: var T2]` whenever `T1 <: T2` is not safe.

The mutable array constructor `[var T]` is invariant in `T`.

```motoko norepl
let nums :  [var Nat] = [var 1, 2, 3];
let numsAsInts :  [var Int] = nums;  // Not allowed because `[var Nat] </: [var Int]`.
```

If this were allowed, then the legal assignment

``` motoko norepl
numsAsInts[0] := -1;
```

would have the side-effect of also setting `nums[0]` to `-1`, yet `num` is supposed to only contain (non-negative) `Nat`s.


## Functions

[Function](https://internetcomputer.org/docs/motoko/fundamentals/types/functions) also support subtyping.
A function type `T1 -> T2` is a subtype of another function type `U1 -> U2` provided

1. `U1 <: T1` : the argument types are related in the opposite direction.
2. `T2 <: U2` :  the return types are related in the same direction.

This means that function subtyping is contravariant in the arguments of the function and covariant in the results.

This might seems confusing at first, but the intuition is quite simple.
A function can be used at a supertype provided that type requires a more specific argument and produces a more general result.
This ensures that the orginal function, when used at the supertype,
will only receive arguments it can consume (since `U1 <: T1`) and produce results than are expected (since `T2 <: U2`).

As a simple example, consider the `magnitude` function that returns the absolute value, or magnitude, of an integer.

```motoko norepl name=magnitude
import Int "mo:base/Int"
func magnitude(i : Int) : Nat { Int.abs(i) };
```

Its most precise type is `Int -> Nat`, but, due to subtyping, it also has types `Nat -> Nat` (making the argument more specific),
`Int -> Int`  (making the result more general) and `Int -> Int` (doing both).

``` motoko norepl include=magnitude
let i2n : Int -> Nat = magnitude;
let n2n : Nat -> Nat = magnitude;
let i2i : Int-> Int = magnitude;
let n2i : Nat-> Int = magnitude;
```

You may be wondering what happens if the one or both of the function types is generic and has type parameters.
Motoko uses the simple rule that two function types with type parameters are subtypes only when both types have the same parameters
(up to renaming), and the arguments and results are related according to 1. and 2. above.

## Modules and actors

Modules and actors support subtyping just like objects, meaning a module(actor) is a subtype of a similar module(actor) with some fields removed,
provided the types of the fields in common are related by subtyping.

For actors, which can only contain shared functions as fields, this means that you can remove some functions or replace the types of common
functions with supertypes.

## Recursive and generic types

[Recursive and generic types](https://internetcomputer.org/docs/motoko/fundamentals/types/advanced-types) can be subtypes of each other when their definitions allow it.

Consider these two recursive point types, the second one adding a `color` field:

```motoko norepl name=Point
type Point = {
  x : Int;
  move: Int -> Point
};

type ColorPoint = {
  color : {#red; #blue};
  x : Int;
  move: Int -> ColorPoint
};
```

`ColorPoint` is a subtype of `Point`:

``` motoko include=Point
let cp : ColorPoint = { color = #red; x = 0; move = func (dx : Int) { { cp with x = cp.x + dx } }};
let c : Point = cp;
```

This also works for recursive variants and even generic types:

For example, a tree with exclusively `#red` nodes is subtype of a tree with both `#red` and `#black` nodes:

```motoko norepl
type RedTree<T> = {
  #leaf;
  #red : (RedTree<T>, RedTree<T>)
};

type RedBlackTree<T> = {
  #leaf;
  #red : (RedBlackTree<T>, RedBlackTree<T>);
  #black : (RedBlackTree<T>, RedBlackTree<T>)
};

let rt : RedTree<Nat> = #leaf;
let rbt : RedBlackTree<Nat> = rt;  // Allowed, since `RedTree<Nat> <: RedBlackTree<Nat>`
```

<img src="https://cdn-assets-eu.frontify.com/s3/frontify-enterprise-files-eu/eyJwYXRoIjoiZGZpbml0eVwvYWNjb3VudHNcLzAxXC80MDAwMzA0XC9wcm9qZWN0c1wvNFwvYXNzZXRzXC8zOFwvMTc2XC9jZGYwZTJlOTEyNDFlYzAzZTQ1YTVhZTc4OGQ0ZDk0MS0xNjA1MjIyMzU4LnBuZyJ9:dfinity:9Q2_9PEsbPqdJNAQ08DAwqOenwIo7A8_tCN4PSSWkAM?width=2400" alt="Logo" width="150" height="150" />
