---
sidebar_position: 14
---

# Subtyping

Subtyping is a fundamental concept in type systems that allows values of one type to be used wherever values of another type are expected, provided
the first type is a subtype of the second.

In Motoko, the simplest subtyping relation is between `Nat` and `Int`. `Nat` is a subtype of `Int`, written `Nat <: Int`.
If you have a value of type `Nat`, you can supply it to a function taking the more general supertype `Int`.
Motoko applies subtyping when necessary to prove that a program is type correct.


:::info

Subtyping is similar to the notion of subset you may have encountered in school.
With sets, one set `A` is a subset of another `B`, written `A ⊆ B`, if every element of the set `A` is contained in the set `B` (which may have additional elements).

Every set `A` is a subset of itself (that is `A ⊆ A`). This property of the subset relation is called _reflexivity_.

If `A` is a subset of `B` (`A ⊆ B`) and `B` is a subset of `C` (`B ⊆ C`) then `A` is a subset of `C` too (`A ⊆ C`). This property of the subset relation is called _transitivity_.

Not all sets are related by subset. Take, for example, the set of natural numbers  `N = {0, 1, 2, …}` and the set of integers `I = {… -1, -2, 0, 1, 2, …}`
only one is a subset of the other: `N ⊆ I` but `I ⊈ N` (`I` is _not_ a subset of `N`).

:::

With types, one type `T` is a subtype of another type `U`, written `T <: U`, if every value of the type `T` is also a value of the type `U`.

Every type `T` is a trivial subtype of itself, `T <: T`. The subtype relation, like subset, is reflexive.

If `T` is subtype of `U` (`T <: U`) and `U` is a subtype of `V` (`U <: V`) then `T` is a subtype of `V` (`T <: V`) too. The subtype relation, like subset, is transitive.

We'll use the notation `T </: U` to mean _not_ `T <: U` (`T` is not a subtype of `U`).

There is no run-time cost to subtyping: using a value at a supertype does not perform any conversion on the value,
but just views the same value through the lens of a different type.

In Motoko, subtyping is used to provide more flexible typing without compromising type safety.

## Variance

Motoko provides type-level syntax for constructing types from other types.

For example,
* The option type `?T` constructs an option type from `T`.
* The array type `[T]` constructs an array type from `T`.
* The mutable array type `[var T]` constructs a mutable array type from `T`.
* The object type `{… f : T; …}` constructs an object type from a field type `T`.
* The variant type `{… #f : T; …}` constructs a variant type from a field type `T`.
* The function type `T -> U` constructs a function type from the argument type `T` and result type `U`.

If replacing an argument to a type constructor by a supertype produces a supertype of the constructed type, then the constructor is
_covariant_ in that argument.

If replacing an argument to a type constructor by a supertype produces a _subtype_ of the constructed type, then the constructor is
_contravariant_ in that argument.

If replacing an argument to a type constructor by a supertype produces a type _unrelated_ to the constructed type, then the constructor is
invariant in that argument.

Note that a type constructor that takes several arguments can have different variance for each argument.

| Variance type | Description | Relationship | Motoko example |
|---------------|-------------|--------------|----------------|
| Covariant | Preserves subtyping | `F[T] <: F[U]` | `?Nat <: ?Int`. An optional  natural is also an optional integer. |
| Contravariant | Inverts subtyping | `F[U] <: F[T]` | `(Int -> ()) <: (Nat -> ())`. A function on integers is also a function on naturals |
| Invariant | No subtyping | `F[T] </: F[U]` | `[var Nat] </: [var Int]`. A mutable array of naturals is not an array of integers |

In this table:

- `T` and `U` are distinct types such that `T <: U` but not `U <: T`.

- `F[T]` represents a type constructor applied to type `T` (e.g. `?T` for option type).

## Numbers `Nat` and `Int`

[`Nat`](https://internetcomputer.org/docs/motoko/base/Nat) is a subtype of [`Int`](https://internetcomputer.org/docs/motoko/base/Int) (`Nat <: Int`), meaning a [`Nat`](https://internetcomputer.org/docs/motoko/base/Nat) value can be used where an `Int` is expected. This works because every [`Nat`](https://internetcomputer.org/docs/motoko/base/Nat) is an `Int`, but not every `Int` is a [`Nat`](https://internetcomputer.org/docs/motoko/base/Nat) (as negative numbers exist in `Int`).


```motoko
let n : Nat = 5;
let i : Int = n;        // Allowed, since `Int <: Nat`
```

```motoko
let i : Int = -5;
let n : Nat = i;        // Not allowed, since `Int </: Nat`
```

## None: the least type

`None` is an empty type in Motoko. It contains no values at all so, and, by definition, `None` is a subtype of every other type: `None <: T`, for any `T`.

None is the least (as in smallest) type.

You might think the `None` type is useless, but it is used to type expressions that never produce a value, like the infinite loop `loop {}`.

``` motoko no-repl
func impossible() : None { loop {} };

impossible() # impossible();  // Allowed, since `None <: Text`
impossible() + impossible();  // Allowed, since `None <: Nat`

if (false == true) impossible(); // Allowed, since `None <: ()`
```

Call the function `impossible()` will force your program to enter an infinite loop, returning no value at all.
Because `None` is the least type, it can be used wherever any other type is expected.

`None` is similar to the empty set: the empty set has no elements and is a subset of every other set `{} ⊆ A`.

## Any: the greatest type

Motoko's `Any` type contains all values. By definition, every type is a subset of `Any`, `T <: Any`.
`Any` is the greatest (as in largest) type in Motoko.

A function that accepts an `Any` argument can be applied to "any" type of argument.

``` motoko no-repl
func discard(a : Any) {};

discard(0); // Allowed, since `Nat <: Any`
discard(true); // Allowed, since `Bool <: Any`
discard("abc"); // Allowed, since `Text <: Any`
```

`Any` is similar to the universal set `U`, the set that contains all possible elements.
Every set `A` is a subset of the universal set, `A ⊆ U`.

## Options

If `T <: U`, then `?T <: ?U` (option subtyping is covariant). This means an [optional value](https://internetcomputer.org/docs/motoko/fundamentals/types/options-results) of a subtype can be used as an optional value of a supertype.

```motoko no-repl
let a : ?Nat = ?5;
let b : ?Int = a;     // Allowed, since `Nat <: Int` implies `?Nat <: ?Int`
```

In Motoko, the literal `null` has type `Null` which subtypes any option type, that is `Null <: ?T` (for any `T`).
This means you can use `null` as the absent value of any optional type.

```motoko no-repl
let n : Null = null;
let oi : ?Int = n;      // Allowed, since `Null <: ?Int`
let ot : ?Text = n;     // Allowed, since `Null <: ?Text`
```

## Records and objects

[Records](https://internetcomputer.org/docs/motoko/fundamentals/types/records) and, more generally,  [objects](https://internetcomputer.org/docs/motoko/fundamentals/types/objects-classes)
support subtyping, both in the required fields and the types of those fields.

An object type `T` is a subtype of another object type `U`, if `T` requires all the fields required by `U`,
with field types that are subtypes of those in `U`.
Note that `T` may provide more fields than `U`.

```motoko no-repl
type A = { name : Text; age : Nat };
type B = { name : Text; age : Int };
type C = { name : Text };

let a : A = { name = "Ghost"; age = 25 };
let b : B = { name = "Other"; age = -1 };
let c : C = { name = "Motoko" };

let b1 : B = a; // Allowed since `age : Nat` implies `age : Int` (`Nat <: Int`).
let c1 : C = b; // Allowed, since `B` has all fields of `C`
let a1 : A = b; // Not allowed, since `age : Int` does not imply `age : Nat` (`Int </: Nat`).
let b2 : B = c; // Not allowed, because `C` is missing field `age`.
```

If the field of an object is mutable (e.g. `var age : Nat`), then any field in the supertype must also be mutable, with equivalent content.

For example, consider these object types with mutable `age` fields:

```motoko no-repl
type A = { name : Text; var age : Nat };
type B = { name : Text; var age : Int };
```

Now neither `A <: B` nor `B <: A` since the contents of the mutable `age` field, `Nat` and `Int`, are not equivalent.
However, we still have `A <: C` and `B <: C` (dropping the `age` field entirely).


## Variants

[Variants](https://internetcomputer.org/docs/motoko/fundamentals/types/variants) also support subtyping, both in the allowed fields and the types of those fields.

An variant type `T` is a subtype of another variant type `U`, if `T` allows only some of the fields allowed by `U`,
with field types that are subtypes of those in `U`.
Note that `T` may allow fewer fields than `U`.

For example, we can define a variant `WeekDay` that is a subtype of `Day` (adding weekends):

```motoko no-repl name=Days
type WeekDay = { #mon; #tue; #wed; #thu; #fri };
type Day = { #mon; #tue; #wed; #thu; #fri; #sat; #sun};
```

Now every weekday is a day:

```motoko no-repl _include=Days
let wd : WeekDay = #mon;
let d : Day = wd;  // Allowed, since `WeekDay <: Day`
```

But not the other way round:

```motoko no-repl _include=Days
let d : Day = #mon;
let wd : WeekDay = d;  // Not allowed, since `Day </: WeekDay` (`d` could also be `#sat`)
```

Variants with arguments can also be related by subtyping:

```motoko no-repl
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

[Immutable arrays (`[T]`)](https://internetcomputer.org/docs/motoko/fundamentals/types/immutable-arrays) support covariant subtyping, meaning if `T <: U`, then `[T] <: [U]`.

If `T <: U`, then an (immutable) array of type `[T]` can be used as an array of type `[U]`.

```motoko no-repl
let nats : [Nat] = [1, 2, 3];
let ints : [Int] = nats;  // Allowed, since `Nat <: Int` we also have `[Nat] <: [Int]`.
```

## Mutable arrays

[Mutable arrays]](https://internetcomputer.org/docs/motoko/fundamentals/types/mutable-arrays) of the form  `[var T]` do not support interesting subtyping.
The mutable array constructor `[var T]` is invariant in `T`.
This means that `[var T] <: [var U]` only when `T` and `U` are equivalent.

Allowing `[var T] <: [var U]` whenever `T <: U`, without requiring `U <: T` would not be safe.

```motoko no-repl
let nats : [var Nat] = [var 1, 2, 3];
let ints : [var Int] = nats;  // Not allowed, because `[var Nat] </: [var Int]`.
```

If subtyping between mutable arrays were allowed, then the legal assignment

``` motoko no-repl
ints[0] := -1;
```

would have the side-effect of also setting `nats[0]` to `-1`, but `nats` must only contain `Nat`s!


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

```motoko no-repl name=magnitude
import Int "mo:base/Int"
func magnitude(i : Int) : Nat { Int.abs(i) };
```

Its most precise type is `Int -> Nat`, but, due to subtyping, it also has types `Nat -> Nat` (making the argument more specific),
`Int -> Int`  (making the result more general) and `Int -> Int` (doing both).

``` motoko no-repl _include=magnitude
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

```motoko no-repl name=Point
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

``` motoko no-repl _include=Point
let cp : ColorPoint = {
  color = #red;
  x = 0;
  move = func (dx : Int) { { cp with x = cp.x + dx } }
};
let c : Point = cp;
```

This also works for recursive variants and even generic types:

For example, a tree with exclusively `#red` nodes is subtype of a tree with both `#red` and `#black` nodes:

```motoko no-repl
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
