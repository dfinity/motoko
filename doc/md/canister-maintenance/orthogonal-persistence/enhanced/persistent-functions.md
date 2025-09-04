---
sidebar_position: 1
---

# Persistent Functions, Persistent Objects, and Persistent Classes

Functions, objects, and classes can also be persisted with enhanced orthogonal persistence, i.e. they been used in Motoko's persistent actor variables (aka stable variables in legacy mode).

Note: This is not supported with classical persistence.

With this, the only non-persistent (non-stable) values remain to be lambdas, async handles (aka continuations, futures), local async function references, and errors.

## Example

With this, the following program can then used for orthogonal persistence. For this example, you need a [specific core library version](https://github.com/dfinity/motoko-core/pull/389), which contains extensions of persistent object-oriented data structures.

```
import Map "mo:core/object-oriented/Map";
import Nat "mo:core/Nat";

persistent actor {
  let map = Map.Map<Nat, Text>(Nat.compare);

  public func main() : async () {
    assert map.isEmpty();
    map.add(1, "One");
    assert map.get(1) == ?"One";
    map.remove(1);
    assert map.get(1) == null;
  }
};
```

## Persistent Modifier

A `persistent` modifier can be applied to functions, classes, and function types, as prerequisite to save functions as part of enhanced orthogonal persistent.

`persistent` function:
```
persistent func tick() { ... }
```

`persistent` class:
```
persistent class Clock() {
  public func tick() { ... }; // implicitly `persistent`
};
```

Functions (methods) of a `persistent` class are implicitly declared `persistent`.

`persistent` function types:
```
type Comparison<T> = persistent (T, T) -> Order.Order`
```

A persistent function implicitly has a corresponding persistent function type.

## Persistent Functions and Persistent Scopes

A persistent function is a function that is declared `persistent` and occurs in a persistent scope.

A persistent scope is:
* the main actor,
* an actor class,
* a module imported with a unique identifier from a persistent scope,
* a `persistent` function in a persistent scope,
* a `persistent` class in a persistent persistent scope, 
* a method in a `persistent` class in a persistent scope, or,
* a named object in a persistent scope.

## Restrictions

Persistent functions have to be named, local, and non-async.

Persistent functions capture only local variables of surrounding functions/classes/objects (closure) if they are of a stable type.
This restriction does not apply to immutable variables (`let`) and actor variables.

Generic type parameters of persistent functions and persistent classes are bounded to stable types.

## Persistent Function Types

A persistent function type is also a stable type. Thus, a persistent function has a stable type.

A persistent function type is a sub-type of a non-persistent function type with type-compatible signature, i.e. `persistent X' -> Y <: X -> Y'` for `X' <: X` and `Y' :< Y`.

## Upgrades of Persistent Functions

Persistent functions are upgraded as follows:
* All persistent functions that are reachable from persistent actor variables are considered alive.
* Each alive persistent function must have a matching declaration in the new program version.
* Persistent functions match between program versions if they have an equal fully qualified name.
* For matching functions, the function type of the new version must be compatible to the previous version (super-type).
* For matching functions, the closure type in the new version must be compatible with the previous version, see below.

All other functions, such as e.g. functions that are not declared `persistent`, functions in a unnamed objects, functions in a non-persistent class, functions in a lambda, or functions imported from a module without a unique import identifier, are transient functions.

## Persistent Closures

The closures of persistent functions are represented in a portable format with the following clearly defined binding of captured variables:
* Local variables (`var`) are captured by name.
* Parameters are captured by position (also considering nested functions).

This offers flexibility for changes of persistent closures in new program versions:
* New program version can capture less variables/parameters in persistent functions.
* One can rename parameters even if they are captured by persistent functions.

On a persistent function upgrade, the closure type of the persistent function must remain compatible:
* The new version of the persistent function does not capture more variables/parameters than the previous version.
* The captured variable in the new version is a valid super-type of the previous version.

Specific aspects apply to generic types used in a persistent closure:
* Generic types used for captured variables must match the previous declaration order (e.g. one cannot swap generic types).
* The generic type bounds must remain compatible.
