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
* However, generic types do not need to be reified, see the reasoning in [generics in stabe closures](GenericsInPersistentClosure.md).

## Runtime System Design

Function references are encoded by a function id in the following representation:
* Persistent function id, encoded as non-negative number:
  A persistent function reference that stays invariant across upgrades.
* Transient function id, encoded as negative number:
  A transient function reference that is invalidated on upgrade.

Each program version defines a set of named local functions that can be used as persistent function references. Each such function obtains a persistent function id on program initialization and upgrade. If the persistent function was already declared in the previous version, its function id is reused on upgrade. Thereby, the compatibility of the function type and closure type are checked. Otherwise, if it is a new persistent function, it obtains a new persistent function id, or a recycled id.

The runtime system supports persistent functions by two mechanisms:

1. **Persistent virtual table** for persistent function calls:
    
   The persistent virtual table maps persistent function ids to Wasm table indices, for supporting dynamic calls of persistent functions. Each entry also stores the hashed name of the persistent function to match and rebind the persistent function ids to the corresponding functions of the new Wasm binary on a program upgrade. Moreover, each entry also records the type of the closure, referring to the persistent type table. The table survives upgrades and is built and updated by the runtime system. To build and update the persistent virtual table, the compiler provides a **persistent function map**, mapping the hashed name of a potentially persistent function to the corresponding Wasm table index, plus its closure type pointing to the new type table. For performance, the persistent function map is sorted by the hashed names.
   
2. **Function literal table** for materializing persistent function literals:

   As the compiler does not yet know the function ids of persistent function literals/constants, this table maps a Wasm table index of the current program version to a persistent function id. The function literal table is re-built on program initialization and upgrade. When a persistent function literal is loaded, it serves for resolving the corresponding function id and thus the persistent function reference. The table is discarded on upgrades and (re-)constructed by the runtime system, based on the information of the **persistent function map**.

The runtime system distinguishes between transient and persistent function references by using a different encoding. This is to avoid complicated conversion logic been inserted by the compiler when a persistent function reference is assigned to transient reference, in particular in the presence of sharing (a function reference can be reached by both a persistent and transient function type) and composed types (function references can be deeply nested in a composed value that is assigned).

## Compatibility Check

A persistent function compatibility check is performed by the runtime system on upgrade.
* It checks for a matching function in the new version.
* The function type compatibility is implicitly covered by the upgrade memory compatibility check, since the persistent function in use needs to be reachable by the persistent actor type.
* The closure compatibility is additionally checked for each mapped persistent function. This covers all captured variables of the persistent function. This check is supported by the information of the persistent virtual table and the persistent function map.

Transient function references are represented as negative function ids determining the Wasm table index, specifically `-wasm_table_index - 1`.

## Garbage Collection

The runtime systems relies on a dedicated garbage collector of persistent functions:
* On pre-upgrade, the runtime systems determines which persistent functions are still alive, i.e. transitively reachable from persistent actor variables.
* Only those alive persistent functions need to exist in the new program version.
* All other persistent functions of the previous version are considered garbage and their slots in the virtual table can be recycled.
* For efficiency, the GC is type-directed such that it only selectively traverses fields that may lead to persistent function type. Note: Same objects may be revisited if appearing by a different static type.

Garbage collection is necessary to allow programs to use persistent classes and persistent functions in only transient contexts or not even using imported persistent classes or functions. Moreover, it allows programs to drop persistent functions and classes, if they are no longer used for persistence.

The runtime system reports the fully qualified name of missing persistent functions (not only the internal name hash).
