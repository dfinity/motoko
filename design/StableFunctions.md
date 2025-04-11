# Stable Functions, Stable Objects, and Stable Classes

Functions, objects, and classes can also be persisted with enhanced orthogonal persistence, i.e. they been used in Motoko's stable variables and persistent actor variables.

Note: This is not supported with classical persistence.

With this, the only non-persistent (non-stable) values remain to be lambdas, async handles (aka continuations, futures), and local async function references.

## Example

With this, the following program can now be used for orthogonal persistence. For this example, you need a [specific base library branch](https://github.com/dfinity/motoko-base/tree/luc%2Fstable-functions), which contains slight modifications for supporting stable functions.

```
import HashMap "base/HashMap";
import Hash "base/Hash";
import Nat "base/Nat";
import Iter "base/Iter";

persistent actor {
    let hashMap = HashMap.HashMap<Nat, Text>(10, Nat.equal, Hash.hash); // finally persistent!

    public func populate(): async() {
        hashMap.put(1, "A");
        hashMap.put(2, "B");
        hashMap.put(3, "C");
    };

    public func list(): async [(Nat, Text)] {
        Iter.toArray<(Nat, Text)>(hashMap.entries());
    };
}
```

## Stable Functions and Stable Scopes

A stable function is a named non-async local function in a stable scope, only closing over variables of a stable type.

A stable scope is:
* the main actor,
* an actor class,
* a module imported with a unique identifier from a stable scope,
* a named non-async function in a stable scope,
* a class in a stable scope, or,
* a named object in a stable scope.

Generic type parameters of stable functions and stable classes are bounded to stable types.

A stable function is also a stable type.

Syntactically, function types are prefixed by `stable` to denote a stable function, e.g.
`stable X -> Y`. Stable functions implicitly have a corresponding stable reference type.

A stable function type is a sub-type of a flexible function type with type-compatible signature, i.e. `stable X' -> Y <: X -> Y'` for `X' <: X` and `Y' :< Y`.

## Upgrades of Stable Functions

Stable functions are upgraded as follows:
* All stable functions that are reachable from stable variables are considered alive.
* Each alive stable function must have a matching declaration in the new program version.
* Stable functions match between program versions if they have an equal fully qualified name.
* For matching functions, the function type of the new version must be compatible to the previous version (super-type).
* For matching functions, the closure type in the new version must be compatible with the previous version, see below.

All other functions, such as lambdas, named functions in a lambda, async functions, or functions imported from a module without a unique import identifier, are flexible functions.

## Stable Closures

The closures of stable functions are represented in a portable format with the following clearly defined binding of captured variables:
* Local variables are captured by name.
* Parameters are captured by position (also considering nested functions).

This offers flexibility for changes of stable closures in new program versions:
* New program version can capture less variables/parameters in stable functions.
* One can rename parameters even if they are captured by stable functions.

On a stable function upgrade, the closure type of the stable function must remain compatible:
* The new version of the stable function does not capture more variables/parameters than the previous version.
* The captured variable in the new version is a valid super-type of the previous version.

Specific aspects apply to generic types used in a stable closure:
* Generic types used for captured variables must match the previous declaration order (e.g. one cannot swap generic types).
* The generic type bounds must remain compatible.
* However, generic types do not need to be reified, see the reasoning in [generics in stabe closures](GenericsInStableClosure.md).

## Runtime System Design

Function references are encoded by a function id in the following representation:
* Stable function id, encoded as non-negative number:
  A stable function reference that stays invariant across upgrades.
* Flexible functiion id, encoded as negative number:
  A flexible function reference that is invalidated on upgrade.

Each program version defines a set of named local functions that can be used as stable function references. Each such function obtains a stable function id on program initialization and upgrade. If the stable function was already declared in the previous version, its function id is reused on upgrade. Thereby, the compatibility of the function type and closure type are checked. Otherwise, if it is a new stable function, it obtains a new stable function id, or a recycled id.

The runtime system supports stable functions by two mechanisms:

1. **Persistent virtual table** for stable function calls:
    
   The persistent virtual table maps stable function ids to Wasm table indices, for supporting dynamic calls of stable functions. Each entry also stores the hashed name of the stable function to match and rebind the stable function ids to the corresponding functions of the new Wasm binary on a program upgrade. Moreover, each entry also records the type of the closure, referring to the persistent type table. The table survives upgrades and is built and updated by the runtime system. To build and update the persistent virtual table, the compiler provides a **stable function map**, mapping the hashed name of a potentially stable function to the corresponding Wasm table index, plus its closure type pointing to the new type table. For performance, the stable function map is sorted by the hashed names.
   
2. **Function literal table** for materializing stable function literals:

   As the compiler does not yet know the function ids of stable function literals/constants, this table maps a Wasm table index of the current program version to a stable function id. The function literal table is re-built on program initialization and upgrade. When a stable function literal is loaded, it serves for resolving the corresponding function id and thus the stable function reference. The table is discarded on upgrades and (re-)constructed by the runtime system, based on the information of the **stable function map**.

The runtime system distinguishes between flexible and stable function references by using a different encoding. This is to avoid complicated conversion logic been inserted by the compiler when a stable function reference is assigned to flexible reference, in particular in the presence of sharing (a function reference can be reached by both a stable and flexible function type) and composed types (function references can be deeply nested in a composed value that is assigned).

## Compatibility Check

A stable function compatibility check is performed by the runtime system on upgrade.
* It checks for a matching function in the new version.
* The function type compatibility is implicitly covered by the upgrade memory compatibility check, since the stable function in use needs to be reachable by the stable actor type.
* The closure compatibility is additionally checked for each mapped stable function. This covers all captured variables of the stable function. This check is supported by the information of the persistent virtual table and the stable function map.

Flexible function references are represented as negative function ids determining the Wasm table index, specifically `-wasm_table_index - 1`.

## Garbage Collection

The runtime systems relies on a dedicated garbage collector of stable functions:
* On pre-upgrade, the runtime systems determines which stable functions are still alive, i.e. transitively reachable from stable variables.
* Only those alive stable functions need to exist in the new program version.
* All other stable functions of the previous version are considered garbage and their slots in the virtual table can be recycled.
* For efficiency, the GC is type-directed such that it only selectively traverses fields that may lead to stable function type. Note: Same objects may be revisited if appearing by a different static type.

Garbage collection is necessary to allow programs to use classes and stable functions in only flexible contexts or not even using imported classes or stable functions. Moreover, it allows programs to drop stable functions and classes, if they are no longer used for persistence.

The runtime system reports the fully qualified name of missing stable functions (not only the internal name hash).
