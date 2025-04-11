---
sidebar_position: 1
---

# Stable Functions, Stable Objects, and Stable Classes

Functions, objects, and classes can also be persisted with enhanced orthogonal persistence, i.e. they been used in Motoko's stable variables and persistent actor variables.

**Note**: This is only supported as part of enhanced orthogonal persistence, not with classical persistence.

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

## Compatibility Check

A stable function compatibility check is performed by the runtime system on upgrade.
* It checks for a matching function in the new version, only for alive stable functions, directly or indirectly reachable via non-transient actor variables.
* The function type compatibility is implicitly covered by the upgrade memory compatibility check, since the stable function in use needs to be reachable by the stable actor type.
* The closure compatibility is additionally checked for each mapped stable function. This covers all captured variables of the stable function. This check is supported by the information of the persistent virtual table and the stable function map.

Flexible function references are represented as negative function ids determining the Wasm table index, specifically `-wasm_table_index - 1`.
