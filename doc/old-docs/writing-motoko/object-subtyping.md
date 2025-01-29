---
sidebar_position: 18
---

# Object subtyping



**Object subtyping** : In Motoko, objects have types that may relate by subtyping. Types with more fields are less general and are subtypes of types with fewer fields. Consider the following general types and subtypes:

-   Most general:

``` motoko no-repl
{ bump : () -> Nat }
```

-   Middle generality:

``` motoko no-repl
{
  inc  : () -> () ;
  read : () -> Nat ;
  bump : () -> Nat ;
}
```

-   Least generality:

``` motoko no-repl
{
  inc  : () -> () ;
  read : () -> Nat ;
  bump : () -> Nat ;
  write : Nat -> () ;
}
```

If a function expects to receive an object of the first type (`{ bump: () → Nat }`), any of the types given above will suffice since they are each equal to or a subtype of the most general type.

However, if a function expects to receive an object of the last, least general type, the other two will *not* suffice, since they each lack the needed `write` operation, to which this function rightfully expects to have access.

## Example

To illustrate the role and use of object subtyping in Motoko, consider implementing a simpler counter with a more general type that has fewer public operations:

``` motoko
object bumpCounter {
  var c = 0;
  public func bump() : Nat {
    c += 1;
    c
  };
};
```

The object `bumpCounter` has the following object type, exposing exactly one operation, `bump`:

``` motoko no-repl
{
  bump : () -> Nat ;
}
```

This type exposes the most common operation, and one that only permits certain behavior. For instance, the counter can only ever increase and can never decrease or be set to an arbitrary value.

In other parts of a system, you may implement and use a less general version with more operations:

``` motoko no-repl
fullCounter : {
  inc   : () -> () ;
  read  : () -> Nat ;
  bump  : () -> Nat ;
  write : Nat -> () ;
}
```

Consider a counter named `fullCounter` with a less general type than any given above. In addition to `inc`, `read` and `bump`, it additionally includes `write`, which permits the caller to change the current count value to an arbitrary one, such as back to `0`.

## Structural subtyping

Object subtyping in Motoko uses structural subtyping, not nominal subtyping.

In nominal typing, the question of two types equality depends on choosing consistent, globally-unique type names across projects and time.

In Motoko, the question of two types' equality is based on their structure, not their names. Due to structural typing, naming a class type provides a convenient abbreviation.

For typing purposes, all that matters is the structure of the corresponding object type. Two classes with different names but equivalent definitions produce type-compatible objects.

When the optional type annotation is supplied in a class declaration, conformance is checked. The object type must be a subtype of the annotation. The annotation does not affect the type of the class, even if it only describes a proper super-type of the object type.

Subtyping relationships in Motoko extend to all types, not just object types.

Most cases are standard and follow conventional programming language theory for *structural* subtyping.

Other notable cases in Motoko for new programmers include array, options, variants and number type inter-relationships.

<img src="https://github.com/user-attachments/assets/844ca364-4d71-42b3-aaec-4a6c3509ee2e" alt="Logo" width="150" height="150" />