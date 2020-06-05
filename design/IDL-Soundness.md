An IDL Soundness Proposition
============================

This document attempts to formalize what it means for an IDL language (like
Candid) to be _sound_, in the presence of service upgrades and (optional)
host-language subtyping. It provides a framework that can be instantiated with
various proposed rules for service type evolution.

In the interest of simplification, this framework combines the notion of
“service type” and “function type” (as if we only had function types, or if
each service had only one function).

Framework parameters
--------------------

An IDL language would instantiate these relations:

Parameter meta-variables:

 * `t`: An argument or result type
 * `s ::= t1 -> t2`: A service type

Parameter relations (to be instantated by a concrete solution):

 * `t1 <: t2`:
   A value encoded at `t1` can be understood at `t2`
 * `s1 in t1 <: s2 in t2`:
   A value encoded at `t1` contains a service reference at type `s1`, which,
   when decoded at `t2`, has type `s2`.
 * `s1 ~> s2`:
   A service may evolve from `s1` to `s2`
 * `s1 <:h s2`:
   Host-language subtyping relation on service types
   (trivial reflexive relation in host languages without subtyping)

Judgements
----------

Further meta-variables:

 * `A`, `B`, …: A service identifier
 * `S : 𝒫(P)`: The global state, a set of assertions
 * `P ::=`: Assertions
   * `A : s`: Service `A` has service type `s`
   * `A has B : s`: Service `A` has a reference to service `B` at type `s`

Judgements:

 * `S1 --> S2`: State transitions

   * Services can be added
     ```
         A fresh in S
     ─────────────────────
       S --> (A : s) ∪ S
     ```

   * Services can evolve
     ```
                  s1 ~> s2
     ────────────────────────────────
     { A : s1, S' } --> { A : s2, S'}
     ```

   * Services can learn about other services
     ```
              A : s ∈ S
     ────────────────────────────
       S  --> (B has A : s) ∪ S
     ```

   * Services can send references to each other (uses auxillary judgments)
     ```
       A has C at s1 ∈ S    [S] A =(t1)=(t2)=> B    s1 in t1 <: s2 in t2
     ────────────────────────────────────────────────────────────────────
                            S --> (B has C : s2) ∪ S
     ```

   * Host-language subtyping may apply
     ```
       A has B at s1 ∈ S   s1 <:h s2
     ──────────────────────────────────
          S --> (A has B : s2) ∪ S
     ```

 * `[S] A =(t1)=(t2)=> B`: In state `S`, `A` can send a message to `B` at type `t1`, and `B` expects the message to be of type `t2`.

   * Calls:
     ```
       A has B : t1 -> _ ∈ S   B : t1' -> _ ∈ S
     ──────────────────────────────────────────────
                  [S] A =(t1)=(t1')=> B
     ```
   * Replies
     ```
       B has A : _ -> t1' ∈ S   A : _ -> t1 ∈ S
     ──────────────────────────────────────────────
                  [S] A =(t1)=(t1')=> B
     ```

Soundness
---------
A state `S` is consistent if

> Forall `[S] A =(t1)=(t2)=> B` we have `t1 <: t2`.

An IDL language is sound if and only if:

> Forall `{} -->* S`, `S` is consistent.


Proof (for canonical subtyping)
-------------------------------

A solution with canonical subtyping (transitive, contravariant) is sound.

Assume a reflexivie and transitive relation `t1 <: t2`.

Define `(t1' -> t2') <: (t1 -> t2) ⟺ (t1 <: t1') ∧ (t2' <: t2')`

Assume that
 * `s1 ~> s2` only if `s2 <: s1` (it can be more restrictive)
 * If `t1 <: t2` and `s1 in t1 <: s2 in t2` then `s1 <: s2`.
 * `s1 <:h s2` only if `s1 <: s2` (it can be more restrictive).

The following is obviously an invariant of `-->`:

> Every serivce `A` has at most one type, i.e.
> for a fixed `{} --> * S`, `A : s ∈ S` is a left-unique relation.

We prove that the following is an invariant of `-->`:

> If `A : s1 ∈ S` and `B has A : s2 ∈ S`, then `s1 <: s2`

Consistency follows from this by inversion on `[S] A =(t1)=(t2)=> B` (to figure
out who called whom):

 * If `A has B : t1 -> _ ∈ S` and `B : t1' -> _ ∈ S`
   then `t1' -> _ <: t1 -> _` (by the invarinat)
   thus `t1 <: t2`
 * If `B has A : _ -> t1' ∈ S` and `A : _ -> t1 ∈ S`
   then `_ -> t1 <: _ -> t2` (by the invariant)
   thus  `t1 <: t2`.

With `S = {}`, the invariant holds trivially.

Assume `S` satisfies the invariant, and consider the various state transitions
`S -> S'`:

 * A service `C` gets added.

   Because `C` is fresh in `S`, it cannot be in any `has` relation,
   so the invariants continues to hold.

 * A service `C` evolves from `s3 ~> s4`

   If `A : s1 ∈ S'` and `B has A : s2 ∈ S'`, then either `C ≠ A`  and`s1 <: s2` follows from the invariant about `S`.

   Or `A = C`, so `s1 = s4` and `A : s3 ∈ S`.
   We have `s3 <: s2` by the invariant on `S` and
   `s4 <: s3` by the assumption on `~>`, so
   `s1 <: s2` by transitivity of `<:`.

 * Services can learn about other services

    This adds `B has A : s` to the state, given `A : s ∈ S`.
    So `s <: s` by reflexitivity.

 * `A` sends a reference to `C` to `B`.

    This adds `B has C : s2` to the state, given
    ```
    A has C at s1 ∈ S
    [S] A =(t1)=(t2)=> B
    s1 in t1 <: s2 in t2
    ```

    Let `C : s3 ∈ S`. By the invariant on `S` we learn `s3 <: s1`.
    By consistency of `S` we have `t1 <: t2`.
    With `s1 in t1 <: s2 in t2` this yields `s1 <: s2`.
    So `s3 <: s2`, as required.


  * Host-language subtyping may apply.

    This adds `A has B : s` to the state, given
    ```
    A has B at s1 ∈ S
    s1 <:h s2
    ```

    Let `B : s3 ∈ S`. By the invariant on `S` we learn `s3 <: s1`.
    The assumption on `<:h` yields `s1 <: s2`.
    So `s3 <: s2`, as required.

Therefore, every `{} -->* S` is consistent, and thus the IDL system is sound.


