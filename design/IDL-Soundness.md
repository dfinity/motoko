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

Parameter meta-variables:

 * `t`: A argument or result type
 * `s ::= t1 -> t2`: A service type

Parameter relations (to be instantated by a concrete solution):

 * `t1 <: t2`:
   A value encoded at `t1` can understood at `t2`
 * `s1 in t1 <: s2 in t2`:
   A value encoded at `t1` contains a service reference at type `s1`, which,
   when decoded at `t2`, has type `s2`.
 * `s1 ~> s2`:
   A service may evolve from `s1` to `s2`
 * `s1 <:h s2`:
   Host-language subtyping relation on service types
   (trivial reflexive relation in host languages without subtyping)

Judgemnets
----------

Further meta-variables:

 * `A`, `B`, …: A service identifier
 * `S : 𝒫(P)`: The global state, a set of assertions
 * `P ::=`: Assertions
   * `A : s`: Service `A` has service state `s`
   * `A has B : s`: Service `A` has a reference to service `B` at type `s`

Judgments:

 * `S1 --> S2`: State transitions

   * Services can be added
     ```
         A fresh in S
     ─────────────────────
       S --> { A : s, S}
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
       S  --> { B has A : s, S'}
     ```

   * Services can send references to each other (uses auxillary judgments)
     ```
       A has C at s1 ∈ S    [S] A =(t1)=(t2)=> B    s1 in t1 <: s2 in t2
     ────────────────────────────────────────────────────────────────────
                            S --> { B has C : s2, S}
     ```

   * Host-language subtyping may apply
     ```
       A has B at s1 ∈ S   s1 <:h s2
     ──────────────────────────────────
          S --> { A has B : s2, S}
     ```

 * `[S] A =(t1)=(t2)=> B`: In state `S`, `A` can send a message to `B` at type `t1`, which is understood at type `t2` by `B`.

   * Calls:
     ```
       A has B : t1 -> t2 ∈ S   B : t1' -> t2' ∈ S
     ──────────────────────────────────────────────
                  [S] A =(t1)=(t1')=> B
     ```
   * Replies
     ```
       A has B : t1 -> t2 ∈ S   B : t1' -> t2' ∈ S
     ──────────────────────────────────────────────
                  [S] B =(t2')=(t2)=> A
     ```

Soundness
---------
Soundness theorem:

 * If `{} -->* S` and `[S] A =(t1)=(t2)=> B` then `t1 <: t2`.
