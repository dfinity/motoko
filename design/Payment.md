# Payment Language and API Support

Brain dump for an approach to structured payments.


## Requirements

* Payments with messages and replies
* Multiple payments per message/reply
* Multiple (open set?) currencies

## Approach

* Treat payments as linear resources
* Apply very simple binding restrictions to ensure linearity


## ActorScript

* Two new types:
  ```
  <typ> ::= ...
     Dfn
     pay <typ>
  ```

* `Dfn` describes an amount of DFN
  - given as a `Nat` (or `Int`, or `Fixed`?)
  - intro and elim syntax TBD
  - stateless

* `pay T` is a payment of all funds occurring in `T` (expresses *owned* funds)
  - in the basic form, `T` is `Dfn`
  - but `pay` is a functorial mapping for other `T`
  - intro syntax `pay <exp>`
  - elim syntax is pattern `pay <pat>`
  - linearly restricted resource; simplest possible implementation: cannot be bound to a variable!

* Static Semantics
  - for `pay T` to be well-formed, `T` must be sharable
  - both `Dfn` and `pay T` are sharable
  - a type `T` is *linear* when it contains a value of type `pay U`
  - term variables must not have linear type
  - no other restrictions

* Dynamic Semantics
  - a value of type `pay T` represents a portion of the canister's account
  - nothing particular happens when creating and passing it around inside a canister
  - but when passed with a message or message result, implies payment via *ownership transfer*
  - payment is implicitly accepted if the receiving method doesn't trap
  - rejected otherwise

* Implementation
  - the runtime representation of `pay T` is no different from `T`, all the magic is in the serialisation (see API/IDL discussion below)

* Open Questions
  - how exactly to deal with (multiple) currencies
  - syntax for funds
  - is more fine-grained control over accepting/rejecting payments needed?
  - could make `pay T` equivalent to (or subtype of) `T` where all covariant occurrences of `Dfn` are replaced with `pay Dfn`


## Examples

Simple receiver:
```
actor A {
  // Note use of pattern: `dfn` has type `Dfn`, not `pay Dfn`!
  public func deposit(pay dfn : Dfn) { /* received */ }
}
```
Simple sender:
```
actor B {
  // Made-up syntax `$amount` for expressing dfn
  public func f() { A.deposit(pay $4); }
}
```

Structured payment:
```
type Partner = actor {
  remunerate : (pay Dfn) -> ();
};

actor C {
  type Name = Text;
  var partners : Map<Name, Partner>;

  public func partners() : async [Name] {
    return partners.keys();
  };
  public func distribute(pay shares : [(Name, Dfn)]) {
    for ((name, dfn) in shares.values()) {
      partners.lookup(name).remunerate(pay dfn);
    }
  }
};

actor D {
  // Assuming we only have DFNs for now
  public func dividend(pay $amount : Dfn) : async (pay Dfn) {
    let partners = await C.partners();
    let n = partners.len();
    let share = amount / n;
    let rest = amount % n;
    C.distribute(Array.map<Name, (Name, Funds)>(partners,
      func name (name, pay share)));
    return pay rest;
  }
}
```


## System API

Possible design:

* Model individual payment funds as a reference type `payref`

* Passing payrefs with a message send/reply transfers funds

* Functions:
  - `pay.create : i64 -> payref`
  - `pay.amount : payref -> i64`

Alternative:

* Pass payments in a separate array


## IDL

* In the IDL, there is just one new type:
  ```
  <typ> ::= ...
     dfn
  ```

* Maps to `pay Dfn` in AS.

* The `M` mapping treats `pay T` the same as `T`, but with all values of type `funds` ignored.

* The `R` mapping extracts funds from payments.

* (The alternative System API would require a third projection, say `P`.)
