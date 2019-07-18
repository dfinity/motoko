# Payment Language and API Support

Brain dump for an approach to structured payments.


## Requirements

* Payments with messages and replies
* Multiple payments per message/reply
* Multiple (open set?) currencies

## Approach

* Treat payments as linear (more precisely, affine) resources
* Apply very simple binding restrictions to ensure linearity


## ActorScript

* Two new types:
  ```
  <typ> ::= ...
     Dfn
     payment <typ>
  ```

* `Dfn` describes an amount of DFN
  - given as a `Nat` (or `Int`, or `Fixed`?)
  - intro and elim syntax TBD
  - stateless

* `payment T` is a payment of all funds occurring in `T` (expresses *owned* funds)
  - in the basic form, `T` is `Dfn`
  - but `payment` is a functorial mapping for other `T`
  - intro syntax `payment <exp>`
  - elim syntax is pattern `payment <pat>`
  - linearly (affinely) restricted resource; simplest possible implementation: cannot be bound to a variable!

* Static Semantics
  - for `payment T` to be well-formed, `T` must be sharable
  - both `Dfn` and `payment T` are sharable
  - a type `T` is *linear* when it contains a value of type `payment U`
  - term variables must not have linear type
  - type variables cannot abstract linear types
  - no other restrictions

* Dynamic Semantics
  - a value of type `payment T` represents a portion of the canister's account
  - nothing particular happens when creating and passing it around inside a canister
  - but when passed with a message or message result, implies payment via *ownership transfer*
  - payment is implicitly accepted if the receiving method doesn't trap
  - rejected otherwise

* Implementation
  - the runtime representation of `payment T` is no different from `T`, all the magic is in the serialisation (see API/IDL discussion below)

* Open Questions
  - how exactly to deal with (multiple) currencies
  - syntax for funds
  - is more fine-grained control over accepting/rejecting payments needed?
  - could make `payment T` equivalent to (or subtype of) `T` where all covariant occurrences of `Dfn` are replaced with `payment Dfn`


## Examples

Simple receiver:
```
actor A {
  // Note use of pattern: `dfn` has type `Dfn`, not `payment Dfn`!
  public func deposit(payment dfn : Dfn) { /* received */ }
}
```
Simple sender:
```
actor B {
  // Made-up syntax `$amount` for expressing dfn
  public func f() { A.deposit(payment $4); }
}
```

Structured payment:
```
type Partner = actor {
  remunerate : (payment Dfn) -> ();
};

actor C {
  type Name = Text;
  var partners : Map<Name, Partner>;

  public func partners() : async [Name] {
    return partners.keys();
  };
  public func distribute(payment shares : [(Name, Dfn)]) {
    for ((name, dfn) in shares.values()) {
      partners.lookup(name).remunerate(payment dfn);
    }
  }
};

actor D {
  // Assuming we only have DFNs for now
  public func dividend(payment $amount : Dfn) : async (payment Dfn) {
    let partners = await C.partners();
    let n = partners.len();
    let share = amount / n;
    let rest = amount % n;
    C.distribute(Array.map<Name, (Name, Funds)>(partners,
      func name (name, payment share)));
    return payment rest;
  }
}
```


## System API

Possible design:

* Model individual payment funds as a reference type `paymentref`

* Passing paymentrefs with a message send/reply transfers funds

* Functions:
  - `payment.create : i64 -> paymentref`
  - `payment.amount : paymentref -> i64`

Alternative:

* Pass payments in a separate array


## IDL

* In the IDL, there is just one new type:
  ```
  <typ> ::= ...
     payment
  ```

* Maps to `payment Dfn` in AS.

* The `M` mapping treats `payment T` the same as `T`, but with all values of type `funds` ignored.

* The `R` mapping extracts funds from payments.

* (The alternative System API would require a third projection, say `P`.)
