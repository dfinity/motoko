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

* Three new types:
  ```
  <typ> ::= ...
     Currency
     Funds
     pay <typ>
  ```

* `Currency` is simply an open enumeration
  - one of DFN, ETH, ...
  - intro syntax TBD (could be a tag like `#DFN` etc, but want open set)
  - no elim form, only equality
  - stateless

* `Funds` is a pair of an amount and currency unit
  - amount is `Nat` (or `Int`, or `Float`?)
  - intro and elim syntax TBD
  - stateless

* `pay T` is a payment of all funds occurring in `T` (expresses *owned* funds)
  - in the basic form, `T` is `Funds`
  - but `pay` is a functorial mapping for other `T`
  - intro syntax `pay <exp>`
  - elim syntax is pattern `pay <pat>`
  - linearly restricted resource; simplest possible implementation: cannot be bound to a variable!

* Static Semantics
  - for `pay T` to be well-formed, `T` must be sharable
  - all of `Currency`, `Funds`, and `pay T` are sharable
  - a type `T` is *linear* when it contains a value of type `pay U`
  - term variables must not have linear type
  - no other restrictions

* Dynamic Semantics
  - a value of type `pay T` represents a portion of the canister's account
  - nothing particular happens when creating and passing it around inside a canister
  - but when passed with a message or message result, implies payment via *ownership transfer*
  - payment is implicitly accepted if the receiving method doesn't trap
  - rejected otherwise

* Open Questions
  - how exactly to deal with (multiple) currencies
  - syntax for funds
  - is more fine-grained control over accepting/rejecting payments needed?


## Examples

Simple receiver:
```
actor A {
  // Note use of pattern: `x` has type `Funds`, not `pay Funds`!
  public func deposit(pay x : Funds) { /* received */ }
}
```
Simple sender:
```
actor B {
  // Made-up syntax `(#currency amount)` for expressing funds
  public func f() { A.deposit(pay (#DFN 4)); }
}
```

Structured payment:
```
type Partner = actor {
  remunerate : (pay Funds) -> ();
};

actor C {
  type Name = Text;
  var partners : Map<Name, Partner>;

  public func partners() : async [Name] {
    return partners.keys();
  };
  public func distribute(pay shares : [(Name, Funds)]) {
    for ((name, x) in shares.values()) {
      partners.lookup(name).remunerate(pay x);
    }
  }
};

actor D {
  // Assuming we only have DFNs for now
  public func dividend(pay (#DFN amount) : Funds) : async (pay Funds) {
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


## API

Possible design:

* Model individual payment funds as a reference type `payref`

* Passing payrefs with a message send/reply transfers funds

* Functions:
  - `pay.create : [i64, currency] -> [payref]`
  - `pay.amount : [payref] -> [i64]`
  - `pay.currency : [payref] -> [currency]`

* Unclear how to best represent currencies

Alternative:

* Pass payments in a separate array


## IDL

* Similar type structure as in AS:
  ```
  <typ> ::= ...
     currency
     funds
     pay <typ>
  ```

* The `M` mapping treats `pay T` the same as `T`, but with all values of type `funds` ignored.

* The `R` mapping extracts funds from payments.
