
Salvaged from https://github.com/dfinity/motoko/pull/1922

As for the long-term solution, I'd like to aim a bit higher.  Payments
and funds are a central and sensitive theme of the platform and should
get adequate language support that helps handling them correctly.  It
seems to be consensus in literature and other contract languages that
they are best thought of as linear resources.  So let's brainstorm a
little on the possibility of adding linearly typed funds to
Motoko. Here are some initial thoughts:

* First, for simplicity, let's say there is some primitive type
  `Funds`, as @crusso mentions.  It is a separate question how to
  represent multiple currencies, so let me ignore that problem for now
  (this may be a similar problem to units of measure, so maybe we can
  steal some of those ideas?).  What's important is that this type
  represents amounts of funds, but not necessarily _owned_ funds.  It
  is a shareable type.  It has operators like addition, comparison,
  etc.

* Separately, we add a notion of ownership, in the form of linear
  types. Let me write that `own T`, to appeal to the intuition of
  average programmers. The usual linear typing rules apply, i.e., an
  own type can only be nested inside another own type, an own
  identifier must be used exactly once, and so on. Lots of details TBD
  about what primitives to provide on linear types (swap etc).

* Now you can form a type `own Funds`. This represents real funds on
  the platform. You can express several things with that:

  - Passing funds to another a`ctor by including `own Funds` in a
    method's parameter type. Serialisation would extract this and use
    the system API to pass the funds.

  - Acquiring funds from an actor by including `own Funds` in a
    method's result type. Short of allowing returning funds in the
    API, this may have to be compiled in some higher-order manner.

  - Recording the currently owned funds in actor variables. In the
    simplest case, `var funds : own Funds` could reflect a canister's
    total. But it might also want to split it up into multiple vars,
    or even into data structures, e.g., `var balances : own
    List<(User, own Funds)>`. (It's gonna be an interesting question
    what library data structures could and should be provided over
    linear types.)

  - By adding funds received by a call to a state variable, you
    implicitly accept them (how would the compiler handle
    that?). Otherwise, linearity requires that you explicitly decline
    them, perhaps via some special construct. There's also a primitive
    to split `own Funds` values into two linear values, so you can
    partially accept. Overall, this ensures that all accessible funds
    are explicitly handled.

* The only legal introduction form for `own Funds` (other than getting
  them from somewhere) would be zero funds, so you can only initialise
  variables with such a type as zero. But you can add more funds to
  them as you receive them. The invariant that the type system is
  supposed to uphold is that the sum of all `own Funds` in an actor's
  state equals the actual funds on the system level.

* Unfortunately, linearity interacts with other platform features in
  delicate ways, so we have to be rather careful:

  - Upgrade: should not erase resources in a program, so all linear
    variables in an actor must be stable!

  - Fork: the forked actor starts out with zero funds (I assume), so
    the compiler/runtime would have to take care of zeroing out all
    owned funds (upon deserialisation?). To make this semantically
    viable, global `own Funds` must only be allowed as part of state
    that's mutable, either directly or indirectly (that's a sensible
    restriction anyway, because what's the point of funds you can
    never consume?).

  - Traps/Rollbacks: I think that just works, as long as the platform
    semantics are coherent.

Obviously, this is just a sketch of ideas. I assume there are various issues lurking. But for Motoko, wouldn't it
be desirable to adopt some such way of representing funds, that's "safe" and somewhat principled?


```
import Prim "mo:prim";
import Funds "mo:base/ExperimentalFunds";

shared {caller = owner} actor class PiggyBank(
  unit : Funds.Unit,
  capacity: Nat,
  benefit : shared () -> async ()) {

  var savings := 0;

  public shared {caller} func getSavings() : async Nat {
    assert (caller == owner);
    return savings;
  };

  public func deposit() : async () {
    let amount = Funds.available(unit);
    let limit = capacity - savings;
    let acceptable =
      if (amount <= limit) amount
      else limit;
    Funds.accept(unit, acceptable);
    savings += acceptable;
  };

  public shared {caller} func withdraw(amount : Nat)
    : async () {
    assert (caller == owner);
    assert (amount <= savings);
    Funds.add(unit, amount);
    await benefit();
    let refund = Funds.refunded(unit);
    savings -= amount - refund;
  };

}
```

```
import Prim "mo:prim";
import Funds "mo:base/ExperimentalFunds";

shared {caller = owner} actor class PiggyBank(
  unit : Funds.Unit,
  capacity: Nat,
  benefit : shared () -> async ()) {

  var savings := own funds();

  public shared {caller} func getSavings() : async Nat {
    assert (caller == owner);
    return savings;
  };

  public func deposit() : async () {
    let amount = Funds.available(unit);
    let limit = capacity - savings;
    let acceptable =
      if (amount <= limit) amount
      else limit;
    Funds.accept(unit, acceptable);
    savings += acceptable;
  };

  public shared {caller} func withdraw(amount : Nat)
    : async () {
    assert (caller == owner);
    assert (amount <= savings);
    Funds.add(unit, amount);
    await benefit();
    let refund = Funds.refunded(unit);
    savings -= amount - refund;
  };

}
```
