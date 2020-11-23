import Prim "mo:prim";
import Cycles "ExperimentalCycles";
//import Cycles "mo:base/ExperimentalCycles";

shared {caller = owner} actor class PiggyBank(
  capacity: Nat,
  benefit : shared () -> async ()) {

  var savings = 0;

  public shared {caller} func getSavings() : async Nat {
    assert (caller == owner);
    return savings;
  };

  public func deposit() : async () {
    let amount = Cycles.available();
    let limit = capacity - savings;
    let acceptable =
      if (amount <= limit) amount
      else limit;
    ignore Cycles.accept(acceptable);
    savings += acceptable;
  };

  public shared {caller} func withdraw(amount : Nat)
    : async () {
    assert (caller == owner);
    assert (amount <= savings);
    Cycles.add(amount);
    await benefit();
    let refund = Cycles.refunded();
    savings -= amount - refund;
  };

}
