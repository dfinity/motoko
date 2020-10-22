import Prim "mo:prim";
import Funds "ExperimentalFunds";
//import Funds "mo:base/ExperimentalFunds";

shared {caller = owner} actor class PiggyBank(
  unit : Funds.Unit,
  capacity: Nat,
  benefit : shared () -> async ()) {

  var savings = 0;

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
