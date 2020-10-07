import Prim "mo:prim";
import Funds "piggy-bank/ExperimentalFunds";
//import Funds "mo:base/ExperimentalFunds";

shared {caller} actor class PiggyBank(
  credit : shared () -> async (),
  unit : Funds.Unit,
  capacity: Nat) {

  let owner = caller;
  var savings = 0;

  public shared {caller} func getSavings() : async Nat {
    assert (caller == owner);
    return savings;
  };

  public func deposit() : async () {
    let amount = Funds.available(unit);
    let maxAcceptable = capacity - savings;
    let acceptable = 
      if (amount <= maxAcceptable) amount
      else maxAcceptable;
    Funds.accept(unit, acceptable);
    savings += acceptable;
  };

  public shared {caller} func withdraw(amount : Nat)
    : async () {
    assert (caller == owner);
    assert (amount <= savings);
    Funds.add(unit, amount);
    await credit();
    let refund = Funds.refunded(unit);
    savings -= amount - refund;
  };

}
