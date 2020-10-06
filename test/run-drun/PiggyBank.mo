import Prim "mo:prim";
import Funds "piggy-bank/ExperimentalFunds";
//import Funds "mo:base/ExperimentalFunds";

shared {caller} actor class PiggyBank(
  credit : shared () -> async (),
  unit : Funds.Unit,
  capacity: Nat) {

  let owner = caller;
  var savings_ = 0;

  public shared {caller} func savings() : async Nat {
    assert (caller == owner);
    return savings_;
  };

  public func deposit() : async () {
    let amount = Funds.available(unit);
    let acceptable =
      if (amount <= capacity - savings_) amount
      else (capacity - savings_);
    Funds.accept(unit, acceptable);
    savings_ += acceptable;
  };

  public shared {caller} func withdraw(amount : Nat)
    : async () {
    assert (caller == owner);
    assert (amount <= savings_);
    Funds.add(unit, amount);
    await credit();
    let refund = Funds.refunded(unit);
    savings_ -= amount - refund;
  };

}
