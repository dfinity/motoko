import Prim "mo:prim";
import Funds "funds";

shared {caller}  actor class () {

  let print = Prim.debugPrint;

  let owner = caller;

  print("wallet created");

  public func balance(u : Funds.Unit) : async Nat64 {
    return Funds.balance(u);
  };

  public func credit(u : Funds.Unit) : async () {
    let bu = Funds.balance(u);
    let du = Funds.available(u);
    Funds.accept(u, du);
    assert Funds.balance(u) == bu + du;
  };

  public shared {caller} func debit(
    u : Funds.Unit,
    amount : Nat64,
    credit : shared Funds.Unit -> async ())
    : async () {
    if (caller != owner) assert false;
    Funds.transfer(u, amount);
    await credit(u);
  };

  public shared {caller} func refund(
    u : Funds.Unit,
    amount : Nat64)
    : async () {
    Funds.accept(u, Funds.available(u) - amount);
  };


}
