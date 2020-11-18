import Prim "mo:prim";
import Funds "funds";

shared {caller} actor class Wallet() {

  let print = Prim.debugPrint;

  let owner = caller;

  public func show() : async () {
    print("wallet: " # debug_show({
      cycles = "<not_shown>";
    }));
  };

  public func balance() : async Nat64 {
    return Funds.balance();
  };

  public func credit() : async () {
    let bu = Funds.balance();
    let du = Funds.available();
    ignore Funds.accept(du);
    assert Funds.balance() == bu + du;
  };

  public shared {caller} func debit(
    amount : Nat64,
    credit : shared () -> async ())
    : async () {
    if (caller != owner) assert false;
    Funds.add(amount);
    await credit();
  };

  public shared {caller} func refund(
    amount : Nat64)
    : async () {
    ignore Funds.accept(Funds.available() - amount);
    print("refunding: " #  debug_show(amount));
  };

  public shared {caller} func available()
    : async Nat64 {
    let available = Funds.available();
    print("available: " #  debug_show(available));
    return available;
  };


}
