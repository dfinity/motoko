import Prim "mo:prim";
import Cycles "cycles";

shared {caller} actor class Wallet() {

  let print = Prim.debugPrint;

  let owner = caller;

  public func show() : async () {
    print("wallet: " # debug_show({
      cycles = "<not_shown>";
    }));
  };

  public func balance() : async Nat64 {
    return Cycles.balance();
  };

  public func credit() : async () {
    let bu = Cycles.balance();
    let du = Cycles.available();
    ignore Cycles.accept(du);
    assert Cycles.balance() == bu + du;
  };

  public shared {caller} func debit(
    amount : Nat64,
    credit : shared () -> async ())
    : async () {
    if (caller != owner) assert false;
    Cycles.add(amount);
    await credit();
  };

  public shared {caller} func refund(
    amount : Nat64)
    : async () {
    ignore Cycles.accept(Cycles.available() - amount);
    print("refunding: " #  debug_show(amount));
  };

  public shared {caller} func available()
    : async Nat64 {
    let available = Cycles.available();
    print("available: " #  debug_show(available));
    return available;
  };


}
