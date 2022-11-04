import Prim "mo:⛔";
import Cycles "cycles";

shared(msg) actor class Wallet() {

  let print = Prim.debugPrint;

  let owner = msg.caller;

  public func show() : async () {
    print("wallet: " # debug_show({
      cycles = "<not_shown>";
    }));
  };

  public func balance() : async Nat {
    return Cycles.balance();
  };

  public func credit() : async () {
    let bu = Cycles.balance();
    let du = Cycles.available();
    ignore Cycles.accept(du);
    assert Cycles.balance() == bu + du;
  };

  public shared(msg) func debit(
    amount : Nat,
    credit : shared () -> async ())
    : async () {
    if (msg.caller != owner) assert false;
    Cycles.add(amount);
    await credit();
  };

  public shared func refund(
    amount : Nat)
    : async () {
    ignore Cycles.accept(Cycles.available() - amount);
    print("refunding: " #  debug_show(amount));
  };

  public shared func available()
    : async Nat {
    let available = Cycles.available();
    print("available: " #  debug_show(available));
    return available;
  };


}
