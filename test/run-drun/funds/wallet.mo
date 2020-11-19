import Prim "mo:prim";
import Funds "funds";

shared {caller} actor class Wallet() {

  let print = Prim.debugPrint;

  let owner = caller;

  public func show() : async () {
    print("wallet: " # debug_show({
      icpt = Funds.balance(#icpt);
      cycles = "<not_shown>";
      // cycles = Funds.balance(#cycle);
    }));
  };

  public func balance(u : Funds.Unit) : async Nat64 {
    return Funds.balance(u);
  };

  public func credit(u : Funds.Unit) : async () {
    let bu = Funds.balance(u);
    let du = Funds.available(u);
    print("wallet credit, available: " #  debug_show (u, du));
    Funds.accept(u, du);
    assert Funds.balance(u) == bu + du;
  };

  public shared {caller} func debit(
    u : Funds.Unit,
    amount : Nat64,
    credit : shared Funds.Unit -> async ())
    : async () {
    if (caller != owner) assert false;
    print("wallet debit, requested: " #  debug_show (u, amount));
    Funds.add(u, amount);
    await credit(u);
  };

  public shared {caller} func refund(
    u : Funds.Unit,
    amount : Nat64)
    : async () {
    print("wallet refund, available: " #  debug_show (u, amount));
    Funds.accept(u, Funds.available(u) - amount);
    print("refunding: " #  debug_show(amount));
  };

  public shared {caller} func available()
    : async (Nat64, Nat64) {
    let available = (Funds.available(#cycle), Funds.available(#icpt));
    print("available: " #  debug_show(available));
    return available;
  };


}
