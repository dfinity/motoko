import Prim = "mo:prim";
import Funds = "funds/funds";
import Wallet = "funds/wallet";

// test state behind funds and refunds works as expected, with
// funds initially zero, additive, reset on context switches
// refunds initially zero, reset on context switches, set on await.

actor a {

 let print = Prim.debugPrint;

 func printBalance(t : Text) {
   let balance = { cycle = "<omitted>"; icpt =Funds.balance(#icpt) };
   print(t # " balance is " #  debug_show(balance));
 };

 public func go() : async () {

  await Funds.dev_set_funds(a, 2_000_000_000_000_000, 2000);

  printBalance("A");


  // check funds reset on context switch
  Funds.add(#cycle, 100);
  Funds.add(#icpt, 10);
  await async {
    printBalance("B");
    assert(Funds.available(#cycle) == (100 : Nat64));
    assert(Funds.available(#icpt) == (10 : Nat64));
    // check funds received
    Funds.add(#cycle, 50);
    Funds.add(#icpt, 5);
    let (cs, is) = await getAvailable();
    assert (cs == (50 : Nat64) and is == (5 : Nat64));
    assert (Funds.refunded(#cycle) == (50 : Nat64) and Funds.refunded(#icpt) == (5 : Nat64));
    printBalance("C");
  };

  printBalance("D");
  print("refunded " # debug_show({ cycle = Funds.refunded(#cycle); icpt = Funds.refunded(#icpt)}));
  // check refund from await async ...
  assert (Funds.refunded(#cycle) == (100 : Nat64));
  assert (Funds.refunded(#icpt) == (10 : Nat64)); // FAILS on dfinity (refund is 0) but succeeds on ic-ref

 };

 public func showBalance() : async () {
   printBalance("E");
 };

 // returns the amounts transferred (without accepting, thus refunding fully)
 public shared {caller} func getAvailable()
    : async (Nat64, Nat64) {
    let available = (Funds.available(#cycle), Funds.available(#icpt));
    print("available: " #  debug_show(available));
    return available;
  };

};

ignore a.go(); //OR-CALL ingress go "DIDL\x00\x00"
ignore a.showBalance(); //OR-CALL ingress showBalance "DIDL\x00\x00"

//SKIP run
//SKIP run-ir
//SKIP run-low

