import Prim = "mo:prim";
import Funds = "funds/funds";
import WalletLib = "funds/wallet";

// test state behind funds and refunds works as expected, with
// funds initially zero, additive, reset on context switches
// refunds initially zero, reset on context switches, set on await.

actor a {

 let print = Prim.debugPrint;

 public func go() : async () {

  let wallet : WalletLib.Wallet = await WalletLib.Wallet();
  await wallet.show();
  print ("setting funds");
  await Funds.dev_set_funds(wallet, 2_000_000_000_000_000, 2000);
  await wallet.show();

  // debit from the waller, crediting this actor via callback
  print ("debit");
  print("balance " # debug_show(Funds.balance(#icpt)));
  let b = Funds.balance(#icpt);
  await wallet.debit(#cycle, 1_000_000_000_000_000, credit);
  await wallet.debit(#icpt, 1000, credit);

  print(debug_show(Funds.balance(#icpt)));

  do {
    // check funds available
    Funds.add(#cycle, 100);
    Funds.add(#icpt, 10);
    let (cs, is) = await wallet.available();
    assert (cs == (100: Nat64) and is == (10 : Nat64));
    assert (Funds.refunded(#cycle) == (100 : Nat64) and Funds.refunded(#icpt) == (10 : Nat64));
  };
  do {
    // check funds reset to zero on send
    let (cs, is) = await wallet.available();
    assert (cs == (0: Nat64) and is == (0 : Nat64));
    assert (Funds.refunded(#cycle) == (0 : Nat64) and Funds.refunded(#icpt) == (0 : Nat64));
  };

  do {
    // check funds additive to zero on send
    Funds.add(#cycle, 100);
    Funds.add(#icpt, 10);
    Funds.add(#cycle, 200);
    Funds.add(#icpt, 20);
    let (cs, is) = await wallet.available();
    assert (cs == (300 : Nat64) and is == (30 : Nat64));
    assert (Funds.refunded(#cycle) == (300: Nat64) and Funds.refunded(#icpt) == (30 : Nat64));
  };

  // check funds reset on context switch
  Funds.add(#cycle, 100);
  Funds.add(#icpt, 10);
  await async {
    assert(Funds.available(#cycle) == (100 : Nat64));
    assert(Funds.available(#icpt) == (10 : Nat64));
    // check funds received
    Funds.add(#cycle, 50);
    Funds.add(#icpt, 5);
    let (cs, is) = await wallet.available();
    assert (cs == (50 : Nat64) and is == (5 : Nat64));
    assert (Funds.refunded(#cycle) == (50 : Nat64) and Funds.refunded(#icpt) == (5 : Nat64));

    // add some unconsumed funds
    Funds.add(#cycle, 200);
    Funds.add(#icpt, 20);
  };
  // check refund from await async ...
  assert (Funds.refunded(#cycle) == (100 : Nat64) and Funds.refunded(#icpt) == (10 : Nat64));
  // check unconsumed funds, declared before await, cleared on context switch
  let (cs, is) = await wallet.available();
  assert (cs == (0: Nat64) and is == (0 : Nat64));

 };

 // callback for accepting funds from wallet.
 public func credit(u : Funds.Unit) : async () {
   let b = Funds.balance(u);
   let a = Funds.available(u);
   Funds.accept(u, a);
   if (u == #icpt) {
     assert (Funds.balance(u) == b + a);
   };
 };


};

a.go(); //OR-CALL ingress go "DIDL\x00\x00"

//SKIP run
//SKIP run-ir
//SKIP run-low

