import Prim = "mo:prim";
import Funds = "funds/funds";
import WalletLib = "funds/wallet";

actor a {

 let print = Prim.debugPrint;


 public func go() : async () {

  print(debug_show(Funds.balance(#icpt)));
  //print(debug_show(Funds.balance(#cycle))); // to volatile to show

  print(debug_show(Funds.available(#icpt)));
  print(debug_show(Funds.available(#cycle)));

  print(debug_show(Funds.accept(#icpt, 0)));
  print(debug_show(Funds.accept(#cycle, 0)));

  let wallet : WalletLib.Wallet = await WalletLib.Wallet();
  await wallet.show();
  print ("setting funds");
  await Funds.dev_set_funds(wallet, 1_000_000_000_000_000, 1000);
  await wallet.show();

  // debit from the waller, crediting this actor via callback
  let amount : Nat64 = 100;
  print ("debit");
  print("balance" # debug_show(Funds.balance(#icpt)));
  let b = Funds.balance(#icpt);
  await wallet.debit(#icpt, amount, credit);
  print("balance" # debug_show(Funds.balance(#icpt)));
  assert (Funds.balance(#icpt) == b + amount);

  print("credit-1");
  // transfer half the amount back to the wallet
  print(debug_show(await wallet.balance(#icpt)));
  Funds.add(#icpt, amount/4);
  await wallet.credit(#icpt);
  print("refunded: " # debug_show(Funds.refunded(#icpt)));
  print(debug_show(await wallet.balance(#icpt)));


  print("credit-2");
  // transfer half the amount back to the wallet
  print(debug_show(await wallet.balance(#icpt)));
  Funds.add(#icpt, amount/4);
  await wallet.credit(#icpt);
  print("refunded: " # debug_show(Funds.refunded(#icpt)));
  print(debug_show(await wallet.balance(#icpt)));


  print("refund");
  // transfer half the amount back to the wallet
  print(debug_show(await wallet.balance(#icpt)));
  Funds.add(#icpt, amount/2);
  await wallet.refund(#icpt, amount/4);
  print("refunded: " # debug_show(Funds.refunded(#icpt)));
  print(debug_show(await wallet.balance(#icpt)));


  // issue a bunch of refund requests, await them in reverse and check the refunds are as expected.
  func testRefunds(n : Nat64) : async () {
     if (n == (0 : Nat64)) { return };
     Funds.add(#icpt, n);
     print("refund(" # debug_show(n) # ")");
     let a = wallet.refund(#icpt, n);
     await testRefunds( n - (1 : Nat64));
     await a;
     print("refunded: " # debug_show(Funds.refunded(#icpt)));
     assert (Funds.refunded(#icpt) == n);
  };

  await testRefunds(5);

  // try to accept funds that aren't available
  // this should trap
  print(debug_show(Funds.accept(#icpt, 1)));

 };

 // callback for accepting funds from wallet.
 public func credit(u : Funds.Unit) : async () {
   print("credit:balance " # debug_show(Funds.balance(u)));
   let b = Funds.balance(u);
   let a = Funds.available(u);
   Funds.accept(u, a);
   print("credit:balance " # debug_show(Funds.balance(u)));
   if (u == #icpt) {
     assert (Funds.balance(u) == b + a);
   };
 };


};

a.go(); //OR-CALL ingress go "DIDL\x00\x00"

//SKIP run
//SKIP run-ir
//SKIP run-low

