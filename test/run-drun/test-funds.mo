import Prim = "mo:prim";
import Funds = "funds/funds";
import W = "funds/wallet";

actor a {

 let print = Prim.debugPrint;


 public func go() : async () {


  print(debug_show(Funds.balance(#icpt)));
  print(debug_show(Funds.balance(#cycle)));

  print(debug_show(Funds.available(#icpt)));
  print(debug_show(Funds.available(#cycle)));

  print(debug_show(Funds.accept(#icpt, 0)));
  print(debug_show(Funds.accept(#cycle, 0)));

  // set up a wallet with thin-air funds
  Prim.unsafeSetInitialFunds({ num_cycles = 10000000; num_icpt = 1000});
  let wallet = await W();


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
  Funds.transfer(#icpt, amount/2);
  await wallet.credit(#icpt);
  print(debug_show(await wallet.balance(#icpt)));


  print("credit-2");
  // transfer half the amount back to the wallet
  print(debug_show(await wallet.balance(#icpt)));
  Funds.transfer(#icpt, amount/2);
  print("credit");
  await wallet.credit(#icpt);
  print(debug_show(await wallet.balance(#icpt)));


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
//SKIP drun
