import Cycles = "piggy-bank/ExperimentalCycles";
//import Cycles = "mo:base/ExperimentalCycles";
import Lib = "piggy-bank/PiggyBank";

actor Alice {

  public func test() : async () {

    let porky = await Lib.PiggyBank(1000_000, Alice.credit);
    assert (0 == (await porky.getSavings()));

    await { Cycles.add(1000); porky.deposit() };
    assert (1000 == (await porky.getSavings()));

    await porky.withdraw(500);
    assert (500 == (await porky.getSavings()));

    await porky.withdraw(500);
    assert (0 == (await porky.getSavings()));

    await { Cycles.add(2000_000); porky.deposit() };
    let refund = Cycles.refunded();
    assert (1000_000 == refund);
    assert (1000_000 == (await porky.getSavings()));

  };

   // callback for accepting funds from PiggyBank
  public func credit() : async () {
    ignore Cycles.accept(Cycles.available());
  }

};

Alice.test(); //OR-CALL ingress test "DIDL\x00\x00"

//SKIP run
//SKIP run-ir
//SKIP run-low
//SKIP ic-ref-run
