import Funds = "piggy-bank/ExperimentalFunds";
//import Funds = "mo:base/ExperimentalFunds";
import Lib = "piggy-bank/PiggyBank";

actor Owner {

  public func test() : async () {

    let porky = await Lib.PiggyBank(Owner.credit, #cycle, 1000_000);
    assert (0 == (await porky.getSavings()));

    await { Funds.add(#cycle, 1000); porky.deposit() };
    assert (1000 == (await porky.getSavings()));

    await porky.withdraw(500);
    assert (500 == (await porky.getSavings()));

    await porky.withdraw(500);
    assert (0 == (await porky.getSavings()));

    await { Funds.add(#cycle, 2000_000); porky.deposit() };
    let refund = Funds.refunded(#cycle);
    assert (1000_000 == refund);
    assert (1000_000 == (await porky.getSavings()));

  };

   // callback for accepting funds from PiggyBank
  public func credit() : async () {
    Funds.accept(#cycle, Funds.available(#cycle));
  }

};

Owner.test(); //OR-CALL ingress test "DIDL\x00\x00"

//SKIP run
//SKIP run-ir
//SKIP run-low
//SKIP ic-ref-run
