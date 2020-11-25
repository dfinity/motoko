import Cycles = "mo:base/ExperimentalCycles";
import Lib = "PiggyBank";

actor Alice {

  public func test() : async () {

    let porky = {
      Cycles.add(50_000_000);
      await Lib.PiggyBank(Alice.credit, 1_000_000_000);
    };
    assert (0 == (await porky.getSavings()));

    await { Cycles.add(1_000_000); porky.deposit() };
    assert (1_000_000 == (await porky.getSavings()));

    await porky.withdraw(500_000);
    assert (500_000 == (await porky.getSavings()));

    await porky.withdraw(500_000);
    assert (0 == (await porky.getSavings()));

    await { Cycles.add(2_000_000_000); porky.deposit() };
    let refund = Cycles.refunded();
    assert (1_000_000_000 == refund);
    assert (1_000_000_000 == (await porky.getSavings()));

  };

  // Callback for accepting cycles from PiggyBank
  public func credit() : async () {
    let available = Cycles.available();
    let accepted = Cycles.accept(available);
    assert (accepted == available);
  }

};
