import Counters "Counters";

actor CountToTen {
  public func CountToTen() : async () {
    let c : Counters.Counter = await Counters.Counter(1);
    while ((await c.get()) < 10) {
      await c.inc();
    };
  }
}
