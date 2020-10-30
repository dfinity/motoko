import Counters "Counters";

actor CountToTen {
  public func CountToTen() : async () {
    let C : Counters.Counter = await Counters.Counter(1);
    while ((await C.read()) < 10) {
      await C.inc();
    };
  }
}
