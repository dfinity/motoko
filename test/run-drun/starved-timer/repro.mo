import {debugPrint = print; cyclesBurn = burn; cyclesBalance; setTimer} = "mo:⛔";

actor {

  var count = 0;

  func tick() : async () { print ("tick: " # debug_show count); count += 1 };

  public func test() : async () {
    let balance = cyclesBalance();
    print (debug_show balance);
    //ignore burn (balance - 1805_000_000);
    print (debug_show cyclesBalance());
  };

  public func poll() : async () {
      while (count < 3) await async ();
  };

  ignore setTimer(2_000_000_000, true, tick)
}
