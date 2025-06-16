import {debugPrint = print; cyclesBurn = burn; cyclesBalance} = "mo:⛔";

actor {

  public func test() : async () {
    let balance = cyclesBalance();
    print (debug_show balance);
    ignore burn (balance - 85_000_000);
    print (debug_show cyclesBalance());
  }
}
