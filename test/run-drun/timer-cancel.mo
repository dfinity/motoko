//MOC-ENV MOC_UNLOCK_PRIM=yesplease
//MOC-FLAG --package base /Users/ggreif/motoko-base/src

import { debugPrint; error } = "mo:⛔";
import { abs } = "mo:base/Int";
import { now } = "mo:base/Time";
import { setTimer; recurringTimer; cancelTimer } = "mo:base/Timer";

actor {


 let solarYearSeconds = 5;
  var counter = 0;

  var t : Nat = 0;

  private func remind() : async () {
    debugPrint("Happy New Year!");
    counter += 1;
    if (counter == 6) cancelTimer t;
  };

  public func get() : async Int {
    // assert t > 0;
    //if (t > 0 and counter == 3) cancelTimer t;
    return counter
  };

  ignore setTimer(#seconds (solarYearSeconds - abs(now() / 1_000_000_000) % solarYearSeconds),
    func () : async () {
      t := recurringTimer(#seconds solarYearSeconds, remind);
      await remind();
  });


  //system func timer(setGlobalTimer : Nat64 -> ()) : async () {
  //    await @timer_helper();
  //};

  var max = 10;
  let raw_rand = (actor "aaaaa-aa" : actor { raw_rand : () -> async Blob }).raw_rand;
  let second : Nat64 = 1_000_000_000;

  public shared func go() : async () {
     var attempts = 0;

     while (counter < max) {
       ignore await get();
       ignore await raw_rand(); // yield to scheduler
       attempts += 1;
       if (attempts >= 500 and counter == 0)
         throw error("he's dead Jim");
     };
     debugPrint(debug_show {counter});
  };
};

//SKIP run
//SKIP run-low
//SKIP run-ir
//SKIP ic-ref-run

//CALL ingress go "DIDL\x00\x00"
