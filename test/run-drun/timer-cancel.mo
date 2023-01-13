import { debugPrint; error; setTimer; cancelTimer } = "mo:⛔";

actor {

  let seconds = 5;
  var counter = 0;
  var t : Nat = 0;

  private func remind() : async () {
    debugPrint("TICK!");
    counter += 1;
  };

  public func get() : async Int {
    // cancel exactly once
    if (t > 0 and counter == 3) { debugPrint("CANCELLING!"); cancelTimer t; t := 0 };
    return counter
  };

  let second : Nat64 = 1_000_000_000;

  ignore setTimer(second, false,
    func () : async () {
      t := setTimer(2 * second, true, remind);
      await remind();
  });

  var max = 4;
  let raw_rand = (actor "aaaaa-aa" : actor { raw_rand : () -> async Blob }).raw_rand;

  public shared func go() : async () {
     var attempts = 0;

     while (counter < max) {
       ignore await get();
       ignore await raw_rand(); // yield to scheduler
       if (counter == 3) attempts += 1;
       if (attempts >= 200)
         counter += 1;
     };
     debugPrint(debug_show {counter});
  };
};

//SKIP run
//SKIP run-low
//SKIP run-ir

//CALL ingress go "DIDL\x00\x00"
