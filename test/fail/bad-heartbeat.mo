import Prim "mo:⛔";

actor {

  let raw_rand = (actor "aaaaa-aa" : actor { raw_rand : () -> async Blob }).raw_rand;

  var count = 0;
  var max = 10;

  system func heartbeat() : () { // reject, should be async ()
    if (count < max) {
      count += 1;
    }
  };

  public shared func go() : async () {
     var attempts = 0;
     while (count < max) {
       ignore await raw_rand(); // yield to scheduler
       attempts += 1;
       if (attempts >= 50 and count == 0)
         throw Prim.error("he's dead Jim");
     };
     Prim.debugPrint(debug_show {count});
  };
};

