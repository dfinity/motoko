import Prim "mo:⛔";

actor {

  let raw_rand = (actor "aaaaa-aa" : actor { raw_rand : () -> async Blob }).raw_rand;

  var count = 0;
  var max = 10;

  public shared func inc() : async () {
    count := count + 1;
    Prim.debugPrint("count = " # debug_show(count));
  };

  system func heartbeat() : async () {
    if (count < max) {
      ignore inc();
    }
  };

  public shared func go() : async () {
     var attempts = 0;
     while (count < max) {
       ignore await raw_rand(); // yield to scheduler
       attempts += 1;
       if (attempts >= 50 and count == 0)
         throw Prim.error("he's dead Jim");
     }
  };
};

//SKIP run
//SKIP run-low
//SKIP run-ir
////SKIP comp-ref
//CALL ingress go "DIDL\x00\x00"