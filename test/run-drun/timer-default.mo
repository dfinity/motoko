import { debugPrint; error; setTimer } = "mo:⛔";

actor {

  // intentionally omit defining `system func timer()`,
  // relying on the compiler filling in a default implementation

  var count = 0;
  var max = 1;
  let raw_rand = (actor "aaaaa-aa" : actor { raw_rand : () -> async Blob }).raw_rand;

  public shared func go() : async () {
     var attempts = 0;

     ignore setTimer(1_000_000_000, false, func () : async () { count += 1; debugPrint "YEP!" });

     while (count < max) {
       ignore await raw_rand(); // yield to scheduler
       attempts += 1;
       if (attempts >= 200 and count == 0)
         throw error("he's dead Jim");
     };
     debugPrint(debug_show {count});
  };

  func bowm() {
      ignore setTimer(1_000_000_000_000, false, func () : async () { debugPrint "BOWM!" });
  }
};

//SKIP run
//SKIP run-low
//SKIP run-ir

//CALL ingress go "DIDL\x00\x00"
