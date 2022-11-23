import { debugPrint; error; setTimer; cancelTimer } = "mo:⛔";

actor {

  // intentionally omit defining `system func timer()`,
  // relying on the compiler filling in a default implementation

  var count = 0;
  var max = 10;
  let raw_rand = (actor "aaaaa-aa" : actor { raw_rand : () -> async Blob }).raw_rand;

  public shared func go() : async () {
     var attempts = 0;

     let id1 = setTimer(1, false, func () : async () { count += 1; debugPrint "YEP!" });
     let id2 = setTimer(2, true, func () : async () { count += 1; debugPrint "DIM!" });
     let id3 = setTimer(3, false, func () : async () {
         count += 1;
         debugPrint "ROOK!";
         ignore setTimer(1, true, func () : async () { count += 1; debugPrint "BATT!" })
     });

     while (count < max) {
       ignore await raw_rand(); // yield to scheduler
       attempts += 1;
       if (count > 5) { cancelTimer id2 };
       if (attempts >= 100 and count == 0)
         throw error("he's dead Jim");
     };
     debugPrint(debug_show {count});
  };
};

//SKIP run
//SKIP run-low
//SKIP run-ir
//SKIP ic-ref-run

//CALL ingress go "DIDL\x00\x00"
