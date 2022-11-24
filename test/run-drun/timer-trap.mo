import { debugPrint; error; setTimer } = "mo:⛔";

actor {

  // intentionally omit defining `system func timer()`,
  // relying on the compiler filling in a default implementation

  var count = 0;
  var max = 3;
  let raw_rand = (actor "aaaaa-aa" : actor { raw_rand : () -> async Blob }).raw_rand;

  public shared func go() : async () {
     var attempts = 0;

     ignore setTimer(1, true,
                     func () : async () { count += 1; debugPrint "YEP!" });
     ignore setTimer(1, false,
                     func () : async () { count += 1; debugPrint "EEK!"; assert false });
     ignore setTimer(1, false,
                     func () : async () { count += 1; debugPrint "BEAM!"; throw error("beam me up Scotty!") });

     while (count < max) {
       ignore await raw_rand(); // yield to scheduler
       attempts += 1;
       if (attempts >= 200 and count == 0)
         throw error("he's dead Jim");
     };
     debugPrint(debug_show {count});
  };
};

//SKIP run
//SKIP run-low
//SKIP run-ir

//CALL ingress go "DIDL\x00\x00"
