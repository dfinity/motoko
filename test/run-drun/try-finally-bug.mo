import { debugPrint; error; call_raw; principalOfActor } =  "mo:⛔";

actor A {

    func t0() : async () {
      try { }
//        catch e {}
        finally {
         ignore async {};
       } : ();
    };

    public func go() : async () {
        await t0();
    };

};

//SKIP ic-ref-run

A.go(); //OR-CALL ingress go "DIDL\x00\x00"

