import Prim "mo:prim";
actor {
  Prim.debugPrint ("init'ed");

  var always10 = 10;
  public query func check() : async () { assert (always10 == 10); };
}

//CALL query check "DIDL\x00\x00"
//CALL upgrade
//CALL query check "DIDL\x00\x00"

//SKIP run
//SKIP run-ir
//SKIP run-low
//SKIP comp
