import Prim "mo:prim";
actor {
  Prim.debugPrint ("init'ed");

  stable var always10 = 10;
  stable var c = 1;
  public func inc() { c += 1; };
  public query func check(n : Int) : async () {
    assert (c == n);
    assert (always10 == 10);
  };
}

//CALL query check "DIDL\x00\x01\x7d\x01"
//CALL ingress inc "DIDL\x00\x00"
//CALL query check "DIDL\x00\x01\x7d\x02"
//CALL ingress inc "DIDL\x00\x00"
//CALL query check "DIDL\x00\x01\x7d\x03"
//CALL upgrade
//CALL query check "DIDL\x00\x01\x7d\x03"
//CALL ingress inc "DIDL\x00\x00"
//CALL query check "DIDL\x00\x01\x7d\x04"

//SKIP run
//SKIP run-ir
//SKIP run-low
