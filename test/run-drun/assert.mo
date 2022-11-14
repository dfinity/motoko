import Lib "assert/Lib";
actor a {

  public func go() : async () {
    Lib.fail();
  };

};
a.go(); //OR-CALL ingress go "DIDL\x00\x00"
