import imported "ic:000000000000040054";
actor a {
  public func go() : async (actor {}) = async imported;
};
//CALL ingress go "DIDL\x00\x00"


//SKIP run
//SKIP run-ir
//SKIP run-low

