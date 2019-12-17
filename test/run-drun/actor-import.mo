//MOC-FLAG  --actor-idl actor-import

// this imports our own IDL, stored in actor-import

// currently hard-codes the ic-stub self id
// once we have actor aliases we can let run.sh set an alias.

import imported "ic:000000000000040054";
actor a {
  public func go() : async (actor {}) = async imported;
  public func go2() : async (actor {}) = async await (imported.go());
};
//CALL ingress go "DIDL\x00\x00"
//CALL ingress go2 "DIDL\x00\x00"


//SKIP run
//SKIP run-ir
//SKIP run-low
//SKIP drun-run

