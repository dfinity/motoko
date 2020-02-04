//MOC-FLAG --actor-idl actor-import
//MOC-FLAG --actor-alias self ic:2A012B

// this imports our own IDL, stored in actor-import

// currently hard-codes the ic-stub self id
// once we have actor aliases we can let run.sh set an alias.

import imported1 "ic:2A012B";
import imported2 "canister:self";
actor a {
  public func go() : async (actor {}) = async imported1;
  public func go2() : async (actor {}) = async await (imported1.go());
  public func go3() : async (actor {}) = async await (imported2.go());
};
//CALL ingress go "DIDL\x00\x00"
//CALL ingress go2 "DIDL\x00\x00"
//CALL ingress go3 "DIDL\x00\x00"


//SKIP run
//SKIP run-ir
//SKIP run-low

