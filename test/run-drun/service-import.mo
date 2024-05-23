//MOC-FLAG --actor-idl service-import
//MOC-FLAG --actor-alias self rwlgt-iiaaa-aaaaa-aaaaa-cai

// this imports our own IDL, stored in actor-import

// currently hard-codes the ic-ref self id
// once we have actor aliases we can let run.sh set an alias.
import Prim = "mo:⛔";
import imported1 "ic:rwlgt-iiaaa-aaaaa-aaaaa-cai";
import imported2 "canister:self";
actor a {
  public func go() : async (actor {}) = async {
    Prim.debugPrint (debug_show (Prim.principalOfActor a));
    imported1;
  };

  public func go2() : async (actor {}) = async await (imported1.go());
  public func go3() : async (actor {}) = async await (imported2.go());
};
//CALL ingress go RElETAAA
//CALL ingress go2 RElETAAA
//CALL ingress go3 RElETAAA


//SKIP run
//SKIP run-ir
//SKIP run-low
