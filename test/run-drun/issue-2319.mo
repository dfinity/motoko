//MOC-FLAG --actor-idl issue-2319
//MOC-FLAG --actor-alias class lg264-qjkae

import imported1 "ic:lg264-qjkae";
import imported2 "canister:class";
actor a {
  public func go() : async (actor {}) = async imported1;
};

//SKIP run
//SKIP run-ir
//SKIP run-low

// Skip running on drun for now; hard to pass a `--actor-alias` that works for
// both drun and ic-ref-run
//SKIP comp

