/* test actor import of eponymous types in separate candid files */
//MOC-FLAG --actor-idl issue-3103

import A "ic:lg264-qjkae";
import B "ic:aaaaa-aa";

actor a {
  public func go() : async () {
     let _ : Bool = await A.getBool(); // rejected, should be accepted
     let _ : Int = await B.getInt();
  };
}

////CALL ingress go "DIDL\x00\x00"


//SKIP run
//SKIP run-ir
//SKIP run-low

// Skip running on drun for now; hard to pass a `--actor-alias` that works for
// both drun and ic-ref-run
//SKIP comp

