import P "mo:⛔";
import SM "stable-mem/StableMemory";

// Our users may not thank us that we only preserve sharing for mutable data, but nothing else.
actor {

  ignore SM.grow(1);

  let page : Blob = SM.loadBlob(0,65536);
  assert (page.size() == 65536);

  public query func overflow() : async [Blob] {
    P.Array_tabulate<Blob>(65536,func _ { page });
  };

  public query func overflowOpt() : async (?[Blob]){
    ? P.Array_tabulate<Blob>(65536,func _ { page });
  };

}

//SKIP run
//SKIP run-low
//SKIP run-ir

//OR-CALL query overflow "DIDL\x00\x00"
//OR-CALL query overflowOpt "DIDL\x00\x00"

