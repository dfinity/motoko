import P "mo:⛔";
import SM "stable-mem/StableMemory";

// Our users may not thank us that we only preserve sharing for mutable data, but nothing else.
actor {

  ignore SM.grow(1);

  let page : Blob = SM.loadBlob(0,65536);
  assert (page.size() == 65536);

  stable
  let a : [Blob] = P.Array_tabulate<Blob>(65536,func _ { page });

  system func preupgrade() {
   P.debugPrint("upgrading...");
  };
}

//SKIP run
//SKIP run-low
//SKIP run-ir

//CALL upgrade ""

