//MOC-FLAG --incremental-gc
import Prim "mo:prim";

actor {

   stable var version = 0;

   version += 1;
   func getStats() : Text {
     debug_show ({
         version = version;
         rts_memory_size = Prim.rts_memory_size();
         rts_heap_size = Prim.rts_heap_size();
         rts_stable_memory_size = Prim.rts_stable_memory_size();
       })
    };
    let blobSize = 512 * 1024 * 1024;
    let stablePageSize = 64 * 1024;
    if (Prim.stableMemorySize() == 0) {
      ignore Prim.stableMemoryGrow(Prim.natToNat64(blobSize / stablePageSize));
    };

    stable var blob = Prim.stableMemoryLoadBlob(0, blobSize);
    Prim.debugPrint (getStats ());

    system func preupgrade() {
    };

    system func postupgrade() {
    };
    
};
//SKIP run
//SKIP run-ir
//SKIP run-low

//CALL upgrade
//CALL upgrade
//CALL upgrade
//CALL upgrade

