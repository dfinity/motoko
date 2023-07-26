//MOC-FLAG 
import P "mo:⛔";
import StableMemory "stable-mem/StableMemory";

// test for correct out-of-bounds detection.
// Both ok() and bad() should fail for the same reason (but don't)
// ok() reports RTS error: region access out of bounds, caught by RTS
// bad() reports: "stable memory out of bounds", not caught by RTS but by IC

// I think one could exploit this bound check failure to break isolation between regions...

actor {
    let 0 = StableMemory.grow(1);

    public func ok() : async Blob {
        StableMemory.loadBlob(0xFFFF_FFFF_FFFF_FFFF, 0);
    };

    public func bad() : async Blob {
        StableMemory.loadBlob(0xFFFF_FFFF_FFFF_FFFF, 2);
    };

    public func go() : async () {

     let b1 =
       try await ok()
       catch e {
         P.debugPrint(P.errorMessage e);
       };

     let b2 =
       try await bad()
       catch e {
         P.debugPrint(P.errorMessage e);
       }

    }

}

//SKIP run
//SKIP run-low
//SKIP run-ir
// too slow on ic-ref-run:
//SKIP comp-ref

//CALL ingress go "DIDL\x00\x00"


