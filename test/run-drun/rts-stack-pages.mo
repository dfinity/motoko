//MOC-FLAG --rts-stack-pages 16
import Prim = "mo:prim";
// tests --rts-stack-pages adjusts stack limit
// test fails with RTS stack underflow with just 16 pages,
// but succeeds with 32 pages (the default).
actor a {

   let x = Prim.stableMemoryGrow(1);
   assert Prim.stableMemorySize() == 1;


   type List<T> = ?(T, List<T>);

   var map : List<(Blob, Blob)> = null;

   var count : Nat32 = 0;

   func fillMB(mb : Nat) : () {
     var c = 1024;
     while (c > 0) {
       count += 1;
       Prim.stableMemoryStoreNat32(0, count);
       var k = Prim.stableMemoryLoadBlob(0, 32);
       var v = Prim.stableMemoryLoadBlob(0, 65536);
       map := ?((k,v), map);
       c -= 1;
     };
     if (Prim.rts_heap_size() < mb * 1024 * 1024) {
      // Difference between incremental and non-incremental GC (due to different object header lengths).
      let toleranceMB = 4;
      Prim.debugPrint(debug_show({heap_MB = Prim.rts_heap_size()/1024/1024/toleranceMB*toleranceMB}));
      fillMB(mb);
     }
   };

   fillMB(768);
   Prim.debugPrint "serializing";
   let blob = to_candid(map);
   Prim.debugPrint "serialized";
   let opt = from_candid(blob) : ?(List<(Blob,Blob)>);
   assert (opt != null);
   // if we got here, deserialization has succeeded
   Prim.debugPrint "deserialized";
   assert false;

}
//SKIP run
//SKIP run-ir
//SKIP run-low
//SKIP ic-ref-run
