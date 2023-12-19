import Prim "mo:⛔";
import Region "stable-region/Region";

let r = Region.new();
let 0 = Region.grow(r, 1);

var i : Nat32 = 0;
while(i <= 0xFFFF_FFFF) {
     Region.storeNat32(r, 0, i);
     let i32 = Region.loadNat32(r, 0);
     let i64 = Region.loadNat64(r, 0);
     if (i32 != i or Prim.nat64ToNat32(i64) != i) {
       Prim.debugPrint(debug_show({i;i64;i32;bytes=Region.loadBlob(r,0,4)}));
       assert(false);
     };
     i += 1;
   };

//SKIP run-low
//SKIP run
//SKIP run-ir
