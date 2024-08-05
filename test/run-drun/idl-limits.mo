import Region "stable-region/Region";
import Prim "mo:prim";

// shows that we can decode up to 15MB of coarse grained data before
// exceeding the candid decoding limit
actor {

   let r = Region.new();
   let pages : Nat64 = 1;
   let 0 = Region.grow(r, pages);
   let page = Region.loadBlob(r, 0, Prim.nat64ToNat(pages*65536-1));

   var cnt = 0;
   while (cnt < 32) {
     let a = Prim.Array_tabulate<Blob>(cnt*16, func i {page});
     let b = to_candid (a);
     let ?_ = from_candid b : ?[Blob];
     Prim.debugPrint(debug_show {MB=cnt});
     cnt += 1;
   };

}

//SKIP run-low
//SKIP run-ir
//SKIP run
//SKIP ic-ref



