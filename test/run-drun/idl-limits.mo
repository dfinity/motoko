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
   label l while (cnt < 256) {
     let a = Prim.Array_tabulate<Blob>(cnt*16, func i {page});
     let b = to_candid (a);
     if (b.size() > 10_000_000)  break l;
     Prim.debugPrint(debug_show {bytes=b.size()});
     let c1 = Prim.performanceCounter(0);
     let ?_ = from_candid b : ?[Blob];
     let c2 = Prim.performanceCounter(0);
     let diff = c2 - c1;
     Prim.debugPrint(debug_show {
       bytes = b.size();
       MB = cnt;
// too noisy for tests
//       instrs = diff ;
//       instr_per_byte = Prim.nat64ToNat diff / b.size()
     });
     cnt += 1;
   };

}

//SKIP run-low
//SKIP run-ir
//SKIP run
//SKIP ic-ref



