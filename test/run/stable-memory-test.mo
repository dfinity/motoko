import Prim "mo:⛔";
import StableMemory "stable-mem/StableMemory";

let 0 = StableMemory.size();
let 0 = StableMemory.grow(64);
assert (64 == StableMemory.size());

do {
   Prim.debugPrint("Nat8");
   type T = Nat8;
   let size : Nat64 = 1;
   let mod : Nat64 = 256;
   let load = StableMemory.loadNat8;
   let store = StableMemory.storeNat8;
   func conv(n : Nat64) : Nat8 { Prim.natToNat8(Prim.nat64ToNat(n % mod)) };
   let max = StableMemory.size()*65536;
   var i : Nat64 = 0;
   while(i < max) {
     store(i, conv(i));
     i += size;
   };
   i := 0;
   while(i < max) {
     assert (conv(i) == load(i));
     i += size;
   };
};

do {
   Prim.debugPrint("Nat16");
   type T = Nat16;
   let size : Nat64 = 2;
   let mod : Nat64 = 65536;
   let load = StableMemory.loadNat16;
   let store = StableMemory.storeNat16;
   func conv(n : Nat64) : Nat16 { Prim.natToNat16(Prim.nat64ToNat(n % mod)) };
   let max = StableMemory.size()*65536;
   var i : Nat64 = 0;
   while(i < max) {
     store(i, conv(i));
     i += size;
   };
   i := 0;
   while(i < max) {
     assert (conv(i) == load(i));
     i += size;
   };
};

do {
   Prim.debugPrint("Nat32");
   type T = Nat32;
   let size : Nat64 = 4;
   let mod : Nat64 = 2**32;
   let load = StableMemory.loadNat32;
   let store = StableMemory.storeNat32;
   func conv(n : Nat64) : Nat32 { Prim.natToNat32(Prim.nat64ToNat(n % mod)) };
   let max = StableMemory.size()*65536;
   var i : Nat64 = 0;
   while(i < max) {
     store(i, conv(i));
     i += size;
   };
   i := 0;
   while(i < max) {
     assert (conv(i) == load(i));
     i += size;
   };
};

do {
   Prim.debugPrint("Nat64");
   type T = Nat8;
   let size : Nat64 = 8;
   let load = StableMemory.loadNat64;
   let store = StableMemory.storeNat64;
   func conv(n : Nat64) : Nat64 { n };
   let max = StableMemory.size()*65536;
   var i : Nat64 = 0;
   while(i < max) {
     store(i, conv(i));
     i += size;
   };
   i := 0;
   while(i < max) {
     assert (conv(i) == load(i));
     i += size;
   };
};

do {
   Prim.debugPrint("Int8");
   type T = Int8;
   let size : Nat64 = 1;
   let mod : Nat64 = 256;
   let load = StableMemory.loadInt8;
   let store = StableMemory.storeInt8;
   func conv(n : Nat64) : Int8 { Prim.intToInt8(Prim.nat64ToNat(n % mod) - 128) };
   let max = StableMemory.size()*65536;
   var i : Nat64 = 0;
   while(i < max) {
     store(i, conv(i));
     i += size;
   };
   i := 0;
   while(i < max) {
     assert (conv(i) == load(i));
     i += size;
   };
};

do {
   Prim.debugPrint("Int16");
   type T = Int16;
   let size : Nat64 = 2;
   let mod : Nat64 = 65536;
   let load = StableMemory.loadInt16;
   let store = StableMemory.storeInt16;
   func conv(n : Nat64) : Int16 { Prim.intToInt16(Prim.nat64ToNat(n % mod) - 32768 )};
   let max = StableMemory.size()*65536;
   var i : Nat64 = 0;
   while(i < max) {
     store(i, conv(i));
     i += size;
   };
   i := 0;
   while(i < max) {
     assert (conv(i) == load(i));
     i += size;
   };
};

do {
   Prim.debugPrint("Int32");
   type T = Int32;
   let size : Nat64 = 4;
   let mod : Nat64 = 2**32;
   let load = StableMemory.loadInt32;
   let store = StableMemory.storeInt32;
   func conv(n : Nat64) : Int32 { Prim.intToInt32(Prim.nat64ToNat(n % mod) - 2147483648) };
   let max = StableMemory.size()*65536;
   var i : Nat64 = 0;
   while(i < max) {
     store(i, conv(i));
     i += size;
   };
   i := 0;
   while(i < max) {
     assert (conv(i) == load(i));
     i += size;
   };
};

do {
   Prim.debugPrint("Int64");
   type T = Int8;
   let size : Nat64 = 8;
   let load = StableMemory.loadInt64;
   let store = StableMemory.storeInt64;
   func conv(n : Nat64) : Int64 { Prim.nat64ToInt64(n)+(-9223372036854775808) };
   let max = StableMemory.size()*65536;
   var i : Nat64 = 0;
   while(i < max) {
     store(i, conv(i));
     i += size;
   };
   i := 0;
   while(i < max) {
     assert (conv(i) == load(i));
     i += size;
   };
};

do {
   Prim.debugPrint("Float");
   type T = Int8;
   let size : Nat64 = 8;
   let load = StableMemory.loadFloat;
   let store = StableMemory.storeFloat;
   func conv(n : Nat64) : Float { Prim.int64ToFloat(Prim.nat64ToInt64(n)+(-9223372036854775808)) };
   let max = StableMemory.size()*65536;
   var i : Nat64 = 0;
   while(i < max) {
     store(i, conv(i));
     i += size;
   };
   i := 0;
   while(i < max) {
     assert (conv(i) == load(i));
     i += size;
   };
};

do {
   Prim.debugPrint("Blob");
   type T = Int8;
   let load = StableMemory.loadBlob;
   let store = StableMemory.storeBlob;
   func conv(n : Nat64) : Blob { load(0, Prim.nat64ToNat(n)) };
   let max : Nat64 = StableMemory.size()*65536;
   var i : Nat64 = 1;
   while(i < max) {
     let b = conv(i);
     store(0, b);
     assert (b == load(0, Prim.nat64ToNat(i)));
     i := i * 2;
   };
};

//SKIP run-low
//SKIP run
//SKIP run-ir
