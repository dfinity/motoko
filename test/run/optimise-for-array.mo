import Prim "mo:⛔";

for (s in ["hello", "world"].vals()) { Prim.debugPrint s };

for (s in [var "hello", "mutable", "world"].vals()) { Prim.debugPrint s };

let arr = ["hello", "immutable", "world"];
for (s in arr.vals()) { Prim.debugPrint s };

