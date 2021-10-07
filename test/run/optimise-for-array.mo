import Prim "mo:⛔";

for (s in ["hello", "world"].vals()) { Prim.debugPrint s };

for (s in [var "hello", "mutable", "world"].vals()) { Prim.debugPrint s }
