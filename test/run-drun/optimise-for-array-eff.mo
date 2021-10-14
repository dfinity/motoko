import Prim "mo:⛔";

//for (check4 in (await async {["hello", "immutable", "world"]}).vals()) { Prim.debugPrint check4 };

for (check5 in ["hello", "immutable", "world"].vals()) { await async { Prim.debugPrint check5 } }
