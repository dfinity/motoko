import Prim "mo:⛔";

for (check1 in (await async ["effect", "hello", "world"]).vals()) { Prim.debugPrint check1 };

for (check2 in ["hello", "world", "effect"].vals()) { await async { Prim.debugPrint check2 } };

let array = ["hello", "bound", "world"];

for (check3 in (await async array).vals()) { Prim.debugPrint check3 };

for (check4 in array.vals()) { await async { Prim.debugPrint check4 } }
