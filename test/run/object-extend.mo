import Prim "mo:⛔";

let b = { b = 6 };
Prim.debugPrint (debug_show { a = 8 in b });
Prim.debugPrint (debug_show { b = 8 in b });
Prim.debugPrint (debug_show { a = 8 in { b = 6; c = "C" } });
Prim.debugPrint (debug_show { a = 8; b = 6 in { c = 'C'; d = "D" } })
