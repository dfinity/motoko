import Prim "mo:prim";

let maxCompactFloat32 = 1073741823.0;
Prim.debugPrint(debug_show (Prim.floatToInt(maxCompactFloat32)));
Prim.debugPrint(debug_show (Prim.floatToInt(maxCompactFloat32 - 1)));
Prim.debugPrint(debug_show (Prim.floatToInt(maxCompactFloat32 + 1)));

let minCompactFloat32 = -1073741824.0;
Prim.debugPrint(debug_show (Prim.floatToInt(minCompactFloat32)));
Prim.debugPrint(debug_show (Prim.floatToInt(minCompactFloat32 - 1)));
Prim.debugPrint(debug_show (Prim.floatToInt(minCompactFloat32 + 1)));

let maxCompactFloat64 = 4611686018427387400.0;
let minNonCompactFloat64 = 4611686018427388000.0;
Prim.debugPrint(debug_show (Prim.floatToInt(maxCompactFloat64)));
Prim.debugPrint(debug_show (Prim.floatToInt(minNonCompactFloat64)));

let minCompactFloat64 = -4611686018427387400.0;
let maxNonCompactFloat64 = -4611686018427388000.0;
Prim.debugPrint(debug_show (Prim.floatToInt(minCompactFloat64)));
Prim.debugPrint(debug_show (Prim.floatToInt(maxNonCompactFloat64)));
