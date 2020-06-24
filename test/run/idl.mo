import Prim "mo:prim";
Prim.debugPrint(debug_show (debug_serialize (1,2,3)));
Prim.debugPrint(debug_show (debug_serialize "Hello World!"));
assert ("Hello World!" == (debug_deserialize (debug_serialize "Hello World!") : Text));

//SKIP run
//SKIP run-ir
//SKIP run-low
