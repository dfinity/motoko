import Prim "mo:prim";
Prim.debugPrint(debug_show (debug_serialize ()));
Prim.debugPrint(debug_show (debug_serialize (1,2,3)));
Prim.debugPrint(debug_show (debug_serialize "Hello World!"));
debug_deserialize (debug_serialize ()) : ();
assert ("Hello World!" == (debug_deserialize (debug_serialize "Hello World!") : Text));
// abusing debug_show for easy structural equality
assert(debug_show (1,2,3) == debug_show (debug_deserialize (debug_serialize (1,2,3)) : (Nat,Nat,Nat)));

//SKIP run
//SKIP run-ir
//SKIP run-low
