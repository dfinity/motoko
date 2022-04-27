//MOC-ENV MOC_UNLOCK_PRIM=yesplease
//MOC-FLAG -v
import Prim "mo:⛔";

func serUnit() : Blob = to_candid ();
func deserUnit(x : Blob) : ?() = from_candid x;

func serNats(x: Nat, y: Nat, z: Nat) : Blob = to_candid (x,y,z);
func deserNats(x: Blob) : ?(Nat, Nat, Nat) = from_candid x;

func serBool(x: Bool) : Blob = to_candid (x);
func deserBool(x: Blob) : ?Bool = from_candid x;
 
// func serText(x: Text) : Blob = to_candid (x,x);
// func deserText(x: Blob) : ?(Text,) = from_candid x;

// Prim.debugPrint(debug_show (serUnit ()));
// Prim.debugPrint(debug_show (serNats (1,2,3)));
// Prim.debugPrint(debug_show (serText "Hello World!"));
// Prim.debugPrint(debug_show (serBool true));
// Prim.debugPrint(debug_show (serBool false));

// assert (?() == deserUnit (serUnit ()));
// assert (?"Hello World!" == deserText (serText "Hello World!"));
// assert(?(1,2,3) == (deserNats (serNats (1,2,3)) : ?(Nat,Nat,Nat)));

// assert(?true == deserBool (serBool true));
// assert(?false == deserBool (serBool false));

// assert (?"Hello World!" == (from_candid (to_candid ("Hello World!")) : ?Text));

// assert(?(1,2,3) == (from_candid (to_candid (1,2,3)) : ?(Nat,Nat,Nat)));

//SKIP run
//SKIP run-ir
//SKIP run-low
