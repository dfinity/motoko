//MOC-ENV MOC_UNLOCK_PRIM=yesplease
import Prim "mo:⛔";

func serUnit() : Blob = (prim "serialize" : () -> Blob) ();
func deserUnit(x : Blob) : () = (prim "deserialize" : Blob -> ()) x;

func serNats(x: Nat, y: Nat, z: Nat) : Blob = (prim "serialize" : (Nat,Nat,Nat) -> Blob) (x,y,z);
func deserNats(x: Blob) : (Nat, Nat, Nat) = (prim "deserialize" : Blob -> (Nat,Nat,Nat)) x;

func serText(x: Text) : Blob = (prim "serialize" : Text -> Blob) (x,);
func deserText(x: Blob) : Text = (prim "deserialize" : Blob -> Text) x;

func serBool(x: Bool) : Blob = (prim "serialize" : Bool -> Blob) (x,);
func deserBool(x: Blob) : Bool = (prim "deserialize" : Blob -> Bool) x;


Prim.debugPrint(debug_show (serUnit ()));
Prim.debugPrint(debug_show (serNats (1,2,3)));
Prim.debugPrint(debug_show (serText "Hello World!"));
Prim.debugPrint(debug_show (serBool true));
Prim.debugPrint(debug_show (serBool false));


deserUnit (serUnit ()) : ();
assert ("Hello World!" == deserText (serText "Hello World!"));
// abusing debug_show for easy structural equality
assert(debug_show (1,2,3) == debug_show (deserNats (serNats (1,2,3)) : (Nat,Nat,Nat)));

assert(true == deserBool (serBool true));
assert(false == deserBool (serBool false));

Prim.debugPrint(debug_show (to_candid ()));
Prim.debugPrint(debug_show (to_candid (1,2,3)));
Prim.debugPrint(debug_show (to_candid ("Hello World!")));
from_candid (to_candid ()) : ();

//assert ("Hello World!" == (from_candid (to_candid ("Hello World!")) : (Text,)));

// abusing debug_show for easy structural equality
//assert(debug_show (1,2,3) == debug_show (from_candid (to_candid (1,2,3)) : (Nat,Nat,Nat)));

//SKIP run
//SKIP run-ir
//SKIP run-low
