//MOC-ENV MOC_UNLOCK_PRIM=yesplease
import Prim "mo:⛔";

func deserUnit(x : Blob) : () = (prim "deserialize" : Blob -> ()) x;
deserUnit "DIDL\01\6d\70\01\00\f0\f0\f0\f0\08";

//SKIP run
//SKIP run-ir
//SKIP run-low
