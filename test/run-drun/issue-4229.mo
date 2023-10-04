import Prim  = "mo:prim";
Prim.debugPrint(debug_show(to_candid(null)));
assert "\44\49\44\4C\00\01\7F" == to_candid (null);
// \80\7F (-128) is IDL_EXT_REGION descriptor,
// and should not appear in a valid (pure) Candid type table
assert ?null == (from_candid "\44\49\44\4C\00\01\7F" : ?Null);
ignore from_candid "\44\49\44\4C\00\01\80\7F" : ?Null; // should trap
assert false; // ohoh

//SKIP run
//SKIP run-low
//SKIP run-ir
