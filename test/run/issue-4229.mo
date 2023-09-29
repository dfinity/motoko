import Prim  = "mo:prim";
let "\44\49\44\4C\00\01\7F" = to_candid (null);
// \80 (-128) is IDL_EXT_REGION descriptor,
// and should not appear in a valid (pure) Candid type table
let null = from_candid "\44\49\44\4C\00\01\80" : ? Null