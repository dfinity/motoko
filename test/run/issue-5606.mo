let candid = to_candid(5, 6);
assert "DIDL\00\02\7d\7d\05\06" == candid;
let ?(5, 6, null) : ?(Nat, Nat, Null) = from_candid(candid) else loop ();

//SKIP run
//SKIP run-ir
//SKIP run-low
