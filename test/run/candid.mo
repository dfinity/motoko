// unit
assert (?() == (from_candid(to_candid ()) : ?()));
assert (?() == (from_candid(to_candid (1)) : ?()));
assert (?() == (from_candid(to_candid (1,2)) :?()));


// singleton
assert (?1 == (from_candid(to_candid(1)) : ?Nat));
assert (null == (from_candid(to_candid(-1)) : ?Nat)); // subtype failure
assert (?-1 == (from_candid(to_candid(-1)) : ?Int));

// pairs
assert (?(1,true) == (from_candid(to_candid(1,true)) : ?(Nat,Bool)));
assert (null == (from_candid(to_candid(-1,true)) : ?(Nat,Bool))); // subtype failure
assert (?(-1,true) == (from_candid(to_candid(-1,true)) : ?(Int,Bool)));

// triples
assert (?(1,true,?'a') == (from_candid(to_candid(1,true,'a')) : ?(Nat,Bool,?Char)));
assert (null == (from_candid(to_candid(-1,true,'a')) : ?(Nat,Bool,?Char)));
assert (?(-1,true,?'a') == (from_candid(to_candid(-1,true,'a')) : ?(Int,Bool,?Char)));


// defaulting
assert (?(1,true,null) == (from_candid(to_candid(1,true,'a')) : ?(Nat,Bool,?Text)));
assert (null == (from_candid(to_candid(-1,true,'a')) : ?(Nat,Bool,?Text)));
assert (?(-1,true,null) == (from_candid(to_candid(-1,true,'a')) : ?(Int,Bool,?Text)));

// etc. (please add more)


// traps on invalid candid (check last)

let trap = from_candid("") : ?Any;

//SKIP run
//SKIP run-ir
//SKIP run-low
