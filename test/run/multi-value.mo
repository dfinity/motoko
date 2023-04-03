func returns_tuple() : (Nat, Nat) = (1,2);

assert ((if true { returns_tuple() } else { returns_tuple() }) == (1,2));
