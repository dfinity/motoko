// checks

ignore (switch (shared {}) { case {a} 42 });

// checks

for ({} in [shared {}].vals()) { print "hey" };

// infers

func foo ({}) : Int = 42;

// infers

shared func baz ({}) : Int = 42;

// call it

ignore (foo (new {}));
ignore (foo (shared {}));
ignore (foo (actor {}));

let a = actor { bar ({}) : async Nat = async 25 };
ignore (foo a);


ignore (baz (new {}));
ignore (baz (shared {}));
ignore (baz (actor {}));

ignore (a.bar (new {}));
ignore (a.bar (shared {}));
ignore (a.bar (actor {}));
