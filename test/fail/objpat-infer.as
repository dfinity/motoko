// checks

ignore (switch (object {}) { case {a} 42 });

// checks

for ({} in [object {}].vals()) { print "hey" };

// infers

func foo ({}) : Int = 42;

// infers

shared func baz (a : actor {}) : async Int { 42 };

// call it

ignore (foo (new {}));
ignore (foo (object {}));
ignore (foo (actor {}));

let a = actor { public func bar ({}) : async Nat = async 25 };
ignore (foo a);


ignore (baz (new {}));
ignore (baz (object {}));
ignore (baz (actor {}));

ignore (a.bar (new {}));
ignore (a.bar (object {}));
ignore (a.bar (actor {}));
