// checks

ignore (switch (shared {}) { case {a} 42 });

// checks

for ({} in [shared {}].vals()) { print "hey" };

// infers

func foo {} : Int = 42;


// call it

ignore (foo (shared {}))
