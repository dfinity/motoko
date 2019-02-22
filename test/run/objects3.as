type Q = {var this : ?Q; x : Nat};
let q : Q = new {var this : ?Q = null; x = 4};
q.this := ?q;
func unopt<T>(x : ?T) : T = switch x { case (?y) y; case _ unopt<T>(x) };
assert(unopt<Q>(unopt<Q>(q.this).this).x == 4);
