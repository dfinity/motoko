// test migration function restrictions

actor (with migration = ()) a = { // reject, not a function
};

actor (with migration = func <T>(x:T) : T {x}) b = { // reject, a generic function
};

actor (with migration = func () : {} {{}}) c = { // reject, domain is not a record
};

actor (with migration = func ({}) : () {}) d = { // reject, co-domain is not a record
};

actor (with migration = func ({f:()->()}) : () {}) e = { // reject domain is unstable
};

actor
  (with migration = func () : {f:()->()}{ {f = func(){}} })
  f = { // reject, co-domain is unstable
   stable let f : Any = ()
};

actor (with migration = (func () : ({} -> {}) {}) ()) // reject, not static
  g = {
   stable let f : Any = ()
};
