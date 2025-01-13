// test migration function restrictions

actor [()] a = { // reject, not a function
};

actor [func <T>(x:T) : T {x}] b = { // reject, a generic function
};

actor [func () : {} {{}}] c = { // reject, domain is not a record
};

actor [func ({}) : () {}] d = { // reject, co-domain is not a record
};

actor [func ({f:()->()}) : () {}] e = { // reject domain is unstable
};

actor [func () : {f:()->()}{ {f = func(){}} }]
  f = { // reject, co-domain is unstable
   stable let f : Any = ()
};

actor [(func () : ({} -> {}) {}) ()] // reject, not static
  g = {
   stable let f : Any = ()
};
