actor A {
  public shared func ping<X>() : async<X> Int = async <Y> {666;} <X>; // normal remote function

  public shared func f<X>() : async<X> () = async <Y> {
    // current scope: Y
    var m : ? (async<Y> Int) = null;

    let i = await (async<Z> {
      // current scope: Z
      m := ? (ping<Y> ()); // this seems well-typed according to “Application (Derived Rule)”;
    } <Y>);

    switch (m) {
     case null  assert false;
     case (? a) assert (await a) == 666; // Boom!
    }
  } <X>;

};

A.f() //OR-CALL ingress f 0x4449444C0000

