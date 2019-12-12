actor A {
  public shared func ping<X>() : async<X> Int = async <Y> {666;} <X>; // normal remote function

  public shared func f<X>() : async<X> (Int,Int) = async <Y> {
    // current scope: Y
    var a : (async<Y> Int) = async 0;

    await (async<Z> {
      // current scope: Z
      a :=  ping<Y> (); // this seems well-typed according to “Application (Derived Rule)”;
    } <Y>);

    let i = await a;
    assert i == 666; // Boom!
    return (0,0)
  } <X>;

  public shared func Test() : async () {
    let (0,0) = await f()
  };

};

A.Test() //OR-CALL ingress Test 0x4449444C0000

