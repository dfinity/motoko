import P "mo:prim";
// test polymorphic async compilation

// If we (in future) allow type parameters in async types, then we should enforce that  `T` in async* `T` is a manifest tuple
// so that async arity is invariant under substition. TBR
actor a {

  private func doUnit<T>(t : T) : async* T {
    return t;
  };


  private func go() : async* () {
    let a_star = doUnit<()>(());
    let _ = await* a_star;

  }

};


//CALL ingress go "DIDL\x00\x00"
