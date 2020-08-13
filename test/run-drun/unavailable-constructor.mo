func f() : async () { ignore await C(); };

actor class C() {

  let _ : Any = C;

  public shared ctxt func m () : async () {
     let _ : actor {} = await C();
  };

};

//contains features that can't be lowered (toplevel awaits)
//SKIP run-low

ignore (await C()).m() //OR-CALL ingress m 0x4449444C0000

