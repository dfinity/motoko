import Prim "mo:prim";
actor a {

  let initial_caller = Prim.caller();

  public query func caller() : async Principal {
    initial_caller;
  };
};

//CALL query caller 0x4449444C0000

//SKIP run
//SKIP run-ir
//SKIP run-low

