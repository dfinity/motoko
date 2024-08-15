//MOC-FLAG --actor-idl issue-4625
//MOC-FLAG --actor-alias call lg264-qjkae
import lib "issue-4625/lib";
import call "canister:call";

actor {
  type T = actor {
    f(x : lib.X) : async ();
  };

  let z : T = call;
};

//SKIP run
//SKIP run-ir
//SKIP run-low
//SKIP ic-ref
