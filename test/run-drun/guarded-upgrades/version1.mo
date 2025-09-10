// default eop, not explicit, upgrade should fail
//MOC-FLAG --incremental-gc -unguarded-enhanced-orthogonal-persistence
actor {
   stable var value : Nat = 666;
};
