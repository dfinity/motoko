// default eop, not explicit, upgrade should fail
//MOC-FLAG -unguarded-enhanced-orthogonal-persistence
actor {
   stable var value : Nat = 666;
};
