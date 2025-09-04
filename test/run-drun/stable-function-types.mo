//ENHANCED-ORTHOGONAL-PERSISTENCE-ONLY
persistent actor {
  persistent func stableFunction1() {};
  persistent func stableFunction2() {};
  persistent func stableFunction3() : Nat { 0 };
  persistent func stableFunction4() {};
  persistent func stableFunction5() {};
  persistent func stableFunction6() : Nat { 1 };
  persistent func stableFunction7() {};
  persistent func stableFunction8() {};
  persistent func stableFunction9() {};
  persistent func stableFunction10() : Nat { 2 };
  persistent func stableFunction11() : Nat { 3 };
  persistent func stableFunction12() {};

  type StableVariant = {
    #first : persistent () -> ();
    #second : (Nat, persistent () -> (), persistent () -> Nat);
  };

  var stableVariant1 : StableVariant = #first(stableFunction1);
  let stableVariant2 : StableVariant = #second(0, stableFunction2, stableFunction3);
  var stableVariant3 = #first(stableFunction4);
  let stableVariant4 = #second(0, stableFunction5, stableFunction6);
  let stableTuple = (0, stableFunction3, 1.23);
  let stableArray1 = [stableFunction8, stableFunction9];
  let stableArray2 = [var ?stableFunction10, ?stableFunction11];
  let stableArray3 : [persistent () -> ()] = [];
  let mutable1 = {
    var first = stableFunction12;
  };
};

//CALL upgrade ""
