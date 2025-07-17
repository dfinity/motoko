//ENHANCED-ORTHOGONAL-PERSISTENCE-ONLY
actor {
  func stableFunction1() {};
  func stableFunction2() {};
  func stableFunction3() : Nat { 0 };
  func stableFunction4() {};
  func stableFunction5() {};
  func stableFunction6() : Nat { 1 };
  func stableFunction7() {};
  func stableFunction8() {};
  func stableFunction9() {};
  func stableFunction10() : Nat { 2 };
  func stableFunction11() : Nat { 3 };
  func stableFunction12() {};

  type StableVariant = {
    #first : stable () -> ();
    #second : (Nat, stable () -> (), stable () -> Nat);
  };

  stable var stableVariant1 : StableVariant = #first(stableFunction1);
  stable let stableVariant2 : StableVariant = #second(0, stableFunction2, stableFunction3);
  stable var stableVariant3 = #first(stableFunction4);
  stable let stableVariant4 = #second(0, stableFunction5, stableFunction6);
  stable let stableTuple = (0, stableFunction3, 1.23);
  stable let stableArray1 = [stableFunction8, stableFunction9];
  stable let stableArray2 = [var ?stableFunction10, ?stableFunction11];
  stable let stableArray3 : [stable () -> ()] = [];
  stable let mutable1 = {
    var first = stableFunction12;
  };
};

//CALL upgrade ""
