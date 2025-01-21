import Prim "mo:prim";

// migration should not run on fresh install
actor 
  [ func (_:{}) : {} {
      Prim.debugPrint "unexpectedly migrating";
      assert false;
      {}
    } ]
  class C()
{

  Prim.debugPrint("installed class");

};


