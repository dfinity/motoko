import Prim "mo:prim";

// migration should not run on fresh install
actor
  [ func (_:{}) : {} {
      Prim.debugPrint "unexpectedly migrating";
      assert false;
      {}
    } ]
{

  Prim.debugPrint("installed actor");

};


