import Prim "mo:prim";
actor a {



public func callshow(): async Text {
   let t = await show();
   Prim.debugPrint t;
   t
 };

public func show() : async Text {
  var accum = "";
  accum #= "██";
  accum #= "  ";
  accum #= "\n";
  accum #= "  ";
  accum #= "██";
  accum #= "\n";
  Prim.debugPrint accum;
  accum
 };
};


ignore a.callshow();  //OR-CALL ingress callshow "DIDL\x00\x00"
