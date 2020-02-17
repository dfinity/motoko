import Prim "mo:prim";
// git reset --hard 788395608cdf75ecdff3c904bef52f2478467d5f
actor a {

let matrix = { unbox = [[true,false,true,true,false,false,false,true],
                        [true,false,true,true,false,false,false,true]] };

var accum = "";

public func callshow(): async Text {
   let t = await show();
   Prim.debugPrint t;
   t
 };

public func show() : async Text {
  for (row in matrix.unbox.vals()) {
    for (val in row.vals()) {
        if val {
          accum #= "██"
        } else {
          accum #= "  "
        };
    };
   accum #= "\n";
  };
  Prim.debugPrint accum;
  accum
 };
};


ignore a.callshow();  //OR-CALL ingress callshow "DIDL\x00\x00"
