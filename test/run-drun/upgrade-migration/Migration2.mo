import Prim "mo:prim";
module {

  public func run( pre : { var three : [var (Nat,Nat)] } ) :
     { var four : [var (Nat,Nat)] } {
    let post =
    {
      var four = pre.three;
    };
    Prim.debugPrint(debug_show{pre;post});
    post
  }

}