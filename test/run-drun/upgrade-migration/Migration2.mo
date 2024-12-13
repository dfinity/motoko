module {

  public func run( o : { var three : [var (Nat,Nat)] } ) :
     { var four : [var (Nat,Nat)] } {
    {
      var four = o.three;
    }
  }

}