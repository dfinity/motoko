module {

  public func run( o: { var one : [var Nat];
                        var two : [var Nat];
                      } ) :
    { var three : [var (Nat,Nat)] } {
    {
      var three = [var (o.one[0], o.two[0])]
    }
  }

}