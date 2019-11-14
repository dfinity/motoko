actor {
  var c = 1;
  public func inc() {
    c += 1;
    Debug.printNat c; Debug.print "\n";
  };
  public func printCounter () {
    Debug.printNat c; Debug.print "\n";
  }
}
//CALL inc 0x4449444C0000 []
//CALL inc 0x4449444C0000 []
//CALL inc 0x4449444C0000 []
//CALL printCounter 0x4449444C0000 []
