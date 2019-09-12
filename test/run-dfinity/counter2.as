actor {
  var c = 1;
  public func inc() {
    c += 1;
    printNat c; print "\n";
  };
  public func printCounter () {
    printNat c; print "\n";
  }
}
//CALL inc 0x4449444C0000 []
//CALL inc 0x4449444C0000 []
//CALL inc 0x4449444C0000 []
//CALL printCounter 0x4449444C0000 []
