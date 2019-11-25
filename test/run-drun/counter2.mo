actor {
  var c = 1;
  public func inc() {
    c += 1;
    debugPrintNat c
  };
  public func printCounter () {
    debugPrintNat c
  }
}
//CALL inc 0x4449444C0000 []
//CALL inc 0x4449444C0000 []
//CALL inc 0x4449444C0000 []
//CALL printCounter 0x4449444C0000 []
