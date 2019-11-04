actor {
  var c = 1;
  public func inc() {
    c += 1;
    debug_print_Nat c; debug_print "\n";
  };
  public func printCounter () {
    debug_print_Nat c; debug_print "\n";
  }
}
//CALL inc 0x4449444C0000 []
//CALL inc 0x4449444C0000 []
//CALL inc 0x4449444C0000 []
//CALL printCounter 0x4449444C0000 []
