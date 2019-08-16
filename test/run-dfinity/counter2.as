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
//CALL inc
//CALL inc
//CALL inc
//CALL printCounter
