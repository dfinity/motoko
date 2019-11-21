module {
  public func print(x : Text) {
    debugPrint(x);
  };

  public func printLn(x : Text) {
    print (x # "\n");
  };

  public func printNat(x : Nat) {
    printLn (show x);
  };

  public func printInt(x : Int) {
    printLn (show x);
  };

  public func printChar(x : Char) {
    printLn (show x);
  };

  // FIXME: how to express types that can be shown?
  // public func show(x : FIXME) : Text {
  //   debug_show x;
  // };

  // This doesn't parse
  // public let show = debug_show;
};
