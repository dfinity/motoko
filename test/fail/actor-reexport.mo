actor test {
  public func exported() {
    Debug.print("exported()\n");
  };
  public let exported_too = exported;
};

actor test2 {
  public let exported_three = test.exported_too;
  public let (exported_four, exported_five) =
    if (true)
      (test.exported_too, test.exported_too)
    else
      (exported_three, exported_three)
};

test.exported();
test.exported_too();
test2.exported_three();
test2.exported_four();
test2.exported_five();
