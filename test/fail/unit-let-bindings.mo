module VarArray {
  public func sortInPlace<T>(_ : [var T]) {};
};

let va = [var 3, 2, 1];
let va2 = VarArray.sortInPlace(va); // warning va2 : ()
ignore VarArray.sortInPlace(va2);
