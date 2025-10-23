module VarArray {
  public func sortInPlace<T>(_ : [var T]) {};
};

let va = [var 3, 2, 1];

func _m1() {
  let va2 = VarArray.sortInPlace(va); // warning
  ignore VarArray.sortInPlace(va2);
};

func _m2() {
  var _v = VarArray.sortInPlace(va); // warning
};

func _noWarnForNow() {
  ignore [VarArray.sortInPlace(va)];
  ignore (VarArray.sortInPlace(va), VarArray.sortInPlace(va));
  VarArray.sortInPlace(VarArray.sortInPlace(va));
};
