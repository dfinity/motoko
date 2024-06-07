let o = object abc {
  let usedVariable = "ea";
  public func usedFunction() : Text { usedVariable };
};
ignore o.usedFunction();
