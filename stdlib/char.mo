module {

  public func isDigit(char : Char) : Bool {
    charToWord32(char) - charToWord32('0') <= (9 : Word32)
  };

}
