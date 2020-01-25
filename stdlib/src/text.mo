import Iter "iter";

module {

  public func append(x : Text, y : Text) : Text {
    x # y;
  };

  public func toIter(text : Text) : Iter.Iter<Char> {
    { next = text.chars().next }
  }

}
