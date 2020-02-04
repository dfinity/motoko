/**
[#mod-text]
= `text` -- Text values

This type describes a valid, human-readable text. It does not contain arbitrary
binary data.
*/

import Iter "iter";

module {

  // remove?
  public func append(x : Text, y : Text) : Text {
    x # y;
  };

  /**
  Creates an <<mod-iter,iterator>> that traverses the characters of the text.
  */
  public let toIter : Text -> Iter.Iter<Char> =
    func(text) = text.chars()

}
