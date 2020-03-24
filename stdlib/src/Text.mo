/**
[#mod-Text]
= `Text` -- Text values

This type describes a valid, human-readable text. It does not contain arbitrary
binary data.
*/

import Iter "Iter";

module {

  // remove?
  public func append(x : Text, y : Text) : Text {
    x # y;
  };

  /**
  Creates an <<mod-Iter,iterator>> that traverses the characters of the text.
  */
  public let toIter : Text -> Iter.Iter<Char> =
    func(text) = text.chars()

}
