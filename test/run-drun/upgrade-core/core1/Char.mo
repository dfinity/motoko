/// Module for working with Characters (Unicode code points).
///
/// Characters in Motoko represent Unicode code points
/// in the range 0 to 0x10FFFF, excluding the surrogate code points
/// (0xD800 through 0xDFFF).
///
/// Import from the core package to use this module.
/// ```motoko name=import
/// import Char "mo:core/Char";
/// ```
///
/// Some built in features not listed in this module:
///
/// * You can create a `Char` literal using single quotes, e.g. 'A', '1', '漢'
/// * You can compare characters using `<`, `<=`, `==`, `!=`, `>=`, `>` operators
/// * You can convert a single-character `Text` to a `Char` using `:Char` type annotation
///
/// For example:
/// ```motoko include=import
/// let char : Char = 'A';
/// let unicodeChar = '漢';
/// let digit = '7';
/// assert Char.isDigit(digit);
/// assert Char.toText(char) == "A";
/// ```

import Prim "mo:⛔";

module {

  /// Characters represented as Unicode code points.
  public type Char = Prim.Types.Char;

  /// Convert character `char` to a word containing its Unicode scalar value.
  ///
  /// Example:
  /// ```motoko include=import
  /// let char = 'A';
  /// let unicode = Char.toNat32(char);
  /// assert unicode == 65;
  /// ```
  public let toNat32 : (char : Char) -> Nat32 = Prim.charToNat32;

  /// Convert `w` to a character.
  /// Traps if `w` is not a valid Unicode scalar value.
  /// Value `w` is valid if, and only if, `w < 0xD800 or (0xE000 <= w and w <= 0x10FFFF)`.
  ///
  /// Example:
  /// ```motoko include=import
  /// let unicode : Nat32 = 65;
  /// let char = Char.fromNat32(unicode);
  /// assert char == 'A';
  /// ```
  public let fromNat32 : (nat32 : Nat32) -> Char = Prim.nat32ToChar;

  /// Convert character `char` to single character text.
  ///
  /// Example:
  /// ```motoko include=import
  /// let char = '漢';
  /// let text = Char.toText(char);
  /// assert text == "漢";
  /// ```
  public let toText : (char : Char) -> Text = Prim.charToText;

  // Not exposed pending multi-char implementation.
  private let _toUpper : (char : Char) -> Char = Prim.charToUpper;

  // Not exposed pending multi-char implementation.
  private let _toLower : (char : Char) -> Char = Prim.charToLower;

  /// Returns `true` when `char` is a decimal digit between `0` and `9`, otherwise `false`.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Char.isDigit('5');
  /// assert not Char.isDigit('A');
  /// ```
  public func isDigit(char : Char) : Bool {
    Prim.charToNat32(char) -% Prim.charToNat32('0') <= (9 : Nat32)
  };

  /// Returns whether `char` is a whitespace character.
  /// Whitespace characters include space, tab, newline, etc.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Char.isWhitespace(' ');
  /// assert Char.isWhitespace('\n');
  /// assert not Char.isWhitespace('A');
  /// ```
  public let isWhitespace : (char : Char) -> Bool = Prim.charIsWhitespace;

  /// Returns whether `char` is a lowercase character.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Char.isLower('a');
  /// assert not Char.isLower('A');
  /// ```
  public let isLower : (char : Char) -> Bool = Prim.charIsLowercase;

  /// Returns whether `char` is an uppercase character.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Char.isUpper('A');
  /// assert not Char.isUpper('a');
  /// ```
  public let isUpper : (char : Char) -> Bool = Prim.charIsUppercase;

  /// Returns whether `char` is an alphabetic character.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Char.isAlphabetic('A');
  /// assert Char.isAlphabetic('漢');
  /// assert not Char.isAlphabetic('1');
  /// ```
  public let isAlphabetic : (char : Char) -> Bool = Prim.charIsAlphabetic;

  /// Returns `a == b`.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Char.equal('A', 'A');
  /// assert not Char.equal('A', 'B');
  /// ```
  ///
  /// Note: The reason why this function is defined in this library (in addition
  /// to the existing `==` operator) is so that you can use it as a function value
  /// to pass to a higher order function.
  public func equal(a : Char, b : Char) : Bool { a == b };

  /// Returns `a != b`.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Char.notEqual('A', 'B');
  /// assert not Char.notEqual('A', 'A');
  /// ```
  ///
  /// Note: The reason why this function is defined in this library (in addition
  /// to the existing `!=` operator) is so that you can use it as a function value
  /// to pass to a higher order function.
  public func notEqual(a : Char, b : Char) : Bool { a != b };

  /// Returns `a < b`.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Char.less('A', 'B');
  /// assert not Char.less('B', 'A');
  /// ```
  ///
  /// Note: The reason why this function is defined in this library (in addition
  /// to the existing `<` operator) is so that you can use it as a function value
  /// to pass to a higher order function.
  public func less(a : Char, b : Char) : Bool { a < b };

  /// Returns `a <= b`.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Char.lessOrEqual('A', 'A');
  /// assert Char.lessOrEqual('A', 'B');
  /// assert not Char.lessOrEqual('B', 'A');
  /// ```
  ///
  /// Note: The reason why this function is defined in this library (in addition
  /// to the existing `<=` operator) is so that you can use it as a function value
  /// to pass to a higher order function.
  public func lessOrEqual(a : Char, b : Char) : Bool { a <= b };

  /// Returns `a > b`.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Char.greater('B', 'A');
  /// assert not Char.greater('A', 'B');
  /// ```
  ///
  /// Note: The reason why this function is defined in this library (in addition
  /// to the existing `>` operator) is so that you can use it as a function value
  /// to pass to a higher order function.
  public func greater(a : Char, b : Char) : Bool { a > b };

  /// Returns `a >= b`.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Char.greaterOrEqual('B', 'A');
  /// assert Char.greaterOrEqual('A', 'A');
  /// assert not Char.greaterOrEqual('A', 'B');
  /// ```
  ///
  /// Note: The reason why this function is defined in this library (in addition
  /// to the existing `>=` operator) is so that you can use it as a function value
  /// to pass to a higher order function.
  public func greaterOrEqual(a : Char, b : Char) : Bool { a >= b };

  /// Returns the order of `a` and `b`.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Char.compare('A', 'B') == #less;
  /// assert Char.compare('B', 'A') == #greater;
  /// assert Char.compare('A', 'A') == #equal;
  /// ```
  public persistent func compare(a : Char, b : Char) : { #less; #equal; #greater } {
    if (a < b) { #less } else if (a == b) { #equal } else { #greater }
  };

}
