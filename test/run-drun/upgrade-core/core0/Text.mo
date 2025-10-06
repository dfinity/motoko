/// Utility functions for `Text` values.
///
/// A `Text` value represents human-readable text as a sequence of characters of type `Char`.
///
/// ```motoko
/// let text = "Hello!";
/// let size = text.size();
/// assert size == 6;
/// let iter = text.chars();
/// assert iter.next() == ?'H';
/// assert iter.next() == ?'e';
/// assert iter.next() == ?'l';
/// assert iter.next() == ?'l';
/// assert iter.next() == ?'o';
/// assert iter.next() == ?'!';
/// assert iter.next() == null;
/// let concat = text # " 👋";
/// assert concat == "Hello! 👋";
/// ```
///
/// The `"mo:core/Text"` module defines additional operations on `Text` values.
///
/// Import the module from the core package:
///
/// ```motoko name=import
/// import Text "mo:core/Text";
/// ```
///
/// Note: `Text` values are represented as ropes of UTF-8 character sequences with O(1) concatenation.
///

import Char "Char";
import Iter "Iter";
import ImperativeIter "imperative/Iter";
import ImperativeStack "imperative/Stack";
import Types "Types";
import Prim "mo:⛔";
import Order "Order";

module {

  /// The type corresponding to primitive `Text` values.
  ///
  /// ```motoko
  /// let hello = "Hello!";
  /// let emoji = "👋";
  /// let concat = hello # " " # emoji;
  /// assert concat == "Hello! 👋";
  /// ```
  public type Text = Prim.Types.Text;

  /// Converts the given `Char` to a `Text` value.
  ///
  /// ```motoko include=import
  /// let text = Text.fromChar('A');
  /// assert text == "A";
  /// ```
  public let fromChar : (c : Char) -> Text = Prim.charToText;

  /// Converts the given `[Char]` to a `Text` value.
  ///
  /// ```motoko include=import
  /// let text = Text.fromArray(['A', 'v', 'o', 'c', 'a', 'd', 'o']);
  /// assert text == "Avocado";
  /// ```
  ///
  /// Runtime: O(a.size())
  /// Space: O(a.size())
  public func fromArray(a : [Char]) : Text = fromIter(a.vals());

  /// Converts the given `[var Char]` to a `Text` value.
  ///
  /// ```motoko include=import
  /// let text = Text.fromVarArray([var 'E', 'g', 'g', 'p', 'l', 'a', 'n', 't']);
  /// assert text == "Eggplant";
  /// ```
  ///
  /// Runtime: O(a.size())
  /// Space: O(a.size())
  public func fromVarArray(a : [var Char]) : Text = fromIter(a.vals());

  /// Iterates over each `Char` value in the given `Text`.
  ///
  /// Equivalent to calling the `t.chars()` method where `t` is a `Text` value.
  ///
  /// ```motoko include=import
  /// let chars = Text.toIter("abc");
  /// assert chars.next() == ?'a';
  /// assert chars.next() == ?'b';
  /// assert chars.next() == ?'c';
  /// assert chars.next() == null;
  /// ```
  public func toIter(t : Text) : Iter.Iter<Char> {
    Iter.Iter(t.chars())
  };

  /// Collapses the characters in `text` into a single value by starting with `base`
  /// and progessively combining characters into `base` with `combine`. Iteration runs
  /// left to right.
  ///
  /// ```motoko include=import
  ///
  /// let text = "Mississippi";
  /// let count =
  ///   Text.foldLeft<Nat>(
  ///     text,
  ///     0, // start the sum at 0
  ///     func(ss, c) = if (c == 's') ss + 1 else ss
  ///   );
  /// assert count == 4;
  /// ```
  ///
  /// Runtime: O(size)
  ///
  /// Space: O(1)
  ///
  /// *Runtime and space assumes that `combine` runs in O(1) time and space.
  public func foldLeft<A>(text : Text, base : A, combine : (A, Char) -> A) : A {
    var acc = base;
    for (c in text.chars()) acc := combine(acc, c);
    acc
  };

  /// Creates a new `Array` containing characters of the given `Text`.
  ///
  /// Equivalent to `Iter.toArray(t.chars())`.
  ///
  /// ```motoko include=import
  /// assert Text.toArray("Café") == ['C', 'a', 'f', 'é'];
  /// ```
  ///
  /// Runtime: O(t.size())
  /// Space: O(t.size())
  public func toArray(t : Text) : [Char] {
    let cs = t.chars();
    // We rely on Array_tabulate's implementation details: it fills
    // the array from left to right sequentially.
    Prim.Array_tabulate<Char>(
      t.size(),
      func _ {
        switch (cs.next()) {
          case (?c) { c };
          case null { Prim.trap("Text.toArray()") }
        }
      }
    )
  };

  /// Creates a new mutable `Array` containing characters of the given `Text`.
  ///
  /// Equivalent to `Iter.toArrayMut(t.chars())`.
  ///
  /// ```motoko include=import
  /// import VarArray "mo:core/VarArray";
  /// import Char "mo:core/Char";
  ///
  /// assert VarArray.equal(Text.toVarArray("Café"), [var 'C', 'a', 'f', 'é'], Char.equal);
  /// ```
  ///
  /// Runtime: O(t.size())
  /// Space: O(t.size())
  public func toVarArray(t : Text) : [var Char] {
    let n = t.size();
    if (n == 0) {
      return [var]
    };
    let array = Prim.Array_init<Char>(n, ' ');
    var i = 0;
    for (c in t.chars()) {
      array[i] := c;
      i += 1
    };
    array
  };

  /// Creates a `Text` value from a `Char` iterator.
  ///
  /// ```motoko include=import
  /// let text = Text.fromIter(['a', 'b', 'c'].values());
  /// assert text == "abc";
  /// ```
  public func fromIter(cs : ImperativeIter.Iter<Char>) : Text {
    var r = "";
    for (c in cs) {
      r #= Prim.charToText(c)
    };
    return r
  };

  /// Returns whether the given `Text` is empty (has a size of zero).
  ///
  /// ```motoko include=import
  /// let text1 = "";
  /// let text2 = "example";
  /// assert Text.isEmpty(text1);
  /// assert not Text.isEmpty(text2);
  /// ```
  public func isEmpty(t : Text) : Bool = t == "";

  /// Returns the number of characters in the given `Text`.
  ///
  /// Equivalent to calling `t.size()` where `t` is a `Text` value.
  ///
  /// ```motoko include=import
  /// let size = Text.size("abc");
  /// assert size == 3;
  /// ```
  public func size(t : Text) : Nat = t.size();

  /// Returns `t1 # t2`, where `#` is the `Text` concatenation operator.
  ///
  /// ```motoko include=import
  /// let a = "Hello";
  /// let b = "There";
  /// let together = a # b;
  /// assert together == "HelloThere";
  /// let withSpace = a # " " # b;
  /// assert withSpace == "Hello There";
  /// let togetherAgain = Text.concat(a, b);
  /// assert togetherAgain == "HelloThere";
  /// ```
  public func concat(t1 : Text, t2 : Text) : Text = t1 # t2;

  /// Returns a new `Text` with the characters of the input `Text` in reverse order.
  ///
  /// ```motoko include=import
  /// let text = Text.reverse("Hello");
  /// assert text == "olleH";
  /// ```
  ///
  /// Runtime: O(t.size())
  /// Space: O(t.size())
  public func reverse(t : Text) : Text {
    fromIter(Iter.Iter(t.chars()).reverse())
  };

  /// Returns true if two text values are equal.
  ///
  /// ```motoko
  /// import Text "mo:core/Text";
  ///
  /// assert Text.equal("hello", "hello");
  /// assert not Text.equal("hello", "world");
  /// ```
  public func equal(t1 : Text, t2 : Text) : Bool { t1 == t2 };

  /// Returns true if two text values are not equal.
  ///
  /// ```motoko
  /// import Text "mo:core/Text";
  ///
  /// assert Text.notEqual("hello", "world");
  /// assert not Text.notEqual("hello", "hello");
  /// ```
  public func notEqual(t1 : Text, t2 : Text) : Bool { t1 != t2 };

  /// Returns true if the first text value is lexicographically less than the second.
  ///
  /// ```motoko
  /// import Text "mo:core/Text";
  ///
  /// assert Text.less("apple", "banana");
  /// assert not Text.less("banana", "apple");
  /// ```
  public func less(t1 : Text, t2 : Text) : Bool { t1 < t2 };

  /// Returns true if the first text value is lexicographically less than or equal to the second.
  ///
  /// ```motoko
  /// import Text "mo:core/Text";
  ///
  /// assert Text.lessOrEqual("apple", "banana");
  /// assert Text.lessOrEqual("apple", "apple");
  /// assert not Text.lessOrEqual("banana", "apple");
  /// ```
  public func lessOrEqual(t1 : Text, t2 : Text) : Bool { t1 <= t2 };

  /// Returns true if the first text value is lexicographically greater than the second.
  ///
  /// ```motoko
  /// import Text "mo:core/Text";
  ///
  /// assert Text.greater("banana", "apple");
  /// assert not Text.greater("apple", "banana");
  /// ```
  public func greater(t1 : Text, t2 : Text) : Bool { t1 > t2 };

  /// Returns true if the first text value is lexicographically greater than or equal to the second.
  ///
  /// ```motoko
  /// import Text "mo:core/Text";
  ///
  /// assert Text.greaterOrEqual("banana", "apple");
  /// assert Text.greaterOrEqual("apple", "apple");
  /// assert not Text.greaterOrEqual("apple", "banana");
  /// ```
  public func greaterOrEqual(t1 : Text, t2 : Text) : Bool { t1 >= t2 };

  /// Compares `t1` and `t2` lexicographically.
  ///
  /// ```motoko include=import
  /// assert Text.compare("abc", "abc") == #equal;
  /// assert Text.compare("abc", "def") == #less;
  /// assert Text.compare("abc", "ABC") == #greater;
  /// ```
  public persistent func compare(t1 : Text, t2 : Text) : Order.Order {
    let c = Prim.textCompare(t1, t2);
    if (c < 0) #less else if (c == 0) #equal else #greater
  };

  private func extract(t : Text, i : Nat, j : Nat) : Text {
    let size = t.size();
    if (i == 0 and j == size) return t;
    assert (j <= size);
    let cs = t.chars();
    var r = "";
    var n = i;
    while (n > 0) {
      ignore cs.next();
      n -= 1
    };
    n := j;
    while (n > 0) {
      switch (cs.next()) {
        case null { assert false };
        case (?c) { r #= Prim.charToText(c) }
      };
      n -= 1
    };
    return r
  };

  /// Join an iterator of `Text` values with a given delimiter.
  ///
  /// ```motoko include=import
  /// let joined = Text.join(", ", ["a", "b", "c"].values());
  /// assert joined == "a, b, c";
  /// ```
  public func join(sep : Text, ts : Iter.Iter<Text>) : Text {
    var r = "";
    if (sep.size() == 0) {
      for (t in ts) {
        r #= t
      };
      return r
    };
    let next = ts.next;
    switch (next()) {
      case null { return r };
      case (?t) {
        r #= t
      }
    };
    loop {
      switch (next()) {
        case null { return r };
        case (?t) {
          r #= sep;
          r #= t
        }
      }
    }
  };

  /// Applies a function to each character in a `Text` value, returning the concatenated `Char` results.
  ///
  /// ```motoko include=import
  /// // Replace all occurrences of '?' with '!'
  /// let result = Text.map("Motoko?", func(c) {
  ///   if (c == '?') '!'
  ///   else c
  /// });
  /// assert result == "Motoko!";
  /// ```
  public func map(t : Text, f : Char -> Char) : Text {
    var r = "";
    for (c in t.chars()) {
      r #= Prim.charToText(f(c))
    };
    r
  };

  /// Returns the result of applying `f` to each character in `ts`, concatenating the intermediate text values.
  ///
  /// ```motoko include=import
  /// // Replace all occurrences of '?' with "!!"
  /// let result = Text.flatMap("Motoko?", func(c) {
  ///   if (c == '?') "!!"
  ///   else Text.fromChar(c)
  /// });
  /// assert result == "Motoko!!";
  /// ```
  public func flatMap(t : Text, f : Char -> Text) : Text {
    var r = "";
    for (c in t.chars()) {
      r #= f(c)
    };
    r
  };

  /// A pattern `p` describes a sequence of characters. A pattern has one of the following forms:
  ///
  /// * `#char c` matches the single character sequence, `c`.
  /// * `#text t` matches multi-character text sequence `t`.
  /// * `#predicate p` matches any single character sequence `c` satisfying predicate `p(c)`.
  ///
  /// A _match_ for `p` is any sequence of characters matching the pattern `p`.
  ///
  /// ```motoko include=import
  /// let charPattern = #char 'A';
  /// let textPattern = #text "phrase";
  /// let predicatePattern : Text.Pattern = #predicate (func(c) { c == 'A' or c == 'B' });
  /// assert Text.contains("A", predicatePattern);
  /// assert Text.contains("B", predicatePattern);
  /// ```
  public type Pattern = Types.Pattern;

  private func take(n : Nat, cs : ImperativeIter.Iter<Char>) : Iter.Iter<Char> {
    var i = n;
    Iter.Iter(object {
      public func next() : ?Char {
        if (i == 0) return null;
        i -= 1;
        return cs.next()
      }
    })
  };

  private func empty() : Iter.Iter<Char> {
    Iter.Iter(object {
      public func next() : ?Char = null
    })
  };

  private type Match = {
    /// #success on complete match
    #success;
    /// #fail(cs,c) on partial match of cs, but failing match on c
    #fail : (cs : ImperativeIter.Iter<Char>, c : Char);
    /// #empty(cs) on partial match of cs and empty stream
    #empty : (cs : ImperativeIter.Iter<Char>)
  };

  private func sizeOfPattern(pat : Pattern) : Nat {
    switch pat {
      case (#text(t)) { t.size() };
      case (#predicate(_) or #char(_)) { 1 }
    }
  };

  private func matchOfPattern(pat : Pattern) : (cs : ImperativeIter.Iter<Char>) -> Match {
    switch pat {
      case (#char(p)) {
        func(cs : ImperativeIter.Iter<Char>) : Match {
          switch (cs.next()) {
            case (?c) {
              if (p == c) {
                #success
              } else {
                #fail(empty(), c)
              }
            };
            case null { #empty(empty()) }
          }
        }
      };
      case (#predicate(p)) {
        func(cs : ImperativeIter.Iter<Char>) : Match {
          switch (cs.next()) {
            case (?c) {
              if (p(c)) {
                #success
              } else {
                #fail(empty(), c)
              }
            };
            case null { #empty(empty()) }
          }
        }
      };
      case (#text(p)) {
        func(cs : ImperativeIter.Iter<Char>) : Match {
          var i = 0;
          let ds = p.chars();
          loop {
            switch (ds.next()) {
              case (?d) {
                switch (cs.next()) {
                  case (?c) {
                    if (c != d) {
                      return #fail(take(i, p.chars()), c)
                    };
                    i += 1
                  };
                  case null {
                    return #empty(take(i, p.chars()))
                  }
                }
              };
              case null { return #success }
            }
          }
        }
      }
    }
  };

  private class CharBuffer(cs : ImperativeIter.Iter<Char>) : ImperativeIter.Iter<Char> = {

    var stack : ImperativeStack.Stack<(ImperativeIter.Iter<Char>, Char)> = ImperativeStack.empty();

    public func pushBack(cs0 : ImperativeIter.Iter<Char>, c : Char) {
      ImperativeStack.push(stack, (cs0, c))
    };

    public func next() : ?Char {
      switch (ImperativeStack.peek(stack)) {
        case (?(buff, c)) {
          switch (buff.next()) {
            case null {
              ignore ImperativeStack.pop(stack);
              return ?c
            };
            case oc {
              return oc
            }
          }
        };
        case null {
          return cs.next()
        }
      }
    }
  };

  /// Splits the input `Text` with the specified `Pattern`.
  ///
  /// Two fields are separated by exactly one match.
  ///
  /// ```motoko include=import
  /// let words = Text.split("This is a sentence.", #char ' ');
  /// assert Text.join("|", words) == "This|is|a|sentence.";
  /// ```
  public func split(t : Text, p : Pattern) : Iter.Iter<Text> {
    let match = matchOfPattern(p);
    let cs = CharBuffer(t.chars());
    var state = 0;
    var field = "";
    Iter.Iter(object {
      public func next() : ?Text {
        switch state {
          case (0 or 1) {
            loop {
              switch (match(cs)) {
                case (#success) {
                  let r = field;
                  field := "";
                  state := 1;
                  return ?r
                };
                case (#empty(cs1)) {
                  for (c in cs1) {
                    field #= fromChar(c)
                  };
                  let r = if (state == 0 and field == "") {
                    null
                  } else {
                    ?field
                  };
                  state := 2;
                  return r
                };
                case (#fail(cs1, c)) {
                  cs.pushBack(cs1, c);
                  switch (cs.next()) {
                    case (?ci) {
                      field #= fromChar(ci)
                    };
                    case null {
                      let r = if (state == 0 and field == "") {
                        null
                      } else {
                        ?field
                      };
                      state := 2;
                      return r
                    }
                  }
                }
              }
            }
          };
          case _ { return null }
        }
      }
    })
  };

  /// Returns a sequence of tokens from the input `Text` delimited by the specified `Pattern`, derived from start to end.
  /// A "token" is a non-empty maximal subsequence of `t` not containing a match for pattern `p`.
  /// Two tokens may be separated by one or more matches of `p`.
  ///
  /// ```motoko include=import
  /// let tokens = Text.tokens("this needs\n an   example", #predicate (func(c) { c == ' ' or c == '\n' }));
  /// assert Text.join("|", tokens) == "this|needs|an|example";
  /// ```
  public func tokens(t : Text, p : Pattern) : Iter.Iter<Text> {
    let fs = split(t, p);
    Iter.Iter(object {
      public func next() : ?Text {
        switch (fs.next()) {
          case (?"") { next() };
          case ot { ot }
        }
      }
    })
  };

  /// Returns `true` if the input `Text` contains a match for the specified `Pattern`.
  ///
  /// ```motoko include=import
  /// assert Text.contains("Motoko", #text "oto");
  /// assert not Text.contains("Motoko", #text "xyz");
  /// ```
  public func contains(t : Text, p : Pattern) : Bool {
    let match = matchOfPattern(p);
    let cs = CharBuffer(t.chars());
    loop {
      switch (match(cs)) {
        case (#success) {
          return true
        };
        case (#empty(_cs1)) {
          return false
        };
        case (#fail(cs1, c)) {
          cs.pushBack(cs1, c);
          switch (cs.next()) {
            case null {
              return false
            };
            case _ {}; // continue
          }
        }
      }
    }
  };

  /// Returns `true` if the input `Text` starts with a prefix matching the specified `Pattern`.
  ///
  /// ```motoko include=import
  /// assert Text.startsWith("Motoko", #text "Mo");
  /// ```
  public func startsWith(t : Text, p : Pattern) : Bool {
    var cs = t.chars();
    let match = matchOfPattern(p);
    switch (match(cs)) {
      case (#success) { true };
      case _ { false }
    }
  };

  /// Returns `true` if the input `Text` ends with a suffix matching the specified `Pattern`.
  ///
  /// ```motoko include=import
  /// assert Text.endsWith("Motoko", #char 'o');
  /// ```
  public func endsWith(t : Text, p : Pattern) : Bool {
    let s2 = sizeOfPattern(p);
    if (s2 == 0) return true;
    let s1 = t.size();
    if (s2 > s1) return false;
    let match = matchOfPattern(p);
    var cs1 = t.chars();
    var diff : Nat = s1 - s2;
    while (diff > 0) {
      ignore cs1.next();
      diff -= 1
    };
    switch (match(cs1)) {
      case (#success) { true };
      case _ { false }
    }
  };

  /// Returns the input text `t` with all matches of pattern `p` replaced by text `r`.
  ///
  /// ```motoko include=import
  /// let result = Text.replace("abcabc", #char 'a', "A");
  /// assert result == "AbcAbc";
  /// ```
  public func replace(t : Text, p : Pattern, r : Text) : Text {
    let match = matchOfPattern(p);
    let size = sizeOfPattern(p);
    let cs = CharBuffer(t.chars());
    var res = "";
    label l loop {
      switch (match(cs)) {
        case (#success) {
          res #= r;
          if (size > 0) {
            continue l
          }
        };
        case (#empty(cs1)) {
          for (c1 in cs1) {
            res #= fromChar(c1)
          };
          break l
        };
        case (#fail(cs1, c)) {
          cs.pushBack(cs1, c)
        }
      };
      switch (cs.next()) {
        case null {
          break l
        };
        case (?c1) {
          res #= fromChar(c1)
        }; // continue
      }
    };
    return res
  };

  /// Strips one occurrence of the given `Pattern` from the beginning of the input `Text`.
  /// If you want to remove multiple instances of the pattern, use `Text.trimStart()` instead.
  ///
  /// ```motoko include=import
  /// // Try to strip a nonexistent character
  /// let none = Text.stripStart("abc", #char '-');
  /// assert none == null;
  /// // Strip just one '-'
  /// let one = Text.stripStart("--abc", #char '-');
  /// assert one == ?"-abc";
  /// ```
  public func stripStart(t : Text, p : Pattern) : ?Text {
    let s = sizeOfPattern(p);
    if (s == 0) return ?t;
    var cs = t.chars();
    let match = matchOfPattern(p);
    switch (match(cs)) {
      case (#success) return ?fromIter(cs);
      case _ return null
    }
  };

  /// Strips one occurrence of the given `Pattern` from the end of the input `Text`.
  /// If you want to remove multiple instances of the pattern, use `Text.trimEnd()` instead.
  ///
  /// ```motoko include=import
  /// // Try to strip a nonexistent character
  /// let none = Text.stripEnd("xyz", #char '-');
  /// assert none == null;
  /// // Strip just one '-'
  /// let one = Text.stripEnd("xyz--", #char '-');
  /// assert one == ?"xyz-";
  /// ```
  public func stripEnd(t : Text, p : Pattern) : ?Text {
    let s2 = sizeOfPattern(p);
    if (s2 == 0) return ?t;
    let s1 = t.size();
    if (s2 > s1) return null;
    let match = matchOfPattern(p);
    var cs1 = t.chars();
    var diff : Nat = s1 - s2;
    while (diff > 0) {
      ignore cs1.next();
      diff -= 1
    };
    switch (match(cs1)) {
      case (#success) return ?extract(t, 0, s1 - s2);
      case _ return null
    }
  };

  /// Trims the given `Pattern` from the start of the input `Text`.
  /// If you only want to remove a single instance of the pattern, use `Text.stripStart()` instead.
  ///
  /// ```motoko include=import
  /// let trimmed = Text.trimStart("---abc", #char '-');
  /// assert trimmed == "abc";
  /// ```
  public func trimStart(t : Text, p : Pattern) : Text {
    let cs = t.chars();
    let size = sizeOfPattern(p);
    if (size == 0) return t;
    var matchSize = 0;
    let match = matchOfPattern(p);
    loop {
      switch (match(cs)) {
        case (#success) {
          matchSize += size
        }; // continue
        case (#empty(cs1)) {
          return if (matchSize == 0) {
            t
          } else {
            fromIter(cs1)
          }
        };
        case (#fail(cs1, c)) {
          return if (matchSize == 0) {
            t
          } else {
            fromIter(cs1) # fromChar(c) # fromIter(cs)
          }
        }
      }
    }
  };

  /// Trims the given `Pattern` from the end of the input `Text`.
  /// If you only want to remove a single instance of the pattern, use `Text.stripEnd()` instead.
  ///
  /// ```motoko include=import
  /// let trimmed = Text.trimEnd("xyz---", #char '-');
  /// assert trimmed == "xyz";
  /// ```
  public func trimEnd(t : Text, p : Pattern) : Text {
    let cs = CharBuffer(t.chars());
    let size = sizeOfPattern(p);
    if (size == 0) return t;
    let match = matchOfPattern(p);
    var matchSize = 0;
    label l loop {
      switch (match(cs)) {
        case (#success) {
          matchSize += size
        }; // continue
        case (#empty(cs1)) {
          switch (cs1.next()) {
            case null break l;
            case (?_) return t
          }
        };
        case (#fail(cs1, c)) {
          matchSize := 0;
          cs.pushBack(cs1, c);
          ignore cs.next()
        }
      }
    };
    extract(t, 0, t.size() - matchSize)
  };

  /// Trims the given `Pattern` from both the start and end of the input `Text`.
  ///
  /// ```motoko include=import
  /// let trimmed = Text.trim("---abcxyz---", #char '-');
  /// assert trimmed == "abcxyz";
  /// ```
  public func trim(t : Text, p : Pattern) : Text {
    let cs = t.chars();
    let size = sizeOfPattern(p);
    if (size == 0) return t;
    var matchSize = 0;
    let match = matchOfPattern(p);
    loop {
      switch (match(cs)) {
        case (#success) {
          matchSize += size
        }; // continue
        case (#empty(cs1)) {
          return if (matchSize == 0) { t } else { fromIter(cs1) }
        };
        case (#fail(cs1, c)) {
          let start = matchSize;
          let cs2 = CharBuffer(cs);
          cs2.pushBack(cs1, c);
          ignore cs2.next();
          matchSize := 0;
          label l loop {
            switch (match(cs2)) {
              case (#success) {
                matchSize += size
              }; // continue
              case (#empty(_cs3)) {
                switch (cs1.next()) {
                  case null break l;
                  case (?_) return t
                }
              };
              case (#fail(cs3, c1)) {
                matchSize := 0;
                cs2.pushBack(cs3, c1);
                ignore cs2.next()
              }
            }
          };
          return extract(t, start, t.size() - matchSize - start)
        }
      }
    }
  };

  /// Compares `t1` and `t2` using the provided character-wise comparison function.
  ///
  /// ```motoko include=import
  /// import Char "mo:core/Char";
  ///
  /// assert Text.compareWith("abc", "ABC", func(c1, c2) { Char.compare(c1, c2) }) == #greater;
  /// ```
  public func compareWith(
    t1 : Text,
    t2 : Text,
    cmp : (Char, Char) -> Order.Order
  ) : Order.Order {
    let cs1 = t1.chars();
    let cs2 = t2.chars();
    loop {
      switch (cs1.next(), cs2.next()) {
        case (null, null) { return #equal };
        case (null, ?_) { return #less };
        case (?_, null) { return #greater };
        case (?c1, ?c2) {
          switch (cmp(c1, c2)) {
            case (#equal) {}; // continue
            case other { return other }
          }
        }
      }
    }
  };

  /// Returns a UTF-8 encoded `Blob` from the given `Text`.
  ///
  /// ```motoko include=import
  /// let blob = Text.encodeUtf8("Hello");
  /// assert blob == "\48\65\6C\6C\6F";
  /// ```
  public let encodeUtf8 : Text -> Blob = Prim.encodeUtf8;

  /// Tries to decode the given `Blob` as UTF-8.
  /// Returns `null` if the blob is not valid UTF-8.
  ///
  /// ```motoko include=import
  /// let text = Text.decodeUtf8("\48\65\6C\6C\6F");
  /// assert text == ?"Hello";
  /// ```
  public let decodeUtf8 : Blob -> ?Text = Prim.decodeUtf8;

  /// Returns the text argument in lowercase.
  /// WARNING: Unicode compliant only when compiled, not interpreted.
  ///
  /// ```motoko include=import
  /// let text = Text.toLower("Good Day");
  /// assert text == "good day";
  /// ```
  public let toLower : Text -> Text = Prim.textLowercase;

  /// Returns the text argument in uppercase. Unicode compliant.
  /// WARNING: Unicode compliant only when compiled, not interpreted.
  ///
  /// ```motoko include=import
  /// let text = Text.toUpper("Good Day");
  /// assert text == "GOOD DAY";
  /// ```
  public let toUpper : Text -> Text = Prim.textUppercase;

  /// Returns the given text value unchanged.
  /// This function is provided for consistency with other modules.
  ///
  /// ```motoko include=import
  /// assert Text.toText("Hello") == "Hello";
  /// ```
  public func toText(t : Text) : Text = t
}
