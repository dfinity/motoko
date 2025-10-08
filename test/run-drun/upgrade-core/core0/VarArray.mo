/// Provides extended utility functions on mutable Arrays (`[var]`).
///
/// Note the difference between mutable (`[var]`) and immutable (`[]`) arrays.
/// Mutable arrays allow their elements to be modified after creation, while
/// immutable arrays are fixed once created.
///
/// WARNING: If you are looking for a list that can grow and shrink in size,
/// it is recommended you use `List` for those purposes.
/// Arrays must be created with a fixed size.
///
/// Import from the core package to use this module.
/// ```motoko name=import
/// import VarArray "mo:core/VarArray";
/// ```

import Types "Types";
import Order "Order";
import Result "Result";
import Option "Option";
import Prim "mo:⛔";

module {

  /// Creates an empty mutable array (equivalent to `[var]`).
  ///
  /// ```motoko include=import
  /// let array = VarArray.empty<Text>();
  /// assert array.size() == 0;
  /// ```
  ///
  /// Runtime: O(1)
  ///
  /// Space: O(1)
  public func empty<T>() : [var T] = [var];

  /// Creates a mutable array containing `item` repeated `size` times.
  ///
  /// ```motoko include=import
  /// import Text "mo:core/Text";
  ///
  /// let array = VarArray.repeat<Text>("Echo", 3);
  /// assert VarArray.equal(array, [var "Echo", "Echo", "Echo"], Text.equal);
  /// ```
  ///
  /// Runtime: O(size)
  ///
  /// Space: O(size)
  public func repeat<T>(item : T, size : Nat) : [var T] = Prim.Array_init<T>(size, item);

  /// Duplicates `array`, returning a shallow copy of the original.
  ///
  /// ```motoko include=import
  /// import Nat "mo:core/Nat";
  ///
  /// let array1 = [var 1, 2, 3];
  /// let array2 = VarArray.clone<Nat>(array1);
  /// array2[0] := 0;
  /// assert VarArray.equal(array1, [var 1, 2, 3], Nat.equal);
  /// assert VarArray.equal(array2, [var 0, 2, 3], Nat.equal);
  /// ```
  ///
  /// Runtime: O(size)
  ///
  /// Space: O(size)
  public func clone<T>(array : [var T]) : [var T] = Prim.Array_tabulateVar<T>(array.size(), func i = array[i]);

  /// Creates a mutable array of size `size`. Each element at index i
  /// is created by applying `generator` to i.
  ///
  /// ```motoko include=import
  /// import Nat "mo:core/Nat";
  ///
  /// let array : [var Nat] = VarArray.tabulate<Nat>(4, func i = i * 2);
  /// assert VarArray.equal(array, [var 0, 2, 4, 6], Nat.equal);
  /// ```
  ///
  /// Runtime: O(size)
  ///
  /// Space: O(size)
  ///
  /// *Runtime and space assumes that `generator` runs in O(1) time and space.
  public func tabulate<T>(size : Nat, generator : Nat -> T) : [var T] = Prim.Array_tabulateVar(size, generator);

  /// Tests if two arrays contain equal values (i.e. they represent the same
  /// list of elements). Uses `equal` to compare elements in the arrays.
  ///
  /// ```motoko include=import
  /// // Use the equal function from the Nat module to compare Nats
  /// import Nat "mo:core/Nat";
  ///
  /// let array1 = [var 0, 1, 2, 3];
  /// let array2 = [var 0, 1, 2, 3];
  /// assert VarArray.equal(array1, array2, Nat.equal);
  /// ```
  ///
  /// Runtime: O(size1 + size2)
  ///
  /// Space: O(1)
  ///
  /// *Runtime and space assumes that `equal` runs in O(1) time and space.
  public func equal<T>(array1 : [var T], array2 : [var T], equal : (T, T) -> Bool) : Bool {
    let size1 = array1.size();
    let size2 = array2.size();
    if (size1 != size2) {
      return false
    };
    var i = 0;
    while (i < size1) {
      if (not equal(array1[i], array2[i])) {
        return false
      };
      i += 1
    };
    true
  };

  /// Returns the first value in `array` for which `predicate` returns true.
  /// If no element satisfies the predicate, returns null.
  ///
  /// ```motoko include=import
  /// let array = [var 1, 9, 4, 8];
  /// let found = VarArray.find<Nat>(array, func x = x > 8);
  /// assert found == ?9;
  /// ```
  /// Runtime: O(size)
  ///
  /// Space: O(1)
  ///
  /// *Runtime and space assumes that `predicate` runs in O(1) time and space.
  public func find<T>(array : [var T], predicate : T -> Bool) : ?T {
    for (element in array.vals()) {
      if (predicate element) {
        return ?element
      }
    };
    null
  };

  /// Returns the first index in `array` for which `predicate` returns true.
  /// If no element satisfies the predicate, returns null.
  ///
  /// ```motoko include=import
  /// let array = [var 'A', 'B', 'C', 'D'];
  /// let found = VarArray.findIndex<Char>(array, func(x) { x == 'C' });
  /// assert found == ?2;
  /// ```
  /// Runtime: O(size)
  ///
  /// Space: O(1)
  ///
  /// *Runtime and space assumes that `predicate` runs in O(1) time and space.
  public func findIndex<T>(array : [var T], predicate : T -> Bool) : ?Nat {
    for ((index, element) in enumerate(array)) {
      if (predicate element) {
        return ?index
      }
    };
    null
  };

  /// Create a new mutable array by concatenating the values of `array1` and `array2`.
  /// Note that `VarArray.concat` copies its arguments and has linear complexity.
  ///
  /// ```motoko include=import
  /// import Nat "mo:core/Nat";
  ///
  /// let array1 = [var 1, 2, 3];
  /// let array2 = [var 4, 5, 6];
  /// let result = VarArray.concat<Nat>(array1, array2);
  /// assert VarArray.equal(result, [var 1, 2, 3, 4, 5, 6], Nat.equal);
  /// ```
  /// Runtime: O(size1 + size2)
  ///
  /// Space: O(size1 + size2)
  public func concat<T>(array1 : [var T], array2 : [var T]) : [var T] {
    let size1 = array1.size();
    let size2 = array2.size();
    tabulate<T>(
      size1 + size2,
      func i {
        if (i < size1) {
          array1[i]
        } else {
          array2[i - size1]
        }
      }
    )
  };

  /// Creates a new sorted copy of the mutable array according to `compare`.
  /// Sort is deterministic and stable.
  ///
  /// ```motoko include=import
  /// import Nat "mo:core/Nat";
  ///
  /// let array = [var 4, 2, 6];
  /// let sorted = VarArray.sort(array, Nat.compare);
  /// assert VarArray.equal(sorted, [var 2, 4, 6], Nat.equal);
  /// ```
  /// Runtime: O(size * log(size))
  ///
  /// Space: O(size)
  /// *Runtime and space assumes that `compare` runs in O(1) time and space.
  public func sort<T>(array : [var T], compare : persistent (T, T) -> Order.Order) : [var T] {
    let newArray = clone(array);
    sortInPlace(newArray, compare);
    newArray
  };

  /// Sorts the elements in a mutable array in place according to `compare`.
  /// Sort is deterministic and stable. This modifies the original array.
  ///
  /// ```motoko include=import
  /// import Nat "mo:core/Nat";
  ///
  /// let array = [var 4, 2, 6];
  /// VarArray.sortInPlace(array, Nat.compare);
  /// assert VarArray.equal(array, [var 2, 4, 6], Nat.equal);
  /// ```
  /// Runtime: O(size * log(size))
  ///
  /// Space: O(size)
  /// *Runtime and space assumes that `compare` runs in O(1) time and space.
  public func sortInPlace<T>(array : [var T], compare : persistent (T, T) -> Order.Order) : () {
    // Stable merge sort in a bottom-up iterative style. Same algorithm as the sort in Buffer.
    let size = array.size();
    if (size == 0) {
      return
    };
    let scratchSpace = Prim.Array_init<T>(size, array[0]);

    let sizeDec = size - 1 : Nat;
    var currSize = 1; // current size of the subarrays being merged
    // when the current size == size, the array has been merged into a single sorted array
    while (currSize < size) {
      var leftStart = 0; // selects the current left subarray being merged
      while (leftStart < sizeDec) {
        let mid : Nat = if (leftStart + currSize - 1 : Nat < sizeDec) {
          leftStart + currSize - 1
        } else { sizeDec };
        let rightEnd : Nat = if (leftStart + (2 * currSize) - 1 : Nat < sizeDec) {
          leftStart + (2 * currSize) - 1
        } else { sizeDec };

        // Merge subarrays elements[leftStart...mid] and elements[mid+1...rightEnd]
        var left = leftStart;
        var right = mid + 1;
        var nextSorted = leftStart;
        while (left < mid + 1 and right < rightEnd + 1) {
          let leftElement = array[left];
          let rightElement = array[right];
          switch (compare(leftElement, rightElement)) {
            case (#less or #equal) {
              scratchSpace[nextSorted] := leftElement;
              left += 1
            };
            case (#greater) {
              scratchSpace[nextSorted] := rightElement;
              right += 1
            }
          };
          nextSorted += 1
        };
        while (left < mid + 1) {
          scratchSpace[nextSorted] := array[left];
          nextSorted += 1;
          left += 1
        };
        while (right < rightEnd + 1) {
          scratchSpace[nextSorted] := array[right];
          nextSorted += 1;
          right += 1
        };

        // Copy over merged elements
        var i = leftStart;
        while (i < rightEnd + 1) {
          array[i] := scratchSpace[i];
          i += 1
        };

        leftStart += 2 * currSize
      };
      currSize *= 2
    }
  };

  /// Creates a new mutable array by reversing the order of elements in `array`.
  /// The original array is not modified.
  ///
  /// ```motoko include=import
  /// import Nat "mo:core/Nat";
  ///
  /// let array = [var 10, 11, 12];
  /// let reversed = VarArray.reverse(array);
  /// assert VarArray.equal(reversed, [var 12, 11, 10], Nat.equal);
  /// ```
  ///
  /// Runtime: O(size)
  ///
  /// Space: O(1)
  public func reverse<T>(array : [var T]) : [var T] {
    let size = array.size();
    tabulate<T>(size, func i = array[size - i - 1])
  };

  /// Reverses the order of elements in a mutable array in place.
  /// This modifies the original array.
  ///
  /// ```motoko include=import
  /// import Nat "mo:core/Nat";
  ///
  /// let array = [var 10, 11, 12];
  /// VarArray.reverseInPlace(array);
  /// assert VarArray.equal(array, [var 12, 11, 10], Nat.equal);
  /// ```
  ///
  /// Runtime: O(size)
  ///
  /// Space: O(1)
  public func reverseInPlace<T>(array : [var T]) : () {
    let size = array.size();
    if (size == 0) {
      return
    };
    var i = 0;
    var j = (size - 1) : Nat;
    while (i < j) {
      let temp = array[i];
      array[i] := array[j];
      array[j] := temp;
      i += 1;
      j -= 1
    }
  };

  /// Calls `f` with each element in `array`.
  /// Retains original ordering of elements.
  ///
  /// ```motoko include=import
  /// var sum = 0;
  /// let array = [var 0, 1, 2, 3];
  /// VarArray.forEach<Nat>(array, func(x) {
  ///   sum += x;
  /// });
  /// assert sum == 6;
  /// ```
  ///
  /// Runtime: O(size)
  ///
  /// Space: O(size)
  ///
  /// *Runtime and space assumes that `f` runs in O(1) time and space.
  public func forEach<T>(array : [var T], f : T -> ()) {
    for (item in array.vals()) {
      f(item)
    }
  };

  /// Creates a new mutable array by applying `f` to each element in `array`. `f` "maps"
  /// each element it is applied to of type `T` to an element of type `R`.
  /// Retains original ordering of elements.
  ///
  /// ```motoko include=import
  /// import Nat "mo:core/Nat";
  ///
  /// let array = [var 0, 1, 2, 3];
  /// let array2 = VarArray.map<Nat, Nat>(array, func x = x * 2);
  /// assert VarArray.equal(array2, [var 0, 2, 4, 6], Nat.equal);
  /// ```
  ///
  /// Runtime: O(size)
  ///
  /// Space: O(size)
  ///
  /// *Runtime and space assumes that `f` runs in O(1) time and space.
  public func map<T, R>(array : [var T], f : T -> R) : [var R] {
    tabulate<R>(
      array.size(),
      func(index) {
        f(array[index])
      }
    )
  };

  /// Applies `f` to each element of `array` in place,
  /// retaining the original ordering of elements.
  /// This modifies the original array.
  ///
  /// ```motoko include=import
  /// import Nat "mo:core/Nat";
  ///
  /// let array = [var 0, 1, 2, 3];
  /// VarArray.mapInPlace<Nat>(array, func x = x * 3);
  /// assert VarArray.equal(array, [var 0, 3, 6, 9], Nat.equal);
  /// ```
  ///
  /// Runtime: O(size)
  ///
  /// Space: O(size)
  ///
  /// *Runtime and space assumes that `f` runs in O(1) time and space.
  public func mapInPlace<T>(array : [var T], f : T -> T) {
    var index = 0;
    let size = array.size();
    while (index < size) {
      array[index] := f(array[index]);
      index += 1
    }
  };

  /// Creates a new mutable array by applying `predicate` to every element
  /// in `array`, retaining the elements for which `predicate` returns true.
  ///
  /// ```motoko include=import
  /// import Nat "mo:core/Nat";
  ///
  /// let array = [var 4, 2, 6, 1, 5];
  /// let evenElements = VarArray.filter<Nat>(array, func x = x % 2 == 0);
  /// assert VarArray.equal(evenElements, [var 4, 2, 6], Nat.equal);
  /// ```
  /// Runtime: O(size)
  ///
  /// Space: O(size)
  /// *Runtime and space assumes that `predicate` runs in O(1) time and space.
  public func filter<T>(array : [var T], f : T -> Bool) : [var T] {
    var count = 0;
    let keep = Prim.Array_tabulate<Bool>(
      array.size(),
      func i {
        if (f(array[i])) {
          count += 1;
          true
        } else {
          false
        }
      }
    );
    var nextKeep = 0;
    tabulate<T>(
      count,
      func _ {
        while (not keep[nextKeep]) {
          nextKeep += 1
        };
        nextKeep += 1;
        array[nextKeep - 1]
      }
    )
  };

  /// Creates a new mutable array by applying `f` to each element in `array`,
  /// and keeping all non-null elements. The ordering is retained.
  ///
  /// ```motoko include=import
  /// import Nat "mo:core/Nat";
  /// import Text "mo:core/Text";
  ///
  /// let array = [var 4, 2, 0, 1];
  /// let newArray =
  ///   VarArray.filterMap<Nat, Text>( // mapping from Nat to Text values
  ///     array,
  ///     func x = if (x == 0) { null } else { ?Nat.toText(100 / x) } // can't divide by 0, so return null
  ///   );
  /// assert VarArray.equal(newArray, [var "25", "50", "100"], Text.equal);
  /// ```
  /// Runtime: O(size)
  ///
  /// Space: O(size)
  /// *Runtime and space assumes that `f` runs in O(1) time and space.
  public func filterMap<T, R>(array : [var T], f : T -> ?R) : [var R] {
    var count = 0;
    let options = Prim.Array_tabulate<?R>(
      array.size(),
      func i {
        let result = f(array[i]);
        switch (result) {
          case (?element) {
            count += 1;
            result
          };
          case null {
            null
          }
        }
      }
    );

    var nextSome = 0;
    tabulate<R>(
      count,
      func _ {
        while (Option.isNull(options[nextSome])) {
          nextSome += 1
        };
        nextSome += 1;
        switch (options[nextSome - 1]) {
          case (?element) element;
          case null {
            Prim.trap "VarArray.filterMap(): malformed array"
          }
        }
      }
    )
  };

  /// Creates a new mutable array by applying `f` to each element in `array`.
  /// If any invocation of `f` produces an `#err`, returns an `#err`. Otherwise
  /// returns an `#ok` containing the new array.
  ///
  /// ```motoko include=import
  /// import Result "mo:core/Result";
  ///
  /// let array = [var 4, 3, 2, 1, 0];
  /// // divide 100 by every element in the array
  /// let result = VarArray.mapResult<Nat, Nat, Text>(array, func x {
  ///   if (x > 0) {
  ///     #ok(100 / x)
  ///   } else {
  ///     #err "Cannot divide by zero"
  ///   }
  /// });
  /// assert Result.isErr(result);
  /// ```
  ///
  /// Runtime: O(size)
  ///
  /// Space: O(size)
  ///
  /// *Runtime and space assumes that `f` runs in O(1) time and space.
  public func mapResult<T, R, E>(array : [var T], f : T -> Result.Result<R, E>) : Result.Result<[var R], E> {
    let size = array.size();

    var error : ?Result.Result<[var R], E> = null;
    let results = tabulate<?R>(
      size,
      func i {
        switch (f(array[i])) {
          case (#ok element) {
            ?element
          };
          case (#err e) {
            switch (error) {
              case null {
                // only take the first error
                error := ?(#err e)
              };
              case _ {}
            };
            null
          }
        }
      }
    );

    switch error {
      case null {
        // unpack the option
        #ok(
          map<?R, R>(
            results,
            func element {
              switch element {
                case (?element) {
                  element
                };
                case null {
                  Prim.trap "VarArray.mapResults(): malformed array"
                }
              }
            }
          )
        )
      };
      case (?error) {
        error
      }
    }
  };

  /// Creates a new array by applying `f` to each element in `array` and its index.
  /// Retains original ordering of elements.
  ///
  /// ```motoko include=import
  /// import Nat "mo:core/Nat";
  ///
  /// let array = [var 10, 10, 10, 10];
  /// let newArray = VarArray.mapEntries<Nat, Nat>(array, func (x, i) = i * x);
  /// assert VarArray.equal(newArray, [var 0, 10, 20, 30], Nat.equal);
  /// ```
  ///
  /// Runtime: O(size)
  ///
  /// Space: O(size)
  ///
  /// *Runtime and space assumes that `f` runs in O(1) time and space.
  public func mapEntries<T, R>(array : [var T], f : (T, Nat) -> R) : [var R] {
    tabulate<R>(array.size(), func i = f(array[i], i))
  };

  /// Creates a new mutable array by applying `k` to each element in `array`,
  /// and concatenating the resulting arrays in order.
  ///
  /// ```motoko include=import
  /// import Int "mo:core/Int"
  ///
  /// let array = [var 1, 2, 3, 4];
  /// let newArray = VarArray.flatMap<Nat, Int>(array, func x = [x, -x].vals());
  /// assert VarArray.equal(newArray, [var 1, -1, 2, -2, 3, -3, 4, -4], Int.equal);
  /// ```
  /// Runtime: O(size)
  ///
  /// Space: O(size)
  /// *Runtime and space assumes that `k` runs in O(1) time and space.
  public func flatMap<T, R>(array : [var T], k : T -> Types.Iter<R>) : [var R] {
    var flatSize = 0;
    let arrays = Prim.Array_tabulate<[var R]>(
      array.size(),
      func i {
        let subArray = fromIter<R>(k(array[i])); // TODO: optimize
        flatSize += subArray.size();
        subArray
      }
    );

    // could replace with a call to flatten,
    // but it would require an extra pass (to compute `flatSize`)
    var outer = 0;
    var inner = 0;
    tabulate<R>(
      flatSize,
      func _ {
        while (inner == arrays[outer].size()) {
          inner := 0;
          outer += 1
        };
        let element = arrays[outer][inner];
        inner += 1;
        element
      }
    )
  };

  /// Collapses the elements in `array` into a single value by starting with `base`
  /// and progessively combining elements into `base` with `combine`. Iteration runs
  /// left to right.
  ///
  /// ```motoko include=import
  /// import {add} "mo:core/Nat";
  ///
  /// let array = [var 4, 2, 0, 1];
  /// let sum =
  ///   VarArray.foldLeft<Nat, Nat>(
  ///     array,
  ///     0, // start the sum at 0
  ///     func(sumSoFar, x) = sumSoFar + x // this entire function can be replaced with `add`!
  ///   );
  /// assert sum == 7;
  /// ```
  ///
  /// Runtime: O(size)
  ///
  /// Space: O(1)
  ///
  /// *Runtime and space assumes that `combine` runs in O(1) time and space.
  public func foldLeft<T, A>(array : [var T], base : A, combine : (A, T) -> A) : A {
    var acc = base;
    for (element in array.vals()) {
      acc := combine(acc, element)
    };
    acc
  };

  /// Collapses the elements in `array` into a single value by starting with `base`
  /// and progessively combining elements into `base` with `combine`. Iteration runs
  /// right to left.
  ///
  /// ```motoko include=import
  /// import {toText} "mo:core/Nat";
  ///
  /// let array = [var 1, 9, 4, 8];
  /// let bookTitle = VarArray.foldRight<Nat, Text>(array, "", func(x, acc) = toText(x) # acc);
  /// assert bookTitle == "1948";
  /// ```
  ///
  /// Runtime: O(size)
  ///
  /// Space: O(1)
  ///
  /// *Runtime and space assumes that `combine` runs in O(1) time and space.
  public func foldRight<T, A>(array : [var T], base : A, combine : (T, A) -> A) : A {
    var acc = base;
    let size = array.size();
    var i = size;
    while (i > 0) {
      i -= 1;
      acc := combine(array[i], acc)
    };
    acc
  };

  /// Combines an iterator of mutable arrays into a single mutable array.
  /// Retains the original ordering of the elements.
  ///
  /// Consider using `VarArray.flatten()` for better performance.
  ///
  /// ```motoko include=import
  /// import Nat "mo:core/Nat";
  ///
  /// let arrays : [[var Nat]] = [[var 0, 1, 2], [var 2, 3], [var], [var 4]];
  /// let joinedArray = VarArray.join<Nat>(arrays.vals());
  /// assert VarArray.equal(joinedArray, [var 0, 1, 2, 2, 3, 4], Nat.equal);
  /// ```
  ///
  /// Runtime: O(number of elements in array)
  ///
  /// Space: O(number of elements in array)
  public func join<T>(arrays : Types.Iter<[var T]>) : [var T] {
    flatten<T>(fromIter(arrays))
  };

  /// Combines a mutable array of mutable arrays into a single mutable array. Retains the original
  /// ordering of the elements.
  ///
  /// This has better performance compared to `VarArray.join()`.
  ///
  /// ```motoko include=import
  /// import Nat "mo:core/Nat";
  ///
  /// let arrays : [var [var Nat]] = [var [var 0, 1, 2], [var 2, 3], [var], [var 4]];
  /// let flatArray = VarArray.flatten<Nat>(arrays);
  /// assert VarArray.equal(flatArray, [var 0, 1, 2, 2, 3, 4], Nat.equal);
  /// ```
  ///
  /// Runtime: O(number of elements in array)
  ///
  /// Space: O(number of elements in array)
  public func flatten<T>(arrays : [var [var T]]) : [var T] {
    var flatSize = 0;
    for (subArray in arrays.vals()) {
      flatSize += subArray.size()
    };

    var outer = 0;
    var inner = 0;
    tabulate<T>(
      flatSize,
      func _ {
        while (inner == arrays[outer].size()) {
          inner := 0;
          outer += 1
        };
        let element = arrays[outer][inner];
        inner += 1;
        element
      }
    )
  };

  /// Create an array containing a single value.
  ///
  /// ```motoko include=import
  /// import Nat "mo:core/Nat";
  ///
  /// let array = VarArray.singleton<Nat>(2);
  /// assert VarArray.equal(array, [var 2], Nat.equal);
  /// ```
  ///
  /// Runtime: O(1)
  ///
  /// Space: O(1)
  public func singleton<T>(element : T) : [var T] = [var element];

  /// Returns the size of a mutable array. Equivalent to `array.size()`.
  public func size<T>(array : [var T]) : Nat = array.size();

  /// Returns whether a mutable array is empty, i.e. contains zero elements.
  public func isEmpty<T>(array : [var T]) : Bool = array.size() == 0;

  /// Converts an iterator to a mutable array.
  public func fromIter<T>(iter : Types.Iter<T>) : [var T] {
    var list : Types.Pure.List<T> = null;
    var size = 0;
    label l loop {
      switch (iter.next()) {
        case (?element) {
          list := ?(element, list);
          size += 1
        };
        case null { break l }
      }
    };
    if (size == 0) { return [var] };
    let array = Prim.Array_init<T>(
      size,
      switch list {
        case (?(h, _)) h;
        case null {
          Prim.trap("VarArray.fromIter(): unreachable")
        }
      }
    );
    var i = size;
    while (i > 0) {
      i -= 1;
      switch list {
        case (?(h, t)) {
          array[i] := h;
          list := t
        };
        case null {
          Prim.trap("VarArray.fromIter(): unreachable")
        }
      }
    };
    array
  };

  /// Returns an iterator (`Iter`) over the indices of `array`.
  /// An iterator provides a single method `next()`, which returns
  /// indices in order, or `null` when out of index to iterate over.
  ///
  /// NOTE: You can also use `array.keys()` instead of this function. See example
  /// below.
  ///
  /// ```motoko include=import
  /// let array = [var 10, 11, 12];
  ///
  /// var sum = 0;
  /// for (element in array.keys()) {
  ///   sum += element;
  /// };
  /// assert sum == 3; // 0 + 1 + 2
  /// ```
  ///
  /// Runtime: O(1)
  ///
  /// Space: O(1)
  public func keys<T>(array : [var T]) : Types.Iter<Nat> = array.keys();

  /// Iterator provides a single method `next()`, which returns
  /// elements in order, or `null` when out of elements to iterate over.
  ///
  /// Note: You can also use `array.values()` instead of this function. See example
  /// below.
  ///
  /// ```motoko include=import
  /// let array = [var 10, 11, 12];
  ///
  /// var sum = 0;
  /// for (element in array.values()) {
  ///   sum += element;
  /// };
  /// assert sum == 33; // 10 + 11 + 12
  /// ```
  ///
  /// Runtime: O(1)
  ///
  /// Space: O(1)
  public func values<T>(array : [var T]) : Types.Iter<T> = array.vals();

  /// Returns an iterator that provides pairs of (index, element) in order, or `null`
  /// when out of elements to iterate over.
  ///
  /// ```motoko include=import
  /// let array = [var 10, 11, 12];
  ///
  /// var sum = 0;
  /// for ((index, element) in VarArray.enumerate(array)) {
  ///   sum += element;
  /// };
  /// assert sum == 33;
  /// ```
  ///
  /// Runtime: O(1)
  ///
  /// Space: O(1)
  public func enumerate<T>(array : [var T]) : Types.Iter<(Nat, T)> = object {
    let size = array.size();
    var index = 0;
    public func next() : ?(Nat, T) {
      if (index >= size) {
        return null
      };
      let i = index;
      index += 1;
      ?(i, array[i])
    }
  };

  /// Returns true if all elements in `array` satisfy the predicate function.
  ///
  /// ```motoko include=import
  /// let array = [var 1, 2, 3, 4];
  /// assert VarArray.all<Nat>(array, func x = x > 0);
  /// ```
  ///
  /// Runtime: O(size)
  ///
  /// Space: O(1)
  ///
  /// *Runtime and space assumes that `predicate` runs in O(1) time and space.
  public func all<T>(array : [var T], predicate : T -> Bool) : Bool {
    for (element in array.vals()) {
      if (not predicate(element)) {
        return false
      }
    };
    true
  };

  /// Returns true if any element in `array` satisfies the predicate function.
  ///
  /// ```motoko include=import
  /// let array = [var 1, 2, 3, 4];
  /// assert VarArray.any<Nat>(array, func x = x > 3);
  /// ```
  ///
  /// Runtime: O(size)
  ///
  /// Space: O(1)
  ///
  /// *Runtime and space assumes that `predicate` runs in O(1) time and space.
  public func any<T>(array : [var T], predicate : T -> Bool) : Bool {
    for (element in array.vals()) {
      if (predicate(element)) {
        return true
      }
    };
    false
  };

  /// Returns the index of the first `element` in the `array`.
  ///
  /// ```motoko include=import
  /// import Char "mo:core/Char";
  ///
  /// let array = [var 'c', 'o', 'f', 'f', 'e', 'e'];
  /// assert VarArray.indexOf<Char>(array, Char.equal, 'c') == ?0;
  /// assert VarArray.indexOf<Char>(array, Char.equal, 'f') == ?2;
  /// assert VarArray.indexOf<Char>(array, Char.equal, 'g') == null;
  /// ```
  ///
  /// Runtime: O(array.size())
  ///
  /// Space: O(1)
  public func indexOf<T>(array : [var T], equal : (T, T) -> Bool, element : T) : ?Nat = nextIndexOf<T>(array, equal, element, 0);

  /// Returns the index of the next occurence of `element` in the `array` starting from the `from` index (inclusive).
  ///
  /// ```motoko include=import
  /// import Char "mo:core/Char";
  ///
  /// let array = [var 'c', 'o', 'f', 'f', 'e', 'e'];
  /// assert VarArray.nextIndexOf<Char>(array, Char.equal, 'c', 0) == ?0;
  /// assert VarArray.nextIndexOf<Char>(array, Char.equal, 'f', 0) == ?2;
  /// assert VarArray.nextIndexOf<Char>(array, Char.equal, 'f', 2) == ?2;
  /// assert VarArray.nextIndexOf<Char>(array, Char.equal, 'f', 3) == ?3;
  /// assert VarArray.nextIndexOf<Char>(array, Char.equal, 'f', 4) == null;
  /// ```
  ///
  /// Runtime: O(array.size())
  ///
  /// Space: O(1)
  public func nextIndexOf<T>(array : [var T], equal : (T, T) -> Bool, element : T, fromInclusive : Nat) : ?Nat {
    var index = fromInclusive;
    let size = array.size();
    while (index < size) {
      if (equal(array[index], element)) {
        return ?index
      } else {
        index += 1
      }
    };
    null
  };

  /// Returns the index of the last `element` in the `array`.
  ///
  /// ```motoko include=import
  /// import Char "mo:core/Char";
  ///
  /// let array = [var 'c', 'o', 'f', 'f', 'e', 'e'];
  /// assert VarArray.lastIndexOf<Char>(array, Char.equal, 'c') == ?0;
  /// assert VarArray.lastIndexOf<Char>(array, Char.equal, 'f') == ?3;
  /// assert VarArray.lastIndexOf<Char>(array, Char.equal, 'e') == ?5;
  /// assert VarArray.lastIndexOf<Char>(array, Char.equal, 'g') == null;
  /// ```
  ///
  /// Runtime: O(array.size())
  ///
  /// Space: O(1)
  public func lastIndexOf<T>(array : [var T], equal : (T, T) -> Bool, element : T) : ?Nat = prevIndexOf<T>(array, equal, element, array.size());

  /// Returns the index of the previous occurence of `element` in the `array` starting from the `from` index (exclusive).
  ///
  /// ```motoko include=import
  /// import Char "mo:core/Char";
  /// let array = [var 'c', 'o', 'f', 'f', 'e', 'e'];
  /// assert VarArray.prevIndexOf<Char>(array, Char.equal, 'c', array.size()) == ?0;
  /// assert VarArray.prevIndexOf<Char>(array, Char.equal, 'e', array.size()) == ?5;
  /// assert VarArray.prevIndexOf<Char>(array, Char.equal, 'e', 5) == ?4;
  /// assert VarArray.prevIndexOf<Char>(array, Char.equal, 'e', 4) == null;
  /// ```
  ///
  /// Runtime: O(array.size());
  /// Space: O(1);
  public func prevIndexOf<T>(array : [var T], equal : (T, T) -> Bool, element : T, fromExclusive : Nat) : ?Nat {
    var i = fromExclusive;
    while (i > 0) {
      i -= 1;
      if (equal(array[i], element)) {
        return ?i
      }
    };
    null
  };

  /// Returns an iterator over a slice of `array` starting at `fromInclusive` up to (but not including) `toExclusive`.
  ///
  /// Negative indices are relative to the end of the array. For example, `-1` corresponds to the last element in the array.
  ///
  /// If the indices are out of bounds, they are clamped to the array bounds.
  /// If the first index is greater than the second, the function returns an empty iterator.
  ///
  /// ```motoko include=import
  /// let array = [var 1, 2, 3, 4, 5];
  /// let iter1 = VarArray.range<Nat>(array, 3, array.size());
  /// assert iter1.next() == ?4;
  /// assert iter1.next() == ?5;
  /// assert iter1.next() == null;
  ///
  /// let iter2 = VarArray.range<Nat>(array, 3, -1);
  /// assert iter2.next() == ?4;
  /// assert iter2.next() == null;
  ///
  /// let iter3 = VarArray.range<Nat>(array, 0, 0);
  /// assert iter3.next() == null;
  /// ```
  ///
  /// Runtime: O(1)
  ///
  /// Space: O(1)
  public func range<T>(array : [var T], fromInclusive : Int, toExclusive : Int) : Types.Iter<T> {
    let size = array.size();
    // Convert negative indices to positive and handle bounds
    let startInt = if (fromInclusive < 0) {
      let s = size + fromInclusive;
      if (s < 0) { 0 } else { s }
    } else {
      if (fromInclusive > size) { size } else { fromInclusive }
    };
    let endInt = if (toExclusive < 0) {
      let e = size + toExclusive;
      if (e < 0) { 0 } else { e }
    } else {
      if (toExclusive > size) { size } else { toExclusive }
    };
    // Convert to Nat (values are non-negative due to bounds checking above)
    let start = Prim.abs(startInt);
    let end = Prim.abs(endInt);
    object {
      var pos = start;
      public func next() : ?T {
        if (pos >= end) {
          null
        } else {
          let elem = array[pos];
          pos += 1;
          ?elem
        }
      }
    }
  };

  /// Returns a new array containing elements from `array` starting at index `fromInclusive` up to (but not including) index `toExclusive`.
  /// If the indices are out of bounds, they are clamped to the array bounds.
  ///
  /// ```motoko include=import
  /// let array = [var 1, 2, 3, 4, 5];
  ///
  /// let slice1 = VarArray.sliceToArray<Nat>(array, 1, 4);
  /// assert slice1 == [2, 3, 4];
  ///
  /// let slice2 = VarArray.sliceToArray<Nat>(array, 1, -1);
  /// assert slice2 == [2, 3, 4];
  /// ```
  ///
  /// Runtime: O(toExclusive - fromInclusive)
  ///
  /// Space: O(toExclusive - fromInclusive)
  public func sliceToArray<T>(array : [var T], fromInclusive : Int, toExclusive : Int) : [T] {
    let size = array.size();
    // Convert negative indices to positive and handle bounds
    let startInt = if (fromInclusive < 0) {
      let s = size + fromInclusive;
      if (s < 0) { 0 } else { s }
    } else {
      if (fromInclusive > size) { size } else { fromInclusive }
    };
    let endInt = if (toExclusive < 0) {
      let e = size + toExclusive;
      if (e < 0) { 0 } else { e }
    } else {
      if (toExclusive > size) { size } else { toExclusive }
    };
    // Convert to Nat (always non-negative due to bounds checking above)
    let start = Prim.abs(startInt);
    let end = Prim.abs(endInt);
    if (start >= end) {
      return []
    };
    Prim.Array_tabulate<T>(end - start, func i = array[start + i])
  };

  /// Returns a new mutable array containing elements from `array` starting at index `fromInclusive` up to (but not including) index `toExclusive`.
  /// If the indices are out of bounds, they are clamped to the array bounds.
  ///
  /// ```motoko include=import
  /// import Nat "mo:core/Nat";
  ///
  /// let array = [var 1, 2, 3, 4, 5];
  ///
  /// let slice1 = VarArray.sliceToVarArray<Nat>(array, 1, 4);
  /// assert VarArray.equal(slice1, [var 2, 3, 4], Nat.equal);
  ///
  /// let slice2 = VarArray.sliceToVarArray<Nat>(array, 1, -1);
  /// assert VarArray.equal(slice2, [var 2, 3, 4], Nat.equal);
  /// ```
  ///
  /// Runtime: O(toExclusive - fromInclusive)
  ///
  /// Space: O(toExclusive - fromInclusive)
  public func sliceToVarArray<T>(array : [var T], fromInclusive : Int, toExclusive : Int) : [var T] {
    let size = array.size();
    // Convert negative indices to positive and handle bounds
    let startInt = if (fromInclusive < 0) {
      let s = size + fromInclusive;
      if (s < 0) { 0 } else { s }
    } else {
      if (fromInclusive > size) { size } else { fromInclusive }
    };
    let endInt = if (toExclusive < 0) {
      let e = size + toExclusive;
      if (e < 0) { 0 } else { e }
    } else {
      if (toExclusive > size) { size } else { toExclusive }
    };
    // Convert to Nat (always non-negative due to bounds checking above)
    let start = Prim.abs(startInt);
    let end = Prim.abs(endInt);
    if (start >= end) {
      return [var]
    };
    Prim.Array_tabulateVar<T>(end - start, func i = array[start + i])
  };

  /// Converts the mutable array to its textual representation using `f` to convert each element to `Text`.
  ///
  /// ```motoko include=import
  /// import Nat "mo:core/Nat";
  ///
  /// let array = [var 1, 2, 3];
  /// assert VarArray.toText<Nat>(array, Nat.toText) == "[var 1, 2, 3]";
  /// ```
  ///
  /// Runtime: O(size)
  ///
  /// Space: O(size)
  ///
  /// *Runtime and space assumes that `f` runs in O(1) time and space.
  public func toText<T>(array : [var T], f : T -> Text) : Text {
    let size = array.size();
    if (size == 0) { return "[var]" };
    var text = "[var ";
    var i = 0;
    while (i < size) {
      if (i != 0) {
        text #= ", "
      };
      text #= f(array[i]);
      i += 1
    };
    text #= "]";
    text
  };

  /// Compares two mutable arrays using the provided comparison function for elements.
  /// Returns #less, #equal, or #greater if `array1` is less than, equal to,
  /// or greater than `array2` respectively.
  ///
  /// If arrays have different sizes but all elements up to the shorter length are equal,
  /// the shorter array is considered #less than the longer array.
  ///
  /// ```motoko include=import
  /// import Nat "mo:core/Nat";
  /// let array1 = [var 1, 2, 3];
  /// let array2 = [var 1, 2, 4];
  /// assert VarArray.compare<Nat>(array1, array2, Nat.compare) == #less;
  ///
  /// let array3 = [var 1, 2];
  /// let array4 = [var 1, 2, 3];
  /// assert VarArray.compare<Nat>(array3, array4, Nat.compare) == #less;
  /// ```
  ///
  /// Runtime: O(min(size1, size2))
  ///
  /// Space: O(1)
  ///
  /// *Runtime and space assumes that `compare` runs in O(1) time and space.
  public persistent func compare<T>(array1 : [var T], array2 : [var T], compare : persistent (T, T) -> Order.Order) : Order.Order {
    let size1 = array1.size();
    let size2 = array2.size();
    var i = 0;
    let minSize = if (size1 < size2) { size1 } else { size2 };
    while (i < minSize) {
      switch (compare(array1[i], array2[i])) {
        case (#less) { return #less };
        case (#greater) { return #greater };
        case (#equal) { i += 1 }
      }
    };
    if (size1 < size2) { #less } else if (size1 > size2) { #greater } else {
      #equal
    }
  };

  /// Performs binary search on a sorted mutable array to find the index of the `element`.
  /// Returns `#found(index)` if the element is found, or `#insertionIndex(index)` with the index
  ///
  /// If there are multiple equal elements, no guarantee is made about which index is returned.
  /// The array must be sorted in ascending order according to the `compare` function.
  ///
  /// ```motoko include=import
  /// import Nat "mo:core/Nat";
  ///
  /// let sorted = [var 1, 3, 5, 7, 9, 11];
  /// assert VarArray.binarySearch<Nat>(sorted, Nat.compare, 5) == #found(2);
  /// assert VarArray.binarySearch<Nat>(sorted, Nat.compare, 6) == #insertionIndex(3);
  /// ```
  ///
  /// Runtime: O(log(size))
  ///
  /// Space: O(1)
  ///
  /// *Runtime and space assumes that `compare` runs in O(1) time and space.
  public func binarySearch<T>(array : [var T], compare : (T, T) -> Order.Order, element : T) : {
    #found : Nat;
    #insertionIndex : Nat
  } {
    var left = 0;
    var right = array.size();
    while (left < right) {
      let mid = (left + right) / 2;
      switch (compare(array[mid], element)) {
        case (#less) left := mid + 1;
        case (#greater) right := mid;
        case (#equal) return #found mid
      }
    };
    #insertionIndex left
  }

}
