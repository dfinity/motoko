/// Provides extended utility functions on immutable Arrays (values of type `[T]`).
///
/// Note the difference between mutable (`[var T]`) and immutable (`[T]`) arrays.
/// Mutable arrays allow their elements to be modified after creation, while
/// immutable arrays are fixed once created.
///
/// WARNING: If you are looking for a list that can grow and shrink in size,
/// it is recommended you use `List` for those purposes.
/// Arrays must be created with a fixed size.
///
/// Import from the core package to use this module.
/// ```motoko name=import
/// import Array "mo:core/Array";
/// ```

import Order "Order";
import VarArray "VarArray";
import Option "Option";
import Types "Types";
import Prim "mo:⛔";

module {

  /// Creates an empty array (equivalent to `[]`).
  ///
  /// ```motoko include=import
  /// let array = Array.empty<Text>();
  /// assert array == [];
  /// ```
  ///
  /// Runtime: O(1)
  ///
  /// Space: O(1)
  public func empty<T>() : [T] = [];

  /// Creates an array containing `item` repeated `size` times.
  ///
  /// ```motoko include=import
  /// let array = Array.repeat<Text>("Echo", 3);
  /// assert array == ["Echo", "Echo", "Echo"];
  /// ```
  ///
  /// Runtime: O(size)
  ///
  /// Space: O(size)
  public func repeat<T>(item : T, size : Nat) : [T] = Prim.Array_tabulate<T>(size, func _ = item);

  /// Creates an immutable array of size `size`. Each element at index i
  /// is created by applying `generator` to i.
  ///
  /// ```motoko include=import
  /// let array : [Nat] = Array.tabulate<Nat>(4, func i = i * 2);
  /// assert array == [0, 2, 4, 6];
  /// ```
  ///
  /// Runtime: O(size)
  ///
  /// Space: O(size)
  ///
  /// *Runtime and space assumes that `generator` runs in O(1) time and space.
  public func tabulate<T>(size : Nat, generator : Nat -> T) : [T] = Prim.Array_tabulate<T>(size, generator);

  /// Transforms a mutable array into an immutable array.
  ///
  /// ```motoko include=import
  /// let varArray = [var 0, 1, 2];
  /// varArray[2] := 3;
  /// let array = Array.fromVarArray<Nat>(varArray);
  /// assert array == [0, 1, 3];
  /// ```
  ///
  /// Runtime: O(size)
  ///
  /// Space: O(1)
  public func fromVarArray<T>(varArray : [var T]) : [T] = Prim.Array_tabulate<T>(varArray.size(), func i = varArray[i]);

  /// Transforms an immutable array into a mutable array.
  ///
  /// ```motoko include=import
  /// import VarArray "mo:core/VarArray";
  /// import Nat "mo:core/Nat";
  ///
  /// let array = [0, 1, 2];
  /// let varArray = Array.toVarArray<Nat>(array);
  /// varArray[2] := 3;
  /// assert VarArray.equal(varArray, [var 0, 1, 3], Nat.equal);
  /// ```
  ///
  /// Runtime: O(size)
  ///
  /// Space: O(1)
  public func toVarArray<T>(array : [T]) : [var T] {
    let size = array.size();
    if (size == 0) {
      return [var]
    };
    let newArray = Prim.Array_init<T>(size, array[0]);
    var i = 0;
    while (i < size) {
      newArray[i] := array[i];
      i += 1
    };
    newArray
  };

  /// Tests if two arrays contain equal values (i.e. they represent the same
  /// list of elements). Uses `equal` to compare elements in the arrays.
  ///
  /// ```motoko include=import
  /// // Use the equal function from the Nat module to compare Nats
  /// import {equal} "mo:core/Nat";
  ///
  /// let array1 = [0, 1, 2, 3];
  /// let array2 = [0, 1, 2, 3];
  /// assert Array.equal(array1, array2, equal);
  /// ```
  ///
  /// Runtime: O(size1 + size2)
  ///
  /// Space: O(1)
  ///
  /// *Runtime and space assumes that `equal` runs in O(1) time and space.
  public func equal<T>(array1 : [T], array2 : [T], equal : (T, T) -> Bool) : Bool {
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
  /// let array = [1, 9, 4, 8];
  /// let found = Array.find<Nat>(array, func x = x > 8);
  /// assert found == ?9;
  /// ```
  /// Runtime: O(size)
  ///
  /// Space: O(1)
  ///
  /// *Runtime and space assumes that `predicate` runs in O(1) time and space.
  public func find<T>(array : [T], predicate : T -> Bool) : ?T {
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
  /// let array = ['A', 'B', 'C', 'D'];
  /// let found = Array.findIndex<Char>(array, func(x) { x == 'C' });
  /// assert found == ?2;
  /// ```
  /// Runtime: O(size)
  ///
  /// Space: O(1)
  ///
  /// *Runtime and space assumes that `predicate` runs in O(1) time and space.
  public func findIndex<T>(array : [T], predicate : T -> Bool) : ?Nat {
    for ((index, element) in enumerate(array)) {
      if (predicate element) {
        return ?index
      }
    };
    null
  };

  /// Create a new array by concatenating the values of `array1` and `array2`.
  /// Note that `Array.concat` copies its arguments and has linear complexity.
  ///
  /// ```motoko include=import
  /// let array1 = [1, 2, 3];
  /// let array2 = [4, 5, 6];
  /// let result = Array.concat<Nat>(array1, array2);
  /// assert result == [1, 2, 3, 4, 5, 6];
  /// ```
  /// Runtime: O(size1 + size2)
  ///
  /// Space: O(size1 + size2)
  public func concat<T>(array1 : [T], array2 : [T]) : [T] {
    let size1 = array1.size();
    let size2 = array2.size();
    Prim.Array_tabulate<T>(
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

  /// Sorts the elements in the array according to `compare`.
  /// Sort is deterministic and stable.
  ///
  /// ```motoko include=import
  /// import Nat "mo:core/Nat";
  ///
  /// let array = [4, 2, 6];
  /// let sorted = Array.sort(array, Nat.compare);
  /// assert sorted == [2, 4, 6];
  /// ```
  /// Runtime: O(size * log(size))
  ///
  /// Space: O(size)
  /// *Runtime and space assumes that `compare` runs in O(1) time and space.
  public func sort<T>(array : [T], compare : persistent (T, T) -> Order.Order) : [T] {
    let varArray : [var T] = toVarArray(array);
    VarArray.sortInPlace(varArray, compare);
    fromVarArray(varArray)
  };

  /// Creates a new array by reversing the order of elements in `array`.
  ///
  /// ```motoko include=import
  /// let array = [10, 11, 12];
  /// let reversed = Array.reverse(array);
  /// assert reversed == [12, 11, 10];
  /// ```
  ///
  /// Runtime: O(size)
  ///
  /// Space: O(1)
  public func reverse<T>(array : [T]) : [T] {
    let size = array.size();
    Prim.Array_tabulate<T>(size, func i = array[size - i - 1])
  };

  /// Calls `f` with each element in `array`.
  /// Retains original ordering of elements.
  ///
  /// ```motoko include=import
  /// var sum = 0;
  /// let array = [0, 1, 2, 3];
  /// Array.forEach<Nat>(array, func(x) {
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
  public func forEach<T>(array : [T], f : T -> ()) {
    for (item in array.vals()) {
      f(item)
    }
  };

  /// Creates a new array by applying `f` to each element in `array`. `f` "maps"
  /// each element it is applied to of type `X` to an element of type `Y`.
  /// Retains original ordering of elements.
  ///
  /// ```motoko include=import
  /// let array1 = [0, 1, 2, 3];
  /// let array2 = Array.map<Nat, Nat>(array1, func x = x * 2);
  /// assert array2 == [0, 2, 4, 6];
  /// ```
  ///
  /// Runtime: O(size)
  ///
  /// Space: O(size)
  ///
  /// *Runtime and space assumes that `f` runs in O(1) time and space.
  public func map<T, R>(array : [T], f : T -> R) : [R] = Prim.Array_tabulate<R>(array.size(), func i = f(array[i]));

  /// Creates a new array by applying `predicate` to every element
  /// in `array`, retaining the elements for which `predicate` returns true.
  ///
  /// ```motoko include=import
  /// let array = [4, 2, 6, 1, 5];
  /// let evenElements = Array.filter<Nat>(array, func x = x % 2 == 0);
  /// assert evenElements == [4, 2, 6];
  /// ```
  /// Runtime: O(size)
  ///
  /// Space: O(size)
  /// *Runtime and space assumes that `predicate` runs in O(1) time and space.
  public func filter<T>(array : [T], f : T -> Bool) : [T] {
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
    Prim.Array_tabulate<T>(
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

  /// Creates a new array by applying `f` to each element in `array`,
  /// and keeping all non-null elements. The ordering is retained.
  ///
  /// ```motoko include=import
  /// import {toText} "mo:core/Nat";
  ///
  /// let array = [4, 2, 0, 1];
  /// let newArray =
  ///   Array.filterMap<Nat, Text>( // mapping from Nat to Text values
  ///     array,
  ///     func x = if (x == 0) { null } else { ?toText(100 / x) } // can't divide by 0, so return null
  ///   );
  /// assert newArray == ["25", "50", "100"];
  /// ```
  /// Runtime: O(size)
  ///
  /// Space: O(size)
  /// *Runtime and space assumes that `f` runs in O(1) time and space.
  public func filterMap<T, R>(array : [T], f : T -> ?R) : [R] {
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
    Prim.Array_tabulate<R>(
      count,
      func _ {
        while (Option.isNull(options[nextSome])) {
          nextSome += 1
        };
        nextSome += 1;
        switch (options[nextSome - 1]) {
          case (?element) element;
          case null {
            Prim.trap "Array.filterMap(): malformed array"
          }
        }
      }
    )
  };

  /// Creates a new array by applying `f` to each element in `array`.
  /// If any invocation of `f` produces an `#err`, returns an `#err`. Otherwise
  /// returns an `#ok` containing the new array.
  ///
  /// ```motoko include=import
  /// let array = [4, 3, 2, 1, 0];
  /// // divide 100 by every element in the array
  /// let result = Array.mapResult<Nat, Nat, Text>(array, func x {
  ///   if (x > 0) {
  ///     #ok(100 / x)
  ///   } else {
  ///     #err "Cannot divide by zero"
  ///   }
  /// });
  /// assert result == #err "Cannot divide by zero";
  /// ```
  ///
  /// Runtime: O(size)
  ///
  /// Space: O(size)
  ///
  /// *Runtime and space assumes that `f` runs in O(1) time and space.
  public func mapResult<T, R, E>(array : [T], f : T -> Types.Result<R, E>) : Types.Result<[R], E> {
    let size = array.size();

    var error : ?Types.Result<[R], E> = null;
    let results = Prim.Array_tabulate<?R>(
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
                  Prim.trap "Array.mapResult(): malformed array"
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
  /// let array = [10, 10, 10, 10];
  /// let newArray = Array.mapEntries<Nat, Nat>(array, func (x, i) = i * x);
  /// assert newArray == [0, 10, 20, 30];
  /// ```
  ///
  /// Runtime: O(size)
  ///
  /// Space: O(size)
  ///
  /// *Runtime and space assumes that `f` runs in O(1) time and space.
  public func mapEntries<T, R>(array : [T], f : (T, Nat) -> R) : [R] = Prim.Array_tabulate<R>(array.size(), func i = f(array[i], i));

  /// Creates a new array by applying `k` to each element in `array`,
  /// and concatenating the resulting arrays in order.
  ///
  /// ```motoko include=import
  /// let array = [1, 2, 3, 4];
  /// let newArray = Array.flatMap<Nat, Int>(array, func x = [x, -x].values());
  /// assert newArray == [1, -1, 2, -2, 3, -3, 4, -4];
  /// ```
  /// Runtime: O(size)
  ///
  /// Space: O(size)
  /// *Runtime and space assumes that `k` runs in O(1) time and space.
  public func flatMap<T, R>(array : [T], k : T -> Types.Iter<R>) : [R] {
    var flatSize = 0;
    let arrays = Prim.Array_tabulate<[R]>(
      array.size(),
      func i {
        let subArray = fromIter<R>(k(array[i]));
        flatSize += subArray.size();
        subArray
      }
    );

    // could replace with a call to flatten,
    // but it would require an extra pass (to compute `flatSize`)
    var outer = 0;
    var inner = 0;
    Prim.Array_tabulate<R>(
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
  /// let array = [4, 2, 0, 1];
  /// let sum =
  ///   Array.foldLeft<Nat, Nat>(
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
  public func foldLeft<T, A>(array : [T], base : A, combine : (A, T) -> A) : A {
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
  /// let array = [1, 9, 4, 8];
  /// let bookTitle = Array.foldRight<Nat, Text>(array, "", func(x, acc) = toText(x) # acc);
  /// assert bookTitle == "1948";
  /// ```
  ///
  /// Runtime: O(size)
  ///
  /// Space: O(1)
  ///
  /// *Runtime and space assumes that `combine` runs in O(1) time and space.
  public func foldRight<T, A>(array : [T], base : A, combine : (T, A) -> A) : A {
    var acc = base;
    let size = array.size();
    var i = size;
    while (i > 0) {
      i -= 1;
      acc := combine(array[i], acc)
    };
    acc
  };

  /// Combines an iterator of arrays into a single array. Retains the original
  /// ordering of the elements.
  ///
  /// Consider using `Array.flatten()` for better performance.
  ///
  /// ```motoko include=import
  /// let arrays = [[0, 1, 2], [2, 3], [], [4]];
  /// let joinedArray = Array.join<Nat>(arrays.values());
  /// assert joinedArray == [0, 1, 2, 2, 3, 4];
  /// ```
  ///
  /// Runtime: O(number of elements in array)
  ///
  /// Space: O(number of elements in array)
  public func join<T>(arrays : Types.Iter<[T]>) : [T] {
    flatten(fromIter(arrays))
  };

  /// Combines an array of arrays into a single array. Retains the original
  /// ordering of the elements.
  ///
  /// This has better performance compared to `Array.join()`.
  ///
  /// ```motoko include=import
  /// let arrays = [[0, 1, 2], [2, 3], [], [4]];
  /// let flatArray = Array.flatten<Nat>(arrays);
  /// assert flatArray == [0, 1, 2, 2, 3, 4];
  /// ```
  ///
  /// Runtime: O(number of elements in array)
  ///
  /// Space: O(number of elements in array)
  public func flatten<T>(arrays : [[T]]) : [T] {
    var flatSize = 0;
    for (subArray in arrays.vals()) {
      flatSize += subArray.size()
    };

    var outer = 0;
    var inner = 0;
    Prim.Array_tabulate<T>(
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
  /// let array = Array.singleton(2);
  /// assert array == [2];
  /// ```
  ///
  /// Runtime: O(1)
  ///
  /// Space: O(1)
  public func singleton<T>(element : T) : [T] = [element];

  /// Returns the size of an array. Equivalent to `array.size()`.
  public func size<T>(array : [T]) : Nat = array.size();

  /// Returns whether an array is empty, i.e. contains zero elements.
  public func isEmpty<T>(array : [T]) : Bool = array.size() == 0;

  /// Converts an iterator to an array.
  public func fromIter<T>(iter : Types.Iter<T>) : [T] {
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
    if (size == 0) { return [] };
    let array = Prim.Array_init<T>(
      size,
      switch list {
        case (?(h, _)) h;
        case null {
          Prim.trap("Array.fromIter(): unreachable")
        }
      }
    );
    var i = size : Nat;
    while (i > 0) {
      i -= 1;
      switch list {
        case (?(h, t)) {
          array[i] := h;
          list := t
        };
        case null {
          Prim.trap("Array.fromIter(): unreachable")
        }
      }
    };
    Prim.Array_tabulate<T>(size, func i = array[i])
  };

  /// Returns an iterator (`Iter`) over the indices of `array`.
  /// An iterator provides a single method `next()`, which returns
  /// indices in order, or `null` when out of index to iterate over.
  ///
  /// Note: You can also use `array.keys()` instead of this function. See example
  /// below.
  ///
  /// ```motoko include=import
  /// let array = [10, 11, 12];
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
  public func keys<T>(array : [T]) : Types.Iter<Nat> = array.keys();

  /// Iterator provides a single method `next()`, which returns
  /// elements in order, or `null` when out of elements to iterate over.
  ///
  /// Note: You can also use `array.values()` instead of this function. See example
  /// below.
  ///
  /// ```motoko include=import
  /// let array = [10, 11, 12];
  ///
  /// var sum = 0;
  /// for (element in array.values()) {
  ///   sum += element;
  /// };
  /// assert sum == 33;
  /// ```
  ///
  /// Runtime: O(1)
  ///
  /// Space: O(1)
  public func values<T>(array : [T]) : Types.Iter<T> = array.vals();

  /// Iterator provides a single method `next()`, which returns
  /// pairs of (index, element) in order, or `null` when out of elements to iterate over.
  ///
  /// ```motoko include=import
  /// let array = [10, 11, 12];
  ///
  /// var sum = 0;
  /// for ((index, element) in Array.enumerate(array)) {
  ///   sum += element;
  /// };
  /// assert sum == 33;
  /// ```
  ///
  /// Runtime: O(1)
  ///
  /// Space: O(1)
  public func enumerate<T>(array : [T]) : Types.Iter<(Nat, T)> = object {
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
  /// let array = [1, 2, 3, 4];
  /// assert Array.all<Nat>(array, func x = x > 0);
  /// ```
  ///
  /// Runtime: O(size)
  ///
  /// Space: O(1)
  ///
  /// *Runtime and space assumes that `predicate` runs in O(1) time and space.
  public func all<T>(array : [T], predicate : T -> Bool) : Bool {
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
  /// let array = [1, 2, 3, 4];
  /// assert Array.any<Nat>(array, func x = x > 3);
  /// ```
  ///
  /// Runtime: O(size)
  ///
  /// Space: O(1)
  ///
  /// *Runtime and space assumes that `predicate` runs in O(1) time and space.
  public func any<T>(array : [T], predicate : T -> Bool) : Bool {
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
  /// let array = ['c', 'o', 'f', 'f', 'e', 'e'];
  /// assert Array.indexOf<Char>(array, Char.equal, 'c') == ?0;
  /// assert Array.indexOf<Char>(array, Char.equal, 'f') == ?2;
  /// assert Array.indexOf<Char>(array, Char.equal, 'g') == null;
  /// ```
  ///
  /// Runtime: O(array.size())
  ///
  /// Space: O(1)
  public func indexOf<T>(array : [T], equal : (T, T) -> Bool, element : T) : ?Nat = nextIndexOf<T>(array, equal, element, 0);

  /// Returns the index of the next occurence of `element` in the `array` starting from the `from` index (inclusive).
  ///
  /// ```motoko include=import
  /// import Char "mo:core/Char";
  /// let array = ['c', 'o', 'f', 'f', 'e', 'e'];
  /// assert Array.nextIndexOf<Char>(array, Char.equal, 'c', 0) == ?0;
  /// assert Array.nextIndexOf<Char>(array, Char.equal, 'f', 0) == ?2;
  /// assert Array.nextIndexOf<Char>(array, Char.equal, 'f', 2) == ?2;
  /// assert Array.nextIndexOf<Char>(array, Char.equal, 'f', 3) == ?3;
  /// assert Array.nextIndexOf<Char>(array, Char.equal, 'f', 4) == null;
  /// ```
  ///
  /// Runtime: O(array.size())
  ///
  /// Space: O(1)
  public func nextIndexOf<T>(array : [T], equal : (T, T) -> Bool, element : T, fromInclusive : Nat) : ?Nat {
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
  /// let array = ['c', 'o', 'f', 'f', 'e', 'e'];
  /// assert Array.lastIndexOf<Char>(array, Char.equal, 'c') == ?0;
  /// assert Array.lastIndexOf<Char>(array, Char.equal, 'f') == ?3;
  /// assert Array.lastIndexOf<Char>(array, Char.equal, 'e') == ?5;
  /// assert Array.lastIndexOf<Char>(array, Char.equal, 'g') == null;
  /// ```
  ///
  /// Runtime: O(array.size())
  ///
  /// Space: O(1)
  public func lastIndexOf<T>(array : [T], equal : (T, T) -> Bool, element : T) : ?Nat = prevIndexOf<T>(array, equal, element, array.size());

  /// Returns the index of the previous occurence of `element` in the `array` starting from the `from` index (exclusive).
  ///
  /// Negative indices are relative to the end of the array. For example, `-1` corresponds to the last element in the array.
  ///
  /// If the indices are out of bounds, they are clamped to the array bounds.
  /// If the first index is greater than the second, the function returns an empty iterator.
  ///
  /// ```motoko include=import
  /// import Char "mo:core/Char";
  /// let array = ['c', 'o', 'f', 'f', 'e', 'e'];
  /// assert Array.prevIndexOf<Char>(array, Char.equal, 'c', array.size()) == ?0;
  /// assert Array.prevIndexOf<Char>(array, Char.equal, 'e', array.size()) == ?5;
  /// assert Array.prevIndexOf<Char>(array, Char.equal, 'e', 5) == ?4;
  /// assert Array.prevIndexOf<Char>(array, Char.equal, 'e', 4) == null;
  /// ```
  ///
  /// Runtime: O(array.size());
  /// Space: O(1);
  public func prevIndexOf<T>(array : [T], equal : (T, T) -> Bool, element : T, fromExclusive : Nat) : ?Nat {
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
  /// let array = [1, 2, 3, 4, 5];
  /// let iter1 = Array.range<Nat>(array, 3, array.size());
  /// assert iter1.next() == ?4;
  /// assert iter1.next() == ?5;
  /// assert iter1.next() == null;
  ///
  /// let iter2 = Array.range<Nat>(array, 3, -1);
  /// assert iter2.next() == ?4;
  /// assert iter2.next() == null;
  ///
  /// let iter3 = Array.range<Nat>(array, 0, 0);
  /// assert iter3.next() == null;
  /// ```
  ///
  /// Runtime: O(1)
  ///
  /// Space: O(1)
  public func range<T>(array : [T], fromInclusive : Int, toExclusive : Int) : Types.Iter<T> {
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
  /// let array = [1, 2, 3, 4, 5];
  ///
  /// let slice1 = Array.sliceToArray<Nat>(array, 1, 4);
  /// assert slice1 == [2, 3, 4];
  ///
  /// let slice2 = Array.sliceToArray<Nat>(array, 1, -1);
  /// assert slice2 == [2, 3, 4];
  /// ```
  ///
  /// Runtime: O(toExclusive - fromInclusive)
  ///
  /// Space: O(toExclusive - fromInclusive)
  public func sliceToArray<T>(array : [T], fromInclusive : Int, toExclusive : Int) : [T] {
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
  /// import VarArray "mo:core/VarArray";
  /// import Nat "mo:core/Nat";
  ///
  /// let array = [1, 2, 3, 4, 5];
  ///
  /// let slice1 = Array.sliceToVarArray<Nat>(array, 1, 4);
  /// assert VarArray.equal(slice1, [var 2, 3, 4], Nat.equal);
  ///
  /// let slice2 = Array.sliceToVarArray<Nat>(array, 1, -1);
  /// assert VarArray.equal(slice2, [var 2, 3, 4], Nat.equal);
  /// ```
  ///
  /// Runtime: O(toExclusive - fromInclusive)
  ///
  /// Space: O(toExclusive - fromInclusive)
  public func sliceToVarArray<T>(array : [T], fromInclusive : Int, toExclusive : Int) : [var T] {
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

  /// Converts the array to its textual representation using `f` to convert each element to `Text`.
  ///
  /// ```motoko include=import
  /// import Nat "mo:core/Nat";
  ///
  /// let array = [1, 2, 3];
  /// let text = Array.toText<Nat>(array, Nat.toText);
  /// assert text == "[1, 2, 3]";
  /// ```
  ///
  /// Runtime: O(size)
  ///
  /// Space: O(size)
  ///
  /// *Runtime and space assumes that `f` runs in O(1) time and space.
  public func toText<T>(array : [T], f : T -> Text) : Text {
    let size = array.size();
    if (size == 0) { return "[]" };
    var text = "[";
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

  /// Compares two arrays using the provided comparison function for elements.
  /// Returns #less, #equal, or #greater if `array1` is less than, equal to,
  /// or greater than `array2` respectively.
  ///
  /// If arrays have different sizes but all elements up to the shorter length are equal,
  /// the shorter array is considered #less than the longer array.
  ///
  /// ```motoko include=import
  /// import Nat "mo:core/Nat";
  ///
  /// let array1 = [1, 2, 3];
  /// let array2 = [1, 2, 4];
  /// assert Array.compare<Nat>(array1, array2, Nat.compare) == #less;
  /// ```
  ///
  /// ```motoko include=import
  /// import Nat "mo:core/Nat";
  ///
  /// let array3 = [1, 2];
  /// let array4 = [1, 2, 3];
  /// assert Array.compare<Nat>(array3, array4, Nat.compare) == #less;
  /// ```
  ///
  /// Runtime: O(min(size1, size2))
  ///
  /// Space: O(1)
  ///
  /// *Runtime and space assumes that `compare` runs in O(1) time and space.
  public persistent func compare<T>(array1 : [T], array2 : [T], compare : persistent (T, T) -> Order.Order) : Order.Order {
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

  /// Performs binary search on a sorted array to find the index of the `element`.
  ///
  /// Returns `#found(index)` if the element is found, or `#insertionIndex(index)` with the index
  /// where the element would be inserted according to the ordering if not found.
  ///
  /// If there are multiple equal elements, no guarantee is made about which index is returned.
  /// The array must be sorted in ascending order according to the `compare` function.
  ///
  /// ```motoko include=import
  /// import Nat "mo:core/Nat";
  ///
  /// let sorted = [1, 3, 5, 7, 9, 11];
  /// assert Array.binarySearch<Nat>(sorted, Nat.compare, 5) == #found(2);
  /// assert Array.binarySearch<Nat>(sorted, Nat.compare, 6) == #insertionIndex(3);
  /// ```
  ///
  /// Runtime: O(log(size))
  ///
  /// Space: O(1)
  ///
  /// *Runtime and space assumes that `compare` runs in O(1) time and space.
  public func binarySearch<T>(array : [T], compare : (T, T) -> Order.Order, element : T) : {
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
