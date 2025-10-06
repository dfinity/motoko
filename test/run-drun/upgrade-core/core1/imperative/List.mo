/// A mutable growable array data structure with efficient random access and dynamic resizing.
/// `List` provides O(1) access time and O(sqrt(n)) memory overhead. In contrast, `pure/List` is a purely functional linked list.
/// Can be declared `stable` for orthogonal persistence.
///
/// This implementation is adapted with permission from the `vector` Mops package created by Research AG.
///
/// Copyright: 2023 MR Research AG
/// Main author: Andrii Stepanov
/// Contributors: Timo Hanke (timohanke), Andy Gura (andygura), react0r-com
///
/// ```motoko name=import
/// import List "mo:core/List";
/// ```

import PureList "../pure/List";
import Prim "mo:⛔";
import Nat32 "../Nat32";
import Array "../Array";
import Iter "Iter";
import Nat "../Nat";
import Order "../Order";
import Option "../Option";
import VarArray "../VarArray";
import Types "../Types";

module {
  /// `List<T>` provides a mutable list of elements of type `T`.
  /// Based on the paper "Resizable Arrays in Optimal Time and Space" by Brodnik, Carlsson, Demaine, Munro and Sedgewick (1999).
  /// Since this is internally a two-dimensional array the access times for put and get operations
  /// will naturally be 2x slower than Buffer and Array. However, Array is not resizable and Buffer
  /// has `O(size)` memory waste.
  ///
  /// The maximum number of elements in a `List` is 2^32.
  public type List<T> = Types.List<T>;

  let INTERNAL_ERROR = "List: internal error";

  /// Creates a new empty List for elements of type T.
  ///
  /// Example:
  /// ```motoko include=import
  /// let list = List.empty<Nat>(); // Creates a new List
  /// ```
  public func empty<T>() : List<T> = {
    var blocks = [var [var]];
    var blockIndex = 1;
    var elementIndex = 0
  };

  /// Returns a new list with capacity and size 1, containing `element`.
  ///
  /// Example:
  /// ```motoko include=import
  /// import Nat "mo:core/Nat";
  ///
  /// let list = List.singleton<Nat>(1);
  /// assert List.toText<Nat>(list, Nat.toText) == "List[1]";
  /// ```
  ///
  /// Runtime: `O(1)`
  ///
  /// Space: `O(1)`
  public func singleton<T>(element : T) : List<T> = repeat(element, 1);

  /// Creates a new List with `size` copies of the initial value.
  ///
  /// Example:
  /// ```motoko include=import
  /// let list = List.repeat<Nat>(2, 4);
  /// assert List.toArray(list) == [2, 2, 2, 2];
  /// ```
  ///
  /// Runtime: `O(size)`
  ///
  /// Space: `O(size)`
  public func repeat<T>(initValue : T, size : Nat) : List<T> {
    let (blockIndex, elementIndex) = locate(size);

    let blocks = new_index_block_length(Nat32.fromNat(if (elementIndex == 0) { blockIndex - 1 } else blockIndex));
    let data_blocks = VarArray.repeat<[var ?T]>([var], blocks);
    var i = 1;
    while (i < blockIndex) {
      data_blocks[i] := VarArray.repeat<?T>(?initValue, data_block_size(i));
      i += 1
    };
    if (elementIndex != 0 and blockIndex < blocks) {
      let block = VarArray.repeat<?T>(null, data_block_size(i));
      var j = 0;
      while (j < elementIndex) {
        block[j] := ?initValue;
        j += 1
      };
      data_blocks[i] := block
    };

    {
      var blocks = data_blocks;
      var blockIndex = blockIndex;
      var elementIndex = elementIndex
    }
  };

  /// Converts a mutable `List` to a purely functional `PureList`.
  ///
  /// Example:
  /// ```motoko include=import
  /// let list = List.fromArray<Nat>([1, 2, 3]);
  /// let pureList = List.toPure<Nat>(list); // converts to immutable PureList
  /// ```
  ///
  /// Runtime: `O(size)`
  ///
  /// Space: `O(size)`
  public func toPure<T>(list : List<T>) : PureList.List<T> {
    PureList.fromIter(values(list)) // TODO: optimize
  };

  /// Converts a purely functional `List` to a mutable `List`.
  ///
  /// Example:
  /// ```motoko include=import
  /// import PureList "mo:core/pure/List";
  ///
  /// let pureList = PureList.fromArray<Nat>([1, 2, 3]);
  /// let list = List.fromPure<Nat>(pureList); // converts to mutable List
  /// ```
  ///
  /// Runtime: `O(size)`
  ///
  /// Space: `O(size)`
  public func fromPure<T>(pure : PureList.List<T>) : List<T> {
    let list = empty<T>();
    PureList.forEach<T>(pure, func(x) = add(list, x));
    list
  };

  /// Add to list `count` copies of the initial value.
  ///
  /// ```motoko include=import
  /// let list = List.repeat<Nat>(2, 4); // [2, 2, 2, 2]
  /// List.addRepeat(list, 2, 1); // [2, 2, 2, 2, 1, 1]
  /// ```
  ///
  /// The maximum number of elements in a `List` is 2^32.
  ///
  /// Runtime: `O(count)`
  public func addRepeat<T>(list : List<T>, initValue : T, count : Nat) {
    let (blockIndex, elementIndex) = locate(size(list) + count);
    let blocks = new_index_block_length(Nat32.fromNat(if (elementIndex == 0) { blockIndex - 1 } else blockIndex));

    let old_blocks = list.blocks.size();
    if (old_blocks < blocks) {
      let old_data_blocks = list.blocks;
      list.blocks := VarArray.repeat<[var ?T]>([var], blocks);
      var i = 0;
      while (i < old_blocks) {
        list.blocks[i] := old_data_blocks[i];
        i += 1
      }
    };

    var cnt = count;
    while (cnt > 0) {
      let db_size = data_block_size(list.blockIndex);
      if (list.elementIndex == 0 and db_size <= cnt) {
        list.blocks[list.blockIndex] := VarArray.repeat<?T>(?initValue, db_size);
        cnt -= db_size;
        list.blockIndex += 1
      } else {
        if (list.blocks[list.blockIndex].size() == 0) {
          list.blocks[list.blockIndex] := VarArray.repeat<?T>(null, db_size)
        };
        let from = list.elementIndex;
        let to = Nat.min(list.elementIndex + cnt, db_size);

        let block = list.blocks[list.blockIndex];
        var i = from;
        while (i < to) {
          block[i] := ?initValue;
          i += 1
        };

        list.elementIndex := to;
        if (list.elementIndex == db_size) {
          list.elementIndex := 0;
          list.blockIndex += 1
        };
        cnt -= to - from
      }
    }
  };

  /// Resets the list to size 0, de-referencing all elements.
  ///
  /// Example:
  /// ```motoko include=import
  /// let list = List.empty<Nat>();
  /// List.add(list, 10);
  /// List.add(list, 11);
  /// List.add(list, 12);
  /// List.clear(list); // list is now empty
  /// assert List.toArray(list) == [];
  /// ```
  ///
  /// Runtime: `O(1)`
  public func clear<T>(list : List<T>) {
    list.blocks := [var [var]];
    list.blockIndex := 1;
    list.elementIndex := 0
  };

  /// Returns a copy of a List, with the same size.
  ///
  /// Example:
  /// ```motoko include=import
  /// let list = List.empty<Nat>();
  /// List.add(list, 1);
  ///
  /// let clone = List.clone(list);
  /// assert List.toArray(clone) == [1];
  /// ```
  ///
  /// Runtime: `O(size)`
  public func clone<T>(list : List<T>) : List<T> = {
    var blocks = VarArray.tabulate<[var ?T]>(
      list.blocks.size(),
      func(i) = VarArray.tabulate<?T>(
        list.blocks[i].size(),
        func(j) = list.blocks[i][j]
      )
    );
    var blockIndex = list.blockIndex;
    var elementIndex = list.elementIndex
  };

  /// Creates a new list by applying the provided function to each element in the input list.
  /// The resulting list has the same size as the input list.
  ///
  /// Example:
  /// ```motoko include=import
  /// import Nat "mo:core/Nat";
  ///
  /// let list = List.singleton<Nat>(123);
  /// let textList = List.map<Nat, Text>(list, Nat.toText);
  /// assert List.toArray(textList) == ["123"];
  /// ```
  ///
  /// Runtime: `O(size)`
  public func map<T, R>(list : List<T>, f : T -> R) : List<R> = {
    var blocks = VarArray.tabulate<[var ?R]>(
      list.blocks.size(),
      func(i) {
        let db = list.blocks[i];
        VarArray.tabulate<?R>(
          db.size(),
          func(j) = switch (db[j]) {
            case (?item) ?f(item);
            case (null) null
          }
        )
      }
    );
    var blockIndex = list.blockIndex;
    var elementIndex = list.elementIndex
  };

  /// Returns a new list containing only the elements from `list` for which the predicate returns true.
  ///
  /// Example:
  /// ```motoko include=import
  /// let list = List.fromArray<Nat>([1, 2, 3, 4]);
  /// let evenNumbers = List.filter<Nat>(list, func x = x % 2 == 0);
  /// assert List.toArray(evenNumbers) == [2, 4];
  /// ```
  ///
  /// Runtime: `O(size)`
  ///
  /// Space: `O(size)`
  ///
  /// *Runtime and space assumes that `predicate` runs in `O(1)` time and space.
  public func filter<T>(list : List<T>, predicate : T -> Bool) : List<T> {
    let filtered = empty<T>();
    forEach<T>(
      list,
      func(x) {
        if (predicate(x)) add(filtered, x)
      }
    );
    filtered
  };

  /// Returns a new list containing all elements from `list` for which the function returns ?element.
  /// Discards all elements for which the function returns null.
  ///
  /// Example:
  /// ```motoko include=import
  /// let list = List.fromArray<Nat>([1, 2, 3, 4]);
  /// let doubled = List.filterMap<Nat, Nat>(list, func x = if (x % 2 == 0) ?(x * 2) else null);
  /// assert List.toArray(doubled) == [4, 8];
  /// ```
  ///
  /// Runtime: `O(size)`
  ///
  /// Space: `O(size)`
  ///
  /// *Runtime and space assumes that `f` runs in `O(1)` time and space.
  public func filterMap<T, R>(list : List<T>, f : T -> ?R) : List<R> {
    let filtered = empty<R>();
    forEach<T>(
      list,
      func(x) {
        switch (f(x)) {
          case (?y) add(filtered, y);
          case null {}
        }
      }
    );
    filtered
  };

  /// Returns the current number of elements in the list.
  ///
  /// Example:
  /// ```motoko include=import
  /// let list = List.empty<Nat>();
  /// assert List.size(list) == 0
  /// ```
  ///
  /// Runtime: `O(1)` (with some internal calculations)
  public func size<T>(list : List<T>) : Nat {
    let d = Nat32.fromNat(list.blockIndex);
    let i = Nat32.fromNat(list.elementIndex);

    // We call all data blocks of the same capacity an "epoch". We number the epochs 0,1,2,...
    // A data block is in epoch e iff the data block has capacity 2 ** e.
    // Each epoch starting with epoch 1 spans exactly two super blocks.
    // Super block s falls in epoch ceil(s/2).

    // epoch of last data block
    // e = 32 - lz
    let lz = Nat32.bitcountLeadingZero(d / 3);

    // capacity of all prior epochs combined
    // capacity_before_e = 2 * 4 ** (e - 1) - 1

    // data blocks in all prior epochs combined
    // blocks_before_e = 3 * 2 ** (e - 1) - 2

    // then size = d * 2 ** e + i - c
    // where c = blocks_before_e * 2 ** e - capacity_before_e

    // there can be overflows, but the result is without overflows, so use addWrap and subWrap
    // we don't erase bits by >>, so to use <>> is ok
    Nat32.toNat((d -% (1 <>> lz)) <>> lz +% i)
  };

  func data_block_size(blockIndex : Nat) : Nat {
    // formula for the size of given blockIndex
    // don't call it for blockIndex == 0
    Nat32.toNat(1 <>> Nat32.bitcountLeadingZero(Nat32.fromNat(blockIndex) / 3))
  };

  func new_index_block_length(blockIndex : Nat32) : Nat {
    if (blockIndex <= 1) 2 else {
      let s = 30 - Nat32.bitcountLeadingZero(blockIndex);
      Nat32.toNat(((blockIndex >> s) +% 1) << s)
    }
  };

  func grow_index_block_if_needed<T>(list : List<T>) {
    if (list.blocks.size() == list.blockIndex) {
      let new_blocks = VarArray.repeat<[var ?T]>([var], new_index_block_length(Nat32.fromNat(list.blockIndex)));
      var i = 0;
      while (i < list.blockIndex) {
        new_blocks[i] := list.blocks[i];
        i += 1
      };
      list.blocks := new_blocks
    }
  };

  func shrink_index_block_if_needed<T>(list : List<T>) {
    let blockIndex = Nat32.fromNat(list.blockIndex);
    // kind of index of the first block in the super block
    if ((blockIndex << Nat32.bitcountLeadingZero(blockIndex)) << 2 == 0) {
      let new_length = new_index_block_length(blockIndex);
      if (new_length < list.blocks.size()) {
        let new_blocks = VarArray.repeat<[var ?T]>([var], new_length);
        var i = 0;
        while (i < new_length) {
          new_blocks[i] := list.blocks[i];
          i += 1
        };
        list.blocks := new_blocks
      }
    }
  };

  /// Adds a single element to the end of a List,
  /// allocating a new internal data block if needed,
  /// and resizing the internal index block if needed.
  ///
  /// Example:
  /// ```motoko include=import
  /// let list = List.empty<Nat>();
  /// List.add(list, 0); // add 0 to list
  /// List.add(list, 1);
  /// List.add(list, 2);
  /// List.add(list, 3);
  /// assert List.toArray(list) == [0, 1, 2, 3];
  /// ```
  ///
  /// The maximum number of elements in a `List` is 2^32.
  ///
  /// Amortized Runtime: `O(1)`, Worst Case Runtime: `O(sqrt(n))`
  public func add<T>(list : List<T>, element : T) {
    var elementIndex = list.elementIndex;
    if (elementIndex == 0) {
      grow_index_block_if_needed(list);
      let blockIndex = list.blockIndex;

      // When removing last we keep one more data block, so can be not empty
      if (list.blocks[blockIndex].size() == 0) {
        list.blocks[blockIndex] := VarArray.repeat<?T>(
          null,
          data_block_size(blockIndex)
        )
      }
    };

    let last_data_block = list.blocks[list.blockIndex];

    last_data_block[elementIndex] := ?element;

    elementIndex += 1;
    if (elementIndex == last_data_block.size()) {
      elementIndex := 0;
      list.blockIndex += 1
    };
    list.elementIndex := elementIndex
  };

  /// Removes and returns the last item in the list or `null` if
  /// the list is empty.
  ///
  /// Example:
  /// ```motoko include=import
  /// let list = List.empty<Nat>();
  /// List.add(list, 10);
  /// List.add(list, 11);
  /// assert List.removeLast(list) == ?11;
  /// assert List.removeLast(list) == ?10;
  /// assert List.removeLast(list) == null;
  /// ```
  ///
  /// Amortized Runtime: `O(1)`, Worst Case Runtime: `O(sqrt(n))`
  ///
  /// Amortized Space: `O(1)`, Worst Case Space: `O(sqrt(n))`
  public func removeLast<T>(list : List<T>) : ?T {
    var elementIndex = list.elementIndex;
    if (elementIndex == 0) {
      shrink_index_block_if_needed(list);

      var blockIndex = list.blockIndex;
      if (blockIndex == 1) {
        return null
      };
      blockIndex -= 1;
      elementIndex := list.blocks[blockIndex].size();

      // Keep one totally empty block when removing
      if (blockIndex + 2 < list.blocks.size()) {
        if (list.blocks[blockIndex + 2].size() == 0) {
          list.blocks[blockIndex + 2] := [var]
        }
      };
      list.blockIndex := blockIndex
    };
    elementIndex -= 1;

    var last_data_block = list.blocks[list.blockIndex];

    let element = last_data_block[elementIndex];
    last_data_block[elementIndex] := null;

    list.elementIndex := elementIndex;
    return element
  };

  func locate(index : Nat) : (Nat, Nat) {
    // see comments in tests
    let i = Nat32.fromNat(index);
    let lz = Nat32.bitcountLeadingZero(i);
    let lz2 = lz >> 1;
    if (lz & 1 == 0) {
      (Nat32.toNat(((i << lz2) >> 16) ^ (0x10000 >> lz2)), Nat32.toNat(i & (0xFFFF >> lz2)))
    } else {
      (Nat32.toNat(((i << lz2) >> 15) ^ (0x18000 >> lz2)), Nat32.toNat(i & (0x7FFF >> lz2)))
    }
  };

  /// Returns the element at index `index`. Indexing is zero-based.
  /// Traps if `index >= size`, error message may not be descriptive.
  ///
  /// Example:
  /// ```motoko include=import
  /// let list = List.empty<Nat>();
  /// List.add(list, 10);
  /// List.add(list, 11);
  /// assert List.at(list, 0) == 10;
  /// ```
  ///
  /// Runtime: `O(1)`
  public func at<T>(list : List<T>, index : Nat) : T {
    // inlined version of:
    //   let (a,b) = locate(index);
    //   switch(list.blocks[a][b]) {
    //     case (?element) element;
    //     case (null) Prim.trap "";
    //   };
    let i = Nat32.fromNat(index);
    let lz = Nat32.bitcountLeadingZero(i);
    let lz2 = lz >> 1;
    switch (
      if (lz & 1 == 0) {
        list.blocks[Nat32.toNat(((i << lz2) >> 16) ^ (0x10000 >> lz2))][Nat32.toNat(i & (0xFFFF >> lz2))]
      } else {
        list.blocks[Nat32.toNat(((i << lz2) >> 15) ^ (0x18000 >> lz2))][Nat32.toNat(i & (0x7FFF >> lz2))]
      }
    ) {
      case (?result) return result;
      case (_) Prim.trap "List index out of bounds in get"
    }
  };

  /// Returns the element at index `index` as an option.
  /// Returns `null` when `index >= size`. Indexing is zero-based.
  ///
  /// Example:
  /// ```motoko include=import
  /// let list = List.empty<Nat>();
  /// List.add(list, 10);
  /// List.add(list, 11);
  /// assert List.get(list, 0) == ?10;
  /// assert List.get(list, 2) == null;
  /// ```
  ///
  /// Runtime: `O(1)`
  ///
  /// Space: `O(1)`
  public func get<T>(list : List<T>, index : Nat) : ?T {
    let (a, b) = locate(index);
    if (a < list.blockIndex or list.elementIndex != 0 and a == list.blockIndex) {
      list.blocks[a][b]
    } else {
      null
    }
  };

  /// Overwrites the current element at `index` with `element`.
  /// Traps if `index` >= size. Indexing is zero-based.
  ///
  /// Example:
  /// ```motoko include=import
  /// let list = List.empty<Nat>();
  /// List.add(list, 10);
  /// List.put(list, 0, 20); // overwrites 10 at index 0 with 20
  /// assert List.toArray(list) == [20];
  /// ```
  ///
  /// Runtime: `O(1)`
  public func put<T>(list : List<T>, index : Nat, value : T) {
    let (a, b) = locate(index);
    if (a < list.blockIndex or a == list.blockIndex and b < list.elementIndex) {
      list.blocks[a][b] := ?value
    } else Prim.trap "List index out of bounds in put"
  };

  /// Sorts the elements in the list according to `compare`.
  /// Sort is deterministic, stable, and in-place.
  ///
  /// Example:
  /// ```motoko include=import
  /// import Nat "mo:core/Nat";
  ///
  /// let list = List.empty<Nat>();
  /// List.add(list, 3);
  /// List.add(list, 1);
  /// List.add(list, 2);
  /// List.sort(list, Nat.compare);
  /// assert List.toArray(list) == [1, 2, 3];
  /// ```
  ///
  /// Runtime: O(size * log(size))
  ///
  /// Space: O(size)
  /// *Runtime and space assumes that `compare` runs in O(1) time and space.
  public func sort<T>(list : List<T>, compare : persistent (T, T) -> Order.Order) {
    if (size(list) < 2) return;
    let arr = toVarArray(list);
    VarArray.sortInPlace(arr, compare);
    for (i in arr.keys()) {
      put(list, i, arr[i])
    }
  };

  /// Finds the first index of `element` in `list` using equality of elements defined
  /// by `equal`. Returns `null` if `element` is not found.
  ///
  /// Example:
  /// ```motoko include=import
  /// import Nat "mo:core/Nat";
  ///
  /// let list = List.empty<Nat>();
  /// List.add(list, 1);
  /// List.add(list, 2);
  /// List.add(list, 3);
  /// List.add(list, 4);
  ///
  /// assert List.indexOf<Nat>(list, Nat.equal, 3) == ?2;
  /// assert List.indexOf<Nat>(list, Nat.equal, 5) == null;
  /// ```
  ///
  /// Runtime: `O(size)`
  ///
  /// *Runtime and space assumes that `equal` runs in `O(1)` time and space.
  public func indexOf<T>(list : List<T>, equal : (T, T) -> Bool, element : T) : ?Nat {
    // inlining would save 10 instructions per entry
    findIndex<T>(list, func(x) = equal(element, x))
  };

  /// Finds the last index of `element` in `list` using equality of elements defined
  /// by `equal`. Returns `null` if `element` is not found.
  ///
  /// Example:
  /// ```motoko include=import
  /// import Nat "mo:core/Nat";
  ///
  /// let list = List.fromArray<Nat>([1, 2, 3, 4, 2, 2]);
  ///
  /// assert List.lastIndexOf<Nat>(list, Nat.equal, 2) == ?5;
  /// assert List.lastIndexOf<Nat>(list, Nat.equal, 5) == null;
  /// ```
  ///
  /// Runtime: `O(size)`
  ///
  /// *Runtime and space assumes that `equal` runs in `O(1)` time and space.
  public func lastIndexOf<T>(list : List<T>, equal : (T, T) -> Bool, element : T) : ?Nat {
    // inlining would save 10 instructions per entry
    findLastIndex<T>(list, func(x) = equal(element, x))
  };

  /// Returns the first value in `list` for which `predicate` returns true.
  /// If no element satisfies the predicate, returns null.
  ///
  /// ```motoko include=import
  /// let list = List.fromArray<Nat>([1, 9, 4, 8]);
  /// let found = List.find<Nat>(list, func(x) { x > 8 });
  /// assert found == ?9;
  /// ```
  /// Runtime: O(size)
  ///
  /// Space: O(1)
  ///
  /// *Runtime and space assumes that `predicate` runs in O(1) time and space.
  public func find<T>(list : List<T>, predicate : T -> Bool) : ?T {
    for (element in values(list)) {
      if (predicate element) {
        return ?element
      }
    };
    null
  };

  /// Finds the index of the first element in `list` for which `predicate` is true.
  /// Returns `null` if no such element is found.
  ///
  /// Example:
  /// ```motoko include=import
  /// let list = List.empty<Nat>();
  /// List.add(list, 1);
  /// List.add(list, 2);
  /// List.add(list, 3);
  /// List.add(list, 4);
  ///
  /// assert List.findIndex<Nat>(list, func(i) { i % 2 == 0 }) == ?1;
  /// assert List.findIndex<Nat>(list, func(i) { i > 5 }) == null;
  /// ```
  ///
  /// Runtime: `O(size)`
  ///
  /// *Runtime and space assumes that `predicate` runs in `O(1)` time and space.
  public func findIndex<T>(list : List<T>, predicate : T -> Bool) : ?Nat {
    let blocks = list.blocks.size();
    var blockIndex = 0;
    var elementIndex = 0;
    var size = 0;
    var db : [var ?T] = [var];
    var i = 0;

    loop {
      if (elementIndex == size) {
        blockIndex += 1;
        if (blockIndex >= blocks) return null;
        db := list.blocks[blockIndex];
        size := db.size();
        if (size == 0) return null;
        elementIndex := 0
      };
      switch (db[elementIndex]) {
        case (?x) if (predicate(x)) return ?i;
        case (_) return null
      };
      elementIndex += 1;
      i += 1
    }
  };

  /// Finds the index of the last element in `list` for which `predicate` is true.
  /// Returns `null` if no such element is found.
  ///
  /// Example:
  /// ```motoko include=import
  /// let list = List.empty<Nat>();
  /// List.add(list, 1);
  /// List.add(list, 2);
  /// List.add(list, 3);
  /// List.add(list, 4);
  ///
  /// assert List.findLastIndex<Nat>(list, func(i) { i % 2 == 0 }) == ?3;
  /// assert List.findLastIndex<Nat>(list, func(i) { i > 5 }) == null;
  /// ```
  ///
  /// Runtime: `O(size)`
  ///
  /// *Runtime and space assumes that `predicate` runs in `O(1)` time and space.
  public func findLastIndex<T>(list : List<T>, predicate : T -> Bool) : ?Nat {
    var i = size(list);
    var blockIndex = list.blockIndex;
    var elementIndex = list.elementIndex;
    var db : [var ?T] = if (blockIndex < list.blocks.size()) {
      list.blocks[blockIndex]
    } else { [var] };

    loop {
      if (blockIndex == 1) {
        return null
      };
      if (elementIndex == 0) {
        blockIndex -= 1;
        db := list.blocks[blockIndex];
        elementIndex := db.size() - 1
      } else {
        elementIndex -= 1
      };
      switch (db[elementIndex]) {
        case (?x) {
          i -= 1;
          if (predicate(x)) return ?i
        };
        case (_) Prim.trap(INTERNAL_ERROR)
      }
    }
  };

  /// Performs binary search on a sorted list to find the index of the `element`.
  /// Returns `#found(index)` if the element is found, or `#insertionIndex(index)` with the index
  /// where the element would be inserted according to the ordering if not found.
  ///
  /// If there are multiple equal elements, no guarantee is made about which index is returned.
  /// The list must be sorted in ascending order according to the `compare` function.
  ///
  /// Example:
  /// ```motoko include=import
  /// import Nat "mo:core/Nat";
  ///
  /// let list = List.fromArray<Nat>([1, 3, 5, 7, 9, 11]);
  /// assert List.binarySearch<Nat>(list, Nat.compare, 5) == #found(2);
  /// assert List.binarySearch<Nat>(list, Nat.compare, 6) == #insertionIndex(3);
  /// ```
  ///
  /// Runtime: `O(log(size))`
  ///
  /// Space: `O(1)`
  ///
  /// *Runtime and space assumes that `compare` runs in `O(1)` time and space.
  public func binarySearch<T>(list : List<T>, compare : (T, T) -> Order.Order, element : T) : {
    #found : Nat;
    #insertionIndex : Nat
  } {
    var left = 0;
    var right = size(list);
    while (left < right) {
      let mid = (left + right) / 2;
      switch (compare(at(list, mid), element)) {
        case (#less) left := mid + 1;
        case (#greater) right := mid;
        case (#equal) return #found mid
      }
    };
    #insertionIndex left
  };

  /// Returns true iff every element in `list` satisfies `predicate`.
  /// In particular, if `list` is empty the function returns `true`.
  ///
  /// Example:
  /// ```motoko include=import
  /// let list = List.empty<Nat>();
  /// List.add(list, 2);
  /// List.add(list, 3);
  /// List.add(list, 4);
  ///
  /// assert List.all<Nat>(list, func x { x > 1 });
  /// ```
  ///
  /// Runtime: `O(size)`
  ///
  /// Space: `O(1)`
  ///
  /// *Runtime and space assumes that `predicate` runs in O(1) time and space.
  public func all<T>(list : List<T>, predicate : T -> Bool) : Bool {
    not any<T>(list, func(x) : Bool = not predicate(x))
  };

  /// Returns true iff some element in `list` satisfies `predicate`.
  /// In particular, if `list` is empty the function returns `false`.
  ///
  /// Example:
  /// ```motoko include=import
  /// let list = List.empty<Nat>();
  /// List.add(list, 2);
  /// List.add(list, 3);
  /// List.add(list, 4);
  ///
  /// assert List.any<Nat>(list, func x { x > 3 });
  /// ```
  ///
  /// Runtime: `O(size)`
  ///
  /// Space: `O(1)`
  ///
  /// *Runtime and space assumes that `predicate` runs in O(1) time and space.
  public func any<T>(list : List<T>, predicate : T -> Bool) : Bool {
    switch (findIndex(list, predicate)) {
      case (null) false;
      case (_) true
    }
  };

  /// Returns an Iterator (`Iter`) over the elements of a List.
  /// Iterator provides a single method `next()`, which returns
  /// elements in order, or `null` when out of elements to iterate over.
  ///
  /// ```motoko include=import
  /// let list = List.empty<Nat>();
  /// List.add(list, 10);
  /// List.add(list, 11);
  /// List.add(list, 12);
  ///
  /// var sum = 0;
  /// for (element in List.values(list)) {
  ///   sum += element;
  /// };
  /// assert sum == 33;
  /// ```
  ///
  /// Note: This does not create a snapshot. If the returned iterator is not consumed at once,
  /// and instead the consumption of the iterator is interleaved with other operations on the
  /// List, then this may lead to unexpected results.
  ///
  /// Runtime: `O(1)`
  public func values<T>(list : List<T>) : Iter.Iter<T> = values_(list);

  /// Returns an Iterator (`Iter`) over the items (index-value pairs) in the list.
  /// Each item is a tuple of `(index, value)`. The iterator provides a single method
  /// `next()` which returns elements in order, or `null` when out of elements.
  ///
  /// ```motoko include=import
  /// import Iter "mo:core/Iter";
  ///
  /// let list = List.empty<Nat>();
  /// List.add(list, 10);
  /// List.add(list, 11);
  /// List.add(list, 12);
  /// assert Iter.toArray(List.enumerate(list)) == [(0, 10), (1, 11), (2, 12)];
  /// ```
  ///
  /// Note: This does not create a snapshot. If the returned iterator is not consumed at once,
  /// and instead the consumption of the iterator is interleaved with other operations on the
  /// List, then this may lead to unexpected results.
  ///
  /// Runtime: `O(1)`
  ///
  /// Warning: Allocates memory on the heap to store ?(Nat, T).
  public func enumerate<T>(list : List<T>) : Iter.Iter<(Nat, T)> = object {
    let blocks = list.blocks.size();
    var blockIndex = 0;
    var elementIndex = 0;
    var size = 0;
    var db : [var ?T] = [var];
    var i = 0;

    public func next() : ?(Nat, T) {
      if (elementIndex == size) {
        blockIndex += 1;
        if (blockIndex >= blocks) return null;
        db := list.blocks[blockIndex];
        size := db.size();
        if (size == 0) return null;
        elementIndex := 0
      };
      switch (db[elementIndex]) {
        case (?x) {
          let ret = ?(i, x);
          elementIndex += 1;
          i += 1;
          return ret
        };
        case (_) return null
      }
    }
  };

  /// Returns an Iterator (`Iter`) over the elements of the list in reverse order.
  /// The iterator provides a single method `next()` which returns elements from
  /// last to first, or `null` when out of elements.
  ///
  /// ```motoko include=import
  /// let list = List.empty<Nat>();
  /// List.add(list, 10);
  /// List.add(list, 11);
  /// List.add(list, 12);
  ///
  /// var sum = 0;
  /// for (element in List.reverseValues(list)) {
  ///   sum += element;
  /// };
  /// assert sum == 33;
  /// ```
  ///
  /// Note: This does not create a snapshot. If the returned iterator is not consumed at once,
  /// and instead the consumption of the iterator is interleaved with other operations on the
  /// List, then this may lead to unexpected results.
  ///
  /// Runtime: `O(1)`
  public func reverseValues<T>(list : List<T>) : Iter.Iter<T> = object {
    var blockIndex = list.blockIndex;
    var elementIndex = list.elementIndex;
    var db : [var ?T] = if (blockIndex < list.blocks.size()) {
      list.blocks[blockIndex]
    } else { [var] };

    public func next() : ?T {
      if (blockIndex == 1) {
        return null
      };
      if (elementIndex == 0) {
        blockIndex -= 1;
        db := list.blocks[blockIndex];
        elementIndex := db.size() - 1
      } else {
        elementIndex -= 1
      };

      db[elementIndex]
    }
  };

  /// Returns an Iterator (`Iter`) over the items in reverse order, i.e. pairs of index and value.
  /// Iterator provides a single method `next()`, which returns
  /// elements in reverse order, or `null` when out of elements to iterate over.
  ///
  /// ```motoko include=import
  /// import Iter "mo:core/Iter";
  ///
  /// let list = List.empty<Nat>();
  /// List.add(list, 10);
  /// List.add(list, 11);
  /// List.add(list, 12);
  /// assert Iter.toArray(List.reverseEnumerate(list)) == [(2, 12), (1, 11), (0, 10)];
  /// ```
  ///
  /// Note: This does not create a snapshot. If the returned iterator is not consumed at once,
  /// and instead the consumption of the iterator is interleaved with other operations on the
  /// List, then this may lead to unexpected results.
  ///
  /// Runtime: `O(1)`
  ///
  /// Warning: Allocates memory on the heap to store ?(T, Nat).
  public func reverseEnumerate<T>(list : List<T>) : Iter.Iter<(Nat, T)> = object {
    var i = size(list);
    var blockIndex = list.blockIndex;
    var elementIndex = list.elementIndex;
    var db : [var ?T] = if (blockIndex < list.blocks.size()) {
      list.blocks[blockIndex]
    } else { [var] };

    public func next() : ?(Nat, T) {
      if (blockIndex == 1) {
        return null
      };
      if (elementIndex == 0) {
        blockIndex -= 1;
        db := list.blocks[blockIndex];
        elementIndex := db.size() - 1
      } else {
        elementIndex -= 1
      };
      switch (db[elementIndex]) {
        case (?x) {
          i -= 1;
          return ?(i, x)
        };
        case (_) Prim.trap(INTERNAL_ERROR)
      }
    }
  };

  /// Returns an Iterator (`Iter`) over the indices (keys) of the list.
  /// The iterator provides a single method `next()` which returns indices
  /// from 0 to size-1, or `null` when out of elements.
  ///
  /// ```motoko include=import
  /// import Iter "mo:core/Iter";
  ///
  /// let list = List.empty<Text>();
  /// List.add(list, "A");
  /// List.add(list, "B");
  /// List.add(list, "C");
  /// Iter.toArray(List.keys(list)) // [0, 1, 2]
  /// ```
  ///
  /// Note: This does not create a snapshot. If the returned iterator is not consumed at once,
  /// and instead the consumption of the iterator is interleaved with other operations on the
  /// List, then this may lead to unexpected results.
  ///
  /// Runtime: `O(1)`
  public func keys<T>(list : List<T>) : Iter.Iter<Nat> = Nat.range(0, size(list));

  /// Creates a new List containing all elements from the provided iterator.
  /// Elements are added in the order they are returned by the iterator.
  ///
  /// Example:
  /// ```motoko include=import
  /// import Nat "mo:core/Nat";
  /// import Iter "mo:core/Iter";
  ///
  /// let array = [1, 1, 1];
  /// let iter = array.vals();
  ///
  /// let list = List.fromIter<Nat>(iter);
  /// assert Iter.toArray(List.values(list)) == [1, 1, 1];
  /// ```
  ///
  /// Runtime: `O(size)`
  public func fromIter<T>(iter : Iter.Iter<T>) : List<T> {
    let list = empty<T>();
    for (element in iter) add(list, element);
    list
  };

  /// Adds all elements from the provided iterator to the end of the list.
  /// Elements are added in the order they are returned by the iterator.
  ///
  /// Example:
  /// ```motoko include=import
  /// import Nat "mo:core/Nat";
  /// import Iter "mo:core/Iter";
  ///
  /// let array = [1, 1, 1];
  /// let iter = array.vals();
  /// let list = List.repeat<Nat>(2, 1);
  ///
  /// List.addAll<Nat>(list, iter);
  /// assert Iter.toArray(List.values(list)) == [2, 1, 1, 1];
  /// ```
  ///
  /// The maximum number of elements in a `List` is 2^32.
  ///
  /// Runtime: `O(size)`, where n is the size of iter.
  public func addAll<T>(list : List<T>, iter : Iter.Iter<T>) {
    for (element in iter) add(list, element)
  };

  /// Creates a new immutable array containing all elements from the list.
  /// Elements appear in the same order as in the list.
  ///
  /// Example:
  /// ```motoko include=import
  /// let list = List.fromArray<Nat>([1, 2, 3]);
  ///
  /// assert List.toArray<Nat>(list) == [1, 2, 3];
  /// ```
  ///
  /// Runtime: `O(size)`
  public func toArray<T>(list : List<T>) : [T] = Array.tabulate<T>(size(list), values_(list).unsafe_next_i);

  private func values_<T>(list : List<T>) : {
    next : () -> ?T;
    unsafe_next : () -> T;
    unsafe_next_i : Nat -> T
  } = object {
    let blocks = list.blocks.size();
    var blockIndex = 0;
    var elementIndex = 0;
    var db_size = 0;
    var db : [var ?T] = [var];

    public func next() : ?T {
      if (elementIndex == db_size) {
        blockIndex += 1;
        if (blockIndex >= blocks) return null;
        db := list.blocks[blockIndex];
        db_size := db.size();
        if (db_size == 0) return null;
        elementIndex := 0
      };
      switch (db[elementIndex]) {
        case (?x) {
          elementIndex += 1;
          return ?x
        };
        case (_) return null
      }
    };

    // version of next() without option type
    // inlined version of
    //   public func unsafe_next() : T = {
    //     let ?x = next() else Prim.trap(INTERNAL_ERROR);
    //     x;
    //   };
    public func unsafe_next() : T {
      if (elementIndex == db_size) {
        blockIndex += 1;
        if (blockIndex >= blocks) Prim.trap(INTERNAL_ERROR);
        db := list.blocks[blockIndex];
        db_size := db.size();
        if (db_size == 0) Prim.trap(INTERNAL_ERROR);
        elementIndex := 0
      };
      switch (db[elementIndex]) {
        case (?x) {
          elementIndex += 1;
          return x
        };
        case (_) Prim.trap(INTERNAL_ERROR)
      }
    };

    // version of next() without option type and throw-away argument
    // inlined version of
    //   public func unsafe_next_(i : Nat) : T = unsafe_next();
    public func unsafe_next_i(i : Nat) : T {
      if (elementIndex == db_size) {
        blockIndex += 1;
        if (blockIndex >= blocks) Prim.trap(INTERNAL_ERROR);
        db := list.blocks[blockIndex];
        db_size := db.size();
        if (db_size == 0) Prim.trap(INTERNAL_ERROR);
        elementIndex := 0
      };
      switch (db[elementIndex]) {
        case (?x) {
          elementIndex += 1;
          return x
        };
        case (_) Prim.trap(INTERNAL_ERROR)
      }
    }
  };

  /// Creates a List containing elements from an Array.
  ///
  /// Example:
  /// ```motoko include=import
  /// import Nat "mo:core/Nat";
  /// import Iter "mo:core/Iter";
  ///
  /// let array = [2, 3];
  /// let list = List.fromArray<Nat>(array);
  /// assert Iter.toArray(List.values(list)) == [2, 3];
  /// ```
  ///
  /// Runtime: `O(size)`
  public func fromArray<T>(array : [T]) : List<T> {
    let (blockIndex, elementIndex) = locate(array.size());

    let blocks = new_index_block_length(Nat32.fromNat(if (elementIndex == 0) { blockIndex - 1 } else blockIndex));
    let data_blocks = VarArray.repeat<[var ?T]>([var], blocks);
    var i = 1;
    var pos = 0;

    func make_block(len : Nat, fill : Nat) : [var ?T] {
      let block = VarArray.repeat<?T>(null, len);
      var j = 0;
      while (j < fill) {
        block[j] := ?array[pos];
        j += 1;
        pos += 1
      };
      block
    };

    while (i < blockIndex) {
      let len = data_block_size(i);
      data_blocks[i] := make_block(len, len);
      i += 1
    };
    if (elementIndex != 0 and blockIndex < blocks) {
      data_blocks[i] := make_block(data_block_size(i), elementIndex)
    };

    {
      var blocks = data_blocks;
      var blockIndex = blockIndex;
      var elementIndex = elementIndex
    };

  };

  /// Creates a new mutable array containing all elements from the list.
  /// Elements appear in the same order as in the list.
  ///
  /// Example:
  /// ```motoko include=import
  /// import Array "mo:core/Array";
  ///
  /// let list = List.fromArray<Nat>([1, 2, 3]);
  ///
  /// let varArray = List.toVarArray<Nat>(list);
  /// assert Array.fromVarArray(varArray) == [1, 2, 3];
  /// ```
  ///
  /// Runtime: `O(size)`
  public func toVarArray<T>(list : List<T>) : [var T] {
    let s = size(list);
    if (s == 0) return [var];
    let arr = VarArray.repeat<T>(Option.unwrap(first(list)), s);
    var i = 0;
    let next = values_(list).unsafe_next;
    while (i < s) {
      arr[i] := next();
      i += 1
    };
    arr
  };

  /// Creates a new List containing all elements from the mutable array.
  /// Elements appear in the same order as in the array.
  ///
  /// Example:
  /// ```motoko include=import
  /// import Nat "mo:core/Nat";
  /// import Iter "mo:core/Iter";
  ///
  /// let array = [var 2, 3];
  /// let list = List.fromVarArray<Nat>(array);
  /// assert Iter.toArray(List.values(list)) == [2, 3];
  /// ```
  ///
  /// Runtime: `O(size)`
  public func fromVarArray<T>(array : [var T]) : List<T> {
    let (blockIndex, elementIndex) = locate(array.size());

    let blocks = new_index_block_length(Nat32.fromNat(if (elementIndex == 0) { blockIndex - 1 } else blockIndex));
    let data_blocks = VarArray.repeat<[var ?T]>([var], blocks);
    var i = 1;
    var pos = 0;

    func make_block(len : Nat, fill : Nat) : [var ?T] {
      let block = VarArray.repeat<?T>(null, len);
      var j = 0;
      while (j < fill) {
        block[j] := ?array[pos];
        j += 1;
        pos += 1
      };
      block
    };

    while (i < blockIndex) {
      let len = data_block_size(i);
      data_blocks[i] := make_block(len, len);
      i += 1
    };
    if (elementIndex != 0 and blockIndex < blocks) {
      data_blocks[i] := make_block(data_block_size(i), elementIndex)
    };

    {
      var blocks = data_blocks;
      var blockIndex = blockIndex;
      var elementIndex = elementIndex
    };

  };

  /// Returns the first element of `list`, or `null` if the list is empty.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert List.first(List.fromArray<Nat>([1, 2, 3])) == ?1;
  /// assert List.first(List.empty<Nat>()) == null;
  /// ```
  ///
  /// Runtime: `O(1)`
  ///
  /// Space: `O(1)`
  public func first<T>(list : List<T>) : ?T {
    if (isEmpty(list)) null else list.blocks[1][0]
  };

  /// Returns the last element of `list`, or `null` if the list is empty.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert List.last(List.fromArray<Nat>([1, 2, 3])) == ?3;
  /// assert List.last(List.empty<Nat>()) == null;
  /// ```
  ///
  /// Runtime: `O(1)`
  ///
  /// Space: `O(1)`
  public func last<T>(list : List<T>) : ?T {
    let e = list.elementIndex;
    if (e > 0) return list.blocks[list.blockIndex][e - 1];

    let b = list.blockIndex - 1 : Nat;
    if (b == 0) null else {
      let block = list.blocks[b];
      block[block.size() - 1]
    }
  };

  /// Applies `f` to each element in `list`.
  ///
  /// Example:
  /// ```motoko include=import
  /// import Nat "mo:core/Nat";
  /// import Debug "mo:core/Debug";
  ///
  /// let list = List.fromArray<Nat>([1, 2, 3]);
  ///
  /// List.forEach<Nat>(list, func(x) {
  ///   Debug.print(Nat.toText(x)); // prints each element in list
  /// });
  /// ```
  ///
  /// Runtime: `O(size)`
  ///
  /// Space: `O(size)`
  ///
  /// *Runtime and space assumes that `f` runs in O(1) time and space.
  public func forEach<T>(list : List<T>, f : T -> ()) {
    let blocks = list.blocks.size();
    var blockIndex = 0;
    var elementIndex = 0;
    var size = 0;
    var db : [var ?T] = [var];

    loop {
      if (elementIndex == size) {
        blockIndex += 1;
        if (blockIndex >= blocks) return;
        db := list.blocks[blockIndex];
        size := db.size();
        if (size == 0) return;
        elementIndex := 0
      };
      switch (db[elementIndex]) {
        case (?x) {
          f(x);
          elementIndex += 1
        };
        case (_) return
      }
    }
  };

  /// Applies `f` to each item `(i, x)` in `list` where `i` is the key
  /// and `x` is the value.
  ///
  /// Example:
  /// ```motoko include=import
  /// import Nat "mo:core/Nat";
  /// import Debug "mo:core/Debug";
  ///
  /// let list = List.fromArray<Nat>([1, 2, 3]);
  ///
  /// List.forEachEntry<Nat>(list, func (i,x) {
  ///   // prints each item (i,x) in list
  ///   Debug.print(Nat.toText(i) # Nat.toText(x));
  /// });
  /// ```
  ///
  /// Runtime: `O(size)`
  ///
  /// Space: `O(size)`
  ///
  /// *Runtime and space assumes that `f` runs in O(1) time and space.
  public func forEachEntry<T>(list : List<T>, f : (Nat, T) -> ()) {
    /* Inlined version of
      let o = object {
        var i = 0;
        public func fx(x : T) { f(i, x); i += 1; };
      };
      iterate<T>(list, o.fx);
    */
    let blocks = list.blocks.size();
    var blockIndex = 0;
    var elementIndex = 0;
    var size = 0;
    var db : [var ?T] = [var];
    var i = 0;

    loop {
      if (elementIndex == size) {
        blockIndex += 1;
        if (blockIndex >= blocks) return;
        db := list.blocks[blockIndex];
        size := db.size();
        if (size == 0) return;
        elementIndex := 0
      };
      switch (db[elementIndex]) {
        case (?x) {
          f(i, x);
          elementIndex += 1;
          i += 1
        };
        case (_) return
      }
    }
  };

  /// Like `forEachEntryRev` but iterates through the list in reverse order,
  /// from end to beginning.
  ///
  /// Example:
  /// ```motoko include=import
  /// import Nat "mo:core/Nat";
  /// import Debug "mo:core/Debug";
  ///
  /// let list = List.fromArray<Nat>([1, 2, 3]);
  ///
  /// List.reverseForEachEntry<Nat>(list, func (i,x) {
  ///   // prints each item (i,x) in list
  ///   Debug.print(Nat.toText(i) # Nat.toText(x));
  /// });
  /// ```
  ///
  /// Runtime: `O(size)`
  ///
  /// Space: `O(size)`
  ///
  /// *Runtime and space assumes that `f` runs in O(1) time and space.
  public func reverseForEachEntry<T>(list : List<T>, f : (Nat, T) -> ()) {
    var blockIndex = list.blockIndex;
    var elementIndex = list.elementIndex;
    var db : [var ?T] = if (blockIndex < list.blocks.size()) {
      list.blocks[blockIndex]
    } else { [var] };
    var i = size(list);

    loop {
      if (blockIndex == 1) {
        return
      };
      if (elementIndex == 0) {
        blockIndex -= 1;
        db := list.blocks[blockIndex];
        elementIndex := db.size() - 1
      } else {
        elementIndex -= 1
      };
      i -= 1;
      switch (db[elementIndex]) {
        case (?x) f(i, x);
        case (_) Prim.trap(INTERNAL_ERROR)
      }
    }
  };

  /// Applies `f` to each element in `list` in reverse order.
  ///
  /// Example:
  /// ```motoko include=import
  /// import Nat "mo:core/Nat";
  /// import Debug "mo:core/Debug";
  ///
  /// let list = List.fromArray<Nat>([1, 2, 3]);
  ///
  /// List.reverseForEach<Nat>(list, func (x) {
  ///   Debug.print(Nat.toText(x)); // prints each element in list in reverse order
  /// });
  /// ```
  ///
  /// Runtime: `O(size)`
  ///
  /// Space: `O(size)`
  ///
  /// *Runtime and space assumes that `f` runs in O(1) time and space.
  public func reverseForEach<T>(list : List<T>, f : T -> ()) {
    var blockIndex = list.blockIndex;
    var elementIndex = list.elementIndex;
    var db : [var ?T] = if (blockIndex < list.blocks.size()) {
      list.blocks[blockIndex]
    } else { [var] };

    loop {
      if (blockIndex == 1) {
        return
      };
      if (elementIndex == 0) {
        blockIndex -= 1;
        db := list.blocks[blockIndex];
        elementIndex := db.size() - 1
      } else {
        elementIndex -= 1
      };
      switch (db[elementIndex]) {
        case (?x) f(x);
        case (_) Prim.trap(INTERNAL_ERROR)
      }
    }
  };

  /// Returns true if the list contains the specified element according to the provided
  /// equality function. Uses the provided `equal` function to compare elements.
  ///
  /// Example:
  /// ```motoko include=import
  /// import Nat "mo:core/Nat";
  ///
  /// let list = List.empty<Nat>();
  /// List.add(list, 2);
  /// List.add(list, 0);
  /// List.add(list, 3);
  ///
  /// assert List.contains<Nat>(list, Nat.equal, 2);
  /// ```
  ///
  /// Runtime: `O(size)`
  ///
  /// Space: `O(1)`
  ///
  /// *Runtime and space assumes that `equal` runs in O(1) time and space.
  public func contains<T>(list : List<T>, equal : (T, T) -> Bool, element : T) : Bool {
    Option.isSome(indexOf(list, equal, element))
  };

  /// Returns the greatest element in the list according to the ordering defined by `compare`.
  /// Returns `null` if the list is empty.
  ///
  /// Example:
  /// ```motoko include=import
  /// import Nat "mo:core/Nat";
  ///
  /// let list = List.empty<Nat>();
  /// List.add(list, 1);
  /// List.add(list, 2);
  ///
  /// assert List.max<Nat>(list, Nat.compare) == ?2;
  /// assert List.max<Nat>(List.empty<Nat>(), Nat.compare) == null;
  /// ```
  ///
  /// Runtime: `O(size)`
  ///
  /// Space: `O(1)`
  ///
  /// *Runtime and space assumes that `compare` runs in O(1) time and space.
  public func max<T>(list : List<T>, compare : (T, T) -> Order.Order) : ?T {
    if (isEmpty(list)) return null;

    var maxSoFar = at(list, 0);
    forEach<T>(
      list,
      func(x) = switch (compare(x, maxSoFar)) {
        case (#greater) maxSoFar := x;
        case _ {}
      }
    );

    return ?maxSoFar
  };

  /// Returns the least element in the list according to the ordering defined by `compare`.
  /// Returns `null` if the list is empty.
  ///
  /// Example:
  /// ```motoko include=import
  /// import Nat "mo:core/Nat";
  ///
  /// let list = List.empty<Nat>();
  /// List.add(list, 1);
  /// List.add(list, 2);
  ///
  /// assert List.min<Nat>(list, Nat.compare) == ?1;
  /// assert List.min<Nat>(List.empty<Nat>(), Nat.compare) == null;
  /// ```
  ///
  /// Runtime: `O(size)`
  ///
  /// Space: `O(1)`
  ///
  /// *Runtime and space assumes that `compare` runs in O(1) time and space.
  public func min<T>(list : List<T>, compare : (T, T) -> Order.Order) : ?T {
    if (isEmpty(list)) return null;

    var minSoFar = at(list, 0);
    forEach<T>(
      list,
      func(x) = switch (compare(x, minSoFar)) {
        case (#less) minSoFar := x;
        case _ {}
      }
    );

    return ?minSoFar
  };

  /// Tests if two lists are equal by comparing their elements using the provided `equal` function.
  /// Returns true if and only if both lists have the same size and all corresponding elements
  /// are equal according to the provided function.
  ///
  /// Example:
  /// ```motoko include=import
  /// import Nat "mo:core/Nat";
  ///
  /// let list1 = List.fromArray<Nat>([1,2]);
  /// let list2 = List.empty<Nat>();
  /// List.add(list2, 1);
  /// List.add(list2, 2);
  ///
  /// assert List.equal<Nat>(list1, list2, Nat.equal);
  /// ```
  ///
  /// Runtime: `O(size)`
  ///
  /// Space: `O(1)`
  ///
  /// *Runtime and space assumes that `equal` runs in O(1) time and space.
  public func equal<T>(list1 : List<T>, list2 : List<T>, equal : (T, T) -> Bool) : Bool {
    let size1 = size(list1);

    if (size1 != size(list2)) return false;

    let next1 = values_(list1).unsafe_next;
    let next2 = values_(list2).unsafe_next;
    var i = 0;
    while (i < size1) {
      if (not equal(next1(), next2())) return false;
      i += 1
    };

    return true
  };

  /// Compares two lists lexicographically using the provided `compare` function.
  /// Elements are compared pairwise until a difference is found or one list ends.
  /// If all elements compare equal, the shorter list is considered less than the longer list.
  ///
  /// Example:
  /// ```motoko include=import
  /// import Nat "mo:core/Nat";
  ///
  /// let list1 = List.fromArray<Nat>([0, 1]);
  /// let list2 = List.fromArray<Nat>([2]);
  /// let list3 = List.fromArray<Nat>([0, 1, 2]);
  ///
  /// assert List.compare<Nat>(list1, list2, Nat.compare) == #less;
  /// assert List.compare<Nat>(list1, list3, Nat.compare) == #less;
  /// assert List.compare<Nat>(list2, list3, Nat.compare) == #greater;
  /// ```
  ///
  /// Runtime: `O(size)`
  ///
  /// Space: `O(1)`
  ///
  /// *Runtime and space assumes that `compare` runs in O(1) time and space.
  public func compare<T>(list1 : List<T>, list2 : List<T>, compare : (T, T) -> Order.Order) : Order.Order {
    let size1 = size(list1);
    let size2 = size(list2);
    let minSize = if (size1 < size2) { size1 } else { size2 };

    let next1 = values_(list1).unsafe_next;
    let next2 = values_(list2).unsafe_next;
    var i = 0;
    while (i < minSize) {
      switch (compare(next1(), next2())) {
        case (#less) return #less;
        case (#greater) return #greater;
        case _ {}
      };
      i += 1
    };
    Nat.compare(size1, size2)
  };

  /// Creates a textual representation of `list`, using `toText` to recursively
  /// convert the elements into Text.
  ///
  /// Example:
  /// ```motoko include=import
  /// import Nat "mo:core/Nat";
  ///
  /// let list = List.fromArray<Nat>([1,2,3,4]);
  ///
  /// assert List.toText<Nat>(list, Nat.toText) == "List[1, 2, 3, 4]";
  /// ```
  ///
  /// Runtime: `O(size)`
  ///
  /// Space: `O(size)`
  ///
  /// *Runtime and space assumes that `toText` runs in O(1) time and space.
  public func toText<T>(list : List<T>, f : T -> Text) : Text {
    let vsize : Int = size(list);
    let next = values_(list).unsafe_next;
    var i = 0;
    var text = "";
    while (i < vsize - 1) {
      text := text # f(next()) # ", "; // Text implemented as rope
      i += 1
    };
    if (vsize > 0) {
      // avoid the trailing comma
      text := text # f(at<T>(list, i))
    };

    "List[" # text # "]"
  };

  /// Collapses the elements in `list` into a single value by starting with `base`
  /// and progessively combining elements into `base` with `combine`. Iteration runs
  /// left to right.
  ///
  /// Example:
  /// ```motoko include=import
  /// import Nat "mo:core/Nat";
  ///
  /// let list = List.fromArray<Nat>([1,2,3]);
  ///
  /// assert List.foldLeft<Text, Nat>(list, "", func (acc, x) { acc # Nat.toText(x)}) == "123";
  /// ```
  ///
  /// Runtime: `O(size)`
  ///
  /// Space: `O(1)`
  ///
  /// *Runtime and space assumes that `combine` runs in O(1)` time and space.
  public func foldLeft<A, T>(list : List<T>, base : A, combine : (A, T) -> A) : A {
    var accumulation = base;

    forEach<T>(
      list,
      func(x) = accumulation := combine(accumulation, x)
    );

    accumulation
  };

  /// Collapses the elements in `list` into a single value by starting with `base`
  /// and progessively combining elements into `base` with `combine`. Iteration runs
  /// right to left.
  ///
  /// Example:
  /// ```motoko include=import
  /// import Nat "mo:core/Nat";
  ///
  /// let list = List.fromArray<Nat>([1,2,3]);
  ///
  /// assert List.foldRight<Nat, Text>(list, "", func (x, acc) { Nat.toText(x) # acc }) == "123";
  /// ```
  ///
  /// Runtime: `O(size)`
  ///
  /// Space: `O(1)`
  ///
  /// *Runtime and space assumes that `combine` runs in O(1)` time and space.
  public func foldRight<T, A>(list : List<T>, base : A, combine : (T, A) -> A) : A {
    var accumulation = base;

    reverseForEach<T>(
      list,
      func(x) = accumulation := combine(x, accumulation)
    );

    accumulation
  };

  /// Reverses the order of elements in `list` by overwriting in place.
  ///
  /// Example:
  /// ```motoko include=import
  /// import Nat "mo:core/Nat";
  /// import Iter "mo:core/Iter";
  ///
  /// let list = List.fromArray<Nat>([1,2,3]);
  ///
  /// List.reverseInPlace<Nat>(list);
  /// assert Iter.toArray(List.values(list)) == [3, 2, 1];
  /// ```
  ///
  /// Runtime: `O(size)`
  ///
  /// Space: `O(1)`
  public func reverseInPlace<T>(list : List<T>) {
    let vsize = size(list);
    if (vsize == 0) return;

    var i = 0;
    var j = vsize - 1 : Nat;
    var temp = at(list, 0);
    while (i < vsize / 2) {
      temp := at(list, j);
      put(list, j, at(list, i));
      put(list, i, temp);
      i += 1;
      j -= 1
    }
  };

  /// Returns a new List with the elements from `list` in reverse order.
  ///
  /// Example:
  /// ```motoko include=import
  /// import Nat "mo:core/Nat";
  /// import Iter "mo:core/Iter";
  ///
  /// let list = List.fromArray<Nat>([1,2,3]);
  ///
  /// let rlist = List.reverse<Nat>(list);
  /// assert Iter.toArray(List.values(rlist)) == [3, 2, 1];
  /// ```
  ///
  /// Runtime: `O(size)`
  ///
  /// Space: `O(1)`
  public func reverse<T>(list : List<T>) : List<T> {
    let rlist = empty<T>();

    reverseForEach<T>(
      list,
      func(x) = add(rlist, x)
    );

    rlist
  };

  /// Returns true if and only if the list is empty.
  ///
  /// Example:
  /// ```motoko include=import
  /// let list = List.fromArray<Nat>([2,0,3]);
  /// assert not List.isEmpty<Nat>(list);
  /// assert List.isEmpty<Nat>(List.empty<Nat>());
  /// ```
  ///
  /// Runtime: `O(1)`
  ///
  /// Space: `O(1)`
  public func isEmpty<T>(list : List<T>) : Bool {
    list.blockIndex == 1 and list.elementIndex == 0
  }
}
