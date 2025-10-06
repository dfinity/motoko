// Implementation is courtesy of Byron Becker.
// Source: https://github.com/canscale/StableHeapBTreeMap
// Copyright (c) 2022 Byron Becker.
// Distributed under Apache 2.0 license.
// With adjustments by the Motoko team.

import VarArray "../VarArray";
import Runtime "../Runtime";

module {
  /// Inserts an element into a mutable array at a specific index, shifting all other elements over
  ///
  /// Parameters:
  ///
  /// array - the array being inserted into
  /// insertElement - the element being inserted
  /// insertIndex - the index at which the element will be inserted
  /// currentLastElementIndex - the index of last **non-null** element in the array (used to start shifting elements over)
  ///
  /// Note: This assumes that there are nulls at the end of the array and that the array is not full.
  /// If the array is already full, this function will overflow the array size when attempting to
  /// insert and will cause the cansiter to trap
  public func insertAtPosition<T>(array : [var ?T], insertElement : ?T, insertIndex : Nat, currentLastElementIndex : Nat) {
    // if inserting at the end of the array, don't need to do any shifting and can just insert and return
    if (insertIndex == currentLastElementIndex + 1) {
      array[insertIndex] := insertElement;
      return
    };

    // otherwise, need to shift all of the elements at the end of the array over one by one until
    // the insert index is hit.
    var j = currentLastElementIndex;
    label l loop {
      array[j + 1] := array[j];
      if (j == insertIndex) {
        array[j] := insertElement;
        break l
      };

      j -= 1
    }
  };

  /// Splits the array into two halves as if the insert has occured, omitting the middle element and returning it so that it can
  /// be promoted to the parent internal node. This is used when inserting an element into an array of elements that
  /// is already full.
  ///
  /// Note: Use only when inserting an element into a FULL array & promoting the resulting midpoint element.
  /// This is NOT the same as just splitting this array!
  ///
  /// Parameters:
  ///
  /// array - the array being split
  /// insertElement - the element being inserted
  /// insertIndex - the position/index that the insertElement should be inserted
  public func insertOneAtIndexAndSplitArray<T>(array : [var ?T], insertElement : T, insertIndex : Nat) : ([var ?T], T, [var ?T]) {
    // split at the BTree order / 2
    let splitIndex = (array.size() + 1) / 2;
    // this function assumes the the splitIndex is in the middle of the kvs array - trap otherwise
    if (splitIndex > array.size()) { assert false };

    let leftSplit = if (insertIndex < splitIndex) {
      VarArray.tabulate<?T>(
        array.size(),
        func(i) {
          // if below the split index
          if (i < splitIndex) {
            // if below the insert index, copy over
            if (i < insertIndex) { array[i] }
            // if less than the insert index, copy over the previous element (since the inserted element has taken up 1 extra slot)
            else if (i > insertIndex) { array[i - 1] }
            // if equal to the insert index add the element to be inserted to the left split
            else { ?insertElement }
          } else { null }
        }
      )
    }
    // index >= splitIndex
    else {
      VarArray.tabulate<?T>(
        array.size(),
        func(i) {
          // right biased splitting
          if (i < splitIndex) { array[i] } else { null }
        }
      )
    };

    let (rightSplit, middleElement) : ([var ?T], ?T) =
    // if insert > split index, inserted element will be inserted into the right split
    if (insertIndex > splitIndex) {
      let right = VarArray.tabulate<?T>(
        array.size(),
        func(i) {
          let adjIndex = i + splitIndex + 1; // + 1 accounts for the fact that the split element was part of the original array
          if (adjIndex <= array.size()) {
            if (adjIndex < insertIndex) { array[adjIndex] } else if (adjIndex > insertIndex) {
              array[adjIndex - 1]
            } else { ?insertElement }
          } else { null }
        }
      );
      (right, array[splitIndex])
    }
    // if inserted element was placed in the left split
    else if (insertIndex < splitIndex) {
      let right = VarArray.tabulate<?T>(
        array.size(),
        func(i) {
          let adjIndex = i + splitIndex;
          if (adjIndex < array.size()) { array[adjIndex] } else { null }
        }
      );
      (right, array[splitIndex - 1])
    }
    // insertIndex == splitIndex
    else {
      let right = VarArray.tabulate<?T>(
        array.size(),
        func(i) {
          let adjIndex = i + splitIndex;
          if (adjIndex < array.size()) { array[adjIndex] } else { null }
        }
      );
      (right, ?insertElement)
    };

    switch (middleElement) {
      case null {
        Runtime.trap("UNREACHABLE_ERROR: file a bug report! In internal/BTreeHelper: insertOneAtIndexAndSplitArray, middle element of a BTree node should never be null")
      };
      case (?el) { (leftSplit, el, rightSplit) }
    }
  };

  /// Context of use: This function is used after inserting a child node into the full child of an internal node that is also full.
  /// From the insertion, the full child is rebalanced and split, and then since the internal node is full, when replacing the two
  /// halves of that rebalanced child into the internal node's children this causes a second split. This function takes in the
  /// internal node's children, and the "rebalanced" split child nodes, as well as the index at which the "rebalanced" left and right
  /// child will be inserted and replaces the original child with those two halves
  ///
  /// Note: Use when inserting two successive elements into a FULL array and splitting that array.
  /// This is NOT the same as just splitting this array!
  ///
  /// Assumptions: this function also assumes that the children array is full (no nulls)
  ///
  /// Parameters:
  ///
  /// children - the internal node's children array being split
  /// rebalancedChildIndex - the index used to mark where the rebalanced left and right children will be inserted
  /// leftChildInsert - the rebalanced left child being inserted
  /// rightChildInsert - the rebalanced right child being inserted
  public func splitArrayAndInsertTwo<T>(children : [var ?T], rebalancedChildIndex : Nat, leftChildInsert : T, rightChildInsert : T) : ([var ?T], [var ?T]) {
    let splitIndex = children.size() / 2;

    let leftRebalancedChildren = VarArray.tabulate<?T>(
      children.size(),
      func(i) {
        // only insert elements up to the split index and fill the rest of the children with nulls
        if (i <= splitIndex) {
          if (i < rebalancedChildIndex) { children[i] }
          // insert the left and right rebalanced child halves if the rebalancedChildIndex comes before the splitIndex
          else if (i == rebalancedChildIndex) {
            ?leftChildInsert
          } else if (i == rebalancedChildIndex + 1) { ?rightChildInsert } else {
            children[i - 1]
          } // i > rebalancedChildIndex
        } else { null }
      }
    );

    let rightRebalanceChildren : [var ?T] =
    // Case 1: if both left and right rebalanced halves were inserted into the left child can just go from the split index onwards
    if (rebalancedChildIndex + 1 <= splitIndex) {
      VarArray.tabulate<?T>(
        children.size(),
        func(i) {
          let adjIndex = i + splitIndex;
          if (adjIndex < children.size()) { children[adjIndex] } else { null }
        }
      )
    }
    // Case 2: if both left and right rebalanced halves will be inserted into the right child
    else if (rebalancedChildIndex > splitIndex) {
      var rebalanceOffset = 0;
      VarArray.tabulate<?T>(
        children.size(),
        func(i) {
          let adjIndex = i + splitIndex + 1;
          if (adjIndex == rebalancedChildIndex) { ?leftChildInsert } else if (adjIndex == rebalancedChildIndex + 1) {
            rebalanceOffset := 1; // after inserting both rebalanced children, any elements coming after are from the previous index
            ?rightChildInsert
          } else if (adjIndex <= children.size()) {
            children[adjIndex - rebalanceOffset]
          } else { null }
        }
      )
    }
    // Case 3: if left rebalanced half was in left child, and right rebalanced half will be in right child
    // rebalancedChildIndex == splitIndex
    else {
      VarArray.tabulate<?T>(
        children.size(),
        func(i) {
          // first element is the right rebalanced half
          if (i == 0) { ?rightChildInsert } else {
            let adjIndex = i + splitIndex;
            if (adjIndex < children.size()) { children[adjIndex] } else {
              null
            }
          }
        }
      )
    };

    (leftRebalancedChildren, rightRebalanceChildren)
  };

  /// Specific to the BTree delete implementation (assumes node ordering such that nulls come at the end of the array)
  ///
  /// Assumptions:
  /// * All nulls come at the end of the array
  /// * Assumes the delete index provided is correct and non null - will trap otherwise
  /// * deleteIndex < array.size()
  ///
  /// Deletes an element from the the array, and then shifts all non-null elements coming after that deleted element by 1
  /// to the left. Returns the element that was deleted.
  public func deleteAndShift<T>(array : [var ?T], deleteIndex : Nat) : T {
    var deleted : T = switch (array[deleteIndex]) {
      case null {
        Runtime.trap("UNREACHABLE_ERROR: file a bug report! In internal/BTreeHelper: deleteAndShift, an invalid/incorrect delete index was passed")
      };
      case (?el) { el }
    };

    array[deleteIndex] := null;

    var i = deleteIndex + 1;
    label l loop {
      if (i >= array.size()) { break l };

      switch (array[i]) {
        case null { break l };
        case (?_) {
          array[i - 1] := array[i]
        }
      };

      i += 1
    };

    array[i - 1] := null;

    deleted
  };

  // replaces two successive elements in the array with a single element and shifts all other elements to the left by 1
  public func replaceTwoWithElementAndShift<T>(array : [var ?T], element : T, replaceIndex : Nat) {
    array[replaceIndex] := ?element;

    var i = replaceIndex + 1;
    let endShiftIndex : Nat = array.size() - 1;
    while (i < endShiftIndex) {
      switch (array[i]) {
        case (?_) { array[i] := array[i + 1] };
        case null { return }
      };

      i += 1
    };

    array[endShiftIndex] := null
  };

  /// BTree specific implementation
  ///
  /// In a single iteration insert at one position of the array while deleting at another position of the array, shifting all
  /// elements as appropriate
  ///
  /// This is used when borrowing an element from an inorder predecessor/successor through the parent node
  public func insertAtPostionAndDeleteAtPosition<T>(array : [var ?T], insertElement : ?T, insertIndex : Nat, deleteIndex : Nat) : T {
    var deleted : T = switch (array[deleteIndex]) {
      case null {
        Runtime.trap("UNREACHABLE_ERROR: file a bug report! In internal/BTreeHelper: insertAtPositionAndDeleteAtPosition, and incorrect delete index was passed")
      }; // indicated an incorrect delete index was passed - trap
      case (?el) { el }
    };

    // Example of this case:
    //
    //    Insert         Delete
    //      V              V
    //[var ?10, ?20, ?30, ?40, ?50]
    if (insertIndex < deleteIndex) {
      var i = deleteIndex;
      while (i > insertIndex) {
        array[i] := array[i - 1];
        i -= 1
      };

      array[insertIndex] := insertElement
    }
    // Example of this case:
    //
    //    Delete         Insert
    //      V              V
    //[var ?10, ?20, ?30, ?40, ?50]
    else if (insertIndex > deleteIndex) {
      array[deleteIndex] := null;
      var i = deleteIndex + 1;
      label l loop {
        if (i >= array.size()) { assert false; break l }; // TODO: remove? this should not happen since the insertIndex should get hit first?

        if (i == insertIndex) {
          array[i - 1] := array[i];
          array[i] := insertElement;
          break l
        } else {
          array[i - 1] := array[i]
        };

        i += 1
      };

    }
    // insertIndex == deleteIndex, can just do a swap
    else { array[deleteIndex] := insertElement };

    deleted
  };

  // which child the deletionIndex is referring to
  public type DeletionSide = { #left; #right };

  // merges a middle (parent) element with the left and right child arrays while deleting the element from the correct child by the deleteIndex passed
  public func mergeParentWithChildrenAndDelete<T>(
    parentElement : ?T,
    childCount : Nat,
    leftChild : [var ?T],
    rightChild : [var ?T],
    deleteIndex : Nat,
    deletionSide : DeletionSide
  ) : ([var ?T], T) {
    let mergedArray = VarArray.repeat<?T>(null, leftChild.size());
    var i = 0;
    switch (deletionSide) {
      case (#left) {
        // BTree implementation expects the deleted element to exist - if null, traps
        let deletedElement = switch (leftChild[deleteIndex]) {
          case (?el) { el };
          case null {
            Runtime.trap("UNREACHABLE_ERROR: file a bug report! In internal/BTreeHelper: mergeParentWithChildrenAndDelete, an invalid delete index was passed")
          }
        };

        // copy over left child until deleted element is hit, then copy all elements after the deleted element
        while (i < childCount) {
          if (i < deleteIndex) {
            mergedArray[i] := leftChild[i]
          } else {
            mergedArray[i] := leftChild[i + 1]
          };
          i += 1
        };

        // insert parent kv in the middle
        mergedArray[childCount - 1] := parentElement;

        // copy over the rest of the right child elements
        while (i < childCount * 2) {
          mergedArray[i] := rightChild[i - childCount];
          i += 1
        };

        (mergedArray, deletedElement)
      };
      case (#right) {
        // BTree implementation expects the deleted element to exist - if null, traps
        let deletedElement = switch (rightChild[deleteIndex]) {
          case (?el) { el };
          case null {
            Runtime.trap("UNREACHABLE_ERROR: file a bug report! In internal/BTreeHelper: mergeParentWithChildrenAndDelete: element at deleted index must exist")
          }
        };
        // since deletion side is #right, can safely copy over all elements from the left child
        while (i < childCount) {
          mergedArray[i] := leftChild[i];
          i += 1
        };

        // insert parent kv in the middle
        mergedArray[childCount] := parentElement;
        i += 1;

        var j = 0;
        // copy over right child until deleted element is hit, then copy elements after the deleted element
        while (i < childCount * 2) {
          if (j < deleteIndex) {
            mergedArray[i] := rightChild[j]
          } else {
            mergedArray[i] := rightChild[j + 1]
          };
          i += 1;
          j += 1
        };

        (mergedArray, deletedElement)
      }
    }
  };

}
