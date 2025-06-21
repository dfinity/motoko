# core/internal/BTreeHelper

## Function `insertAtPosition`
``` motoko no-repl
func insertAtPosition<T>(array : [var ?T], insertElement : ?T, insertIndex : Nat, currentLastElementIndex : Nat)
```

Inserts an element into a mutable array at a specific index, shifting all other elements over

Parameters:

array - the array being inserted into
insertElement - the element being inserted
insertIndex - the index at which the element will be inserted
currentLastElementIndex - the index of last **non-null** element in the array (used to start shifting elements over)

Note: This assumes that there are nulls at the end of the array and that the array is not full.
If the array is already full, this function will overflow the array size when attempting to
insert and will cause the cansiter to trap

## Function `insertOneAtIndexAndSplitArray`
``` motoko no-repl
func insertOneAtIndexAndSplitArray<T>(array : [var ?T], insertElement : T, insertIndex : Nat) : ([var ?T], T, [var ?T])
```

Splits the array into two halves as if the insert has occured, omitting the middle element and returning it so that it can
be promoted to the parent internal node. This is used when inserting an element into an array of elements that
is already full.

Note: Use only when inserting an element into a FULL array & promoting the resulting midpoint element.
This is NOT the same as just splitting this array!

Parameters:

array - the array being split
insertElement - the element being inserted
insertIndex - the position/index that the insertElement should be inserted

## Function `splitArrayAndInsertTwo`
``` motoko no-repl
func splitArrayAndInsertTwo<T>(children : [var ?T], rebalancedChildIndex : Nat, leftChildInsert : T, rightChildInsert : T) : ([var ?T], [var ?T])
```

Context of use: This function is used after inserting a child node into the full child of an internal node that is also full.
From the insertion, the full child is rebalanced and split, and then since the internal node is full, when replacing the two
halves of that rebalanced child into the internal node's children this causes a second split. This function takes in the
internal node's children, and the "rebalanced" split child nodes, as well as the index at which the "rebalanced" left and right
child will be inserted and replaces the original child with those two halves

Note: Use when inserting two successive elements into a FULL array and splitting that array.
This is NOT the same as just splitting this array!

Assumptions: this function also assumes that the children array is full (no nulls)

Parameters:

children - the internal node's children array being split
rebalancedChildIndex - the index used to mark where the rebalanced left and right children will be inserted
leftChildInsert - the rebalanced left child being inserted
rightChildInsert - the rebalanced right child being inserted

## Function `deleteAndShift`
``` motoko no-repl
func deleteAndShift<T>(array : [var ?T], deleteIndex : Nat) : T
```

Specific to the BTree delete implementation (assumes node ordering such that nulls come at the end of the array)

Assumptions:
* All nulls come at the end of the array
* Assumes the delete index provided is correct and non null - will trap otherwise
* deleteIndex < array.size()

Deletes an element from the the array, and then shifts all non-null elements coming after that deleted element by 1
to the left. Returns the element that was deleted.

## Function `replaceTwoWithElementAndShift`
``` motoko no-repl
func replaceTwoWithElementAndShift<T>(array : [var ?T], element : T, replaceIndex : Nat)
```


## Function `insertAtPostionAndDeleteAtPosition`
``` motoko no-repl
func insertAtPostionAndDeleteAtPosition<T>(array : [var ?T], insertElement : ?T, insertIndex : Nat, deleteIndex : Nat) : T
```

BTree specific implementation

In a single iteration insert at one position of the array while deleting at another position of the array, shifting all
elements as appropriate

This is used when borrowing an element from an inorder predecessor/successor through the parent node

## Type `DeletionSide`
``` motoko no-repl
type DeletionSide = {#left; #right}
```


## Function `mergeParentWithChildrenAndDelete`
``` motoko no-repl
func mergeParentWithChildrenAndDelete<T>(parentElement : ?T, childCount : Nat, leftChild : [var ?T], rightChild : [var ?T], deleteIndex : Nat, deletionSide : DeletionSide) : ([var ?T], T)
```

