# Motoko base package

* [Array](Array.md) Provides extended utility functions on Arrays.
* [AssocList](AssocList.md) Map implemented as a linked-list of key-value pairs ("Associations").
* [Blob](Blob.md) `Blob` is an immutable, iterable sequence of bytes. Unlike `[Nat8]`, which is less compact (using 4 bytes per logical byte), `Blob` provides a more efficient representation.
* [Bool](Bool.md) Boolean type and operations.
* [Buffer](Buffer.md) Class `Buffer<X>` provides a mutable list of elements of type `X`.
* [CertifiedData](CertifiedData.md) The Internet Computer allows canister smart contracts to store a small amount of data during
* [Char](Char.md) 
* [Debug](Debug.md) Utility functions for debugging.
* [Deque](Deque.md) Double-ended queue (deque) of a generic element type `T`.
* [Error](Error.md) Error values and inspection.
* [ExperimentalCycles](ExperimentalCycles.md) Managing cycles within actors on the Internet Computer (ICP).
* [ExperimentalInternetComputer](ExperimentalInternetComputer.md) Low-level interface to the Internet Computer.
* [ExperimentalStableMemory](ExperimentalStableMemory.md) Byte-level access to (virtual) _stable memory_.
* [Float](Float.md) Double precision (64-bit) floating-point numbers in IEEE 754 representation.
* [Func](Func.md) Create functions from simpler inputs, most commonly used when programming in functional style using higher-order functions.
* [Hash](Hash.md) 
* [HashMap](HashMap.md) Class `HashMap<K, V>` provides a hashmap from keys of type `K` to values of type `V`.
* [Heap](Heap.md) Class `Heap<X>` provides a priority queue of elements of type `X`.
* [Int](Int.md) Signed integer numbers with infinite precision (also called big integers).
* [Int16](Int16.md) Provides utility functions on 16-bit signed integers.
* [Int32](Int32.md) Provides utility functions on 32-bit signed integers.
* [Int64](Int64.md) Provides utility functions on 64-bit signed integers.
* [Int8](Int8.md) Provides utility functions on 8-bit signed integers.
* [Iter](Iter.md) 
* [IterType](IterType.md) 
* [List](List.md) Purely-functional, singly-linked lists.
* [Nat](Nat.md) Natural numbers with infinite precision.
* [Nat16](Nat16.md) Provides utility functions on 16-bit unsigned integers.
* [Nat32](Nat32.md) Provides utility functions on 32-bit unsigned integers.
* [Nat64](Nat64.md) Provides utility functions on 64-bit unsigned integers.
* [Nat8](Nat8.md) Provides utility functions on 8-bit unsigned integers.
* [None](None.md) The `None` type represents a type with _no_ value.
* [Option](Option.md) Optional values can be seen as a typesafe `null`. A value of type `?Int` can
* [Order](Order.md) 
* [OrderedMap](OrderedMap.md) Stable key-value map implemented as a red-black tree with nodes storing key-value pairs.
* [OrderedSet](OrderedSet.md) Stable ordered set implemented as a red-black tree.
* [Prelude](Prelude.md) This prelude file proposes standard library features that _may_
* [Principal](Principal.md) Module for interacting with Principals (users, canisters, or other entities).
* [RBTree](RBTree.md) Key-value map implemented as a red-black tree (RBTree) with nodes storing key-value pairs.
* [Random](Random.md) A module for obtaining randomness on the Internet Computer (IC).
* [Region](Region.md) Byte-level access to isolated, (virtual) stable memory _regions_.
* [Result](Result.md) Error handling with the `Result` type.
* [Stack](Stack.md) Class `Stack<X>` provides a minimal LIFO stack of elements of type `X`.
* [Text](Text.md) Utility functions for `Text` values.
* [Time](Time.md) System time
* [Timer](Timer.md) Timers for one-off or periodic tasks. Applicable as part of the default mechanism.
* [Trie](Trie.md) Functional key-value hash map.
* [TrieMap](TrieMap.md) Class `TrieMap<K, V>` provides a map from keys of type `K` to values of type `V`.
* [TrieSet](TrieSet.md) 
