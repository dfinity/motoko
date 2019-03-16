ActorScript Standard Library
==============================

Prior Documentation
-------------------------

- [(Jira Story)](https://dfinity.atlassian.net/browse/AST-31):
- Older [See #127](https://github.com/dfinity-lab/actorscript/issues/127)

--------------------------------------------------------------

Produce Exchange
=================

We focus on explaining the role of these collections from perspective
of the [produce exchange example](produce-exchange).

See also, the [library modules by priority](#library-modules-by-priority).

`Map`
-------

Finite maps.

See implementations: `Trie` and `AssocList`.


[`Trie`]()
----------

Represent a finite map with a _canonical binary tree_, based on hashing each key.

To do

`Trie2D`
----------

A Trie2D is a trie that maps each key to a _secondary trie_, representing a _second dimension_.

A Trie2D represents a 2D mapping, but in a kind of _curried form_,
that permits efficient, eagerly-applied partial application of the
first dimension's keys.

To do

`TrieCursor`
---------------

Provide navigational controls for a trie, e.g., holding query results.

To do


[`AssocList`]
------------

Represent a finite map with an _association list_ a list of key-value pairs.

To do

[`List`]()
-----------

Linked lists

To do

`Bits`
---------

Bit strings

Convert "everything", e.g., each kind of entity in a DFINITY canister
application, into its canonical iterable bit string, for the purposes of:

 - hashing it, e.g., for use as complex keys in databases or caches
 - serializing it and de-serializing it
 - representing in IDL / system messages / wire format

To do

**Open questions**

- If, how and when will the compiler/language just support this stuff "for free"?
- versus, how much should we write as ActorScript-based abstractions?


`Array`
-----------

To do

Concatenate arrays; query outputs are arrays of ids, represented
either directly, or with a cursor.

`Hash`
-----------

To do

Convert iterable bit strings into compact hashes.

Convert each compact hash into its canonical iterable bit string (but
not reversing the hash, of course!).

---------------------------------------------------------------

Library Modules, by priority
==============================

Primary modules
-----------------------

See also, the [produce exchange](#produce-exchange) use case.

 - [x] **List**: See [`List` module from SML Basis library](http://sml-family.org/Basis/list.html).
 - [x] **Hashtrie**: Persistent maps, as functional hash tries.
 - [x] **Set**: Persistent sets, based directly on persistent maps.
 - [ ] **Hashtable**: Mutable maps, as imperative hash tables.

Secondary modules
------------------------

These modules _may_ be useful in the collections library:
 - [ ] **Stream**: Type def done; most operations are pending...

Other modules
-----------------

These modules are merely exercises (toys/examples), and _not_ essential to the collections library:
 - [ ] **Thunk**: Type def and some operations are done.


----------------------------------------------------------------


Performance considerations
====================================================================

After having a functioning specification, we will employ the thoughts
below toward getting better performance.

The main thrust of the work on this canister is currently focused on
creating an executable prototype.

At some point (near the end of our test suite components), we will
want to consider botj the **asymptotic** and **constant-factor** performance 
properties of our implementation.

In particular, this performance is phrased in terms of **workloads**,
executing update & query behavior over the PESS server definition
implementation.

We shall vary workloads in kind and size, and measure space and time
usage of the Wasm VM running this implementation, in terms of the
standard library of collections implemented here.

We shall compare the performance on fixed workloads across varying
representations of the `Map` data structure that we use.

Notably, the one and only collection type used in this implementation
is the `Map` type.  With two implementations:

 - [Association lists]()
 - [Hash tries]()

We use purely-functional data structures for `Map` since their design
permits `O(1)`-time/space for sharing, and their immutability makes
them suitable for mathematical reasoning.

As explained below, the hash trie representation is asymptotically
efficient for large sizes; while association lists are not, they are
suitable for small sub-cases, including those where hash collisions
occur in the trie structure.

These mathematical properties are practically important for affording
a reasonably-efficient executable specification, but they also suggest
even more optimized representations, with the same mathematical
properties.  First, 

Hash tries
---------------------------------

Before considering other variations, we review the basic properties of
the hash trie representation.

Crucially, the hash trie implementation of `Map` uses a _functional
representation_, with expected times as follows (expected time
analysis, because of hashing):

```
   Trie.copy                      : O(1)
   Trie.find                      : O(log n)
   Trie.replace, .insert, .remove : O(log n)
   Trie.merge, .split             : O(log n)
   Trie.union                     : O(n)
   Trie.intersect                 : O(n)
```

Alternative representations
----------------------------

We consider variations of how to represent a `Map`, both as variations
of the hash trie, and as other representations as well (see below).

First, we might consider validating the followin claim:

>    **Claim:** The asymptotic properties of the hash trie are ideal
>    for a practical (infinitely-scalable) implementation of PESS.

Before considering other representations, we should evaluate this
claim on randomly-generated use-cases of varying size, to simulate
realistic (but synthetic) workloads, and measure time and space usage
by the Wasm VM.

Once we can generate performance plots, we should consider comparing
different representations for `Map` that still use a hash trie.

Chunks
-------

A simple variation of the hash trie uses **"chunks"** at the leaves,
to represent sub-maps of the threshhold size where the pointers
involved in the per-hash-bit branching no longer pays off.

So, we may first consider additional implementations by varying the
details of these chunks:
    
 - when the basecase of the hash trie occurs, and
 - how the basecase of the hash trie is represented

We consider several simple, but practical representations of chunks below.


Association array representation:
---------------------------------------

Association arrays are optimized for cache locality.  They each store
a key-value mapping as two arrays: one of keys, and one of values.  To
find a key-value pair, do a linear-scan in the array of keys to
find the corresponding position of the value, in that array. Regrow
the two arrays by doubling, or some other scheme.

```
   Aa.copy                      : O(n)
   Aa.find                      : O(n)
   Aa.replace, .insert, .remove : O(n)
   Aa.merge, .split             : O(n)
?? Aa.union                     : O(n)
?? Aa.intersect                 : O(n)
```

Hashtable representation:
---------------------------------------

A traditional hash table uses an array as a table, indexed by hashes.
It handles hash collisions somehow, perhaps by doing a simple linear
scan.  It regrows the table by doubling, or some other scheme.  It may
or may not shrink the table.

```
   Htbl.copy                      : O(n)
   Htbl.find                      : O(1)
   Htbl.replace, .insert, .remove : O(1)
   Htbl.merge, .split             : O(n)
?? Htbl.union                     : O(n)
?? Htbl.intersect                 : O(n)
```
