Motoko Standard Library
==============================

The standard library of Motoko.

See [The Appendix Chapter](https://sdk.dfinity.org/language-guide/index.html#appendix-stdlib)
of [the Motoko Book](https://sdk.dfinity.org/language-guide/) for documentation with examples.

## Overview of modules

We group the modules conceptually, and indicate the status of each as planned
(`[ ]`) or implemented and tested, at least partially (`[x]`).

### Collections:

- [x] `Array` module
- [x] `List` module
- [x] `AssocList` module
- [ ] `Map` interface (e.g., implemented by `Trie` below, via a class)
- [ ] `Set` interface (e.g., implemented by `TrieSet` below, via a class)

### Low-level collections (wrappers provide above interfaces):

- [x] `TrieSet` module -- can implement `Set` above
- [x] `Trie` module -- can implement `Map` above

### Misc utilities

- [x] `Prelude` module
- [x] `Hash` module
