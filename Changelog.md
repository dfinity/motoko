= Motoko compiler changelog

* BREAKING CHANGE: actor creation is regarded as asynchronous:
** Actor declarations are asynchronous and can only be used in asynchronous contexts.
** The return type of an actor class, if specified, must be an
   async actor type.
** To support actor declaration, the top-level context of an interpreted program   is an asynchronous context, allowing implicit and explicit await expressions.
** (Though breaking, this change mostly affects interpreted programs and compiled programs with explicate actor class return types)

* RTS: Injecting a non-null value into an option type (`? <exp>`)
  is the identity and no longer requires heap allocation.
  This removes the memory-tax of using iterators.

== 0.4.6 (2020-11-13)

* Significant documentation improvements
* Various bugfixes
* Improved error messages
* Initial DWARF support
* Candid compliance improvements:
  - Strict checking of utf8 strings
  - More liberal parsing of leb128-encoded numbers
* New motoko-based:
  - The Random library is added

== 0.4.5 (2020-10-06)

* BREAKING CHANGE: a library containing a single actor class is
  imported as a module, providing access to both the class type and
  class constructor function as module components. Restores the
  invariant that imported libraries are modules.
* Backend: Compile captured actor class parameters statically (#2022)
* flip the default for -g (#1546)
* Bug fix: reject array indexing as non-static (could trap) (#2011)
* Initialize tuple length fields (#1992)
* Warns for structural equality on abstract types (#1972)
* Funds Imperative API  (#1922)
* Restrict subtyping (#1970)
* Continue labels always have unit codomain (#1975)
* Compile.ml: target and use new builder call pattern (#1974)
* fix scope var bugs (#1973)

== 0.4.4 (2020-09-21)

* Actor class export
* Accept unit installation args for actors
* Reject platform actor (class) programs with additional decs
* Handle IO exceptions at the top-level
* RTS: Remove duplicate array and blob allocation code
* RTS: Fix pointer arithmetic in BigInt collection function

== 0.4.3 (2020-09-14)

* Preliminary support for actor class import and dynamic canister installation.
  Surface syntax may change in future.
* BREAKING CHANGE: a compilation unit/file defining an actor or actor class may *only* have leading `import` declarations; other leading declarations (e.g. `let` or `type`) are no longer supported.
* Rust GC

== 0.4.2 (2020-08-18)

* Polymorphic equality.  `==` and `!=` now work on all shareable types.

== 0.4.1 (2020-08-13)

* Switching to bumping the third component of the version number
* Bugfix: clashing declarations via function and class caught (#1756)
* Bugfix: Candid `bool` decoding rejects invalid input (#1783)
* Canisters can take installation arguments (#1809)
  NB: Communicating the type of the canister installation methods is still
  missing.
* Optimization: Handling of `Bool` in the backend.

== 0.4 (2020-08-03)

* Candid pretty printer to use shorthand when possible (#1774)
* fix candid import to use the new id format (#1787)

== 0.3 (2020-07-31)

* Fixes an issue with boolean encoding to Candid
* Converts the style guide to asciidocs

== 0.2 (2020-07-30)

* The `Blob` type round-trips through candid type export/import (#1744)
* Allow actor classes to know the caller of their constructor (#1737)
* Internals: `Prim.time()` provided (#1747)
* Performance: More dead code removal (#1752)
* Performance: More efficient arithmetic with unboxed values (#1693, #1757)
* Canister references are now parsed and printed according to the new
  base32-based textual format (#1732).
* The runtime is now embedded into `moc` and need not be distributed separately
  (#1772)

== 0.1 (2020-07-20)

* Beginning of the changelog. Released with dfx-0.6.0.
