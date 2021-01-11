= Motoko compiler changelog

* new `moc` command-line arguments `--args <file>` and `--args0 <file>` for reading newline/NUL terminated arguments from `<file>`.

== 0.5.4 (2021-01-07)

* _Option blocks_ `do ? <block>` and _option checks_ `<exp> !`.
  Inside an option block, an option check validates that its operand expression is not `null`.
  If it is, the entire option block is aborted and evaluates to `null`.
  This simplifies consecutive null handling by avoiding verbose `switch` expressions.

  For example, the expression `do? { f(x!, y!) + z!.a }` evaluates to `null` if either `x`, `y` or `z` is `null`;
  otherwise, it takes the options' contents and ultimately returns `?r`, where `r` is the result of the addition.

* BREAKING CHANGE (Minor):
  The light-weight `do <exp>` form of the recently added, more general `do <block-or-exp>` form,
  is no longer legal syntax.
  That is, the argument to a `do` or `do ?` expression *must* be a block `{ ... }`,
  never a simple expression.

== 0.5.3 (2020-12-10)

* Nothing new, just release moc.js to CDN

== 0.5.2 (2020-12-04)

* Bugfix: gracefully handle importing ill-typed actor classes

== 0.5.1 (2020-11-27)

* BREAKING CHANGE: Simple object literals of the form `{a = foo(); b = bar()}`
  no longer bind the field names locally. This enables writing expressions
  like `func foo(a : Nat) { return {x = x} }`.

  However, this breaks expressions like `{a = 1; b = a + 1}`. Such object
  shorthands now have to be written differently, e.g., with an auxiliary
  declaration, as in `let a = 1; {a = a; b = a + 1}`, or by using the "long"
  object syntax `object {public let a = 1; public let b = a + 1}`.

== 0.5.0 (2020-11-27)

* BREAKING CHANGE: Free-standing blocks are disallowed

  Blocks are only allowed as sub-expressions of control flow expressions like `
  if`, `loop`, `case`, etc. In all other places, braces are always considered
  to start an object literal.

  To use blocks in other positions, the new `do <block>` expression can be
  used.

  The more liberal syntax is still allowed for now but deprecated, i.e.,
  produces a warning.

* BREAKING CHANGE: actor creation is regarded as asynchronous:

  * Actor declarations are asynchronous and can only be used in asynchronous
    contexts.
  * The return type of an actor class, if specified, must be an async actor
    type.
  * To support actor declaration, the top-level context of an interpreted
    program is an asynchronous context, allowing implicit and explicit await
    expressions.

  (Though breaking, this change mostly affects interpreted programs and
  compiled programs with explicate actor class return types)

* Candid support is updated to latest changes of the Candid spec, in particular
  the ability to extend function with optional parameters in a backward
  compatible way.

  Motoko passes the official Candid compliance test suite.

* RTS: Injecting a value into an option type (`? <exp>`) no longer
  requires heap allocation in most cases. This removes the memory-tax
  of using iterators.

* Bugfix: Passing cycles to the instantiation of an actor class works now.

* Various bug fixes and documentation improvements.

== 0.4.6 (2020-11-13)

* Significant documentation improvements
* Various bugfixes
* Improved error messages
* Initial DWARF support
* Candid compliance improvements:
  - Strict checking of utf8 strings
  - More liberal parsing of leb128-encoded numbers
* New motoko-base:
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
* Funds Imperative API (#1922)
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
