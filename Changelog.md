# Motoko compiler changelog

## 0.6.30 (2022-08-11)

* motoko (`moc`)

  * add primitives
    ```motoko
    shiftLeft : (Nat, Nat32) -> Nat
    shiftRight : (Nat, Nat32) -> Nat
    ```
    for efficiently multiplying/dividing a `Nat` by a power of 2
    (#3112)

  * add primitives
    ```motoko
    rts_mutator_instructions : () -> Nat
    rts_collector_instructions : () -> Nat
    ```
    to report approximate IC instruction costs of the last message
    due to mutation (computation) and collection (GC), respectively (#3381)

* motoko-base

  * Add
    ```motoko
    Buffer.fromArray
    Buffer.fromVarArray
    ```
    for efficiently adding an array to a `Buffer`
    (dfinity/motoko-base#389)

  * Add
    ```motoko
    Iter.sort : (xs : Iter<A>, compare : (A, A) -> Order) : Iter<A>
    ```
    for sorting an `Iter` given a comparison function
    (dfinity/motoko-base#406)

  * Performance: `HashMap` now avoids re-computing hashes on `resize`
    (dfinity/motoko-base#394)

## 0.6.29 (2022-06-10)

* motoko (`moc`)

  * The language server now supports explicit symbol imports (thanks
    to rvanasa) (#3282)
  * The language server now has improved support for navigating to
    definitions in external modules (thanks to rvanasa)  (#3263)
  * Added a primitive `textCompare` allowing more efficient three-way
    `Text` comparisons (#3298)
  * Fixed a typing bug with annotated, recursive records (#3268)

* motoko-base

  * Add
    ```motoko
    ExperimentalInternetComputer.countInstruction : (comp : () -> ()) -> Nat64
    ```
    to count the Wasm instructions performed during execution of `comp()` (dfinity/motoko-base#381)

  * Add
    ```motoko
    ExperimentalStableMemory.stableVarQuery : () -> (shared query () -> async {size : Nat64})
    ```
    for estimating stable variable storage requirements during upgrade
    (dfinity/motoko-base#365)
  * Performance improvement to `Text.compare` (dfinity/motoko-base#382)

## 0.6.28 (2022-05-19)

* motoko (`moc`)

  * Add `to_candid`, `from_candid` language constructs for Candid serialization to/from Blobs (#3155)
  * New `system` field 'inspect' for accepting/declining canister ingress messages (see doc) (#3210)

## 0.6.27 (2022-05-04)

* motoko (`moc`)

  * Importing modules by relative path is now more robust (#3215).
  * Performance: persisting stable variables to stable memory is now
    performed in streaming fashion, reducing heap consumption and
    copying during an upgrade (#3149).
  * Performance: local 32- and 64-bit numeric values are now stored in
    using unboxed form when possible (thanks to nomeata) (#3207).

* motoko-base

  * Fixed a bug in `Trie.filter` (and `Trie.mapFilter`) which could
    lead to missing matches in some cases (dfinity/motoko-base#371).

## 0.6.26 (2022-04-20)

* motoko (`moc`)

  * Performance: inline prim-wrapping functions (thanks to nomeata) (#3159)
  * Improve type pretty printer to mirror type parser (avoids producing unparseable stable variable signatures) (#3190)
  * Adds new flag `--omit-metadata` to omit certain metadata sections from `actor` (and `actor class`) Wasm (#3164)
  * Performance: avoid redundant heap allocation when deserializing compact Candid `int` and  `nat` values (#3173)
  * Added a primitive to obtain stable variable memory footprint (#3049)

* motoko-base

  * Fixed the 32-bit range limitation of `Hash.hash: Nat -> Nat32` and
    deprecate most functions in `Hash` (dfinity/motoko-base#366).
  * Add `List.toIter` (thanks to hoosan) (dfinity/motoko-base#336).

## 0.6.25 (2022-03-07)

* motoko (`moc`)

  * bugfix: fix bogus elision of type constructors sharing names with primitive types in `--stable-types` section and `.most` file (#3140)

## 0.6.24 (2022-03-06)

* motoko (`moc`)

  * bugfix: fix bogus identification of distinct type constructors
    in --stable-types section and .most file (#3140)

## 0.6.23 (2022-03-05)

* motoko (`moc`)

  * bugfix: fix pretty printing of (stable) types and #3128 (#3130)

    * Collect constructors  *transitively* before emitting a .most file.
    * Modifies type pretty printer to produce well-formed types and stable type signatures.

## 0.6.22 (2022-02-24)

* motoko (`moc`)

  * Fix: remove bogus error when transitively importing module with
    selective field imports (#3121)
  * Fix: Treating eponymous types from separate candid files (#3103)

* Various reports from CI are now pushed to
  https://dfinity.github.io/motoko (#3113)

## 0.6.21 (2022-01-31)

* motoko (`moc`)

  * Emit new ICP metadata custom section 'motoko:compiler' with compiler release or revision in UTF8 (e.g. "0.6.21"). Default is `icp:private` (#3091).
  * Generalized `import` supporting pattern matching and selective field imports (#3076).
  * Fix: insert critical overflow checks preventing rare heap corruptions
    in out-of-memory allocation and stable variable serialization (#3077).
  * Implement support for 128-bit Cycles-API (#3042).

* motoko-base

  * `ExperimentalInternetComputer` library, exposing low-level, binary `call` function (a.k.a. "raw calls") (dfinity/motoko-base#334, Motoko #3806).
  * `Principal.fromBlob` added (dfinity/motoko-base#331).

## 0.6.20 (2022-01-11)

* motoko

  * Implement support for `heartbeat` system methods (thanks to ninegua) (#2971)

* motoko-base

  * Add `Iter.filter : <A>(Iter<A>, A -> Bool) -> Iter<A>` (thanks to jzxchiang1) (dfinity/motoko-base#328).

## 0.6.19 (2022-01-05)

* motoko-base

  * Fixed a bug in the `RBTree.size()` method.

## 0.6.18 (2021-12-20)

* moc

  * Add runtime support for low-level, direct access to 64-bit IC stable memory, including documentation.
  * Add compiler flag `--max-stable-pages <n>` to cap any use of `ExperimentalStableMemory.mo` (see below), while reserving space for stable variables.
  Defaults to 65536 (4GiB).

* motoko-base

  * (Officially) add `ExperimentalStableMemory.mo` library, exposing 64-bit IC stable memory

* BREAKING CHANGE (Minor):
  The previously available (but unadvertised) `ExperimentalStableMemory.mo` used
  `Nat32` offsets. This one uses `Nat64` offsets to (eventually) provide access to more address space.

## 0.6.17 (2021-12-10)

* Improved handling of one-shot messages facilitating zero-downtime
  upgrades (#2938).
* Further performance improvements to the mark-compact garbage
  collector (#2952, #2973).
* Stable variable checking for `moc.js` (#2969).
* A bug was fixed in the scoping checker  (#2977).

## 0.6.16 (2021-12-03)

* Minor performance improvement to the mark-compact garbage collector


## 0.6.15 (2021-11-26)

* Fixes crash when (ill-typed) `switch` expression on non-variant
  value has variant alternatives (#2934)

## 0.6.14 (2021-11-19)

* The compiler now embeds the existing Candid interface  and  new
  _stable signature_ of a canister in additional Wasm custom sections,
  to be selectively exposed by the IC, and to be used by tools such as `dfx`
  to verify upgrade compatibility (see extended documentation).

  New compiler options:

    * `--public-metadata <name>`: emit ICP custom section `<name>` (`candid:args` or `candid:service` or `motoko:stable-types`) as `public` (default is `private`)
    * `--stable-types`: emit signature of stable types to `.most` file
    * `--stable-compatible <pre> <post>`: test upgrade compatibility between stable-type signatures  `<pre>` and `<post>`

  A Motoko canister upgrade is safe provided:

    * the canister's Candid interface evolves to a Candid subtype; and
    * the canister's Motoko stable signature evolves to a _stable-compatible_ one.

  (Candid subtyping can be verified using tool `didc` available at:
   https://github.com/dfinity/candid.)

* BREAKING CHANGE (Minor):
  Tightened typing for type-annotated patterns (including function parameters)
  to prevent some cases of unintended and counter-intuitive type propagation.

  This may break some rare programs that were accidentally relying on that
  propagation. For example, the indexing `xs[i]` in the following snippet
  happend to type-check before, because `i` was given the more precise
  type `Nat` (inferred from `run`'s parameter type), regardless of the
  overly liberal declaration as an `Int`:

  ```motoko
  func run(f : Nat -> Text) {...};
  let xs = ["a", "b", "c"];
  run(func(i : Int) { xs[i] });
  ```
  This no longer works, `i` has to be declared as `Nat` (or the type omitted).

  If you encounter such cases, please adjust the type annotation.

* Improved garbage collection scheduling

* Miscellaneous performance improvements
  - code generation for `for`-loops over arrays has improved
  - slightly sped up `Int` equality comparisons

## 0.6.13 (2021-11-19)

*Pulled*

## 0.6.12 (2021-10-22)

* `for` loops over arrays are now converted to more efficient
  index-based iteration (#2831). This can result in significant cycle
  savings for tight loops, as well as slightly less memory usage.

* Add type union and intersection. The type expression

  ```motoko
  T and U
  ```
  produces the greatest lower bound of types `T` and `U`, that is,
  the greatest type that is a subtype of both. Dually,

  ```motoko
  T or U
  ```
  produces the least upper bound of types `T` and `U`, that is,
  the smallest type that is a supertype of both.

  One use case of the former is "extending" an existing object type:

  ``` motoko
  type Person = {name : Text; address : Text};
  type Manager = Person and {underlings : [Person]};
  ```
  Similarly, the latter can be used to "extend" a variant type:
  ```motoko
  type Workday = {#mon; #tue; #wed; #thu; #fri};
  type Weekday = Workday or {#sat; #sun};
  ```

## 0.6.11 (2021-10-08)

* Assertion error messages are now reproducible (#2821)

## 0.6.10 (2021-09-23)

* moc

  * documentation changes

* motoko-base

  * documentation changes

## 0.6.9 (2021-09-15)

* motoko-base

  * add Debug.trap : Text -> None (dfinity/motoko-base#288)

## 0.6.8 (2021-09-06)

* Introduce primitives for `Int` ⇔ `Float` conversions (#2733)
* Bump LLVM toolchain to version 12 (#2542)
* Support extended name linker sections (#2760)
* Fix crashing bug for formatting huge floats (#2737)

## 0.6.7 (2021-08-16)

* moc

  *  Optimize field access by exploiting field ordering (#2708)
  *  Fix handling of self references in mark-compact GC (#2721)
  *  Restore CI reporting of perf-regressions (#2643)

* motoko-base:

  * Fix bug in `AssocList.diff` (dfinity/motoko-base#277)
  * Deprecate unsafe or redundant functions in library `Option` ( `unwrap`, `assertSome`, `assertNull`) (#275)

## 0.6.6 (2021-07-30)

* Vastly improved garbage collection scheduling: previously Motoko runtime would do GC
  after every update message. We now schedule a GC when

  1. Heap grows more than 50% and 10 MiB since the last GC, or
  2. Heap size is more than 3 GiB

  (1) is to make sure we don't do GC on tiny heaps or after only small amounts of allocation.
  (2) is to make sure that on large heaps we will have enough allocation space during the next message.

  This scheduling reduces cycles substantially, but may moderately increase memory usage.

  New flag `--force-gc` restores the old behavior.

* Fix bug in compacting gc causing unnecessary memory growth (#2673)

* Trap on attempt to upgrade when canister not stopped and there are outstanding callbacks.
  (This failure mode can be avoided by stopping the canister before upgrade.)

* Fix issue #2640 (leaked `ClosureTable` entry when awaiting futures fails).

## 0.6.5 (2021-07-08)

* Add alternative, _compacting_ gc, enabled with new moc flag `--compacting-gc`.
  The compacting gc supports larger heap sizes than the default, 2-space copying collector.

  NOTE: Dfx 0.7.6 adds optional field `"args"` to `dfx.json` files,
  so Motoko canisters can specify `moc` command-line arguments. E.g.,

  ```json
  ...
     "type" : "motoko"
     ...
     "args" : "--compacting-gc"
  ...
  ```

* Documentation fixes.
* Command line tools: `--help` option provides better documentation of command line
  options that have arguments.
* Fix issue #2319 (crash on import of Candid class).

## 0.6.4 (2021-06-12)

* For release builds, the banner (`moc --version`) now includes the release
  version.

* Fix MacOS release builds (the 0.6.3 tarball for MacOS contained the linux binaries)

## 0.6.3 (2021-06-10)

* Motoko is now open source!

* Better internal consistency checking of the intermediate representation

## 0.6.2 (2021-05-24)

* motoko-base:

  * reformat to style guidelines
  * add type bindings `Nat.Nat`, `Nat8.Nat8` etc. to libraries for primitive types.

* Bugfix: generation of candid from Motoko:

  * no longer confused by distinct, but eponymous, type definitions (Bug: #2529);
  * numbers eponymous types and specializations from 1 (not 2);
  * avoids long chains of type equalities by normalizing before translation.

## 0.6.1 (2021-04-30)

* Internal: Update to IC interface spec 0.17 (adapt to breaking change to signature of `create_canister`)

## 0.6.0 (2021-04-16)

* BREAKING CHANGE:
  The old-style object and block syntax deprecated in 0.5.0 is finally removed.

* Record punning: As already supported in patterns, short object syntax in
  expressions now allows omitting the right-hand side if it is an identifier
  of the same name as the label. That is,

  ```motoko
  {a; b = 1; var c}
  ```

  is short for

  ```motoko
  {a = a; b = 1; var c = c}
  ```

  assuming respective variables are in scope.

* BREAKING CHANGE:
  The types `Word8`, `Word16`, `Word32` and `Word64` have been removed.
  This also removed the `blob.bytes()` iterator.

  Motoko base also dropped the `Word8`, `Word16`, `Word32` and `Word64`
  modules.

  This concludes the transition to the other fixed-width types that began with
  version 0.5.8

* BREAKING CHANGE (Minor):
 `await` on a completed future now also commits state and suspends
  computation, to ensure every await, regardless of its future's state,
  is a commit point for state changes and tentative message sends.

  (Previously, only awaits on pending futures would force a commit
   and suspend, while awaits on completed futures would continue
   execution without an incremental commit, trading safety for speed.)

* motoko-base: fixed bug in `Text.compareWith`.

## 0.5.15 (2021-04-13)

* Bugfix: `Blob.toArray` was broken.

## 0.5.14 (2021-04-09)

* BREAKING CHANGE (Minor): Type parameter inference will no longer default
  under-constrained type parameters that are invariant in the result, but
  require an explicit type argument.
  This is to avoid confusing the user by inferring non-principal types.

  For example, given (invariant) class `Box<A>`:

  ```motoko
    class Box<A>(a : A) { public var value = a; };
  ```

  the code

  ```motoko
    let box = Box(0); // rejected
  ```

  is rejected as ambiguous and requires an instantiation, type annotation or
  expected type. For example:

  ```motoko
    let box1 = Box<Int>(0); // accepted
    let box2 : Box<Nat> = Box(0); // accepted
  ```

  Note that types `Box<Int>` and `Box<Nat>` are unrelated by subtyping,
  so neither is best (or principal) in the ambiguous, rejected case.

* Bugfix: Type components in objects/actors/modules correctly ignored
  when involved in serialization, equality and `debug_show`, preventing
  the compiler from crashing.

* motoko-base: The `Text.hash` function was changed to a better one.
  If you stored hashes as stable values (which you really shouldn't!)
  you must rehash after upgrading.

* motoko-base: Conversion functions between `Blob` and `[Nat8]` are provided.

* When the compiler itself crashes, it will now ask the user to report the
  backtrace at the DFINITY forum

## 0.5.13 (2021-03-25)

* The `moc` interpreter now pretty-prints values (as well as types) in the
  repl, producing more readable output for larger values.

* The family of `Word` types are deprecated, and mentioning them produces a warning.
  These type will be removed completely in a subsequent release.
  See the user’s guide, section “Word types”, for a migration guide.

* motoko base: because of this deprecation, the `Char.from/toWord32()`
  functions are removed. Migrate away from `Word` types, or use
  `Word32.from/ToChar` for now.

## 0.5.12 (2021-03-23)

* The `moc` compiler now pretty-prints types in error messages and the repl,
  producing more readable output for larger types.

* motoko base: fixed bug in `Text.mo` affecting partial matches in,
  for example, `Text.replace` (GH issue #234).

## 0.5.11 (2021-03-12)

* The `moc` compiler no longer rejects occurrences of private or
  local type definitions in public interfaces.

  For example,

  ```motoko
  module {
    type List = ?(Nat, List); // private
    public func cons(n : Nat, l : List) : List { ?(n , l) };
  }
  ```

  is now accepted, despite `List` being private and appearing in the type
  of public member `cons`.

* Type propagation for binary operators has been improved. If the type of one of
  the operands can be determined locally, then the other operand is checked
  against that expected type. This should help avoiding tedious type annotations
  in many cases of literals, e.g., `x == 0` or `2 * x`, when `x` has a special
  type like `Nat8`.

* The `moc` compiler now rejects type definitions that are non-_productive_ (to ensure termination).

  For example, problematic types such as:

  ```motoko
  type C = C;
  type D<T, U> = D<U, T>;
  type E<T> = F<T>;
  type F<T> = E<T>;
  type G<T> = Fst<G<T>, Any>;
  ```

  are now rejected.

* motoko base: `Text` now contains `decodeUtf8` and `encodeUtf8`.

## 0.5.10 (2021-03-02)

* User defined deprecations

  Declarations in modules can now be annotated with a deprecation comment, which make the compiler emit warnings on usage.

  This lets library authors warn about future breaking changes:

  As an example:

  ```motoko
  module {
    /// @deprecated Use `bar` instead
    public func foo() {}

    public func bar() {}
  }
  ```

  will emit a warning whenever `foo` is used.

* The `moc` compiler now rejects type definitions that are _expansive_, to help ensure termination.
  For example, problematic types such as `type Seq<T> = ?(T, Seq<[T]>)` are rejected.

* motoko base: `Time.Time` is now public

## 0.5.9 (2021-02-19)

* The `moc` compiler now accepts the `-Werror` flag to turn warnings into errors.

* The language server now returns documentation comments alongside
  completions and hover notifications

## 0.5.8 (2021-02-12)

* Wrapping arithmetic and bit-wise operations on `NatN` and `IntN`

  The conventional arithmetic operators on `NatN` and `IntN` trap on overflow.
  If wrap-around semantics is desired, the operators `+%`, `-%`, `*%` and `**%`
  can be used. The corresponding assignment operators (`+%=` etc.) are also available.

  Likewise, the bit fiddling operators (`&`, `|`, `^`, `<<`, `>>`, `<<>`,
  `<>>` etc.) are now also available on `NatN` and `IntN`. The right shift
  operator (`>>`) is an unsigned right shift on `NatN` and a signed right shift
  on `IntN`; the `+>>` operator is _not_ available on these types.

  The motivation for this change is to eventually deprecate and remove the
  `WordN` types.

  Therefore, the wrapping arithmetic operations on `WordN` are deprecated and
  their use will print a warning. See the user’s guide, section “Word types”,
  for a migration guide.

* For values `x` of type `Blob`, an iterator over the elements of the blob
  `x.vals()` is introduced. It works like `x.bytes()`, but returns the elements
  as type `Nat8`.

* `mo-doc` now generates cross-references for types in signatures in
  both the Html as well as the Asciidoc output. So a signature like
  `fromIter : I.Iter<Nat> -> List.List<Nat>` will now let you click on
  `I.Iter` or `List.List` and take you to their definitions.

* Bugfix: Certain ill-typed object literals are now prevented by the type
  checker.

* Bugfix: Avoid compiler aborting when object literals have more fields than
  their type expects.

## 0.5.7 (2021-02-05)

* The type checker now exploits the expected type, if any,
  when typing object literal expressions.
  So `{ x = 0 } : { x : Nat8 }` now works as expected
  instead of requiring an additional type annotation on `0`.

## 0.5.6 (2021-01-22)

* The compiler now reports errors and warnings with an additional _error code_
  This code can be used to look up a more detailed description for a given error by passing the `--explain` flag with a code to the compiler.
  As of now this isn't going to work for most codes because the detailed descriptions still have to be written.
* Internal: The parts of the RTS that were written in C have been ported to Rust.

## 0.5.5 (2021-01-15)

* new `moc` command-line arguments `--args <file>` and `--args0 <file>` for
  reading newline/NUL terminated arguments from `<file>`.
* motoko base: documentation examples are executable in the browser

## 0.5.4 (2021-01-07)

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

## 0.5.3 (2020-12-10)

* Nothing new, just release moc.js to CDN

## 0.5.2 (2020-12-04)

* Bugfix: gracefully handle importing ill-typed actor classes

## 0.5.1 (2020-11-27)

* BREAKING CHANGE: Simple object literals of the form `{a = foo(); b = bar()}`
  no longer bind the field names locally. This enables writing expressions
  like `func foo(a : Nat) { return {x = x} }`.

  However, this breaks expressions like `{a = 1; b = a + 1}`. Such object
  shorthands now have to be written differently, e.g., with an auxiliary
  declaration, as in `let a = 1; {a = a; b = a + 1}`, or by using the "long"
  object syntax `object {public let a = 1; public let b = a + 1}`.

## 0.5.0 (2020-11-27)

* BREAKING CHANGE: Free-standing blocks are disallowed

  Blocks are only allowed as sub-expressions of control flow expressions like
  `if`, `loop`, `case`, etc. In all other places, braces are always considered
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

## 0.4.6 (2020-11-13)

* Significant documentation improvements
* Various bugfixes
* Improved error messages
* Initial DWARF support
* Candid compliance improvements:
  * Strict checking of utf8 strings
  * More liberal parsing of leb128-encoded numbers
* New motoko-base:
  * The Random library is added

## 0.4.5 (2020-10-06)

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

## 0.4.4 (2020-09-21)

* Actor class export
* Accept unit installation args for actors
* Reject platform actor (class) programs with additional decs
* Handle IO exceptions at the top-level
* RTS: Remove duplicate array and blob allocation code
* RTS: Fix pointer arithmetic in BigInt collection function

## 0.4.3 (2020-09-14)

* Preliminary support for actor class import and dynamic canister installation.
  Surface syntax may change in future.
* BREAKING CHANGE: a compilation unit/file defining an actor or actor class may *only* have leading `import` declarations; other leading declarations (e.g. `let` or `type`) are no longer supported.
* Rust GC

## 0.4.2 (2020-08-18)

* Polymorphic equality.  `==` and `!=` now work on all shareable types.

## 0.4.1 (2020-08-13)

* Switching to bumping the third component of the version number
* Bugfix: clashing declarations via function and class caught (#1756)
* Bugfix: Candid `bool` decoding rejects invalid input (#1783)
* Canisters can take installation arguments (#1809)
  NB: Communicating the type of the canister installation methods is still
  missing.
* Optimization: Handling of `Bool` in the backend.

## 0.4 (2020-08-03)

* Candid pretty printer to use shorthand when possible (#1774)
* fix candid import to use the new id format (#1787)

## 0.3 (2020-07-31)

* Fixes an issue with boolean encoding to Candid
* Converts the style guide to asciidocs

## 0.2 (2020-07-30)

* The `Blob` type round-trips through candid type export/import (#1744)
* Allow actor classes to know the caller of their constructor (#1737)
* Internals: `Prim.time()` provided (#1747)
* Performance: More dead code removal (#1752)
* Performance: More efficient arithmetic with unboxed values (#1693, #1757)
* Canister references are now parsed and printed according to the new
  base32-based textual format (#1732).
* The runtime is now embedded into `moc` and need not be distributed separately
  (#1772)

## 0.1 (2020-07-20)

* Beginning of the changelog. Released with dfx-0.6.0.
