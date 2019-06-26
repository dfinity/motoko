## Motivation

* Provide a coherent model for what a source file is
* Support actors, libraries, and programs
* Define the semantics of imports
* Allow separate compilation and (potentially) dynamic linking
* Resolve [issue #400](https://github.com/dfinity-lab/actorscript/issues/400)
* Refine [existing import design](https://dfinity.atlassian.net/wiki/spaces/AST/pages/147357892/ActorScript+library+import)

## Overview

A single AS file is called a *unit*. There are three use cases of a unit:

1. *Programs*: stand-alone AS scripts that are meant to be run, e.g., for tests; also, the input to the REPL.

2. *Libraries*: components that are meant to be imported into other units.

3. *Actors*: stand-alone actors that are meant to be deployed to the platform as part of a canister.

As an additional requirement, we want to be able to dual-use an actor source file as a library, e.g., to write unit tests.

The aim of this proposal is to make each of these use cases possible and convenient while unifying them as much as possible.


### Context

This proposal is essentially a mixture of A and B as suggested [here](https://github.com/dfinity-lab/actorscript/issues/400#issuecomment-492195603). It is mostly like B, except that libraries are as in A.

This has two advantages over both of these:

* It avoids the need to wrap every library module file into a tedious `module` expression (which in principle also implies that its entire content would have to be indented).

* It allows actor unit source files to play the dual role as a library module for import into a test program.

The proposal also has some flavour of C, in that imports now have a special status and can prefix the definition of the actor in an actor unit.
As a consequence, unlike with B, it is possible to name parameter types of an actor class, as long as they are imported from a library.
(Like with B vs C, we could also extend this proposal further to allow static declarations in an actor unit. But for now I suggest to remain conservative and forbid that.)


## Syntax and Semantics

The syntax of a unit -- and therefore an AS source file -- is a sequence of *imports* followed by a sequence of *field definitions* (as in a module or object body).

```
<unit>   ::= <imp>;* <field>;*

<imp>    ::= import <imppat> =? "<url>"
<imppat> ::= <id> | { <id>;* }

<field>  ::= public? <dec>
```

Imports are no longer allowed anywhere else.

Each import binds the identifiers in its *import pattern*. If the pattern is a plain `<id>` then the contents of the imported unit is reified as a module object bound to that id. If the pattern is a *destructuring* import pattern then the respective public fields of the unit are bound to the respective label identifiers.

As a crucial restriction, a unit that has at least one public field must be *static*, see below.

Notes:

* This inverts the current public/private default, as it should.
* The optional `=` in an import may be removed as a more general syntax cleanup.
* There are various ways in which we might extend the syntax of import patterns, e.g., allowing type annotations or allowing field patterns of the form `<id> = <id>` to support renaming. This is just the most basic form.

### Import URLs

An import statement addresses the unit to be imported via an `<url>`. The following URL forms are specfied as of now:

* Relative URLs, such as `import "utils"`, `import "./utils"` or `import ./more/utils`.

  These relative URLs are interpreted as relative to the directory that contains the file that contains the `import` statement.

* Package urls, using the scheme `as:`, such as `import "std/list"` or `import "pkg/module"`.

  The first path component specifies a _package name_, while the remainig paths are relative to the directory where this package is stored.

  The compiler maintains a mapping from package names to directories. It is an implementation detail of the compiler how this mapping is maintained: It could read this mapping from the command line (e.g. -package foo=/usr/share/dmt/packages/foo), environment variables or an `.asc-env` file, or (for early releases) have a hard-coded a mapping containing only the `std` package.

Using URLs for the imports allows for future extensions (e.g. via additional schemes, or by allowing and giving meaning to the authority, query or fragment part of an URL).

After interpreting the path `foo` relative to the current directory resp. the package directory, the compiler resolves it to a file by considering the following file locations:

1. `foo.wasm`
2. `foo/lib.wasm`
3. `foo.as`
4. `foo/lib.as`

It is an error if 1+3, 1+4, 2+3 or 3+4 exist. Otherwise, the compiler picks the first existing file in this list.

If the import URL ends with a slash (e.g. `import "foo/"`), then only locations 3. and 4. are considered.

### Programs

Programs are expressed as units that have no public fields.

A program executes by evaluating its field declarations in sequence, triggering respective side effects.


### Libraries

A library is a unit that has at least one public field.

That implies the additional requirement that all its declarations must be *static*. This is a syntactic approximation guaranteeing the freedom from state and side effects other than non-termination or uncatchable traps (similar to the value restriction in ML).

With this restriction, it becomes unobservable whether a library module is instantiated and linked multiple times. That is relevant for cases such as compiling local actors, which can close over imports by simply relinking them. For this purpose, the prelude can be treated as a library module that is implicitly imported.

Other than this restriction, there is no difference between programs and libraries.


### Actors

An actor unit must contain exactly one field declaration, which is either a manifest actor class or (as a short hand) an actor. It can also have imports.

The actor or actor class may be named or given as an anonymous expression. It may also be either public or private. Writing it as a public named declaration allows the same unit source file to be viewed and compiled as a library module, e.g., for the purpose of importing it into a unit test.

Even if named, an actor class defining an actor unit is not allowed to refer to its own class recursively.
The defined actor can still bind and refer to a self variable for the resulting actor.
Similarly, the actor short-hand can be recursive.
(That is, `actor class C() = this {...this...}` and `actor this {...this...}` are allowed, but `actor class C() = {...C...}` is not. If needs be, this restriction may be lifted later.)


## Compilation

The compilation scheme uses the natural mechanisms in Wasm to express units as modules.
That enables the best possible integration and interoperation with the wider Wasm eco system and existing tools.
(However, it does not automatically enable functional interoperability with other languages, since AS types typically have a representation that cannot directly be interpreted externally. For this, additional interop support would be needed, which is beyond the scope of this proposal.)

Consequently, all units are compiled to Wasm modules.
Their public fields become Wasm exports.
These are either Wasm functions, for public fields of function type,
or Wasm globals, for all others.
Compiling arbitrary closures into exported Wasm functions may require eta-expanding the closure and storing its environment into an internal global.

A Wasm module compiled from AS contains a Dfinity-relevant custom section as well as an AS-specific custom section describing the AS type of the module or actor.

Imports expect the import URL to resolve to a Wasm module compiled from AS and link its exports accordingly.
An import that is not destructured via a module pattern is reified into a module object at the import site.

Programs and libraries are compiled exactly the same.
That is, both create dynlib sections.

Actors are different.
Their exported functions are wrapped into methods de/serialising their arguments and results according to the IDL epcification.
Furthermore, they are complemented with system exports for initialising the actor (given the actor class'es arguments) and for in/externalising the actor's state for upgrades (details TBD).

(For release 0.5, we do not yet intend to support separate compilation, and imports will resolve to source files compiled in a whole program manner instead.)


### Compiler

We need two compilation modes, one for programs/libraries, the other for actors. We could differentiate based on file extensions, but that would get in the way of dual-using an actor source file as a library.

Hence we will need different compiler mode flags, e.g. strawman:

* `-c`: compile as program or library
* `-a`: compile as actor


### Execution and Dynamic Linking

A compiled program is executed by instantiating its module in a suitable engine.
In general, it will import other library modules, which in turn may import modules.
They are instantiated and linked in some topological order.
Fortunately, the specific order is not observable and does not matter, due to the requirement that libraries be static.

A deployed actor, when instantiated on the platform, proceeds similarly, except that the libraries would come from either the same canister or potentially an on-chain library repository.

(For release 1.0, we do not yet intend to support dynamic linking of library modules. Instead, programs or actors must be linked statically.)


### Static Linking

Compiled modules following the platform conventions can also be linked *statically*, by merging multiple modules, wiring up export/import edges between them, and creating a single module from them.
Libraries imported multiple times will only be included once.

Details TBD. This may work only for linking libraries into programs or actors. Ideally, the linking tool would be language-agnostic.


## Roadmap

Possible roadmap sketch:

* v0.5: support unit and import syntax; support whole-program compilation via loading imports from source; no separate compilation, no linking

* v1.0: support AS type custom section; support incremental compilation and static linking of libraries; no dynamic linking

* v1.5: support dynamic linking?
