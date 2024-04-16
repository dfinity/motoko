# Writing incomplete code

## Overview

In the midst of writing a program, you may want to run an incomplete version or a version where one or more execution paths are either missing or simply invalid.

To accommodate these situations, Motoko uses the `xxx`, `nyi` and `unreachable` functions from the base `Prelude` library, explained below. Each is a simple wrapper around a more [general trap mechanism](traps.md).

## Short-term holes

Short-term holes are never committed to a source repository and only ever exist in a single development session for a developer that is still writing the program.

Assuming that one has imported the prelude as follows:

``` motoko name=prelude
import P "mo:base/Prelude";
```

The developer can fill any missing expression with the following:

``` motoko include=prelude
P.xxx()
```

The result will always type check at compile time, and always trap at run time, if and when this expression executes.

## Long-term holes

By convention, longer-term holes can be considered "not yet implemented" (`nyi`) features, and marked as such with a similar function from the Prelude module:

``` motoko include=prelude
P.nyi()
```

## `unreachable` code paths

In contrast to the situations above, sometimes code will never be filled. It will never be evaluated assuming the coherence of the internal logic of the programs' invariants.

To document a code path as logically impossible, or unreachable, use the base library function `unreachable`:

``` motoko include=prelude
P.unreachable()
```

As in the situations above, this function type-checks in all contexts, and when evaluated, traps in all contexts.