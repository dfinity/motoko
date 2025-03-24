# Persistence Modes

This Motoko build includes two substantially different persistence modes in one build:

* [Classical Persistence](OldStableMemory.md) (default): 
    This is the traditional Motoko compiler design based on 32-bit memory and Candid-based stabilization for upgrades.
    This mode is known to have severe scalability problems on upgrades, because the stabilization may exceed upgrade instruction limit for stable data amounts, besides other problems such as exponential duplication or stack overflows depending on the data structures.
    The mode is temporarily retained to allow beta testing of the new enhanced orthogonal persistence until the new persistence is officialized.
* [Enhanced Orthogonal Persistence](OrthogonalPersistence.md) (new, for beta testing):
    This implements scalable persistence with 64-bit main memory that is retained across upgrades without stabilization to stable memory.
    The mode needs to be enabled by the compiler flag `--enhanced-orthogonal-persistence` and is intended to become the future default mode, deprecating classical persistence.

The reason for having one build instead of two separate branches and release artefact is for having a unified branch, and ensure that new features are implemented and tested for both persistence modes, passing the same CI.

## Compiler Flags

* (no flag): Use classical persistence
* `--enhanced-orthogonal-persistence`: Use enhanced orthogonal persistence. NOTE: This is currently in the **beta testing** phase.

Certain compiler flags are only applicable to a specific persistence mode:

Flag              | Applicable Mode
------------------|----------------
--rts-stack-pages | Classical persistence only
--stabilization-instruction-limit | Enhanced persistence only
--copying-gc      | Classical persistence only
--compacting-gc   | Classical persistence only
--generational-gc | Classical persistence only

(All other flags are applicable to both modes.)

Incremental graph copy stabilization with `__motoko_stabilize_before_upgrade` and `__motoko_destabilize_after_upgrade` are only available with enhanced orthogonal persistence and only needed in a seldom case of memory layout upgrade.

## Source Structure

## Runtime System
The Motoko runtime system (RTS) is a combined source base supporting 3 modes, each with a debug and release build:
* 32-bit classical persistence, with classical non-incremental GCs
* 32-bit classical persistence, with the incremental GC
* 64-bit enhanced orthogonal persistence

## Compiler
For pragmatic purposes, the compiler backend is split/duplicated in two parts
* `compile-enhanced.ml`: Enhanced orthogonal persistence, 64-bit, passive data segments, incremental graph copy.
* `compile-classical.ml`: Classical persistence, 32-bit, Candid stabilization.

The linker integrates both persistence modes and 32-bit and 64-bit in one package.

## Tests
Most tests run on both modes. Specific tests apply to selected modes, as defined by the `ENHANCED-ORTHOGONAL-PERSISTENCE` or `CLASSICAL-PERSISTENCE` tags.
