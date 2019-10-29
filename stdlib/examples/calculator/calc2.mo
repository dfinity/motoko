// Single-cell calculuator, third version (version 2).

// (Compare to versions 0 and 1, which are simpler).

// Here, we use `calc_lang.mo`, a separate (purely-functional) module
// that defines the entire language of calculator instructions, 
// mathematically.

// This solution gives a more "PL way" of solving the problem:
//   The `calc_lang.mo` file gives a _mathematical definition_
//   of the calculator as a DSL (domain-specific language), 
//   in terms of pure data and functions (no state mutation, 
//   network interaction, or other complex stuff).

// This file just uses that DSL definition, calling its `eval` function.

import Lang "calc_lang.mo";

actor class Calc(init: Nat) {
  var value : Nat = init;
  
  public func eval(instr: Lang.Instr) : async ?Nat {
    switch (Lang.eval({cell=value}, instr)) {
      case null { null };
      case (?state) {
        // do the state update
        value := state.cell ;
        ?value
      };
    }
  };
}

// Final point: 
// Imagine if the single cell were an entire sheet of cells;
// This application/program structure would all be the same.
// Rather than pass a single value back and forth, we'd pass
// the "current sheet" back and forth to the `eval` function.
