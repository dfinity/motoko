// Single-cell calculuator, third version (version 2).

// (Compare to versions 0 and 1, which are simpler).

// Here, we use `calc_lang.mo`, a separate (purely-functional) module
// that defines the entire language of calculator instructions,
// mathematically.

// This solution gives a more "PL way" of solving the problem:
//   The `calc_lang.mo` file gives a _mathematical definition_
//   of the calculator as a DSL (domain-specific language),
//   in terms of pure data and functions (no state mutation,
//   network interaction, or other complex Motoko stuff.

// This file just uses that DSL definition, calling its `eval` function.

// Next, in version 3 we consider logging.

import Lang "calc_lang.mo";

actor class Calc(init: Nat) {
  var state : Lang.State = { cell = 0 };

  public func eval(instr: Lang.Instr) : async ?Nat {
    switch (Lang.eval(state, instr)) {
      case null { null };
      case (? updatedState) {
        // do the state update
        state := updatedState;
        ?state.cell
      };
    }
  };
}

// Final point: Generalizing to a spreadsheet calculator
// ------------------------------------------------------
// Imagine if the single cell were an entire sheet of cells;
// This application/program structure would all be the same.
// Rather than pass a state with a single cell value back
// and forth, we'd pass the "current sheet" back and forth
// to the `eval` function.
