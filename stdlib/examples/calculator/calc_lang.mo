// DSL for "single-cell calculator"

// a single-cell calculator has one "currrent value"
type State = {
  cell : Nat;
};

// each instruction updates the cell;
// each instruction has one additional operand.
type Instr = {
  #add : Nat;
  #sub : Nat;
  #mul : Nat;
  #div : Nat;
};

// define eval function as a _pure_ function 
// (is mutation-free has no access to state)
func eval(state:State, instr:Instr) : ?State {
    switch(instr) {
      case (#add n) { ?{ cell = state.cell + n } };
      case (#sub n) { ?{ cell = state.cell - n } };
      case (#mul n) { ?{ cell = state.cell * n } };
      case (#div n) { 
        if ( n == 0 ) { 
          // null encodes div-by-zero error
          return null 
        } else { 
          ?{ cell = state.cell / n }
        }
      };
    }   
  }
