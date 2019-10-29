// Single-cell calculuator, third version (version 2).

// (Compare to versions 0, 1 and 2, which are each simpler).

// This version adds an "instruction log" to the application,
// and it trims this log at some maximum size.

// For now, the log is just a list,
// but it should have a better datatype (for a sequence and/or a FIFO queue) in the future.

// to do: efficient toArray function for linked lists? (have this for tries, but still missing for lists)

import Lang = "calc_lang.mo";
import List = "list.as";
type List<X> = List.List<X>;

type LogEntry = {
  instr  : Lang.Instr ;
  result : ? Nat ;
};

type Log = List<LogEntry>;

actor class Calc(init: Nat) {
  var value: Nat = init;
  var log: Log = null;
  let maxLog: Nat = 5;

  public func getLog() : async Log {
    log
  };

  //
  // `eval` logs what `Lang.eval` does after it does it, before returning
  //
  public func eval(reqInstr: Lang.Instr) : async ?Nat {
    // 1. do the evaluation, using the language definition
    let reqResult: ?Nat = {
      switch (Lang.eval({cell=value}, reqInstr)) {
        case null {
          // failure: no state update
          null
        };
        case (?state) {
          // success: do the state update
          value := state.cell ;
          ?value
        };
      }
    };
    // 2. prepare a record of this evaluation, for the log:
    let logEntry = {
      instr = reqInstr ;
      result = reqResult ;
    };
    // 3. prepend new log entry and remove oldest entry, if necessary:
    let newLog = ?(logEntry, log);
    log := List.take<LogEntry>(newLog, maxLog);
    reqResult
  };
}
