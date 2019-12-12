import P "prelude.mo";
import Hash "hash.mo";
import FunMap "funMap.mo";
import FunSeq "funSeq.mo";

module {

  public type Seq<X> = FunSeq.Seq<X>;
  public type Map<X, Y> = FunMap.Map<X, Y>;
  
  public type Key = Text;
  public type Val = Nat;
  public type Timestamp = Nat;

  public type LogEvent = {
    #write: (Key, Val, Bool); // true=currently-invoked; false=currently-revoked.
    #undo: Timestamp;
    #redo: Timestamp;
  };
  
  public type LinearLog = Seq<LogEvent>;

  public type ByKey = Map<Key, Seq<LogEvent>>;


  // Invariants and properties:
  // --------------------------
  //
  // 1. LinearLog (a seqeuence):
  //    Each timestamp present in this sequence addresses an index _earlier_ in the sequence
  //    (Proper scoping says that a timestamp X only appears _after_ position X in the sequence).
  //
  //   
  // 2. ByKey (a map of seqeuences):
  //    Each timestamp present in each variable's sequence addresses a common linear log.
  //    ByKey has less information than the common LinearLog, since it looses the global order of effects.
  //    However, all local orderings are consistent with a LinearLog.


  public class Log {
    var lin : LinearLog = FunSeq.empty<LogEvent>();
    var byKey : ByKey = FunMap.empty<Seq<LogEvent>>();

    public func write(k:Key, v:Val) : Timestamp {
      P.nyi();
    };

    public func read(k:Key) {
      P.nyi();
    };

    // attempt to undo ts; if not already in a revoked status, generates new timestamp.
    public func undo(ts:Timestamp) : ?Timestamp {
      P.nyi();
    };

    // attempt to redo ts; if not already in an invoked status, generates new timestamp.
    public func redo(ts:Timestamp) : ?Timestamp {
      P.nyi();
    };

  };

}
