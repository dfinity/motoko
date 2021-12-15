import Sequence "mo:sequence/Sequence";
import Iter "mo:base/Iter";

module {
  public class Seq<X>(equal : (X, X) -> Bool, initial : ?Sequence.Sequence<X>)
  {
    var seq = switch initial {
      case null { Sequence.empty<X>() };
      case (?s) { s };
    };
    var _append = Sequence.defaultAppend<X>();

    public func sequence() : Sequence.Sequence<X> {
      seq
    };

    public func clone() : Seq<X> {
      Seq<X>(equal, ?seq) // O(1), by sharing immutable rep of seq.
    };

    public func append(t : Seq<X>) {
      seq := _append(seq, t.sequence())
    };

    public func prepend(t : Seq<X>) {
      seq := _append(t.sequence(), seq)
    };

    public func add(x : X) {
      seq := _append(seq, Sequence.make(x))
    };

    public func vals() : Iter.Iter<X> {
      Sequence.iter(seq, #fwd)
    };

    public func revVals() : Iter.Iter<X> {
      Sequence.iter(seq, #bwd)
    };

    public func getLast() : ?X {
      do ? {
        let (_, x) = Sequence.popBack(seq)!;
        x
      }
    };
  };
}
