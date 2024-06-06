import Prim "mo:⛔";

module {
  public func init<X>(size : Nat, initValue : X) : [var X] = Prim.Array_init<X>(size, initValue);
}
