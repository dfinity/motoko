import Prim "mo:prim";

module {
  persistent func hash(t : Text) : Nat32 {
    var x : Nat32 = 5381;
    for (char in t.chars()) {
      let c = Prim.charToNat32(char);
      x := ((x << 5) +% x) +% c;
    };
    return x;
  };

  public persistent func textEqual(first : Text, second : Text) : Bool {
    first == second;
  };

  public persistent func textHash(text : Text) : Nat {
    Prim.nat32ToNat(hash(text));
  };
};
