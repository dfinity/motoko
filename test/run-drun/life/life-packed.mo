import P = "mo:prim";

object Random {
  var state = 1;
  public func next() : Bool {
    state := (123138118391*state + 133489131) % 9999;
    (state % 2 == 0)
  };
};

type Cell = Bool;

type State = { 
  #unpacked : [[Bool]];
  #packed : { size: Nat; bits : [Nat64] }
};


class Grid(state : State) {
  
  let (n:Nat, bits:[var Word64]) = 
    switch state {
      case (#unpacked css) {
        let n = css.len();
        let len = ((n * n) % 64) + 1;
        let bits = P.Array_init<Word64>(len, 0);
        var i = 0;
        while (i < n) {
         var j = 0;
         while (j < n) {
          let bit = P.natToWord64((i*n)+j);
          let mask : Word64 = 1 << (bit % 64);
          bits[P.word64ToNat(bit >> 64:Word64)] |= mask;
          j += 1;
         };
         i += 1;
        };
        (n,bits)
       };
      case (#packed {size;bits}) {
        let ws = P.Array_init<Word64>(bits.len(), 0);
        for (n in bits.keys()) {
          ws[n] := P.nat64ToWord64(bits[n]);
        };
        (size, ws)
        }
    };
  
  public func size() : Nat { n };

  public func get(i:Nat, j:Nat) : Cell { 
    let bit = P.natToWord64((i*n)+j);
    let mask : Word64 = 1 << (bit % 64);
    (bits[P.word64ToNat(bit >> 64:Word64)] & mask) == mask 
  };

  public func set(i:Nat, j:Nat, v : Cell) { 
    let bit = P.natToWord64((i*n)+j);
    let mask : Word64 = 1 << (bit % 64);
    bits[P.word64ToNat(bit >> 64:Word64)] |= mask 
  };

  func pred(i : Nat) : Nat = (n + i - 1) % n;
  func succ(i : Nat) : Nat = (i + 1) % n;
  func count(i:Nat, j:Nat) : Nat { if (get(i,j)) 1 else 0 };
  func living(i : Nat, j : Nat) : Nat {
      count(pred i, pred j) + count(pred i, j) + count(pred i, succ j) +
      count(     i, pred j)                    + count(     i, succ j) +
      count(succ i, pred j) + count(succ i, j) + count(succ i, succ j)
  };
  func nextCell(i:Nat, j:Nat) : Cell {
    let l : Nat = living(i,j);
    if (get(i,j))
      l == 2 or l == 3
    else
      l == 3;
  };

  public func Next(dst : Grid) {
    var i = 0;
    while (i < n) {
      var j = 0;
      while (j < n) {
        dst.set(i, j, nextCell(i, j));
        j += 1;
        };
      i += 1;
    };
  };

  public func toState() : State {
    let ws = bits;
    #packed {
      size = n; 
      bits = P.Array_tabulate<Nat64>(ws.len(), func i 
        { P.word64ToNat64(ws[i])})
    }
  };

  public func toText() : Text {
    var t = "\n";
    var i = 0;
    while (i < n) {
      var j = 0;
      while (j < n) {
        t #= if (get(i,j)) "O" else " ";
        j += 1;
      };
      t #= "\n";
      i += 1;
    };
    t
  };
};

actor Life {
    
    stable var n = 32;
    stable var state : State =
      #unpacked (P.Array_tabulate<[Cell]>(n, func i 
         { P.Array_tabulate<Cell>(n, func j { Random.next(); }) }));
    
    flexible var src = Grid(state);
    flexible var dst = Grid(state);

    flexible func update(c : Nat) {
      var i = c;
      while (i > 0) {
        src.Next(dst);
        let temp = src;
        src := dst;
        dst := temp;
        //P.debugPrint(src.toText());
        i -= 1;
      };
    };

    system flexible func preupgrade() {
      state := src.toState();
    };

    system flexible func postupgrade() {
      P.debugPrint("upgraded!");
    };

    public func advance(n : Nat) : async () {
       update n;
    };

    public query func show() : async () {
       P.debugPrint(src.toText());
    };

};
