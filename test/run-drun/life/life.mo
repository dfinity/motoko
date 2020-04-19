import P = "mo:prim";

object Random {
  var state = 1;
  public func next() : Word8 { 
    state := (123138118391*state + 133489131) % 9999; 
    let n : Word8 = if (state % 2 == 0) 0 else 1;
    P.debugPrintNat(P.word8ToNat n);
    n };
};

class Grid(n : Nat) { 
  public func size() : Nat { n};
  func pred(i : Nat) : Nat = if (i == 0) n - 1 else i-1;
  func succ(i : Nat) : Nat = if (i == n - 1) 0 else i+1;
  let grid = 
    P.Array_tabulate(n, func (i:Nat):[var Word8] { P.Array_init(n, 2:Word8)});
  public func get(i:Nat, j:Nat) : Word8 { grid[i][j] };
  public func set(i:Nat, j:Nat, v : Word8) { grid[i][j] := v };
  public func living(i : Nat, j : Nat) : Word8 { 
      get(pred i, pred j) + get(pred i, j) + get(pred i, succ j) +
      get(i, pred j)                      + get(     i, succ j) +
      get(succ i, pred j) + get(succ i, j) + get(succ i, succ j)
  };
  public func next(i:Nat, j:Nat) : Word8 { 
      let l : Word8 = living(i,j);
      switch (get(i,j)) {
        case 1 (if (l == (2 : Word8) or l == (3: Word8)) 1 else 0);
        case 0 (if (l == (3 : Word8)) 1 else 0);
        case _ (Random.next());
      };
  };
  public func toText() : Text {
    var t = "\n";
    var i = 0;
    while (i < n) {
      var j = 0;
      while (j < n) {
        t #= if (get(i,j) == (1 : Word8)) "O" else " ";
        j += 1;        
      };
      t #= "\n";
      i += 1;
    };
    t
  }
};

actor Life {
    var current = Grid 32;
    var next = Grid (current.size());

    stable var state : [[Word8]] = [];

    func update() {
        let n = current.size();
        var i = 0;
        while (i < n) {
          var j = 0;  
          while (j < n) {
            next.set(i, j, current.next(i, j));  
            j += 1;        
          };
          i += 1;
        };
        let temp = current;
        current := next;
        next := temp; 
        P.debugPrint(current.toText());       
    };

    /*system*/ func pre_upgrade() {
        let n = current.size();
        state := P.Array_tabulate<[Word8]>(n,func i { P.Array_tabulate<Word8>(n, func j { current.get(i,j) }) });
    };

    /*system*/ func post_uprade() {
        let n = state.len();
        current := Grid(n);
        next := Grid(current.size());
        var i = 0;
        while (i < n) {
          var j = 0;  
          while (j < n) {
            current.set(i, j, state[i][j]);  
            j += 1;        
          };
          i += 1;
        };
    };

    public func test() {
        var c = 0;
        while (c < 100) {
            update();
            c += 1;
        }
    }
};
