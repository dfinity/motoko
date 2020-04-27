import P = "mo:prim";

object Random {
  var state = 1;
  public func next() : Bool {
    state := (123138118391*state + 133489131) % 9999;
    (state % 2 == 0)
  };
};

type Cell = Bool;

class Grid(state : [[Cell]]) {

  let n = state.len();

  public func size() : Nat { n };

  let grid = P.Array_tabulate(n, func (i : Nat) : [var Cell] {
      let a = P.Array_init(n, false);
      let si = state[i];
      assert (si.len() == n);
      for (j in si.keys()) {
        a[j] := si[j];
      };
      a
    });

  public func get(i : Nat, j : Nat) : Cell { grid[i][j] };

  public func set(i : Nat, j : Nat, v : Cell) { grid[i][j] := v };

  func pred(i : Nat) : Nat { (n + i - 1) % n };
  func succ(i : Nat) : Nat { (i + 1) % n };
  func count(i : Nat, j : Nat) : Nat { if (grid[i][j]) 1 else 0 };
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

  public func toState() : [[Cell]] {
    P.Array_tabulate<[Cell]>(n,
      func i { P.Array_tabulate<Cell>(n, func j { get(i,j) }) });
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
    stable var state : [[Cell]] =
      P.Array_tabulate<[Cell]>(n, func i { P.Array_tabulate<Cell>(n, func j { Random.next(); }) });

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
