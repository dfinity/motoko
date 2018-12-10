class QS<T>(cmp : (T, T) -> Int) {
  quicksort(a : var T[], lo : Nat, hi : Nat) {
   	if (lo < hi) {
      let p = partition(a, lo, hi);
	    quicksort(a, lo, p);
	    quicksort(a, p + 1, hi); 
	  }
  };

  private swap(a : var T[], i : Nat, j : Nat) {
    let temp = a[i];
    a[i] := a[j];
    a[j] := temp;
  };

  private trace<T>(v : T) {};
   
  private partition(a : var T[], lo : Nat, hi : Nat) : Nat {
    trace<var T[]>(a);
    let pivot = a[lo];
    var i = lo;
    var j = hi;

    loop {
      while (cmp(a[i], pivot) < 0) {
        i += 1;
      };

      while (cmp(a[j], pivot) > 0) {
        j -= 1;
      };

      if (i >= j) return j;

      swap(a, i, j);
    };
  };
};

func cmpi(i : Int, j : Int) : Int = i - j;

let qs = QS<Int>(cmpi);

let a : var Int[] = [var 8, 3, 9, 5, 2];

qs.quicksort(a, 0, 4);

assert(a[0] == 2);
assert(a[1] == 3);
assert(a[2] == 5);
assert(a[3] == 8);
assert(a[4] == 9);
