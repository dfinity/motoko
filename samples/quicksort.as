/* a *generic* quicksort class
   - inplace sorting of mutable arrays of T, for any type T
*/

type Array<T> = var T[];

class QS<T>(cmp : (T, T) -> Int) {
  quicksort(a : Array<T>, lo : Nat, hi : Nat) {
   	if (lo < hi) {
      let p = partition(a, lo, hi);
	    quicksort(a, lo, p);
	    quicksort(a, p + 1, hi); 
	  }
  };

  private swap(a : Array<T>, i : Nat, j : Nat) {
    let temp = a[i];
    a[i] := a[j];
    a[j] := temp;
  };

  private trace<T>(v : T) {};
   
  private partition(a : Array<T>, lo : Nat, hi : Nat) : Nat {
    trace<Array<T>>(a);
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
let a : Array<Int> = [8, 3, 9, 5, 2];
qs.quicksort(a, 0, 4);
