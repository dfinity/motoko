type array<t> = var t[];

class QS<T>(cmp : (T, T) -> Int) {
  quicksort(a : array<T>, lo : Int, hi : Int) {
   	if (lo < hi) {
      let p = partition(a, lo, hi);
	    quicksort(a, lo, p);
	    quicksort(a, p + 1, hi); 
	  }
  };

  private swap(a : array<T>, i : Int, j : Int) {
    let temp = a[i];
    a[i] := a[j];
    a[j] := temp;
  };

  private trace<T>(v : T) {};
   
  private partition(a : array<T>, lo : Int, hi : Int) : Int {
    trace<array<T>>(a);
    let pivot = a[lo];
    var i : Int = lo - 1;
    var j : Int = hi + 1;
    loop {
      loop {
        i += 1;
      } while (cmp(a[i], pivot) < 0);

      loop {
        j -= 1;
      } while (cmp(a[j], pivot) > 0);

      if (i >= j) return j;

      swap(a, i, j);
    };
  };
};

func cmpi(i : Int, j : Int) : Int = i - j;

let qs = QS<Int>(cmpi);

let a = [var 8, 3, 9, 5, 2];

let _ = qs.quicksort(a, 0, 4);
