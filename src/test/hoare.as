
func swap(a : var Int[], i : Nat, j : Nat) {
  let temp = a[i];
  a[i] := a[j];
  a[j] := temp;
};

func partition(a : var Int[], lo : Nat, hi : Nat) : Nat {
  let pivot = a[lo];
  var i : Nat = lo;
  var j : Nat = hi;
  
  loop {
    while (a[i] < pivot) i += 1;
    while (a[j] > pivot) j -= 1;
    if (i >= j) return j;
    swap(a, i, j);
  };
};

func quicksort(a : var Int[], lo : Nat, hi : Nat) {
  if (lo < hi) {
    let p = partition(a, lo, hi);
	  quicksort(a, lo, p);
	  quicksort(a, p + 1, hi);  
	};
};

let a = [var 8, 3, 9, 5, 2];

let x = quicksort(a, 0, 4); 
