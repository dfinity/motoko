
func swap(a:var Int[], i:Int, j:Int) : () {
     let temp = a[i];
     a[i] := a[j];
     a[j] := temp;
   };

func partition(a:var Int[], lo:Int, hi:Int) : Int {
     let pivot = a[lo];
     var i : Int = lo - 1;
     var j : Int = hi + 1;
  
     loop {
       loop {
          i += 1;
       } while (a[i] < pivot);
      
       loop {
         j -= 1;
       } while (a[j] > pivot);

       if (i >= j) return j;

       swap(a,i,j);
     };
  };

func quicksort(a:var Int[], lo:Int, hi:Int): () {
   	if (lo < hi)
	{ let p = partition(a, lo, hi);
	  quicksort(a, lo, p);
	  quicksort(a, p + 1, hi);  
	};
   };

let a = [var 8, 3, 9, 5, 2];

let x = quicksort(a,0,4); 
