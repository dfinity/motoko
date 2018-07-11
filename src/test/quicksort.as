actor class QS<T>(cmp:(T,T)->Int)
{
   quicksort(a:var T[], lo:Int, hi:Int): () {
   	if (lo < hi) then
	{ let p = partition(a, lo, hi);
	  quicksort(a, lo, p);
	  quicksort(a, p + 1, hi); 
	}
	else {};
   };

   private swap(a:var T[], i:Int, j:Int) : () {
     let temp = a[i];
     a[i] := a[j];
     a[j] := temp;
   };

   private partition(a:var T[], lo:Int, hi:Int) : Int {
     let pivot = a[0];
     var i : Int = lo - 1;
     var j : Int = hi + 1;
     loop {

       loop {
          i += 1;
       } while (cmp(a[i], pivot) < 0);

       loop {
         j -= 1;
       } while (cmp(a[j], pivot) > 0);

       if (i >= j)
       then return j
       else {};

       swap(a,i,j);
     };
  };
};