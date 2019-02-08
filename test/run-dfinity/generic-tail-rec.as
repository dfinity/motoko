/* test generic self-tail-recursion */

let bound:Int = 100000;

{
func Loop(n:Int){
     if (n >= bound) {
          print "done 1\n";
	  return;
     };
     Loop(n+1);
     };
Loop(0);     
};


{
func Loop(n:Int){
     if (n >= bound) {
          print "done 2\n";
	  return;
     };
     return Loop(n+1);
     assert(false);
     };

Loop(0);     
};

{
func Loop<T>(n:Int){
     if (n >= bound) {
          print "done 3\n";
	  return;
     };
     Loop<T>(n+1);
     };	

Loop<Int>(0);
};

{
func Loop<T,U>(n:Int){
     if (n >= bound) {
          print "done 4\n";
	  return;
     };
     Loop<T,U>(n+1);
     };	

Loop<Int,Bool>(0);
};



