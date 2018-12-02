(* test generic self-tail-recursion *)

let bound:Int = 100000;

{
func Loop(n:Int){
     if (n >= bound) {
          print "done";
	  return;
     };
     Loop(n+1);
     };
Loop(0);     
};


{
func Loop(n:Int){
     if (n >= bound) {
          print "done";
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
          print "done";
	  return;
     };
     Loop<T>(n+1);
     };	

Loop<Int>(0);
};

{
func Loop<T,U>(n:Int){
     if (n >= bound) {
          print "done";
	  return;
     };
     Loop<T,U>(n+1);
     };	

Loop<Int,Bool>(0);
};



