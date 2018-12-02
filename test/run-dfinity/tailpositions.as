/* test tail-position calculation; failure would stack overflow in dvm*/

let bound:Int = 100000;

{
func Loop(n:Int){
     if (n >= bound) {
          print "done1";
          return;
     };
     Loop(n+1);
     };
Loop(0);     
};

{
func Loop(n:Int){
     if (n >= bound) {
          print "done2";
          return;
     };
     if (true)
        Loop(n+1)
     else
        Loop(n+1);
     };
Loop(0);     
};


{
func Loop(n:Int){
     if (n >= bound) {
          print "done3";
          return;
     };
     switch (n % 2) {
       case 0 Loop(n+1);
       case 1 Loop(n+1);
       case _ assert(false);
     };
};

Loop(0);     
};

{
func Loop(n:Int){
     if (n >= bound) {
          print "done4";
          return;
     };
     { let m = n;
       Loop(m +1);
     };
};
Loop(0);     
};


{
func Loop(n:Int){
     if (n >= bound) {
          print "done5";
          return;
     };
     let _ = (return Loop(n+1)) + 1;
};
Loop(0);     
};


{
func Loop(n:Int):Bool{
     if (n >= bound) {
          print "done6";
          return true;
     };
     true and Loop(n+1);
};
let true = Loop(0);     
};

{
func Loop(n:Int):Bool {
     if (n >= bound) {
          print "done7";
          return true;
     };
     false or Loop(n+1);
};
let true = Loop(0);     
};
