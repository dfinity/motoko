func Loop(n:Int){
     if (n > 100000) {
          print "done";
	  return;
     };
     printInt(n);
     Loop(n+1);
     };	

Loop(0);



      