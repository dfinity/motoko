actor class Control() {

  Test():Bool = false;   

  While() : () {
     do l  
     while(Test()) {
       if true then break l ()
       else continue l;
     };
  };

  Loop() : () {
     do l 
     loop
     {
       if true then break l ()
       else continue l;
     };
  };
  
  LoopWhile() : () {
     do l 
     loop
     {
       if true then break l ()
       else continue l;
     } while (Test());
  };

  NestedWhile() : () {
     do l
     while(Test())
     {
       if true then break l ()
       else continue l;
       do m
       while(Test()) {
          if true
	  then continue l
	  else break m ();
       };
     };
  };
};



