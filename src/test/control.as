actor class Control() {

  Test():Bool = false;

  While() : () {
     var x : Int = 1;
     do l 
     while(Test(x))
     {
       if true then break l 
       else continue l;
     };
  };

  Loop() : () {
     var x : Int = 1;
     do l 
     loop
     {
       if true then break l ()
       else continue l;
     };
  };
  
  LoopWhile() : () {
     var x : Int = 1;
     do l 
     loop
     {
       if true then break l
       else continue l;
     } while (Test());
  };

  NestedWhile() : () {
     var x : Int = 1;
     do l
     while(Test())
     {
       if true then break l
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



