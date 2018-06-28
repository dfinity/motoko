actor class Control() {

  Test():Bool = false;

  While() : () {

     var x : Int = 1;
     label l 
     while(Test(x))
     {
       if true then break l ()
       else continue l;
     };
  };

  Loop() : () {
     var x : Int = 1;
     label l 
     loop
     {
       if true then break l ()
       else continue l;
     };
  };
  
  LoopWhile() : () {
     var x : Int = 1;
     label l 
     loop
     {
       if true then break l ()
       else continue l;
     } while (true);
  };

  NestedWhile() : () {
     var x : Int = 1;
     label l
     while(Test())
     {
       if true then break l ()
       else continue l;
       label m
       while(Test()) {
          if true
	  then continue l
	  else break m ();
       };
     };
  };

  NestedWhile() : () {
     var x : Int = 1;
     label l
     while(Test())
     {
       if true then break l ()
       else continue l;
       label m
       while(Test()) {
          if true
	  then continue l
	  else break m ();
       };
     };
  };

};



