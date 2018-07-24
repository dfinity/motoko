actor class Control() {

  private Test():Bool = false;   

  Block() : () {
    do l1 {
      break l1
    };
    do l2 break l2;
    do l2 (let m = 1 + break l2 : Int);
    let n = (do l3 (break l3(2))) : Int;
    let (x, y, z) = (do l3 (break l3(2, true, ""))) : (Int, Bool, Text);
  };

  While() : () {
     do l  
     while(Test()) {
       if true then break l()
       else continue l;
     };
  };

  Loop() : () {
     do l 
     loop
     {
       if true then break l()
       else continue l;
     };
  };
  
  LoopWhile() : () {
     do l 
     loop
     {
       if true then break l()
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



