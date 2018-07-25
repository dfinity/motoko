actor class Control() {

  private Test():Bool = false;   

  Block() : () {
    label l1 {
      break l1
    };
    label l2 break l2();
    label l2 (let m = 1 + (break l2) : Int);
    let n = (label l3 (break l3(2))) : Int;
    let (x, y, z) = (label l3 (break l3(2, true, ""))) : (Int, Bool, Text);
  };

  While() : () {
     label l while(Test()) {
       if true break l
       else continue l;
     };
  };

  Loop() : () {
     label l loop {
       if true break l
       else continue l;
     };
  };
  
  LoopWhile() : () {
     label l loop {
       if true break l
       else continue l;
     } while (Test());
  };

  NestedWhile() : () {
     label l while (Test()) {
       if true break l ()
       else continue l;
       label m while(Test()) {
          if true continue l
          else break m ();
       };
     };
  };
};



