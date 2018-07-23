actor class Counter(i : Int) {
  private var c = i;    

  Dec() : () {
   Show(c);
   c -= 1;
  };

  Read() : async Int {
   c;
  };
};

func Show(c:Int):(){};

let c = Counter(10);

func Test(){
      var i:Int = 10;
      while (i  > 0)
      {
        c.Dec();
	i-=1;
      }
    };

let _ = Test();