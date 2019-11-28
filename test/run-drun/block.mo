actor a {
  public func go() = ignore {
    let a = async {
       let (a,b) = ("a1","b1"); 
       debugPrint a;
       debugPrint b;
    };

    let b = async {
       let (a,b) = await (async ("a2","b2"));
       debugPrint a;
       debugPrint b;
    };

    let c = async {
       func f(a:Text,b:Text):(){ debugPrint a; debugPrint b;};
       let (a,b) = await (async ("a3","b3"));
       let _ = f(a,b);
    };

    let d = async {
       var f = 1;
       debugPrintNat (f);
       let (a,b) = await (async ("a4","b4"));
       f += 2;
       debugPrintNat (f);
    };


    let e = async {
       var f = await (async 5);
       debugPrintNat (f);
       let (a,b) = await (async ("a5","b5"));
       f += 1;
       debugPrintNat (f);
    };
  }
};
a.go(); //OR-CALL ingress go "DIDL\x00\x00"
