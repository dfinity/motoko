let a = async {
   let (a,b) = ("a1","b1"); 
   Debug.print a;
   Debug.print b;
};

let b = async {
   let (a,b) = await (async ("a2","b2"));
   Debug.print a;
   Debug.print b;
};

let c = async {
   func f(a:Text,b:Text):(){ Debug.print a; Debug.print b;};
   let (a,b) = await (async ("a3","b3"));
   let _ = f(a,b);
};

let d = async {
   var f = 1;
   Debug.printNat (f);
   let (a,b) = await (async ("a4","b4"));
   f += 2;
   Debug.printNat (f);
};


let e = async {
   var f = await (async 5);
   Debug.printNat (f);
   let (a,b) = await (async ("a5","b5"));
   f += 1;
   Debug.printNat (f);
};

