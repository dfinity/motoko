let a = future {
   let (a,b) = ("a1","b1"); 
   print a;
   print b;
};

let b = future {
   let (a,b) = await (future ("a2","b2"));
   print a;
   print b;
};

let c = future {
   func f(a:Text,b:Text):(){ print a; print b;};
   let (a,b) = await (future ("a3","b3"));
   let _ = f(a,b);
};

let d = future {
   var f = 1;
   printNat (f);
   let (a,b) = await (future ("a4","b4"));
   f += 2;
   printNat (f);
};


let e = future {
   var f = await (future 5);
   printNat (f);
   let (a,b) = await (future ("a5","b5"));
   f += 1;
   printNat (f);
};

